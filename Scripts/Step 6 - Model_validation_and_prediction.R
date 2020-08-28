# In this script I hope to do some model validation work, going to use CPO to compare a subset of the models.
# Then something like a 5 fold cross validation for a few specific models of interest.
# So idea is to do model comparisons to see how well the model predicts out of sample data
# And to see if the underlying relationships change significantly when you do the model validation for different 
# subset of data.  I think a random removal and a spatial removal both make sense.  
# Step 1 is to calculated the CPO and PIT scores:
# 1: For models with same fixed effects but the different spatial fields
# 2: Then with the best spatial field chosen we do the same with a subset of fixed effects that seemed useful to see what the best model is there.

# After that we can play around with some 5 fold cross validation to see how well the models are doing when larger piecies of data are missing.


################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
#----
rm(list=ls())
#direct.fun <- "Y:/Offshore/Assessment/"
direct.fun <- "D:/Github/Offshore/Assessment_fns/DK/"; dir.ftmp <- direct.fun
#direct.proj <- "Y:/Projects/GB_time_area_closure_SPERA/"
direct.proj <- "d:/NAS/Projects/GB_time_area_closure_SPERA/"; dir.tmp <- direct.proj

library(INLA)
library(boot)
library(fields)
library(PBSmapping)
require(tidyverse)
require(reshape2)
require(GGally)
require(boot)
require(bbmle)
require(MASS)
require(leaps)
require(COUNT)
require(cowplot)
require(viridis)
require(maptools)
library(maps)
require(gridExtra)
library(sp)
library(rgeos)
library(splancs)
library(marmap)
library(dplyr)
library(spatstat)
library(gridBase)
library(animation)
library(sf)
library(raster)
library(rgdal)
library(maptools)
library(mapdata)
library(sdmTMB)
library(caret)
library(data.table)
library(units)
#library(SDMTools)
#devtools::install_github("pbs-assess/sdmTMB") # You may need to install the sdmTMB package from Sean Anderson
#inla.upgrade(testing = F)
# Set the number of threads INLA uses, this is done automatically and set the number of threads to number of logical procesors a computer has
# But this doesn't really seem to push the computer all that hard as a default...  Not clear if increasing this would be useful or not
# So the control option inside INLA for another way to tweak how many resources are dedicated to the model...
#inla.setOption( num.threads = 12) 
# Bring in the functions we'll need
source(paste(direct.fun,"Maps/pectinid_projector_sf.R",sep=""))
source(paste(direct.fun,"Maps/convert_coords.R",sep=""))
source(paste(direct.fun,"Maps/add_alpha_function.r",sep=""))
source(paste(direct.fun,"Maps/combo_shp.R",sep=""))
source(paste(direct.fun,"Maps/centre_of_gravity.R",sep=""))
# A function for calculating the cpo for a model...
fcpo <- function(m, id)
  -sum(log(m$cpo$cpo[id]), na.rm=TRUE) # Good to log this because of distributional reasons.
factor.2.number <- function(x) {as.numeric(levels(x))[x]}

# Here is the data we need, this comes from Step 3 INLA_mesh_for_gb_surveys_and_scallop_survey.R
load(paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))
load(paste0(direct.proj,"Data/INLA_meshes.RData"))
load(paste0(direct.proj,"Data/SST_and_Depth_covariates_and_boundary_for_prediction.RData"))
load(paste0(direct.proj,"Data/2017_2020_data/Survey_data_with_covars_2017_2020.RData"))

#load(paste0(direct.proj,"Results/INLA_st_3_output.RData"))
direct.proj <- dir.tmp 
direct.fun <- dir.ftmp 

# Now I need to stick my UTM coordinates into the data
dat.final$X <- loc.gf@coords[,1]
dat.final$Y <- loc.gf@coords[,2]

# Remove the nmfs fall data here as I won't run these additional diagonstics/validations on these data...
dat.val <- dat.final %>% dplyr::filter(survey != "nmfs-fall") 
# At this point I really start the analysis, and what I want to do is split up each survey, I might as well loop this, or at least allow for a loop
# so we can move through each survey easily, this will get complicated with a number of models for each survey, especially when I split out the time...
surveys <- unique(dat.val$survey)
num.surveys <- length(surveys)
# The  INLA parameters
sigma <- 0.5
s.alpha <- 0.05
r.alpha <- 0.05
# I need a loop for the number of "species" i'm looking at, Now all I care about are Cod and Yellowtail, everything 
# else is taken care of in Step 4.
species <- c("cod_PA","yt_PA") 
#species <- c("cod_PA_can","yt_PA_can") # These take the cod and YT data that are on the Canadian side of GB. 
num.species <- length(species)
##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########
##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########


##########################  Section 2     Run the CPO analyses   ##########################  Section 2     Run the CPO analyses ###########################
############  First up we run a model with the main covariates identified for each species for the different spatial fields 
############  To see who has the best CPO and PIT results, I run this just on the 2 surveys that are most relevant to the closures
############  The cod RV survey and the Yellowtail NMFS Spring surveys.

st.mods <- c(10,5,3)
num.fields <- length(st.mods)
# Need to initialize some objects for later
res <- NULL
#mod.diag <- NULL
mod.output <- NULL
rand.field <- NULL
mod.diagnostics <- NULL
dat.list <- NULL
w.index.list <- NULL
# Going to run the st models once for cod (RV) and once for YT (nmfs-spring)
for(st in 1:num.fields)
{
  for(s in 1:num.species)
  {
    if(species[s] == 'cod_PA') dat <- dat.val %>% dplyr::filter(survey == "RV")
    if(species[s] == 'yt_PA') dat <- dat.val %>% dplyr::filter(survey == "nmfs-spring")
    if(grepl("cod", species[s])) resp <- "cod_PA"
    if(grepl("yt", species[s])) resp <- "yt_PA"
    response <- which(names(dat) == resp)
    names(dat)[response] <- "response"
    # Lets log transform depth and chl_rg, then center depth, chl_rg and sst_avg, also make SEDNUM a factor
    dat$depth_log <- log(-dat$comldepth)
    dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
    dat$sst_avg_cen <- scale(dat$sst_avg)
    dat$chl_rg_log <- log(dat$chl_rg)
    dat$chl_rg_cen <- scale(dat$chl_rg_log)
    # There really isn't enough data for 2,5,6,8,9, so I am going to exclude those from the analysis for the moment.
    # The 5 groups represent around 5% of the total data (~450 samples between the 5 levels) .
    dat$SEDNUM[dat$SEDNUM %in% c(2,5,6,8,9)] <- NA
    dat$fSEDNUM <- as.factor(dat$SEDNUM)
    # I wan the year group to be a categorical variable.
    dat$years_10 <- as.factor(dat$years_10)
    dat$years_5 <- as.factor(dat$years_5)
    dat$years_3 <- as.factor(dat$years_3) # I won't run the 3 year era models unless the 5 years are better than the 10 years.
    # Get the location of our data...
    loc <- cbind(dat$X,dat$Y)
    #We also need to decide Observation Likelihood
    fam <- "binomial"
    # If looking at all the data use the gf mesh
    mesh <- mesh.gf
    range <- range.gf
    # The amount of data we have
    N = nrow(dat)
    # For both the beta and binomial families we'll need to determine the number of trials.
    Ntrials <- 1 # For each record there is only 1 trial.
    
    # For both our scenarios we are going to be using the logit model (note that this isn't scrictly necessary to write as the logit is the
    # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
    control.fam = list(control.link=list(model="logit"))
    # Now make the A matrix for the the model with the spatio-temporal random field we need a different A matrix.
    # First the 10 year era
    if(st.mods[st] == 10) eras <- as.numeric(dat$years_10)
    # Now the 3 and 5 year model
    if(st.mods[st] ==5) eras <- as.numeric(dat$years_5)
    if(st.mods[st] == 3) eras <- as.numeric(dat$years_3)
    
    era.names <- unique(eras)
    n.eras <- length(unique(eras))
    if(st.mods[st] != 3) A.era <- inla.spde.make.A(mesh, loc,repl = eras)
    if(st.mods[st] == 3) A.era <- inla.spde.make.A(mesh, loc,group = eras,n.groups =n.eras)


    range <- range.gf
    spde <- inla.spde2.pcmatern(mesh,    
                                prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
        # and now we define the spatio-temporal random field.  I want to use
    # group of the 3 year era so that I can make the temporal field be an AR1 process.
    if(st.mods[st] != 3) w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
    if(st.mods[st] == 3) w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.group = n.eras)
    # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
    # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
    # certainly isn't entirely clear to me!
    #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
    
    dat$depth_cen_g <- inla.group(dat$depth_cen,n=100)
    #dat$chl_rg_cen_g <- inla.group(dat$chl_rg_cen,n=100)
    dat$sst_avg_cen_g <- inla.group(dat$sst_avg_cen,n=100)
    options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
    X.matrix <- model.matrix(~ 0+ depth_cen_g +  sst_avg_cen_g + fSEDNUM, data = dat)
    # And then make a covariate matrix
    X <- data.frame(depth =        X.matrix[,1],
                    sst   =        X.matrix[,2],
                    #chl    =       X.matrix[,3],
                    fsed_3     =   X.matrix[,3],
                    fsed_4     =   X.matrix[,4])
    
    stk = inla.stack(tag="est",
                     data=list(y = dat$response, link=1L),
                     effects=list(intercept = rep(1, nrow(dat)), 
                                  X = X,
                                  w = w.index),
                     A = list(1,1,A.era))
    # This is neeeded for the cpo calcs...
    stack.id <- inla.stack.index(stk,'est')$data
    
    intercept <- 1 # intercept
    # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
    # overfit less, to do this I need to bin the covariates using the inla.group function above, problem is 
    # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
    # and he recommends these priors to make sure it doesn't get too funky
    U <- 0.5
    hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
    
    # For yellowtail toss we'll use these models for comparison
    if(species[s] == "yt_PA")
    {
      if(st.mods[st] == 3)         
      { 
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                     f(sst , model = "rw1", hyper = hyp.rw2)  + fsed_3 + fsed_4 +
                     f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
      } # end if(species[s] == "yt_PA")
      
      if(st.mods[st] != 3)         
      {
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                     f(sst , model = "rw1", hyper = hyp.rw2)  + fsed_3 + fsed_4 +
                     f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
      } # end if(st.mods[st] != 3)  
    } # end if(species[s] == "yt_PA")
    
    if(species[s] == "cod_PA")
    {
      
      if(st.mods[st] == 3)         
      { 
        model <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                  f(sst , model = "rw1", hyper = hyp.rw2)  + 
                  f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
        
      } # end if(st.mods[st] == 3)         
      
      if(st.mods[st] != 3)         
      {
        model <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)  + 
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
      } # end if(st.mods[st] != 3)   
    } # end if(species[s] == "yt_PA")
    
    
    
    run.name <- paste0(species[s],"_st_",st.mods[st])
    # Now we run the models
    r.out <- inla(model, family=fam, data = inla.stack.data(stk),
                  control.predictor=list(A=inla.stack.A(stk)),
                  #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                  #verbose=TRUE,
                  control.compute = list(dic=T,waic=T,cpo=T)) 
    # The fitted model, residuals and the covariates, both the standardized and on the original scale.
    mo.out <- data.frame(fitted = r.out$summary.fitted.values[1:N,"mean"] , # The expected values can be found with this
                         resid = dat$response - r.out$summary.fitted.values[1:N,"mean"],
                         response = dat$response,
                         dep = dat$depth_cen,
                         sst = dat$sst_avg_cen,
                         sed = dat$fSEDNUM,
                         chl = dat$chl_rg_cen,
                         depth = dat$comldepth,
                         sst_avg = dat$sst_avg,
                         SEDNUM = dat$SEDNUM,
                         chl_rg = dat$chl_rg,
                         years_10 = dat$years_10,
                         years_3 = dat$years_3,
                         years_5 = dat$years_5
    )
    
    #  a couple of other variables to calculated
    mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
    mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
    
    # Now the model fits using dic and waic, results very similar.
    md.out <- data.frame(dic = r.out$dic$dic, 
                         dic.p.eff = r.out$dic$p.eff,
                         waic = r.out$waic$waic, 
                         waic.p.eff = r.out$waic$p.eff,
                         Dispersion = sum(),
                         cpo = fcpo(r.out, stack.id)) 
    md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
    
    
    res[[run.name]] <- r.out
    #res[[mod.names[m]]]$model <- mod.names[m]
    mod.output[[run.name]] <- mo.out
    mod.output[[run.name]]$model <- run.name
    mod.output[[run.name]]$species <- species[s]
    #mod.output[[run.name]]$survey <- surveys[i]
    #mod.output[[run.name]]$model.id <- mod.names[m]
    mod.output[[run.name]]$st.era <- st.mods[st]
    
    mod.diagnostics[[run.name]] <- md.out
    mod.diagnostics[[run.name]]$model <- run.name
    mod.diagnostics[[run.name]]$species <- species[s]
    #mod.diagnostics[[run.name]]$survey <- surveys[i]
    #mod.diagnostics[[run.name]]$model.id <- mod.names[m]
    mod.diagnostics[[run.name]]$st.era <-  st.mods[st]
    
    rand.field[[run.name]] <- r.out$summary.random$w # THis will contain mutliple random fields for the spatio-temporal models.
    rand.field[[run.name]]$model <- run.name
    rand.field[[run.name]]$species <- species[s]
    #rand.field[[run.name]]$surveys <- surveys[i]
    #rand.field[[run.name]]$model.id <- mod.names[m]
    rand.field[[run.name]]$st.era <- st.mods[st]
    
    # Stick a print in here so we know this is moving forward
    print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
    # Write a message to the ESS so I can see progress remotely....
    #fileConn<-file(paste0(direct.proj,"Results/status.txt"))
    #writeLines(messy, fileConn)
    #close(fileConn)
    
  } # end for(j in 1:num.species)
} # end for(st in 1:num.fields)

# So from this all the diagonstics agree that that for cod it's the 5 year model and yellowtail is the 3 year model.  Would have been
# handy if they were all pointing to the 5 year model, but oh well...

# Some other things to look at with the Conditional Predictive Ordinates and Probability Integral Transform to see how the model is doing...
n.cod <- nrow(dat.val %>% dplyr::filter(survey == "RV"))
n.yt <- nrow(dat.val %>% dplyr::filter(survey == "nmfs-spring"))

# Extract the CPOS and PITS into something that is ggplot ready...
cpos <- NULL
for(i in 1:length(res)) 
{
  cpos[[i]] <- data.frame(cpo = res[[i]]$cpo$cpo,
                          pit = res[[i]]$cpo$pit, 
                          fitted = res[[i]]$summary.fitted.values$mean[1:length(res[[i]]$cpo$pit)],
                          pit.ordered = sort(res[[i]]$cpo$pit),
                          index = 1:length(res[[i]]$cpo$pit),
                          uniqunat = (1:length(res[[i]]$cpo$pit))/(length(res[[i]]$cpo$pit) +1),
                          species = mod.diagnostics[[i]]$species,
                          st = mod.diagnostics[[i]]$st.era)
}
cpo <- do.call("rbind",cpos)
#save.image(paste0(direct.proj,"Results/INLA_st_3_5_10_model_validation_cpo_output.RData"))
#load(paste0(direct.proj,"Results/INLA_st_3_5_10_model_validation_cpo_output.RData"))

# OK so with that done we focus on the 5 year models and look at a few of the close run models to see what's what...
# It is values approaching 0 that are bad, good to see most data is closer to 1, but there are some of these that are likely worth exploring further...
# I believe in these models this is bounded by 0 and 1 since we have a binomial response...
# The CPO gives a very similar result to doing a jackkife residual test, so think if this as doing a jackknife to look for particularly
# influential points... with things approaching 0 being important...  Not there is very little difference between the 3 era tests....
ggplot(cpo) + geom_text(aes(x=index,y=cpo,label=index),size=0.3) + facet_wrap(~species+st)

#The PIT is the probability of a new response less than the observed response using a model based on the rest of the data. 
# We'd expect the PIT values to be uniformly distributed if the model assumptions are correct.
# But I have a feeling this gets funny in a binomial given we have a lot of 0's...  
ggplot(cpo) + geom_text(aes(x=uniqunat,y=pit.ordered,label=index),size=0.3) + facet_wrap(~species+st)
# You can also compare the PIT again fitted values to look for weirdness, again being a binomial likely makes this immediately weird
# Similar to looking are binomial residual plots being pointless.  Don't think the PIT stuff is particularly useful for binomial models
# but I'm not 100% sure.
ggplot(cpo) + geom_text(aes(x=fitted,y=pit,label=index),size=0.3) + facet_wrap(~species+st)

# We can also plot the difference in the CPO indexs to compare models, for example Cod 5 vs 3 year fields...
cpo.mods <- c(10,5,3)
cod.diff <- cpo %>% dplyr::filter(species == "cod_PA" & st == cpo.mods[2]) %>% dplyr::select(cpo)-
            cpo %>% dplyr::filter(species == "cod_PA" & st == cpo.mods[3]) %>% dplyr::select(cpo)
cod.diff$model <- "Cod RV survey"
cod.diff$comp <- "5 year vs 3 year"
yt.diff <- cpo %>% dplyr::filter(species == "yt_PA" & st == cpo.mods[2]) %>% dplyr::select(cpo)-
           cpo %>% dplyr::filter(species == "yt_PA" & st == cpo.mods[3]) %>% dplyr::select(cpo)
yt.diff$model <- "YT NMFS survey"
yt.diff$comp <- "5 year vs 3 year"
# 10 to 3 difference
cod.diff.10.3 <- cpo %>% dplyr::filter(species == "cod_PA" & st == cpo.mods[1]) %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "cod_PA" & st == cpo.mods[3]) %>% dplyr::select(cpo)
cod.diff.10.3$model <- "Cod RV survey"
cod.diff.10.3$comp <- "10 year vs 3 year"
yt.diff.10.3 <- cpo %>% dplyr::filter(species == "yt_PA" & st == cpo.mods[1]) %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "yt_PA" & st == cpo.mods[3]) %>% dplyr::select(cpo)
yt.diff.10.3$model <- "YT NMFS survey"
yt.diff.10.3$comp <- "10 year vs 3 year"
# 10 to 5 difference
cod.diff.10.5 <- cpo %>% dplyr::filter(species == "cod_PA" & st == cpo.mods[1]) %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "cod_PA" & st == cpo.mods[2]) %>% dplyr::select(cpo)
cod.diff.10.5$model <- "Cod RV survey"
cod.diff.10.5$comp <- "10 year vs 5 year"
yt.diff.10.5 <- cpo %>% dplyr::filter(species == "yt_PA" & st == cpo.mods[1]) %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "yt_PA" & st == cpo.mods[2]) %>% dplyr::select(cpo)
yt.diff.10.5$model <- "YT NMFS survey"
yt.diff.10.5$comp <- "10 year vs 5 year"

cpo.comps <- rbind(cod.diff,cod.diff.10.3,cod.diff.10.5,
                   yt.diff,yt.diff.10.3,yt.diff.10.5)
mns <- cpo.comps %>% group_by(model,comp) %>% dplyr::summarise(mn = median(cpo))
cpo.comps.fields <- left_join(cpo.comps,mns,by = c('model','comp'))
#Postive values would suggest that the first model is better than the second you can see the difference is pretty small, but it is slightly in favour of 5 for
# cod, and yellowtail for 3 year

ggplot(cpo.comps) + geom_histogram(aes(cpo))  + geom_vline(aes(xintercept = mn)) + facet_wrap(~model + comp,nrow=3,dir='v') + 
                    geom_text(x = 0, y= 900,aes(label = paste("Median =", signif(mn,digits=2)))) + theme_bw() + ylim(c(0,900))
# We can see what the RMSE is for each of these models too...

mod.output.fields <- do.call('rbind',mod.output)
mod.rmse <- mod.output %>% group_by(model,species,st.era) %>% summarise(RMSE = RMSE(fitted,response), MAE = MAE(fitted,response))
mod.rmse

# Grab the info from the model diagnostics
mod.diag.fields <- do.call('rbind',mod.diagnostics)
mod.diag.fields <- left_join(mod.diag.fields,mod.rmse,by = c('model','species',"st.era"))
mod.diag.fields <- mod.diag.fields %>% group_by(species) %>% mutate(min_2 = (min(dic)+2),min_10 = (min(dic) + 10))
# Grab the random field too..
rand.field.fields <- do.call('rbind',rand.field)

ggplot(mod.diag.fields) + geom_point(aes(y =dic,x=as.factor(st.era))) + 
                   geom_hline(aes(yintercept = min_2), col = 'red',size=1.25) +
                   geom_hline(aes(yintercept = min_10), col = 'blue',size=1.25) + 
                   facet_wrap(~species,scales = 'free_y') + theme_bw()
#save(cpo.comps.fields,mod.diag.fields,mod.output.fields,rand.field.fields,file = paste0(direct.proj,"Results/INLA_st_3_5_10_model_diagnostics.RData"))


############################# 


############################# SECTION 3 ################################ SECTION 3###################################################################
############################# SECTION 3 ################################ SECTION 3###################################################################
############################# SECTION 3 ################################ SECTION 3###################################################################
# Here we run 3 different fixed effects models and see what the CPO's look like between them...
# So next up is getting CPO's for a series of different models, I think we run 4 different models here.  
# For the sake of the models ever finishing I do this validation on the 5 year stack, which is the best for cod, but the 
# second best for yellowtail.  Results show that WAIC/DIC/CPO are all giving the same best models which is the point for here.
# Run the intercept, model with the best covariates from WAIC/DIC, and a model with 2-3 factors in it to see how different they are.

res.fe <- NULL
#mod.diag <- NULL
mod.output.fe <- NULL
rand.field.fe <- NULL
mod.diagnostics.fe <- NULL
#dat.list.fe <- NULL
#w.index.list.fe <- NULL
# So we're going for 3 models at the moment, they will differ slightly for cod and yt... but both will have the intercept only model...
num.mods <- 4
for(mod in 1:num.mods)
{
  for(s in 1:num.species)
  {
    if(species[s] == 'cod_PA') dat <- dat.val %>% dplyr::filter(survey == "RV")
    if(species[s] == 'yt_PA') dat <- dat.val %>% dplyr::filter(survey == "nmfs-spring")
    if(grepl("cod", species[s])) resp <- "cod_PA"
    if(grepl("yt", species[s])) resp <- "yt_PA"
    response <- which(names(dat) == resp)
    names(dat)[response] <- "response"
    # Lets log transform depth and chl_rg, then center depth, chl_rg and sst_avg, also make SEDNUM a factor
    dat$depth_log <- log(-dat$comldepth)
    dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
    dat$sst_avg_cen <- scale(dat$sst_avg)
    dat$chl_rg_log <- log(dat$chl_rg)
    dat$chl_rg_cen <- scale(dat$chl_rg_log)
    # There really isn't enough data for 2,5,6,8,9, so I am going to exclude those from the analysis for the moment.
    # The 5 groups represent around 5% of the total data (~450 samples between the 5 levels) .
    dat$SEDNUM[dat$SEDNUM %in% c(2,5,6,8,9)] <- NA
    dat$fSEDNUM <- as.factor(dat$SEDNUM)
    # I wan the year group to be a categorical variable.
    dat$years_10 <- as.factor(dat$years_10)
    dat$years_5 <- as.factor(dat$years_5)
    dat$years_3 <- as.factor(dat$years_3) # I won't run the 3 year era models unless the 5 years are better than the 10 years.
    # Get the location of our data...
    loc <- cbind(dat$X,dat$Y)
    #We also need to decide Observation Likelihood
    fam <- "binomial"
    # If looking at all the data use the gf mesh
    mesh <- mesh.gf
    range <- range.gf
    # The amount of data we have
    N = nrow(dat)
    # For both the beta and binomial families we'll need to determine the number of trials.
    Ntrials <- 1 # For each record there is only 1 trial.
    
    # For both our scenarios we are going to be using the logit model (note that this isn't scrictly necessary to write as the logit is the
    # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
    control.fam = list(control.link=list(model="logit"))
    # Now make the A matrix for the the model with the spatio-temporal random field we need a different A matrix.
    # First the 10 year era
    #if(st.mods[st] == 10) eras <- as.numeric(dat$years_10)
    # Now we march onwards with the 5 year model
    eras <- as.numeric(dat$years_5)
    #if(st.mods[st] == 3) eras <- as.numeric(dat$years_3)
    
    era.names <- unique(eras)
    n.eras <- length(unique(eras))
    A.era <- inla.spde.make.A(mesh, loc,repl = eras)
    #if(st.mods[st] == 3) A.era <- inla.spde.make.A(mesh, loc,group = eras,n.groups =n.eras)
    
    
    
    spde <- inla.spde2.pcmatern(mesh,    
                                prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
    # and now we define the spatio-temporal random field.  I want to use
    # group of the 3 year era so that I can make the temporal field be an AR1 process.
    w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
    #if(st.mods[st] == 3) w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.group = n.eras)
    # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
    # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
    # certainly isn't entirely clear to me!
    #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
    
    dat$depth_cen_g <- inla.group(dat$depth_cen,n=100)
    dat$chl_rg_cen_g <- inla.group(dat$chl_rg_cen,n=100)
    dat$sst_avg_cen_g <- inla.group(dat$sst_avg_cen,n=100)
    options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
    X.matrix <- model.matrix(~ 0+ depth_cen_g +  sst_avg_cen_g + chl_rg_cen_g + fSEDNUM, data = dat)
    # And then make a covariate matrix
    X <- data.frame(depth =        X.matrix[,1],
                    sst   =        X.matrix[,2],
                    chl    =       X.matrix[,3],
                    fsed_3     =   X.matrix[,4],
                    fsed_4     =   X.matrix[,5])
    
    stk = inla.stack(tag="est",
                     data=list(y = dat$response, link=1L),
                     effects=list(intercept = rep(1, nrow(dat)), 
                                  X = X,
                                  w = w.index),
                     A = list(1,1,A.era))
    # This is neeeded for the cpo calcs...
    stack.id <- inla.stack.index(stk,'est')$data
    
    intercept <- 1 # intercept
    # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
    # overfit less, to do this I need to bin the covariates using the inla.group function above, problem is 
    # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
    # and he recommends these priors to make sure it doesn't get too funky
    U <- 0.5
    hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
    
    # Both cod and yt go with the mod 1 that is the intercept model 
    if(mod == 1)         
    { 
      model <- y ~ 0 + intercept + f f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X # The w.repl is found inside w.index.X
      mod.name <- 'intercept_cod'
      if(species[s] == "yt_PA") mod.name <- 'intercept_yt'
    } # end if(mod == 1)
    
    # For yellowtail toss we'll use these models for comparison
    if(species[s] == "yt_PA")
    {
      
      if(mod == 2)         
      {
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        mod.name <- "depth_yt"
      } # end  if(mod == 2) 
      
      if(mod == 3)         
      {
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)  +
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        mod.name <- "depth_sst_yt"
      } # end  if(mod == 3) 
      if(mod == 4)         
      {
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)  + fsed_3 + fsed_4 +
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        mod.name <- "depth_sst_sed_yt"
      } # end  if(mod == 4) 
      
    } # end if(species[s] == "yt_PA")
    
    if(species[s] == "cod_PA")
    {
      
      if(mod == 2)         
      { 
        model <-  y ~ 0 + intercept + 
          f(sst , model = "rw1", hyper = hyp.rw2)  + 
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
         mod.name <- "sst_cod"
      } # end if(st.mods[st] == 3)         
      
      if(mod == 3)         
      {
        model <-  y ~ 0 + intercept + f(chl , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)  + 
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        mod.name <- "sst_chl_cod"
      } # end if(st.mods[st] != 3)   
      
      if(mod == 4)         
      {
        model <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)  + 
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        mod.name <- "depth_sst_cod"
      } # end if(st.mods[st] != 4)  
    } # end if(species[s] == "yt_PA")
    
    
    
    run.name <- paste0(species[s],"_st_",mod.name)
    # Now we run the models
    r.out <- inla(model, family=fam, data = inla.stack.data(stk),
                  control.predictor=list(A=inla.stack.A(stk)),
                  #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                  #verbose=TRUE,
                  control.compute = list(dic=T,waic=T,cpo=T)) 
    # The fitted model, residuals and the covariates, both the standardized and on the original scale.
    mo.out <- data.frame(fitted = r.out$summary.fitted.values[1:N,"mean"] , # The expected values can be found with this
                         resid = dat$response - r.out$summary.fitted.values[1:N,"mean"],
                         response = dat$response,
                         dep = dat$depth_cen,
                         sst = dat$sst_avg_cen,
                         sed = dat$fSEDNUM,
                         chl = dat$chl_rg_cen,
                         depth = dat$comldepth,
                         sst_avg = dat$sst_avg,
                         SEDNUM = dat$SEDNUM,
                         chl_rg = dat$chl_rg,
                         years_10 = dat$years_10,
                         years_3 = dat$years_3,
                         years_5 = dat$years_5
    )
    
    #  a couple of other variables to calculated
    mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
    mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
    
    # Now the model fits using dic and waic, results very similar.
    md.out <- data.frame(dic = r.out$dic$dic, 
                         dic.p.eff = r.out$dic$p.eff,
                         waic = r.out$waic$waic, 
                         waic.p.eff = r.out$waic$p.eff,
                         Dispersion = sum(),
                         cpo = fcpo(r.out, stack.id)) 
    md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
    
    
    res.fe[[run.name]] <- r.out
    #res[[mod.names[m]]]$model <- mod.names[m]
    mod.output.fe[[run.name]] <- mo.out
    mod.output.fe[[run.name]]$model <- run.name
    mod.output.fe[[run.name]]$species <- species[s]
    #mod.output[[run.name]]$survey <- surveys[i]
    mod.output.fe[[run.name]]$model.id <- mod.name
    mod.output.fe[[run.name]]$st.era <- 5
    
    mod.diagnostics.fe[[run.name]] <- md.out
    mod.diagnostics.fe[[run.name]]$model <- run.name
    mod.diagnostics.fe[[run.name]]$species <- species[s]
    #mod.diagnostics[[run.name]]$survey <- surveys[i]
    mod.diagnostics.fe[[run.name]]$model.id <- mod.name
    mod.diagnostics.fe[[run.name]]$st.era <-  5
    
    rand.field.fe[[run.name]] <- r.out$summary.random$w # THis will contain mutliple random fields for the spatio-temporal models.
    rand.field.fe[[run.name]]$model <- run.name
    rand.field.fe[[run.name]]$species <- species[s]
    #rand.field[[run.name]]$surveys <- surveys[i]
    rand.field.fe[[run.name]]$model.id <- mod.name
    rand.field.fe[[run.name]]$st.era <- 5
    
    # Stick a print in here so we know this is moving forward
    print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
    # Write a message to the ESS so I can see progress remotely....
    #fileConn<-file(paste0(direct.proj,"Results/status.txt"))
    #writeLines(messy, fileConn)
    #close(fileConn)
    
  } # end for(j in 1:num.species)
} # end for(mod in 1:num.mods)

n.cod <- nrow(dat.val %>% dplyr::filter(survey == "RV"))
n.yt <- nrow(dat.val %>% dplyr::filter(survey == "nmfs-spring"))

# Extract the CPOS and PITS into something that is ggplot ready...
cpos <- NULL
for(i in 1:length(res.fe)) 
{
  cpos[[i]] <- data.frame(cpo = res.fe[[i]]$cpo$cpo,
                          pit = res.fe[[i]]$cpo$pit, 
                          fitted = res.fe[[i]]$summary.fitted.values$mean[1:length(res.fe[[i]]$cpo$pit)],
                          pit.ordered = sort(res.fe[[i]]$cpo$pit),
                          index = 1:length(res.fe[[i]]$cpo$pit),
                          uniqunat = (1:length(res.fe[[i]]$cpo$pit))/(length(res.fe[[i]]$cpo$pit) +1),
                          species = mod.diagnostics.fe[[i]]$species,
                          st = mod.diagnostics.fe[[i]]$st.era,
                          model = mod.diagnostics.fe[[i]]$model.id)
}
cpo <- do.call("rbind",cpos)


# OK so with that done we focus on the 5 year models and look at a few of the close run models to see what's what...
# It is values approaching 0 that are bad, good to see most data is closer to 1, but there are some of these that are likely worth exploring further...
# I believe in these models this is bounded by 0 and 1 since we have a binomial response...
# The CPO gives a very similar result to doing a jackkife residual test, so think if this as doing a jackknife to look for particularly
# influential points... with things approaching 0 being important...  Not there is very little difference between the 3 era tests....
ggplot(cpo) + geom_text(aes(x=index,y=cpo,label=index),size=0.3) + facet_wrap(~species+model)

#The PIT is the probability of a new response less than the observed response using a model based on the rest of the data. 
# We'd expect the PIT values to be uniformly distributed if the model assumptions are correct.
# But I have a feeling this gets funny in a binomial given we have a lot of 0's...  
ggplot(cpo) + geom_text(aes(x=uniqunat,y=pit.ordered,label=index),size=0.3) + facet_wrap(~species+model)
# You can also compare the PIT again fitted values to look for weirdness, again being a binomial likely makes this immediately weird
# Similar to looking are binomial residual plots being pointless.  Don't think the PIT stuff is particularly useful for binomial models
# but I'm not 100% sure.
ggplot(cpo) + geom_text(aes(x=fitted,y=pit,label=index),size=0.3) + facet_wrap(~species+st)

# We can also plot the difference in the CPO indexs to compare models, for example Cod 5 vs 3 year fields... 
# Positive means first model is better than second, these are almost always negative indicating the more complex models are better.
cod.int.sst <- cpo %>% dplyr::filter(species == "cod_PA" & model == "intercept_cod") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "cod_PA" & model == "sst_cod") %>% dplyr::select(cpo)
cod.int.sst$model <- "Int vs SST"
cod.int.sst$species <- "Cod"
cod.sst.sstchl <- cpo %>% dplyr::filter(species == "cod_PA" & model == "sst_cod") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "cod_PA" & model == "sst_chl_cod") %>% dplyr::select(cpo)
cod.sst.sstchl$model <- "SST vs SST+Chl"
cod.sst.sstchl$species <- "Cod"
cod.sst.sstdepth <- cpo %>% dplyr::filter(species == "cod_PA" & model == "sst_cod") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "cod_PA" & model == "depth_sst_cod") %>% dplyr::select(cpo)
cod.sst.sstdepth$model <- "SST vs SST+Dep"
cod.sst.sstdepth$species <- "Cod"
cod.sstchl.sstdepth <- cpo %>% dplyr::filter(species == "cod_PA" & model == "sst_chl_cod") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "cod_PA" & model == "depth_sst_cod") %>% dplyr::select(cpo)
cod.sstchl.sstdepth$model <- "SST+Chl vs SST+Dep"
cod.sstchl.sstdepth$species <- "Cod"
cod.cpo.comps <- rbind(cod.int.sst,cod.sst.sstchl,cod.sst.sstdepth,cod.sstchl.sstdepth)
  
# Only need 3 comparisons here because there are no models at the same level...
yt.int.depth <- cpo %>% dplyr::filter(species == "yt_PA" & model == "intercept_yt") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "yt_PA" & model == "depth_yt") %>% dplyr::select(cpo)
yt.int.depth$model <- "Int vs Dep"
yt.int.depth$species <- "Yellowtail"
yt.depth.depthsst <- cpo %>% dplyr::filter(species == "yt_PA" & model == "depth_yt") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "yt_PA" & model == "depth_sst_yt") %>% dplyr::select(cpo)
yt.depth.depthsst$model <- "Dep vs Dep+SST"
yt.depth.depthsst$species <- "Yellowtail"
yt.depth.depthsstsed <- cpo %>% dplyr::filter(species == "yt_PA" & model == "depth_yt") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "yt_PA" & model == "depth_sst_sed_yt") %>% dplyr::select(cpo)
yt.depth.depthsstsed$model <- "Dep vs Dep+SST+Sed"
yt.depth.depthsstsed$species <- "Yellowtail"
yt.depthsst.depthsstsed <- cpo %>% dplyr::filter(species == "yt_PA" & model == "depth_sst_yt") %>% dplyr::select(cpo)-
  cpo %>% dplyr::filter(species == "yt_PA" & model == "depth_sst_sed_yt") %>% dplyr::select(cpo)
yt.depthsst.depthsstsed$model <- "Dep+SST vs Dep+SST+Sed"
yt.depthsst.depthsstsed$species <- "Yellowtail"


yt.cpo.comps  <- rbind(yt.int.depth,yt.depth.depthsst,yt.depth.depthsstsed,yt.depthsst.depthsstsed)

cpo.comps.fixed <- rbind(cod.cpo.comps,yt.cpo.comps)

mns <- cpo.comps.fixed %>% group_by(model,species) %>% dplyr::summarise(mn = median(cpo))
cpo.comps.fixed <- left_join(cpo.comps.fixed,mns,by = c('model','species'))
#Postive values would suggest that the first model is better than the second you can see the difference is pretty small, but it is slightly in favour of 5 for
# cod, and yellowtail for 3 year

ggplot(cpo.comps.fixed) + geom_histogram(aes(cpo))  + geom_vline(aes(xintercept = mn)) + facet_wrap(~model + species,nrow=3,dir='v',scales = "free_y") + 
  geom_text(x = -0.3, y= 250,aes(label = paste("Median =", signif(mn,digits=2)))) + theme_bw() #+ ylim(c(0,900))
# We can see what the RMSE is for each of these models too...

mod.output.fixed <- do.call('rbind',mod.output.fe)
mod.rmse.fixed <- mod.output.fixed %>% group_by(model,species) %>% summarise(RMSE = RMSE(fitted,response), MAE = MAE(fitted,response))
mod.rmse.fixed

# Grab the info from the model diagnostics
mod.diag.fixed <- do.call('rbind',mod.diagnostics.fe)
mod.diag.fixed <- mod.diag.fixed[order(mod.diag.fixed$dic),]
mod.diag.fixed <- left_join(mod.diag.fixed,mod.rmse.fixed,by = c('model','species'))

mod.diag.fixed <- mod.diag.fixed %>% group_by(species) %>% mutate(min_2 = (min(dic)+2),min_10 = (min(dic) + 10))
# Grab the random field too..
rand.field.fixed <- do.call('rbind',rand.field.fe)


#save(cpo.comps.fixed,mod.diag.fixed,mod.output.fixed,rand.field.fixed,file = paste0(direct.proj,"Results/INLA_fixed_effect_model_diagnostics_field_5.RData"))
#load(paste0(direct.proj,"Results/INLA_fixed_effect_model_validation_cpo_output.RData"))

############################# 



############################# SECTION 4 ################################ SECTION 4###################################################################
############################# SECTION 4 ################################ SECTION 4###################################################################
############################# SECTION 4 ################################ SECTION 4###################################################################
############################# SECTION 4 ################################ SECTION 4###################################################################
# This is the hard one, what I'm gonna do here is some proper validation, likely do an X-fold cross valiation using
# by using a validation stack. These cross validations are comparing different fixed effects models, an intercept model, a simple
# 1 covariate model, and the 'best' model based on DIC,WAIC, and CPO (which is consistent across the board).
# For YT I decided to roll with the sst_depth model and not include the sediment because there are a fair number of places we don't have
# the sediment information we want so it's not going to perform all that well, a more apples to apples is comparing intercept, 1 term, and 2 term models
# for both cod and YT. In this validation we want to compare 3 fixed effects models for the optimaal random field.  Then we also want to
# have the second best spatial field with optimal fixed effects compared to best spatial field with same fixed effects.  
# I also want the same data in the folds for each model (i.e. I don't want to resample the folds for each model). 

# Need to initialize some objects for later
st.mods <- c(5,3)
num.fields <- length(st.mods)
num.mods <- 3 # The number of models I'm gonna run validation on

# I want to set the data up here so we can get the folds identified now
dat.cod <-  dat.val %>% dplyr::filter(survey == "RV")
dat.yt <- dat.val %>% dplyr::filter(survey == "nmfs-spring")
# Number of folds, going for 5 fold cross validation
num.folds <- 5
# Now this creates an ID vector (you can make it a list if you like) to subset the data...
set.seed(26) # Set the seed here so we can recreate both of these 
cod.folds <- createFolds(1:nrow(dat.cod), k = num.folds, list = F, returnTrain = F)
set.seed(26) # Set the seed here so we can recreate both of these 
yt.folds <- createFolds(1:nrow(dat.yt), k = num.folds, list = F, returnTrain = F)


#res.fd <- NULL
#mod.diag <- NULL
mod.output.fd <- NULL
mod.diagnostics.fd <- NULL
pred.res.fd <- NULL
for(s in 1:num.species)
{
  for(mod in 1:num.mods)
  {
    for(fd in 1:num.folds)
    {
      if(species[s] == 'cod_PA'){ dat <- dat.cod; fold <- cod.folds }
      if(species[s] == 'yt_PA') { dat <- dat.yt; fold <- yt.folds }
      if(grepl("cod", species[s])) resp <- "cod_PA"
      if(grepl("yt", species[s])) resp <- "yt_PA"
      response <- which(names(dat) == resp)
      names(dat)[response] <- "response"
      # Lets log transform depth and chl_rg, then center depth, chl_rg and sst_avg, also make SEDNUM a factor
      dat$depth_log <- log(-dat$comldepth)
      dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
      dat$sst_avg_cen <- scale(dat$sst_avg)
      dat$chl_rg_log <- log(dat$chl_rg)
      dat$chl_rg_cen <- scale(dat$chl_rg_log)
      # There really isn't enough data for 2,5,6,8,9, so I am going to exclude those from the analysis for the moment.
      # The 5 groups represent around 5% of the total data (~450 samples between the 5 levels) .
      dat$SEDNUM[dat$SEDNUM %in% c(2,5,6,8,9)] <- NA
      dat$fSEDNUM <- as.factor(dat$SEDNUM)
      # I wan the year group to be a categorical variable.
      dat$years_10 <- as.factor(dat$years_10)
      dat$years_5 <- as.factor(dat$years_5)
      dat$years_3 <- as.factor(dat$years_3) # I won't run the 3 year era models unless the 5 years are better than the 10 years.
      # Get the location of our data...
      loc <- cbind(dat$X,dat$Y)
      #We also need to decide Observation Likelihood
      fam <- "binomial"
      # If looking at all the data use the gf mesh
      mesh <- mesh.gf
      range <- range.gf
      # The amount of data we have
      N = nrow(dat)
      # For both the beta and binomial families we'll need to determine the number of trials.
      Ntrials <- 1 # For each record there is only 1 trial.
      
      # For both our scenarios we are going to be using the logit model (note that this isn't scrictly necessary to write as the logit is the
      # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
      control.fam = list(control.link=list(model="logit"))
      # Now the 3 and 5 year model
      eras <- as.numeric(dat$years_5)
      #if(species[s] == 'yt_PA') eras <- as.numeric(dat$years_3)
      
      era.names <- unique(eras)
      n.eras <- length(unique(eras))
      A.era <- inla.spde.make.A(mesh, loc,repl = eras)
      #if(species[s] == 'yt_PA') A.era <- inla.spde.make.A(mesh, loc,group = eras,n.groups =n.eras)
  
      spde <- inla.spde2.pcmatern(mesh,    
                                  prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                  prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
      # and now we define the spatio-temporal random field.  I want to use
      # group of the 3 year era so that I can make the temporal field be an AR1 process.
      w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
      #w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.group = n.eras)
      # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
      # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
      # certainly isn't entirely clear to me!
      #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
      
      dat$depth_cen_g <- inla.group(dat$depth_cen,n=100)
      #dat$chl_rg_cen_g <- inla.group(dat$chl_rg_cen,n=100)
      dat$sst_avg_cen_g <- inla.group(dat$sst_avg_cen,n=100)
      options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
      X.matrix <- model.matrix(~ 0+ depth_cen_g +  sst_avg_cen_g , data = dat)
      # And then make a covariate matrix
      X <- data.frame(depth =        X.matrix[,1],
                      sst   =        X.matrix[,2])
                      #chl    =       X.matrix[,3],
                      #fsed_3     =   X.matrix[,3],
                      #fsed_4     =   X.matrix[,4])
      
      # The new fangled stacks
      stk.e = inla.stack(tag="est",
                       data=list(y = dat$response[fold != fd], link=1L),
                       effects=list(intercept = rep(1, nrow(dat[fold != fd,])), 
                                    X = X[fold != fd,],
                                    w = w.index),
                       A = list(1,1,A.era[fold != fd,]))
      
      stk.v = inla.stack(tag="val",
                         data=list(y = dat$response[fold == fd], link=1L),
                         effects=list(intercept = rep(1, nrow(dat[fold == fd,])), 
                                      X = X[fold == fd,],
                                      w = w.index),
                         A = list(1,1,A.era[fold == fd,]))
      #### join data stacks and extracts the data index
      stk <- inla.stack(stk.e, stk.v)
      e.id <- inla.stack.index(stk, 'est')$data
      v.id <- inla.stack.index(stk, 'val')$data
   
      intercept <- 1 # intercept
      # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
      # overfit less, to do this I need to bin the covariates using the inla.group function above, problem is 
      # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
      # and he recommends these priors to make sure it doesn't get too funky
      U <- 0.5
      hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
      
      # Both cod and yt go with the mod 1 that is the intercept model 
      
      
      # For yellowtail toss we'll use these models for comparison
      if(species[s] == "yt_PA")
      {
        if(mod == 1)         
        { 
          model <- y ~ 0 + intercept   + 
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          mod.name <- 'intercept_yt'
        } # end if(mod == 1)
        if(mod == 2)         
        {
          model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)    + 
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          mod.name <- "depth_yt"
        } # end  if(mod == 2) 
        if(mod == 3)         
        {
          model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            f(sst , model = "rw1", hyper = hyp.rw2)  + 
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          mod.name <- "depth_sst_yt"
        } # end  if(mod == 4) 
        
      } # end if(species[s] == "yt_PA")
      
      if(species[s] == "cod_PA")
      {
        if(mod == 1)         
        { 
          model <- y ~ 0 + intercept +  f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X # The w.repl is found inside w.index.X
          mod.name <- 'intercept_cod'
        } # end if(mod == 1) 
        
        if(mod == 2)         
        { 
          model <-  y ~ 0 + intercept + 
            f(sst , model = "rw1", hyper = hyp.rw2)  + 
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          mod.name <- "sst_cod"
        } # end if(st.mods[st] == 3)         
  
        if(mod == 3)         
        {
          model <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            f(sst , model = "rw1", hyper = hyp.rw2)  + 
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          mod.name <- "depth_sst_cod"
        } # end if(st.mods[st] != 4)  
      } # end if(species[s] == "yt_PA")
      
      
      
      run.name <- paste0(species[s],"_model_",mod.name,"_fold_",fd)
      # Now we run the models
      print(paste("Model run started at ",Sys.time()))
      r.out <- inla(model, family=fam, data = inla.stack.data(stk),
                    control.predictor=list(A=inla.stack.A(stk)),
                    #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                    #verbose=TRUE,
                    control.compute = list(dic=T,waic=T,cpo=T)) 
      print(paste("Model run completed at ",Sys.time()))
      # The fitted model, residuals and the covariates, both the standardized and on the original scale.
      mo.out <- data.frame(fitted = r.out$summary.fitted.values[e.id,"mean"] , # The expected values can be found with this
                           resid = dat$response[fold != fd] - r.out$summary.fitted.values[e.id,"mean"],
                           response = dat$response[fold != fd],
                           dep = dat$depth_cen[fold != fd],
                           sst = dat$sst_avg_cen[fold != fd],
                           sed = dat$fSEDNUM[fold != fd],
                           chl = dat$chl_rg_cen[fold != fd],
                           depth = dat$comldepth[fold != fd],
                           sst_avg = dat$sst_avg[fold != fd],
                           SEDNUM = dat$SEDNUM[fold != fd],
                           chl_rg = dat$chl_rg[fold != fd],
                           years_10 = dat$years_10[fold != fd],
                           years_3 = dat$years_3[fold != fd],
                           years_5 = dat$years_5[fold != fd]
      )
      
      #  a couple of other variables to calculated
      mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
      mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
      
      p.out <- data.frame(pred = r.out$summary.fitted.values[v.id,"mean"] , # The expected values can be found with this, note these need to be transformed...
                          pred.err = dat$response[fold == fd] - r.out$summary.fitted.values[v.id,"mean"],
                          response = dat$response[fold == fd],
                          dep = dat$depth_cen[fold == fd],
                          sst = dat$sst_avg_cen[fold == fd],
                          sed = dat$fSEDNUM[fold == fd],
                          chl = dat$chl_rg_cen[fold == fd],
                          depth = dat$comldepth[fold == fd],
                          sst_avg = dat$sst_avg[fold == fd],
                          SEDNUM = dat$SEDNUM[fold == fd],
                          chl_rg = dat$chl_rg[fold == fd],
                          years_10 = dat$years_10[fold == fd],
                          years_3 = dat$years_3[fold == fd],
                          years_5 = dat$years_5[fold == fd]
      )
      
      # Now the model fits using dic and waic, results very similar.
      md.out <- data.frame(dic = r.out$dic$dic, 
                           dic.p.eff = r.out$dic$p.eff,
                           waic = r.out$waic$waic, 
                           waic.p.eff = r.out$waic$p.eff,
                           Dispersion = sum(),
                           cpo = fcpo(r.out, e.id)) 
      md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
      
      
      # Really scaling down what we keep so the output object is manageable.  Key here is looking at predictions
      #res.fd[[run.name]] <- r.out
      #res[[mod.names[m]]]$model <- mod.names[m]
      mod.output.fd[[run.name]] <- mo.out
      mod.output.fd[[run.name]]$model <- run.name
      mod.output.fd[[run.name]]$model.id <- mod.name
      mod.output.fd[[run.name]]$species <- species[s]
      mod.output.fd[[run.name]]$fold <- fd
      
      mod.diagnostics.fd[[run.name]] <- md.out
      mod.diagnostics.fd[[run.name]]$model <- run.name
      mod.diagnostics.fd[[run.name]]$model.id <- mod.name
      mod.diagnostics.fd[[run.name]]$species <- species[s]
      mod.diagnostics.fd[[run.name]]$fold <- fd
      
      pred.res.fd[[run.name]] <- p.out
      pred.res.fd[[run.name]]$model <- run.name
      pred.res.fd[[run.name]]$model.id <- mod.name
      pred.res.fd[[run.name]]$species <- species[s]
      pred.res.fd[[run.name]]$fold <- fd
      
      # Stick a print in here so we know this is moving forward
      print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
      # Write a message to the ESS so I can see progress remotely....
      #fileConn<-file(paste0(direct.proj,"Results/status.txt"))
      #writeLines(messy, fileConn)
      #close(fileConn)
    } # end models
  } # end folds
} # end species

#############
# This is not related to the below, you'd run this if re-running the model...
pred.fd <- do.call('rbind',pred.res.fd)
mod.fd <- do.call('rbind',mod.output.fd)
mod.diag <- do.call("rbind",mod.diagnostics.fd)
mod.fd$fold <- as.factor(mod.fd$fold)
pred.fd$fold <- as.factor(pred.fd$fold)

#save.image(paste0(direct.proj,"Results/INLA_model_5_fold_cross_valiation.RData"))
#load(paste0(direct.proj,"Results/INLA_model_5_fold_cross_valiation.RData"))


# So I screwed up dat indexing when I ran these models the first time, to get everything square I need to do this for each fold...
# Now I need to repeat for the model data, roles eyes!!
# If I run the model again I wouldn't need to do any of this commented stuff.  I also have the model output saved so this is all taken care of
# But I keep it here for historical purposes...
# dat.yt <- dat.val %>% dplyr::filter(survey == "nmfs-spring")
# pred.fd <- NULL
# mod.fd <- NULL
# for(i in 1:length(pred.res.fd))
# {
# if(grepl('yt',names(pred.res.fd)[i])) t.dat <- dat.yt %>% filter(yt.folds == pred.res.fd[[i]]$fold[1])
# if(grepl('cod',names(pred.res.fd)[i])) t.dat <- dat.cod %>% filter(cod.folds == pred.res.fd[[i]]$fold[1])
# pred <- data.frame(t.dat,
#                    pred = logit(pred.res.fd[[i]]$pred),
#                    model = pred.res.fd[[i]]$model,
#                    model.id = pred.res.fd[[i]]$model.id,
#                    species = pred.res.fd[[i]]$species,
#                    fold = pred.res.fd[[i]]$fold)
# pred.fd[[names(pred.res.fd)[[i]]]] <- pred
# # Now for the model data...
# if(grepl('cod',names(pred.res.fd)[i])) m.dat <- dat.cod %>% filter(cod.folds != pred.res.fd[[i]]$fold[1])
# if(grepl('yt',names(pred.res.fd)[i])) m.dat <- dat.yt %>% filter(yt.folds != pred.res.fd[[i]]$fold[1])
# mod.d <- data.frame(m.dat,
#                             fitted = mod.output.fd[[i]]$fitted,
#                             model = mod.output.fd[[i]]$model,
#                             model.id = mod.output.fd[[i]]$model.id,
#                             species = mod.output.fd[[i]]$species,
#                             fold = mod.output.fd[[i]]$fold)
# mod.fd[[names(mod.output.fd)[i]]] <- mod.d
# }
# pred.res.fd <- pred.fd
# mod.output.fd <- mod.fd
#pred.fd <- do.call('rbind',pred.fd)
#mod.fd <- do.call('rbind',mod.fd)
# pred.fd$pred.err <- pred.fd$cod_PA - pred.fd$pred
# mod.fd$resid <- mod.fd$cod_PA - mod.fd$fitted
# mod.fd$response <- mod.fd$cod_PA
# pred.fd$response <- pred.fd$cod_PA




# So this tells me that there isn't much prediction bias in any of the models.  The standard deviation of the 
# predictions is higher than the standard deviation of the model itself which makes sense
# Compare the MSE for the validation data, again last model is best.
# The caret package has some really nice tools for this...
pred.error <- pred.fd %>% group_by(model,fold,model.id,species) %>% summarise(mn = mean(pred.err), sd = sd(pred.err),rmse = RMSE(pred,response), mae = MAE(pred,response))
pred.error$type <- 'Prediction'
# While if anything here it suggests that the model would tend to have a small biased towards not predicting scallop when there are there.
mod.resid <- mod.fd %>% group_by(model,fold,model.id,species) %>% summarise(mn = mean(resid),sd =sd(resid),rmse = RMSE(fitted,response), mae = MAE(fitted,response))
mod.resid$type <- 'Residuals'

fold.res <- dplyr::bind_rows(pred.error,mod.resid)
fold.res$model.id[grepl('intercept',fold.res$model.id)] <- 'Intercept'
fold.res$model.id[grepl('depth_sst',fold.res$model.id)] <- 'Depth + SST'
fold.res$model.id[grepl('^sst',fold.res$model.id)] <- 'SST'
fold.res$model.id[grepl('^depth',fold.res$model.id)] <- 'Depth'
fold.res$species <- as.factor(fold.res$species)
levels(fold.res$species) <- c("Cod","Yellowtail")

ggplot(fold.res) + geom_point(aes(y = mn, x= model.id,colour = type),position = position_dodge(width=0.2)) + 
                   facet_wrap(species~.,scales = 'free_x') + theme_bw() + scale_color_manual(values = c("blue","black"))

# So really what all this shows is the the predictions from the model are really good because the random field is so flexible it can 
# easily predict missing points within the structure.  So new thought for Section 5, what if we instead try and see
# how well the model predicts if we leave our a year (which is really what we are interested in).  So let's see how well the model
# predicts 2018 data when we leave it out.


#save(fold.res,file = paste0(direct.proj,"Results/INLA_5_fold_cross_valiation_pred_error_and_residual.RData"))


### SECTION 5 is kinda pointless now that I"ve done the predictions for 2017-2019 and they look solid..#######
############################# SECTION 5 ################################ SECTION 5  ###################################################################
############################# SECTION 5 ################################ SECTION 5  ###################################################################
############################# SECTION 5 ################################ SECTION 5  ###################################################################
# Here I'm going to see how well the model predicts 2016 data given the model with the rest of the years in there.  This is really 
# a good test of how well the model works in the context of what we are interested it.  The other stuff just shows the model is
# flexible enough with some missing data to still predict well....


# Need to initialize some objects for later
st.mods <- c(5,3)
num.fields <- length(st.mods)
num.mods <- 1 # The number of models I'm gonna run validation on

# I want to set the data up here so we can get the folds identified now
dat.cod <-  dat.val %>% dplyr::filter(survey == "RV")
dat.yt <- dat.val %>% dplyr::filter(survey == "nmfs-spring")
# Number of folds, going for 5 fold cross validation



#res.fd <- NULL
#mod.diag <- NULL
mod.output.st.yr <- NULL
mod.diagnostics.st.yr <- NULL
pred.res.st.yr <- NULL
for(s in 1:num.species)
{
  for(st in 1:num.fields)
  {
      if(species[s] == 'cod_PA'){ dat <- dat.cod }
      if(species[s] == 'yt_PA') { dat <- dat.yt }
      if(grepl("cod", species[s])) resp <- "cod_PA"
      if(grepl("yt", species[s])) resp <- "yt_PA"
      response <- which(names(dat) == resp)
      names(dat)[response] <- "response"
      # Lets log transform depth and chl_rg, then center depth, chl_rg and sst_avg, also make SEDNUM a factor
      dat$depth_log <- log(-dat$comldepth)
      dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
      dat$sst_avg_cen <- scale(dat$sst_avg)
      dat$chl_rg_log <- log(dat$chl_rg)
      dat$chl_rg_cen <- scale(dat$chl_rg_log)
      # There really isn't enough data for 2,5,6,8,9, so I am going to exclude those from the analysis for the moment.
      # The 5 groups represent around 5% of the total data (~450 samples between the 5 levels) .
      dat$SEDNUM[dat$SEDNUM %in% c(2,5,6,8,9)] <- NA
      dat$fSEDNUM <- as.factor(dat$SEDNUM)
      # I wan the year group to be a categorical variable.
      dat$years_10 <- as.factor(dat$years_10)
      dat$years_5 <- as.factor(dat$years_5)
      dat$years_3 <- as.factor(dat$years_3) # I won't run the 3 year era models unless the 5 years are better than the 10 years.
      # Get the location of our data...
      loc <- cbind(dat$X,dat$Y)
      #We also need to decide Observation Likelihood
      fam <- "binomial"
      # If looking at all the data use the gf mesh
      mesh <- mesh.gf
      range <- range.gf
      # The amount of data we have
      N = nrow(dat)
      # For both the beta and binomial families we'll need to determine the number of trials.
      Ntrials <- 1 # For each record there is only 1 trial.
      
      # For both our scenarios we are going to be using the logit model (note that this isn't strictly necessary to write as the logit is the
      # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
      control.fam = list(control.link=list(model="logit"))
      # Now the 3 and 5 year model
      if(st.mods[st] == 5) eras <- as.numeric(dat$years_5)
      if(st.mods[st] == 3) eras <- as.numeric(dat$years_3)
      
      era.names <- unique(eras)
      n.eras <- length(unique(eras))
      if(st.mods[st] == 5)A.era <- inla.spde.make.A(mesh, loc,repl = eras)
      if(st.mods[st] == 3) A.era <- inla.spde.make.A(mesh, loc,group = eras,n.groups =n.eras)
      
      spde <- inla.spde2.pcmatern(mesh,    
                                  prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                  prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
      # and now we define the spatio-temporal random field.  I want to use
      # group of the 3 year era so that I can make the temporal field be an AR1 process.
      if(st.mods[st] == 5) w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
      if(st.mods[st] == 3) w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.group = n.eras)
      # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
      # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
      # certainly isn't entirely clear to me!
      #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
      
      dat$depth_cen_g <- inla.group(dat$depth_cen,n=100)
      #dat$chl_rg_cen_g <- inla.group(dat$chl_rg_cen,n=100)
      dat$sst_avg_cen_g <- inla.group(dat$sst_avg_cen,n=100)
      options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
      X.matrix <- model.matrix(~ 0+ depth_cen_g +  sst_avg_cen_g + fSEDNUM, data = dat)
      # And then make a covariate matrix
      X <- data.frame(depth =        X.matrix[,1],
                      sst   =        X.matrix[,2],
                      #chl    =       X.matrix[,3],
                      fsed_3     =   X.matrix[,3],
                      fsed_4     =   X.matrix[,4])
      
      # The new fangled stacks
      stk.e = inla.stack(tag="est",
                         data=list(y = dat$response[dat$year !=2016], link=1L),
                         effects=list(intercept = rep(1, nrow(dat[dat$year !=2016,])), 
                                      X = X[which(dat$year !=2016),],
                                      w = w.index),
                         A = list(1,1,A.era[which(dat$year !=2016),]))
      
      stk.v = inla.stack(tag="val",
                         data=list(y = NA, link=1L),
                         effects=list(intercept = rep(1, nrow(dat[dat$year ==2016,])), 
                                      X = X[which(dat$year ==2016),],
                                      w = w.index),
                         A = list(1,1,A.era[which(dat$year ==2016),]))
      #### join data stacks and extracts the data index
      stk <- inla.stack(stk.e, stk.v)
      e.id <- inla.stack.index(stk, 'est')$data
      v.id <- inla.stack.index(stk, 'val')$data
      
      intercept <- 1 # intercept
      # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
      # overfit less, to do this I need to bin the covariates using the inla.group function above, problem is 
      # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
      # and he recommends these priors to make sure it doesn't get too funky
      U <- 0.5
      hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
      
      # Both cod and yt go with the mod 1 that is the intercept model 
      
      
      # For yellowtail toss we'll use these models for comparison
      if(species[s] == "yt_PA")
      {
        if(st.mods[st] == 3)
        {
          model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            f(sst , model = "rw1", hyper = hyp.rw2)   + 
            f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
          mod.name <- "depth_sst_yt"
        }
        
        if(st.mods[st] == 5)         
        {
          model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            f(sst , model = "rw1", hyper = hyp.rw2)  +
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          mod.name <- "depth_sst_yt"
        } # end  if(st.mods[st] == 3) 
        
      } # end if(species[s] == "yt_PA")
      
      if(species[s] == "cod_PA")
      {
        if(st.mods[st] == 5)
        {
          model <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            f(sst , model = "rw1", hyper = hyp.rw2)  + 
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          mod.name <- "depth_sst_cod"
        }
          if(st.mods[st] == 3)
          {
            model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
              f(sst , model = "rw1", hyper = hyp.rw2)   + 
              f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
            mod.name <- "depth_sst_cod"
          }
       }
        

      
      run.name <- paste0(species[s],"_model_",mod.name,"_missing_2016_field_",st.mods[st])
      # Now we run the models
      r.out <- inla(model, family=fam, data = inla.stack.data(stk),
                    control.predictor=list(A=inla.stack.A(stk)),
                    #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                    #verbose=TRUE,
                    control.compute = list(dic=T,waic=T,cpo=T)) 
      # The fitted model, residuals and the covariates, both the standardized and on the original scale.
      mo.out <- data.frame(fitted = r.out$summary.fitted.values[e.id,"mean"] , # The expected values can be found with this
                           resid = dat$response[dat$year !=2016] - r.out$summary.fitted.values[e.id,"mean"],
                           response = dat$response[dat$year !=2016],
                           dep = dat$depth_cen[dat$year !=2016],
                           sst = dat$sst_avg_cen[dat$year !=2016],
                           sed = dat$fSEDNUM[dat$year !=2016],
                           chl = dat$chl_rg_cen[dat$year !=2016],
                           depth = dat$comldepth[dat$year !=2016],
                           sst_avg = dat$sst_avg[dat$year !=2016],
                           SEDNUM = dat$SEDNUM[dat$year !=2016],
                           chl_rg = dat$chl_rg[dat$year !=2016],
                           years_10 = dat$years_10[dat$year !=2016],
                           years_3 = dat$years_3[dat$year !=2016],
                           years_5 = dat$years_5[dat$year !=2016],
                           X = dat$X[dat$year !=2016],
                           Y = dat$Y[dat$year !=2016]
      )
      
      #  a couple of other variables to calculated
      mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
      mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
      
      p.out <- data.frame(pred = r.out$summary.fitted.values[v.id,"mean"] , # The expected values can be found with this, note these need to be transformed...
                          pred.err = dat$response[dat$year ==2016] - r.out$summary.fitted.values[v.id,"mean"],
                          response = dat$response[dat$year ==2016],
                          dep = dat$depth_cen[dat$year ==2016],
                          sst = dat$sst_avg_cen[dat$year ==2016],
                          sed = dat$fSEDNUM[dat$year ==2016],
                          chl = dat$chl_rg_cen[dat$year ==2016],
                          depth = dat$comldepth[dat$year ==2016],
                          sst_avg = dat$sst_avg[dat$year ==2016],
                          SEDNUM = dat$SEDNUM[dat$year ==2016],
                          chl_rg = dat$chl_rg[dat$year ==2016],
                          years_10 = dat$years_10[dat$year ==2016],
                          years_3 = dat$years_3[dat$year ==2016],
                          years_5 = dat$years_5[dat$year ==2016],
                          X = dat$X[dat$year ==2016],
                          Y = dat$Y[dat$year ==2016]
      )
      
      # Now the model fits using dic and waic, results very similar.
      md.out <- data.frame(dic = r.out$dic$dic, 
                           dic.p.eff = r.out$dic$p.eff,
                           waic = r.out$waic$waic, 
                           waic.p.eff = r.out$waic$p.eff,
                           Dispersion = sum(),
                           cpo = fcpo(r.out, e.id)) 
      md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
      
      
      # Really scaling down what we keep so the output object is manageable.  Key here is looking at predictions
      #res.fd[[run.name]] <- r.out
      #res[[mod.names[m]]]$model <- mod.names[m]
      mod.output.st.yr[[run.name]] <- mo.out
      mod.output.st.yr[[run.name]]$model <- run.name
      mod.output.st.yr[[run.name]]$model.id <- mod.name
      mod.output.st.yr[[run.name]]$species <- species[s]
     # mod.output.st.yr[[run.name]]$fold <- fd
      mod.output.st.yr[[run.name]]$field <- st.mods[st]
      
      mod.diagnostics.st.yr[[run.name]] <- md.out
      mod.diagnostics.st.yr[[run.name]]$model <- run.name
      mod.diagnostics.st.yr[[run.name]]$model.id <- mod.name
      mod.diagnostics.st.yr[[run.name]]$species <- species[s]
      #mod.diagnostics.st.yr[[run.name]]$fold <- fd
      mod.diagnostics.st.yr[[run.name]]$field <- st.mods[st]
      
      pred.res.st.yr[[run.name]] <- p.out
      pred.res.st.yr[[run.name]]$model <- run.name
      pred.res.st.yr[[run.name]]$model.id <- mod.name
      pred.res.st.yr[[run.name]]$species <- species[s]
      #pred.res.st.yr[[run.name]]$fold <- fd
      pred.res.st.yr[[run.name]]$field <- st.mods[st]
      
      # Stick a print in here so we know this is moving forward
      print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
      # Write a message to the ESS so I can see progress remotely....
      #fileConn<-file(paste0(direct.proj,"Results/status.txt"))
      #writeLines(messy, fileConn)
      #close(fileConn)
    } # end models
} # end species


#save.image(paste0(direct.proj,"Results/INLA_model_predict_2016.RData"))


#load(paste0(direct.proj,"Results/INLA_model_predict_2016.RData"))
# Now we can see how well the field was estiamted in 2016 compared to reality!!

dat$response[dat$year ==2016]

# Get the X and Y coordinates on the cod models...
pred.res.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_5$X <- dat.cod$X[dat.cod$year ==2016]
pred.res.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_3$X <- dat.cod$X[dat.cod$year ==2016]
pred.res.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_5$Y <- dat.cod$Y[dat.cod$year ==2016]
pred.res.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_3$Y <- dat.cod$Y[dat.cod$year ==2016]
# Now the YT models....
pred.res.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_5$X <- dat.yt$X[dat.yt$year ==2016]
pred.res.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_3$X <- dat.yt$X[dat.yt$year ==2016]
pred.res.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_5$Y <- dat.yt$Y[dat.yt$year ==2016]
pred.res.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_3$Y <- dat.yt$Y[dat.yt$year ==2016]

# Now the same for the modeled data...
# Get the X and Y coordinates on the cod models...
mod.output.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_5$X <- dat.cod$X[dat.cod$year !=2016]
mod.output.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_3$X <- dat.cod$X[dat.cod$year !=2016]
mod.output.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_5$Y <- dat.cod$Y[dat.cod$year !=2016]
mod.output.st.yr$cod_PA_model_depth_sst_cod_missing_2016_field_3$Y <- dat.cod$Y[dat.cod$year !=2016]
# Now the YT models....
mod.output.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_5$X <- dat.yt$X[dat.yt$year !=2016]
mod.output.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_3$X <- dat.yt$X[dat.yt$year !=2016]
mod.output.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_5$Y <- dat.yt$Y[dat.yt$year !=2016]
mod.output.st.yr$yt_PA_model_depth_sst_yt_missing_2016_field_3$Y <- dat.yt$Y[dat.yt$year !=2016]


pred.res <- do.call('rbind',pred.res.st.yr)
mod.res <- do.call('rbind',mod.output.st.yr)
pred.res$pred <- inv.logit(pred.res$pred)
pred.res$pred.err <- pred.res$response - pred.res$pred 
sd.pred <- pred.res %>% group_by(model) %>% summarise(sd=sd(pred.err),rmse = RMSE(pred,response))
sd.mod <- mod.res %>% group_by(model) %>% summarise(sd=sd(resid),rmse = RMSE(fitted,response))

pred.res <- st_as_sf(pred.res,coords = c("X","Y"), crs= 32619,remove=F)
mod.res <- st_as_sf(mod.res,coords = c("X","Y"), crs= 32619,remove=F)

ggplot(pred.res) + geom_sf(aes(colour = pred.err)) + facet_wrap(~model) + scale_colour_viridis()
ggplot(pred.res) + geom_histogram(aes(x=pred.err)) + facet_wrap(~model)
head(pred.res)


######################

############################# SECTION 6 ################################ SECTION 6  ###################################################################
############################# SECTION 6 ################################ SECTION 6  ###################################################################
############################# SECTION 6 ################################ SECTION 6  ###################################################################
############################# SECTION 6 ################################ SECTION 6  ###################################################################
# This used to be final section is MODEL PREDICTIONS, which is pretty simple, we take the best Cod/YT models
# for each survey and and we add a prediction stack to them which will predict at the vertex of
# every mesh node using the SST and depth information along with the random field.
# Problem is this blows up the A matrix and really grinds down the computation so
# we need to really trim our prediction box down to the real core part of GBa even more
# than our nice 'clp' poly...
# Here is a nicely trimmed up polygon we can use to clip down our clipped area...
#loc.gf.sf <- st_as_sf(loc.gf)
clp.poly <- st_as_sf(data.frame(X = c(508000,508000,900000,650000,600000,550000),
                                Y=c(4540000,4350000,4674000,4674000,4661000,4622000),ID=1),coords = c("X","Y"),crs= 32619)
# Now make this a polygon
clp.poly <- st_cast(st_combine(clp.poly),"POLYGON")
clp.pred <- st_intersection(clp,clp.poly)
# How does this compare to the GB grid size, these are about 44km2 so about 6.5 by 6.5..
# GB.grid <- read.csv("D:/NAS/Projects/GB_time_area_closure_SPERA/Results/GB_grid.csv")
# GB.grid <- as.PolySet(GB.grid,projection = "LL")
# GB.grid <- st_as_sf(maptools::PolySet2SpatialPolygons(GB.grid),crs = "4326")
# GB.grid <- st_transform(GB.grid,crs=32619)
# st_area(GB.grid[20,])/1e6 # around 44 km^2 so almost 7 km ^2.  I'd like 
# Now I can make the prediction grid, the n is the number of cells in the x and y direction
# 42 is 1200 boxes and a 6 x 6 resolution. 51 is 1800 points and a 5 x 5 resolution 
#63 is 2700 points and a 4 x 4 resolution, 84 is 4700 points and a 3 x 3 resolution,
# 127 is 11000 points and a 2 x 2 resolution, and 
# 200 gives boxes that are around 2 km by 2 km
mesh.grid <- st_make_grid(clp.pred,n = 84) # So we'd get almost 5 cells inside every 1 of the closure grids, that's decent...
st_area(mesh.grid[1,])/1e6
#ggplot(mesh.grid)+ geom_sf()
# Now I just want to predict in the middle of the mesh grid..
mesh.points <- st_centroid(mesh.grid)
# Both the mesh grid and the mesh points are very useful objects I want, so let's save them out
#save(mesh.grid,mesh.points,file = "D:/NAS/Projects/GB_time_area_closure_SPERA/Results/Prediction_mesh.RData")

# A plot of all this fun stuff
ggplot(clp) + geom_sf() + 
  #geom_sf(data=loc.gf.sf)  + 
  #geom_sf(data = clp.poly,fill=NA,colour='blue') + 
  geom_sf(data = clp.pred,fill=NA,colour='green',size=3) + 
  #geom_sf(data=mesh.grid)+ 
  #geom_sf(data=GB.grid) +
  geom_sf(data=mesh.points,size=1) + 
  coord_sf(datum=st_crs(32619))


# Now all I have to do is intersect the clip polying with the sst and depth fields and we got ourselves our covariates
pred.covars <- st_intersection(sst.sf,mesh.points)
names(pred.covars)[1] <- "sst_avg"
pred.covars <- st_intersection(depth.sf,pred.covars)
names(pred.covars)[1] <- 'comldepth'
pred.covars$X <- st_coordinates(pred.covars)[,1]
pred.covars$Y <- st_coordinates(pred.covars)[,2]
st_geometry(pred.covars) <- NULL
# If I have any positive depths toss those...
pred.covars$comldepth[pred.covars$comldepth >0] <- 0
# Need to initialize some objects for later
st.mods <- c(5,3)
num.fields <- length(st.mods)

surveys <- unique(dat.final$survey)
num.surveys <- length(surveys)
# We are going to run the 5 year model for cod and the 3 year model for yellowtail
# This will have everything ever.  Note the best model is always the depth-sst additive model, so that's
# at least handy...
mesh <- mesh.gf
#res.fd <- NULL
#mod.diag <- NULL
mod.output.pred <- NULL
mod.diagnostics.pred <- NULL
res.pred <- NULL
rand.field.pred <- NULL
pred.output.pred <- NULL


  for(s in 1:num.species) 
  {
    # Loop through each of the surveys
    for(i in 1:num.surveys) 
    {
      # Now lets get our input data sorted
      # Let's select the data for the particular survey of interest
      dat <- dat.final[dat.final$survey == surveys[i],]
      # Rename the varialbe of interest to "response"
      if(species[s] == "cod_PA") resp <- "cod_PA"
      if(species[s] == "yt_PA") resp <- "yt_PA"
      response <- which(names(dat) == resp)
      names(dat)[response] <- "response"
      # Next I need to add the era information to the pred data, which means I need
      # a full copy of it for every random field in the model, gross, yes!
      # if(species[s] == "cod_PA") 
      # {
       eras.p <- unique(dat$years_5)
       num.eras.p <- length(eras.p)
       tmp <- NULL
       for(p in 1:num.eras.p) tmp[[p]] <- data.frame(pred.covars,years_5 = eras.p[p],year=NA)
       covar.pred <- do.call('rbind',tmp)
       covar.pred$response <- NA
       dat <- dat %>% dplyr::select(comldepth,sst_avg,X,Y,years_5,response,year)
       dat <- rbind(dat,covar.pred)
      # } # end if(species[s] == "cod_PA") 
      
      # if(species[s] == "yt_PA") 
      # {
      #   eras.p <- unique(dat$years_3)
      #   num.eras.p <- length(eras.p)
      #   tmp <- NULL
      #   for(p in 1:num.eras.p) tmp[[p]] <- data.frame(pred.covars,years_3 = p,years_5 = NA)
      #   covar.pred <- do.call('rbind',tmp)
      #   covar.pred$response <- NA
      #   dat <- dat %>% dplyr::select(comldepth,sst_avg,X,Y,years_5,years_3,response)
      #   dat <- rbind(dat,covar.pred)
      # } # end if(species[s] == "yt_PA") 

      # Lets log transform depth and chl_rg, then center depth, chl_rg and sst_avg, also make SEDNUM a factor
      dat$depth_log <- log(-dat$comldepth)
      dat$depth_cen <-  dat$depth_log - mean(dat$depth_log,na.rm=T) # Log transform should help with issues related to skew of the depth data.
      dat$sst_avg_cen <- scale(dat$sst_avg)

  
      # I wan the year group to be a categorical variable.
      #if(species[s] == 'cod_PA')
      dat$years_5 <- as.factor(dat$years_5)
      #if(species[s] == 'yt_PA') dat$years_3 <- as.factor(dat$years_3) # I won't run the 3 year era models unless the 5 years are better than the 10 years.
      
      # Get the location of our data...
      loc <- cbind(dat$X,dat$Y)
     
      #We also need to decide Observation Likelihood
      fam <- "binomial"
      
      # The amount of data we have
      N = nrow(dat)
      # For both the beta and binomial families we'll need to determine the number of trials.
      Ntrials <- 1 # For each record there is only 1 trial.
      
      # For both our scenarios we are going to be using the logit model (note that this isn't scrictly necessary to write as the logit is the
      # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
      control.fam = list(control.link=list(model="logit"))
      # Now make the A matrix for the the model with the spatio-temporal random field we need a different A matrix.
      # First the 10 year era
      # Now the 3 and 5 year model
      #if(species[s] == "cod_PA") 
      eras <- as.numeric(dat$years_5)
      #if(species[s] == "yt_PA") eras <- as.numeric(dat$years_3)
      
      era.names <- unique(eras)
      n.eras <- length(unique(eras))
      #if(species[s] == "cod_PA") 
      A.era <- inla.spde.make.A(mesh, loc,repl = eras)
      #if(species[s] == "yt_PA") A.era <- inla.spde.make.A(mesh, loc,group = eras,n.groups =n.eras)
      
      # While I have my range for my spatial priors I don't have my sigma or the probabilites for the prirors
      # The priors here can be pretty informative, the SPDE approximation improves if the corrleation length (i.e. roughly the range) of the process
      # is similar, but larger, than the maximum edge length ofthe mesh
      # so here we define our spde
      range <- range.gf
      spde <- inla.spde2.pcmatern(mesh,    
                                  prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                  prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
      
      
      # and now we define the spatio-temporal random field.  I want to use
      # group of the 3 year era so that I can make the temporal field be an AR1 process.
      #if(species[s] == "cod_PA") 
      w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
      #if(species[s] == "yt_PA") w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.group = n.eras)
      # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
      # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
      # certainly isn't entirely clear to me!
      #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
      
      
      # For all random walk models I use inla.group to group the data so that the minimum difference
      # between values is greater than the threshold for the random walk models, these groups are very
      # fine and really just bin things that are essentially the same/
      # First up we only need to do this with the variables that appear interesting from
      # the spatial model runs.  These likely differ by species so might need a YT and a Cod list
      # guess it depends if any of them come out from the modelling as significant?
      dat$depth_cen_g <- inla.group(dat$depth_cen,n=100)
      dat$sst_avg_cen_g <- inla.group(dat$sst_avg_cen,n=100)
      
      # First we need to make the model matrix, this needs to align with our formula below, this doesn't strictly need to be 
      # done unless we have a categorical covariate
      options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
      # The nice thing here is that we can make this a complex as we want and just run submodels from within this model
      # structure.
      # Now we need to make a model matrix for the covariates that come out as useful
      # First I want to convert SEDNUM to a factor for this analysis.
      X.matrix <- model.matrix(~ 0+ depth_cen_g +  sst_avg_cen_g , data = dat)
      # And then make a covariate matrix
      X <- data.frame(depth =        X.matrix[,1],
                      sst   =        X.matrix[,2])
      
      # Next up we make the stack...
      # I need a stack, probably should eventually split this into an estimation, validation and prediction stack, but for now
      # will stick with the one estimation stack....
      # Make the stack for the spatial models without spatio-temporal correlation
      stk.e = inla.stack(tag="est",
                       data=list(y = dat$response[!is.na(dat$response)], link=1L),
                       effects=list(intercept = rep(1, length(dat$response[!is.na(dat$response)])), 
                                    X = X[!is.na(dat$response),],
                                    w = w.index),
                       A = list(1,1,A.era[!is.na(dat$response),]))
      
      stk.p = inla.stack(tag="pred",
                       data=list(y = dat$response[is.na(dat$response)], link=1L),
                       effects=list(intercept = rep(1, length(dat$response[is.na(dat$response)])), 
                                    X = X[is.na(dat$response),],
                                    w = w.index),
                       A = list(1,1,A.era[is.na(dat$response),]))
      
      
      #### join data stacks and extracts the data index
      stk <- inla.stack(stk.e, stk.p)
      e.id <- inla.stack.index(stk, 'est')$data
      p.id <- inla.stack.index(stk, 'pred')$data
      # Now let's make our formula, intercept with each observation set as a random variable, this makes 
      # As soon as you make a spatial model make your own intercept.  Here is an initial suite of models I like...
      intercept <- 1 # intercept
      # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
      # overfit less, to do this I need to bin the covariates using the inla.group function below, problem is 
      # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
      # and he recommends these priors to make sure it doesn't get too funky
      U <- 0.5
      hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
      
     # The best cod and yellowtail models
        # if(species[s] == "cod_PA") 
        # {
           model.depth.sst <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            f(sst , model = "rw1", hyper = hyp.rw2)  + 
            f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        #}
      
        # if(species[s] == "yt_PA") 
        # {
        #   model.depth.sst <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
        #     f(sst , model = "rw1", hyper = hyp.rw2)  +
        #     f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
        # } # end if(species[s] == "yt_PA" && st.mods[st] == 3) 
        # 
      
      # Let's giver, make the spatial model.
      # Now we can loop this to run over each model and for each stack, 
      # I was going to run a stack loop, but that is amazingly slow, so instead I'm going to do the model selection with the
      # 10 year field, and using the best model from that I'll then run a one off script for the st.5
      # model and see which I prefer....
      
      #if(st == 2) {stk <- skt.5; st.mod <- "st.5"}
        run.name <- paste0(species[s]," ", surveys[i],"_survey")
        print(paste("Model run started at ",Sys.time()))
        r.out <- inla(model.depth.sst, family=fam, data = inla.stack.data(stk),
                      control.predictor=list(A=inla.stack.A(stk),link =1), # The link = 1 is the reason I was getting the transformed predictions, it predicts on the identity scale unless the link is specified (1 means the 1st likelihoods link, which is the only likelihood in this case)
                      #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                      #verbose=TRUE,
                      control.compute = list(dic=T,waic=T,cpo=T,openmp.strategy="huge"))  # The Openmp.strategy will use more cores, if set to 'huge' it uses everything...
        print(paste("Model run completed at ",Sys.time()))
        # The fitted model, residuals and the covariates, both the standardized and on the original scale.
        mo.out <- data.frame(fitted = r.out$summary.fitted.values[e.id,"mean"] , # The expected values can be found with this
                             resid = dat$response[!is.na(dat$response)] - r.out$summary.fitted.values[e.id,"mean"],
                             sd = r.out$summary.fitted.values[e.id,"sd"] ,
                             lci = r.out$summary.fitted.values[e.id,"0.025quant"] ,
                             uci = r.out$summary.fitted.values[e.id,"0.975quant"] ,
                             response = dat$response[!is.na(dat$response)],
                             dep = dat$depth_cen[!is.na(dat$response)],
                             sst = dat$sst_avg_cen[!is.na(dat$response)],
                             depth = dat$comldepth[!is.na(dat$response)],
                             sst_avg = dat$sst_avg[!is.na(dat$response)],
                             #years_3 = dat$years_3[!is.na(dat$response)],
                             years_5 = dat$years_5[!is.na(dat$response)],
                             X = dat$X[!is.na(dat$response)],
                             Y = dat$Y[!is.na(dat$response)],
                             year = dat$year[!is.na(dat$response)]
        )
        
        #  a couple of other variables to calculated
        mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
        mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
        
        # Now get the predictions
        p.out <- data.frame(pred = r.out$summary.fitted.values[p.id,"mean"] , # The predictions  can be found with this, note these need to be transformed...
                            pred.sd = r.out$summary.fitted.values[p.id,"sd"] ,
                            pred.lci = r.out$summary.fitted.values[p.id,"0.025quant"] ,
                            pred.uci = r.out$summary.fitted.values[p.id,"0.975quant"] ,
                            dep = dat$depth_cen[is.na(dat$response)],
                            #sst = dat$sst_avg_cen[is.na(dat$response)],
                            depth = dat$comldepth[is.na(dat$response)],
                            sst_avg = dat$sst_avg[is.na(dat$response)],
                            #years_3 = dat$years_3[is.na(dat$response)],
                            years_5 = dat$years_5[is.na(dat$response)],
                            X = dat$X[is.na(dat$response)],
                            Y = dat$Y[is.na(dat$response)],
                            year = dat$year[is.na(dat$response)]
        )
        
        # Now the model fits using dic and waic, results very similar.
        md.out <- data.frame(dic = r.out$dic$dic, 
                             dic.p.eff = r.out$dic$p.eff,
                             waic = r.out$waic$waic, 
                             waic.p.eff = r.out$waic$p.eff,
                             Dispersion = sum()) 
        md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
        
        
        res.pred[[run.name]] <- r.out
        #res[[mod.names[m]]]$model <- mod.names[m]
        mod.output.pred[[run.name]] <- mo.out
        mod.output.pred[[run.name]]$model <- run.name
        mod.output.pred[[run.name]]$species <- species[s]
        mod.output.pred[[run.name]]$survey <- surveys[i]
        #mod.output[[run.name]]$model.id <- mod.names[m]
        #mod.output[[run.name]]$st.era <- st.mods[st]
        pred.output.pred[[run.name]] <- p.out
        pred.output.pred[[run.name]]$model <- run.name
        pred.output.pred[[run.name]]$species <- species[s]
        pred.output.pred[[run.name]]$survey <- surveys[i]
        #mod.output[[run.name]]$model.id <- mod.names[m]
        #mod.output[[run.name]]$st.era <- st.mods[st]
        
        
        mod.diagnostics.pred[[run.name]] <- md.out
        mod.diagnostics.pred[[run.name]]$model <- run.name
        mod.diagnostics.pred[[run.name]]$species <- species[s]
        mod.diagnostics.pred[[run.name]]$survey <- surveys[i]
        #mod.diagnostics[[run.name]]$model.id <- mod.names[m]
        #mod.diagnostics[[run.name]]$st.era <-  st.mods[st]
        
        rand.field.pred[[run.name]] <- r.out$summary.random$w # THis will contain mutliple random fields for the spatio-temporal models.
        rand.field.pred[[run.name]]$model <- run.name
        rand.field.pred[[run.name]]$species <- species[s]
        rand.field.pred[[run.name]]$surveys <- surveys[i]
        #rand.field[[run.name]]$model.id <- mod.names[m]
        #rand.field[[run.name]]$st.era <- st.mods[st]
        
        
      # Save the image on the way through just in case the computer decides to shut down...
      save.image(paste0(direct.proj,"Results/INLA_output_full_predicted_field_",species[s],"_",surveys[i],".RData"))
    }# end for(i in 1:n.surveys)
    # Save the image on the way through just in case the computer decides to shut down, worst case here I lose about half a day of modelling.
    # This thing is gonna be big...
    #save.image(paste0(direct.proj,"Results/INLA_output_full_predicted_field",species[s],"tmp.RData"))
  } # end for(s in 1:n.species)

save.image(paste0(direct.proj,"Results/INLA_model_full_predicted_field_yt_mods.RData"))

# It's on this predicted field that I want to do Center of gravity and get the 
# bounds for area in which you are likely to see the species....
# For that model this is really going to come from that predicted output.  So while waiting for that
# to finish up, we could play with the results from one of the models with a validation stack
# and preted that this is our preditced field, should be similar methods.



# Just so I don't have to reload crap from the above saved temp runs..
pred.dat <- NULL ; pt <- NULL
# This has the predictied field for all the results except the cod NMFS spring model which is in a different
# file...
load(paste0(direct.proj,"Results/INLA_output_full_predicted_field_cod_PA_nmfs-spring.RData"))
pt[['cod_PA nmfs-spring_survey']]<- pred.output.pred$`cod_PA nmfs-spring_survey`
load(paste0(direct.proj,"Results/INLA_output_full_predicted_field_yt_PA_RV.RData"))
pred.dat <- pred.output.pred
pred.dat[['cod_PA nmfs-spring_survey']] <- pt[['cod_PA nmfs-spring_survey']]
# Now I'm going to save this prediction field so we can play around with it in a Dashboard.
#save(pred.dat,file = paste0(direct.proj,"Results/Prediction_fields_all_models.RData"))

# These other models all contain redundant information to what is the the yt_PA_RV.RData file so shouldn't need loaded...
#load(paste0(direct.proj,"Results/INLA_output_full_predicted_field_cod_PA_nmfs-fall.RData"))
#pred.op.tmp[['cod_PA nmfs-fall_survey']]<- pred.output.pred$`cod_PA nmfs-fall_survey`
#load(paste0(direct.proj,"Results/INLA_output_full_predicted_field_cod_PA_RV.RData"))
#pred.op.tmp[['cod_PA RV_survey']]<- pred.output.pred$`cod_PA RV_survey`
#load(paste0(direct.proj,"Results/INLA_output_full_predicted_field_yt_PA_nmfs-spring.RData"))
#pred.op.tmp[['yt_PA nmfs-spring_survey']]<- pred.output.pred$`yt_PA nmfs-spring_survey`
#load(paste0(direct.proj,"Results/INLA_output_full_predicted_field_yt_PA_nmfs-fall.RData"))
#pred.op.tmp[['yt_PA nmfs-fall_survey']]<- pred.output.pred$`yt_PA nmfs-fall_survey`


# For later purposes to speed things up, I will save an output that is just the prediction fields so I only need to load these
# for my maybe someday Dashboard.

# Once we have all the models run I can toss the above hack...
# Need to reload pectinid as the above have a old version or something...
source(paste(direct.fun,"Maps/pectinid_projector_sf.R",sep=""))


# I know I load this at the start, but a reminder that we need this for inside this function to get the dat.final object...
load(paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))

pred.fields(pred.dat = pred.op.tmp,p.grid=mesh.grid)
# Now let's get what I want out of each of these predicted field models.
pred.fields <- function(plt.area = "GOM",pred.dat,prob = 0.8 ,p.grid = NULL, 
                        p.crs = st_crs(mesh.grid),loc.text = c(600000,4450000),
                        plots = c('video','cog','facet')) 
{

if(length(pred.dat) <=6) {n.pred.mods <- length(pred.dat)} else {n.pred.mods =1}

bp <- pecjector(area=plt.area,plot=F,direct_fns = 'github',add_layer = list(eez = 'eez',nafo = 'main',scale.bar = 'tl'),c_sys = 32619,buffer = 0.05)
# Set up my colour ramp for the maps, stolen from pectinid
col <- addalpha(pals::viridis(100),1)
brk <- seq(prob,1,by=0.05)
lims <- range(brk)
sf <- scale_fill_gradientn(colours = col, limits=lims,breaks=brk,name="Probability")
sc <- scale_colour_gradientn(colours = col, limits=lims,breaks=brk,name="Probability")

for(i in 1:n.pred.mods) {

if(n.pred.mods == 1) res <- pred.dat
if(n.pred.mods >1) res <- pred.dat[[i]]
n.eras <- length(unique(res$years_5))
eras <- factor.2.number(unique(res$years_5))
# Get the species
if(grepl("yt",res$species[1])) sp <- "YT"
if(grepl("cod",res$species[1])) sp <- "cod"
# Get the survey name
surv <- res$survey[1]
# Get the data we want...
dat <- dat.final %>% filter(survey == surv)
# Fro the RV survey I need to adjust this...
#if(surv == "RV") dat$years_5 <- dat$years_5-4
# This puts the mesh grids in here instead of using the points in the object, this just makes for a nicer output object.
if(!is.null(p.grid)) st_geometry(res) <- st_geometry(rep(p.grid,n.eras)) 
# We can also just use the points in the object if we don't specify the prediction grid.
if(is.null(p.grid)) res <- st_as_sf(res,coords = c("X","Y"),crs = p.crs)

for(n in min(eras):max(eras))
{
  yrs <- paste0(substr(dat %>% filter(years_5 == n) %>% summarise(min = min(year)),3,4),"-",
                substr(dat %>% filter(years_5 == n) %>% summarise(max = max(year)),3,4))
  if(substr(yrs[1],1,2) > 30) { yrs <- paste0(19,yrs)} else {yrs <- paste0(20,yrs)}
  res$yrs[res$years_5==n] <- yrs
}
# So calculating area is smart using that set units, though they are all idenitcal...
res$area <- res %>% st_area() %>% set_units("km^2")


# Calculate the center of gravity Here's a nice way to return an object with multiple ouptuts
cog <- as.data.table(res)[,cog.calc(X,Y,pred), by = yrs]
cog <- st_as_sf(cog,coords = c('x','y'), crs= p.crs, remove=F)

# Here I'm making a column name that is nice year values (for the figure title) rather than just era 1, etc... 

# Now get the area in which the probability of encounter is >= prob
area.era <- data.frame(res) %>% dplyr::filter(pred >= 0.7) %>% group_by(yrs) %>% summarize(tot.area = sum(area))
#area.era
area.era$X <- loc.text[1] 
area.era$Y <- loc.text[2]
area.era <- st_as_sf(area.era,crs=p.crs,coords = c("X","Y"), remove=F)

res <- res %>% left_join(data.frame(area.era) %>% dplyr::select(yrs,tot.area),by= "yrs")

# Add the area to the res object so it's easy to plot...
res$tot.area <- round(res$tot.area,digits=0)

mn.prop <- data.frame(res) %>% group_by(yrs) %>% dplyr::summarise(mn = mean(pred),med = median(pred), sd = sd(pred))
mn.prop

cog.map.plt <-  bp + geom_sf_text(data = cog, aes(label=substr(yrs,3,8)),size=2) + 
              #geom_errorbar(data = cog,aes(x= x,ymin=y - 3*se.y,ymax=y + 3*se.y),colour = "blue",width=0)  + 
              #geom_errorbar(data = cog,aes(y= y,xmin=x - 3*se.x,xmax=x + 3*se.x),colour = "blue",width=0)  + 
                 theme_bw()

cog.sd.plt <-  ggplot(data = cog) + geom_label(aes(x=x, y = y+3.5*se.y,label=substr(yrs,3,8)),size=4) + 
                          geom_errorbar(aes(x= x,ymin=y - 3*se.y,ymax=y + 3*se.y),colour = "blue",width=0)  + 
                          geom_errorbar(aes(y= y,xmin=x - 3*se.x,xmax=x + 3*se.x),colour = "blue",width=0)  + 
                          theme_bw() + xlab("") + ylab("")

#SOMETHING WEIRD WITH THE VIDEO SAVE, MAYBE JUST RESTART R??
#windows(11,11)
saveGIF(
  {
    ani.options(interval = 2, nmax = 50,ffmpeg = 'C:/Program Files/ImageMagick-7.0.8-Q16/ffmpeg.exe')
    for (p in min(eras):max(eras)) 
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      res.t <- res %>% filter(years_5 == p)
      area.t <- area.era %>% filter(yrs ==res.t$yrs[1])
      area.t$tot.area <- round(area.t$tot.area,digits =0)
      plt<-bp + geom_sf(data = res.t %>% dplyr::filter(pred >= prob),aes(fill = pred,colour=pred))+ # facet_wrap(~years_5) + 
        coord_sf(datum=p.crs) + sf + sc +
        #geom_sf_text(data=area.t,aes(label =as.expression(bquote(Area== .(tot.area)~km^2)),parse=T) ) +
        annotate('text',x=area.t$X,y=area.t$Y, label=as.expression(bquote(Area== .(area.t$tot.area)~km^2)),parse=T) +
        ggtitle(paste0("Encounter probability \u2265 ",prob,": ",sp,"-",surv,' survey(',area.t$yrs[1],")")) 
      print(plt)
      ani.pause()
    }
  }, movie.name = paste0(direct.proj,'Results/Figures/INLA/Encounter_probability_lte_',prob,"-",sp,"-",surv,'_survey.gif'), ani.width = 800, ani.height = 800)

# Now to get a fancy label set up we can do this, then attached this to the area.era object
lab <- NA
for(k in 1:n.eras) lab[k] <- paste0("Area==~",round(area.era$tot.area[k],digits=0),"*km^2")
area.era$lab <- lab

# Facet version of the above NOTE HOW THE YEARS ARE F'd up now...
plt<-bp + geom_sf(data = res %>% dplyr::filter(pred >= prob ) ,aes(fill = pred,colour=pred))+ 
  facet_wrap(~as.factor(yrs)) + 
  coord_sf(datum=p.crs) + sf + sc + theme_map() +
  geom_sf_text(data = area.era , aes(label = lab),parse=T) +
  #geom_text(data=area.t,aes(label =as.expression(bquote(Area== .(tot.area)~km^2)),parse=T) ) +
  #annotate('text',x=area.era$X,y=area.era$Y, label=tst,parse=T) +
  # Note for the >= symbol to show up correctly in a pdf use cairo_pdf rather than just pdf! Just trying to avoid using an expression here..
  ggtitle(paste0("Encounter probability \u2265 ",prob," - ", sp, " - ", surv, " survey")) 
ggsave(plt, file = paste0(direct.proj,'Results/Figures/INLA/Encounter_probability_lte_',prob,"-",sp,"-",surv,'_survey.png'),width = 11,height = 11,units = 'in')

  }
}
############################# 


############################# SECTION 7 ################################ SECTION 7  ###################################################################
############################# SECTION 7 ################################ SECTION 7  ###################################################################
############################# SECTION 7 ################################ SECTION 7  ###################################################################
# Here I'm going to bring in the 2017-2019 data and see how well the full model predicts on that data.


# Need to initialize some objects for later
st.mods <- c(5,3)
num.fields <- length(st.mods)
num.mods <- 1 # The number of models I'm gonna run validation on


dat.val.new <- dat.val %>% dplyr::select("comldepth","lat","lon","survey","year","unique_set_ID","cod_PA","yt_PA","X","Y","sst_avg","years_3","years_5")
new.dat.final <- new.dat.final %>% dplyr::select("comldepth","lat","lon","survey","year","unique_set_ID","cod_PA","yt_PA","X","Y","sst_avg","years_3","years_5")
#new.dat.final$comldepth <- -new.dat.final$comldepth
st_geometry(new.dat.final) <- NULL
dat.val.new <- rbind(dat.val.new,new.dat.final)
# I want to set the data up here so we can get the folds identified now
dat.cod <-  dat.val.new %>% dplyr::filter(survey == "RV")
dat.yt <- dat.val.new %>% dplyr::filter(survey == "nmfs-spring")
# Number of folds, going for 5 fold cross validation



#res.fd <- NULL
#mod.diag <- NULL
mod.output.st.yr <- NULL
mod.diagnostics.st.yr <- NULL
pred.res.st.yr <- NULL
for(s in 1:num.species)
{
  for(st in 1:num.fields)
  {
    if(species[s] == 'cod_PA'){ dat <- dat.cod }
    if(species[s] == 'yt_PA') { dat <- dat.yt }
    if(grepl("cod", species[s])) resp <- "cod_PA"
    if(grepl("yt", species[s])) resp <- "yt_PA"
    response <- which(names(dat) == resp)
    names(dat)[response] <- "response"
    # Lets log transform depth and chl_rg, then center depth, chl_rg and sst_avg, also make SEDNUM a factor
    dat$depth_log <- log(-dat$comldepth)
    dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
    dat$sst_avg_cen <- scale(dat$sst_avg)

    # There really isn't enough data for 2,5,6,8,9, so I am going to exclude those from the analysis for the moment.
    # The 5 groups represent around 5% of the total data (~450 samples between the 5 levels) .
     # I wan the year group to be a categorical variable.
    dat$years_5 <- as.factor(dat$years_5)
    dat$years_3 <- as.factor(dat$years_3) # I won't run the 3 year era models unless the 5 years are better than the 10 years.
    # Get the location of our data...
    loc <- cbind(dat$X,dat$Y)
    #We also need to decide Observation Likelihood
    fam <- "binomial"
    # If looking at all the data use the gf mesh
    mesh <- mesh.gf
    range <- range.gf
    # The amount of data we have
    N = nrow(dat)
    # For both the beta and binomial families we'll need to determine the number of trials.
    Ntrials <- 1 # For each record there is only 1 trial.
    
    # For both our scenarios we are going to be using the logit model (note that this isn't strictly necessary to write as the logit is the
    # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
    control.fam = list(control.link=list(model="logit"))
    # Now the 3 and 5 year model
    if(st.mods[st] == 5) eras <- as.numeric(dat$years_5)
    if(st.mods[st] == 3) eras <- as.numeric(dat$years_3)
    
    era.names <- unique(eras)
    n.eras <- length(unique(eras))
    if(st.mods[st] == 5)A.era <- inla.spde.make.A(mesh, loc,repl = eras)
    if(st.mods[st] == 3) A.era <- inla.spde.make.A(mesh, loc,group = eras,n.groups =n.eras)
    
    spde <- inla.spde2.pcmatern(mesh,    
                                prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
    # and now we define the spatio-temporal random field.  I want to use
    # group of the 3 year era so that I can make the temporal field be an AR1 process.
    if(st.mods[st] == 5) w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
    if(st.mods[st] == 3) w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.group = n.eras)
    # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
    # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
    # certainly isn't entirely clear to me!
    #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
    
    dat$depth_cen_g <- inla.group(dat$depth_cen,n=100)
    #dat$chl_rg_cen_g <- inla.group(dat$chl_rg_cen,n=100)
    dat$sst_avg_cen_g <- inla.group(dat$sst_avg_cen,n=100)
    options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
    X.matrix <- model.matrix(~ 0+ depth_cen_g +  sst_avg_cen_g , data = dat)
    # And then make a covariate matrix
    X <- data.frame(depth =        X.matrix[,1],
                    sst   =        X.matrix[,2])
    
    # The new fangled stacks
    stk.e = inla.stack(tag="est",
                       data=list(y = dat$response[dat$year <=2016], link=1L),
                       effects=list(intercept = rep(1, nrow(dat[dat$year <=2016,])), 
                                    X = X[which(dat$year <=2016),],
                                    w = w.index),
                       A = list(1,1,A.era[which(dat$year <=2016),]))
    
    stk.v = inla.stack(tag="val",
                       data=list(y = NA, link=1L),
                       effects=list(intercept = rep(1, nrow(dat[dat$year >2016,])), 
                                    X = X[which(dat$year >2016),],
                                    w = w.index),
                       A = list(1,1,A.era[which(dat$year >2016),]))
    #### join data stacks and extracts the data index
    stk <- inla.stack(stk.e, stk.v)
    e.id <- inla.stack.index(stk, 'est')$data
    v.id <- inla.stack.index(stk, 'val')$data
    
    intercept <- 1 # intercept
    # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
    # overfit less, to do this I need to bin the covariates using the inla.group function above, problem is 
    # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
    # and he recommends these priors to make sure it doesn't get too funky
    U <- 0.5
    hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
    
    # Both cod and yt go with the mod 1 that is the intercept model 
    
    # For yellowtail toss we'll use these models for comparison
    if(species[s] == "yt_PA")
    {
      if(st.mods[st] == 3)
      {
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)   + 
          f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
        mod.name <- "depth_sst_yt"
      }
      
      if(st.mods[st] == 5)         
      {
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)  +
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        mod.name <- "depth_sst_yt"
      } # end  if(st.mods[st] == 3) 
      
    } # end if(species[s] == "yt_PA")
    
    if(species[s] == "cod_PA")
    {
      if(st.mods[st] == 5)
      {
        model <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)  + 
          f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        mod.name <- "depth_sst_cod"
      }
      if(st.mods[st] == 3)
      {
        model <- y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
          f(sst , model = "rw1", hyper = hyp.rw2)   + 
          f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
        mod.name <- "depth_sst_cod"
      }
    }
    
    
    run.name <- paste0(species[s],"_model_",mod.name,"_predict_2017_2020_field_",st.mods[st])
    # Now we run the models
    r.out <- inla(model, family=fam, data = inla.stack.data(stk),
                  control.predictor=list(A=inla.stack.A(stk)),
                  #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                  #verbose=TRUE,
                  control.compute = list(dic=T,waic=T,cpo=T)) 
    # The fitted model, residuals and the covariates, both the standardized and on the original scale.
    mo.out <- data.frame(fitted = r.out$summary.fitted.values[e.id,"mean"] , # The expected values can be found with this
                         resid = dat$response[dat$year <=2016] - r.out$summary.fitted.values[e.id,"mean"],
                         response = dat$response[dat$year<=2016],
                         dep = dat$depth_cen[dat$year<=2016],
                         sst = dat$sst_avg_cen[dat$year<=2016],
                         depth = dat$comldepth[dat$year<=2016],
                         sst_avg = dat$sst_avg[dat$year<=2016],
                         years_3 = dat$years_3[dat$year<=2016],
                         years_5 = dat$years_5[dat$year<=2016],
                         X = dat$X[dat$year<=2016],
                         Y = dat$Y[dat$year<=2016]
    )
    
    #  a couple of other variables to calculated
    mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
    mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
    
    p.out <- data.frame(pred = r.out$summary.fitted.values[v.id,"mean"] , # The expected values can be found with this, note these need to be transformed...
                        pred.err = dat$response[dat$year > 2016] - r.out$summary.fitted.values[v.id,"mean"],
                        response = dat$response[dat$year > 2016],
                        dep = dat$depth_cen[dat$year > 2016],
                        sst = dat$sst_avg_cen[dat$year > 2016],
                        depth = dat$comldepth[dat$year > 2016],
                        sst_avg = dat$sst_avg[dat$year > 2016],
                        years_3 = dat$years_3[dat$year >2016],
                        years_5 = dat$years_5[dat$year >2016],
                        X = dat$X[dat$year >2016],
                        Y = dat$Y[dat$year >2016]
    )
    
    # Now the model fits using dic and waic, results very similar.
    md.out <- data.frame(dic = r.out$dic$dic, 
                         dic.p.eff = r.out$dic$p.eff,
                         waic = r.out$waic$waic, 
                         waic.p.eff = r.out$waic$p.eff,
                         Dispersion = sum(),
                         cpo = fcpo(r.out, e.id)) 
    md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
    
    
    # Really scaling down what we keep so the output object is manageable.  Key here is looking at predictions
    #res.fd[[run.name]] <- r.out
    #res[[mod.names[m]]]$model <- mod.names[m]
    mod.output.st.yr[[run.name]] <- mo.out
    mod.output.st.yr[[run.name]]$model <- run.name
    mod.output.st.yr[[run.name]]$model.id <- mod.name
    mod.output.st.yr[[run.name]]$species <- species[s]
    # mod.output.st.yr[[run.name]]$fold <- fd
    mod.output.st.yr[[run.name]]$field <- st.mods[st]
    
    mod.diagnostics.st.yr[[run.name]] <- md.out
    mod.diagnostics.st.yr[[run.name]]$model <- run.name
    mod.diagnostics.st.yr[[run.name]]$model.id <- mod.name
    mod.diagnostics.st.yr[[run.name]]$species <- species[s]
    #mod.diagnostics.st.yr[[run.name]]$fold <- fd
    mod.diagnostics.st.yr[[run.name]]$field <- st.mods[st]
    
    pred.res.st.yr[[run.name]] <- p.out
    pred.res.st.yr[[run.name]]$model <- run.name
    pred.res.st.yr[[run.name]]$model.id <- mod.name
    pred.res.st.yr[[run.name]]$species <- species[s]
    #pred.res.st.yr[[run.name]]$fold <- fd
    pred.res.st.yr[[run.name]]$field <- st.mods[st]
    
    # Stick a print in here so we know this is moving forward
    print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
    # Write a message to the ESS so I can see progress remotely....
    #fileConn<-file(paste0(direct.proj,"Results/status.txt"))
    #writeLines(messy, fileConn)
    #close(fileConn)
  } # end models
} # end species


#save.image(paste0(direct.proj,"Results/INLA_model_predict_2017_2019.RData"))


#load(paste0(direct.proj,"Results/INLA_model_predict_2017_2019.RData"))
# Now we can see how well the field was estimated in 2017-2019, really we'll want to look year by year to see if the predictions
# deteriorate over time

# Get the X and Y coordinates on the cod models...
pred.res.st.yr$cod_PA_model_depth_sst_cod_predict_2017_2020_field_5$year <- dat.cod$year[dat.cod$year >2016]
pred.res.st.yr$cod_PA_model_depth_sst_cod_predict_2017_2020_field_3$year <- dat.cod$year[dat.cod$year >2016]

# Now the YT models....
pred.res.st.yr$yt_PA_model_depth_sst_yt_predict_2017_2020_field_5$year <- dat.yt$year[dat.yt$year >2016]
pred.res.st.yr$yt_PA_model_depth_sst_yt_predict_2017_2020_field_3$year <- dat.yt$year[dat.yt$year >2016]
# Now the same for the modeled data...
mod.output.st.yr$cod_PA_model_depth_sst_cod_predict_2017_2020_field_3$year <- dat.cod$year[dat.cod$year <=2016]
mod.output.st.yr$cod_PA_model_depth_sst_cod_predict_2017_2020_field_5$year <-  dat.cod$year[dat.cod$year <=2016]
# Now the YT models....
mod.output.st.yr$yt_PA_model_depth_sst_yt_predict_2017_2020_field_5$year <- dat.yt$year[dat.yt$year <=2016]
mod.output.st.yr$yt_PA_model_depth_sst_yt_predict_2017_2020_field_3$year <-  dat.yt$year[dat.yt$year <=2016]

pred.res <- do.call('rbind',pred.res.st.yr)
pred.res$type <- 'prediction'
mod.res <- do.call('rbind',mod.output.st.yr)
mod.res$type <- 'residual'
pred.res$pred <- inv.logit(pred.res$pred)
pred.res$pred.err <- pred.res$response - pred.res$pred 
# Combine them..
mod.res <- mod.res %>% dplyr::select(fitted,resid,response,dep,sst,depth,sst_avg,years_3,years_5,X,Y,model,model.id,species,field,year,type)
pred.res <- pred.res %>% dplyr::select(pred,pred.err,response,dep,sst,depth,sst_avg,years_3,years_5,X,Y,model,model.id,species,field,year,type)
names(pred.res) <- c("fitted","resid","response","dep","sst","depth","sst_avg","years_3",'years_5',"X","Y","model","model.id","species","field","year","type")
# These 2 steps will go away once I get the new model runs where I tidy this all up...
pred.res <- st_as_sf(pred.res,coords = c("X","Y"), crs= 32619,remove=F)
mod.res <- st_as_sf(mod.res,coords = c("X","Y"), crs= 32619,remove=F)
all.resids <- rbind(mod.res,pred.res)

sd.pred <- pred.res %>% group_by(model,year) %>% summarise(sd=sd(resid),rmse = RMSE(fitted,response))
sd.mod <- mod.res %>% group_by(model,year) %>% summarise(sd=sd(resid),rmse = RMSE(fitted,response))

#save(all.resids,file = paste0(direct.proj,"Results/INLA_2017_2019_prediction_error_summary.RData"))

# This simulates entirely random data between 0 and 1, so if I had no ability to predict the future this is what we'd see...
null.rmse <- NA
for(i in 1:10000) null.rmse[i] <- RMSE(runif(50,0,1),rbinom(50,1,0.5))
# So the null model expectation is an RMSE of around 0.577, probability itself doesn't actually matter which makes some sense since everything is bounded and random
summary(null.rmse) 
# So that result basicaly means the cod model is terrible predictor, while the yellowtail is able to predict future years.

all.resids %>% filter(year > 2013) %>% group_by(species,year) %>% summarise(mean(response))
all.resids %>% filter(year > 2013) %>% group_by(species,year) %>% summarise(mean(resid))


# Now make some figures...
ggplot(pred.res) + geom_sf(aes(colour = pred.err)) + facet_wrap(~model) + scale_colour_viridis()
ggplot(pred.res) + geom_histogram(aes(x=pred.err,fill=as.factor(year))) + facet_wrap(~model)
ggplot(mod.res) + geom_histogram(aes(x=resid,fill=as.factor(year))) + facet_wrap(~model)

ggplot(all.resids) + geom_histogram(aes(x=resid,after_stat(density),fill= type)) + facet_wrap(~model)
ggplot(all.resids) + geom_histogram(aes(x=resid,after_stat(density),fill= type)) + facet_wrap(~model)


ggplot(all.resids) + geom_histogram(aes(x=response,after_stat(density),fill= type)) + facet_wrap(~model)
windows(11,11)
ggplot(all.resids) + geom_point(aes(x=year,y =resid,fill=type),alpha=0.2,shape = 21) + facet_wrap(~model)

windows(11,11)
ggplot(all.resids %>% filter(year== 2017)) + geom_sf_text(aes(label = signif(dep,digits=2)))

sfp <- scale_fill_manual(values = c('yellow', 'black'))
windows(11,11)
ggplot(all.resids) + geom_sf(aes(response, fill = as.factor(response)),shape=21) + sfp

p <- pecjector(area = "GOM",add_layer = list(bathy = 10))
p1 <- p + geom_sf_text(data = all.resids %>% filter(year %in%  2010:2019, species == "cod_PA"),aes(label = signif(1.83*depth,digits=2),color=as.factor(year)))

plotly::ggplotly(p1)


    
 
  
  