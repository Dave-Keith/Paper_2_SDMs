# Here's where I'm putting all the model diagnostic crap together

# 1:  This is going to be AIC/WAIC summaries from the models I need to summarize...
################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
#----
rm(list=ls())
direct.proj <- "d:/Github"

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
#inla.upgrade(testing = F)

# Bring in the functions we'll need
source(paste(direct.fun,"Assessment_fns/Maps/pectinid_projector.R",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/convert_coords.R",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/add_alpha_function.r",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/combine_shapefile_layers.R",sep=""))


# Here is the data we need, this comes from Step 3 INLA_mesh_for_gb_surveys_and_scallop_survey.R
load(paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))
load(paste0(direct.proj,"Data/INLA_meshes.RData"))

# I tried to make a little function (o.k. stole...) to add the minimum + X to the diagnostic plots, intent of this is for comparison of wAIC and DIC for models
# You give your stat a name, the second is the "type" of geom, here we are making a stat (could be geom, position, scale)
# the compute_group is returned once per group (we have a faceted plot so have several groups, I think that's what that does)
# stat.min.plus <- ggproto("stat.min.plus", Stat,
#                         compute_group = function(data, scales) 
#                         {
#                           transform(data, yintercept=min(y))
#                         },
#                         required_aes = c("x", "y"))
# 
# # And this function actually draws the line there for us
# stat_min_line <- function(mapping = NULL, data = NULL, geom = "hline",position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) 
# {
#   layer(stat = stat.min.plus, data = data, mapping = mapping, geom = geom, 
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(na.rm = na.rm, ...))
#}

##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########
##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########


##########################  Section 2     Run the analyses   ##########################  Section 2     Run the analyses ###########################
############  Here's where we do the INLA'ing and get our model results #####################
##########################  Section 2     Run the analyses   ##########################  Section 2     Run the analyses ###########################
############ Start here for analysis, load in the data we need and go nuts...
#----
# Now I need to stick my UTM coordinates into the two data files...
scal.surv.dat$X <- loc.sc@coords[,1]
scal.surv.dat$Y <- loc.sc@coords[,2]
dat.final$X <- loc.gf@coords[,1]
dat.final$Y <- loc.gf@coords[,2]
# At this point I really start the analysis, and what I want to do is split up each survey, I might as well loop this, or at least allow for a loop
# so we can move through each survey easily, this will get complicated with a number of models for each survey, especially when I split out the time...
surveys <- c(unique(dat.final$survey),unique(scal.surv.dat$survey))
num.surveys <- length(surveys)

# Lets center our depth so 0 represents the mean depth of the region of study
#dat.final$depth_cen <- dat.final$comldepth - mean(dat.final$comldepth)

# We are going to need to specify our sigma for each model.  From Zuur chapter 19 pages 414-416 he does give some guidance of the highest this should 
# ever be, think about your random spatial field, if we were just mapping the spatial field that field could range between the maximum and
# minimum value in the data, for our data we are following the logit distribution, so we can play around a bit with this, think about how variable
# you expect the surface to be on it's own, for example if expecting values to be between 0.2 and 0.8 what would sigma be (around 0.75)
#sigma <- seq(0,4,by=0.25)
# Assuming the field is 0 mean a 0.5 would let the field itself vary between 0.27 and 0.72
#CI <- cbind(exp(-1.96*sigma)/ (1+exp(-1.96*sigma)),exp(1.96*sigma)/ (1+exp(1.96*sigma)),sigma)
#CI
# Zuur tends to us sigma = 0.5 for these in the logit cases with a low probability that the sigma is above this value
# So this will tend to make the surface relatively smooth, but we will want to compare this with that the model estimates
sigma <- 0.5
s.alpha <- 0.05
# Similarly for the range, we've already settled on a value when making the mesh previously following these same sections of zuur
# But here for the range our alpha is the likelihood that the range is smaller than our set value, again Zuur tends
# to make this small to avoid overfitting
r.alpha <- 0.05

# Now we can run the analyses through each survey, the next question to sort out is how to build this to account for the different types of analyses
# Currently I want 1 for each PCA, I need one for the scallop biomass, I want something acounting for year (maybe the era analysis), 
# also I want an analysis without the PCA's in it, I need one for cod and one for yt don't forget, 
# do I want to set these up explicitly or toss into a loop... hmm 
# Might also be cool to do a "last 10 year" average scallop biomass as something that could be use as a longer term predicitve surface and
# One with all the scallop biomasses as well.

# I need a loop for the number of "species" i'm looking at, yt, cod, and up to 4 scallop size classes will all be informative
# Here I take "species" as the head in the file so it's easy to select which is being done
# To minimize code tweaking I also add in a model for the scallop that only looks at the field using the most recent year of data
species <- c("cod_PA","yt_PA","com_stan","rec_stan","bin_lt_50_stan","bin_50_70_stan", # These are the full suite of models
             "com_stan_ly","rec_stan_ly","bin_lt_50_stan_ly","bin_50_70_stan_ly") # These are the distributions for most recent year) 
#species <- c("com_stan_ly","rec_stan_ly","bin_lt_50_stan_ly","bin_50_70_stan_ly") # These are the distributions for most recent year) 

num.species <- length(species)
# Need to initialize some objects for later
res <- NULL
#mod.diag <- NULL
mod.output <- NULL
rand.field <- NULL
mod.diagnostics <- NULL
dat.list <- NULL
w.index.list <- NULL
for(s in 1:num.species) 
{
  # Loop through each of the surveys
  for(i in 1:num.surveys) 
  {
    # and here we go into the scallop dat and extract the data we want...
    if(surveys[i] == 'scallop' && !grepl("PA",species[s])) 
    {
      # Let's select the data for the particular survey of interest
      dat <- scal.surv.dat
      # If we are just looking at the last year of data, in this case we want to compare the RV survey data from 2016 (the last year we have)
      # With the scallop data from 2015, this would give the sample closure area for 2017, compare to what we closed...
      if(grepl("_ly",species[s])) dat <- dat[dat$year == 2016,]
      # Rename the varialbe of interest to "response", this doesn't quite work so nicely since I hacked the species to be everything...
      if(grepl("com", species[s])) resp <- "com_stan"
      if(grepl("rec", species[s])) resp <- "rec_stan"
      if(grepl("bin_lt_50", species[s])) resp <- "bin_lt_50_stan"      
      if(grepl("bin_50_70", species[s])) resp <- "bin_50_70_stan"
      response <- which(names(dat) == resp)
      names(dat)[response] <- "response"
      # Lets log transform and center our depth data
      dat$depth_log <- log(dat$depth)
      dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
      # I wan the year group to be a categorical variable.
      loc <- cbind(dat$X,dat$Y)
      # now the mesh
      mesh <-mesh.sc 
      range <- range.sc
      # We also need to decide Observation Likelihood, given these will be specific to the survey we can start that here...
      fam <- "beta"
      #control.fam <- list(control.link = list(model="log"))
    } # end if(surveys[i] == 'scallop' && !species[s] %in%  c("cod_PA","yt_PA")) 
    
    # If we have the other surveys and aren't into the scallop
    if(surveys[i] != 'scallop' && grepl("PA",species[s])) 
    {
      # Now lets get our input data sorted
      # Let's select the data for the particular survey of interest
      dat <- dat.final[dat.final$survey == surveys[i],]
       # Rename the varialbe of interest to "response"
      if(grepl("cod", species[s])) resp <- "cod_PA"
      if(grepl("yt", species[s])) resp <- "yt_PA"
      response <- which(names(dat) == resp)
      names(dat)[response] <- "response"
      # Lets log transform and center our depth data. loyd of transforming needed to get it back to a real number!
      dat$depth_log <- log(-dat$comldepth)
      dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
      # Get the location of our data...
      loc <- cbind(dat$X,dat$Y)
      #loc <- loc.gf
      mesh <- mesh.gf
      range <- range.gf
      #We also need to decide Observation Likelihood
      fam <- "binomial"
    } # end if(surveys[i] != 'scallop' && species[s] %in%  c("cod_PA","yt_PA")) 

    # now only run the rest of this script if we have data
    if((surveys[i] == 'scallop' && !grepl("PA",species[s])) ||
       (surveys[i] != 'scallop' && grepl("PA",species[s])))
    {
      # The amount of data we have
      N = nrow(dat)
      # For both the beta and binomial families we'll need to determine the number of trials.
      Ntrials <- 1 # For each record there is only 1 trial.  TRhese are Bernoulli trials...
      
      # For both our scenarios we are going to be using the logit model (note that this isn't scrictly necessary to write as the logit is the
      # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
      control.fam = list(control.link=list(model="logit"))
      # Now make the A matrix, combination of your mesh and locations of our survey
      A <- inla.spde.make.A(mesh, loc)
      dim(A)
       #table(eras) we should have a decent number of samples for each era.
      # While I have my range for my spatial priors I don't have my sigma or the probabilites for the prirors
      # The priors here can be pretty informative, the SPDE approximation improves if the corrleation length (i.e. roughly the range) of the process
      # is similar, but larger, than the maximum edge length ofthe mesh
      # so here we define our spde

      spde <- inla.spde2.pcmatern(mesh,    
                                  prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                  prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
      
      
      # and now we define the spatial random field.
      w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde)
      
      # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
      # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
      # certainly isn't entirely clear to me!
      #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
      
      # For all random walk models I use inla.group to group the data so that the minimum difference
      # between values is greater than the threshold for the random walk models, these groups are very
      # fine and really just bin things that are essentially the same, tested with 75 groups for depth and 
      # this doesn't really do anything to the data as the data are vitually identical but allows for 
      # the random walk smoother to do its thing
      dat$depth_cen_g <- inla.group(dat$depth_cen,n=75)
      # Only do up the PC's for the Groundfish models, use 150 for these data.
      if(surveys[i] != 'scallop' && grepl("PA",species[s]))
      {
          dat$PC1_g <- inla.group(dat$PC1,n=150)
          dat$PC2_g <- inla.group(dat$PC2,n=150)
          dat$PC3_g <- inla.group(dat$PC3,n=150)
          dat$PC4_g <- inla.group(dat$PC4,n=150)
          for(v in 1:length(vif.variables)) dat[paste0(vif.variables[v],"_stan")] <- scale(dat[,vif.variables[v]],)
          for(v in 1:length(vif.variables)) dat[paste0(vif.variables[v],"_g")] <- inla.group(dat[,paste0(vif.variables[v],"_stan")],n=100)
          # Let's log transform the slope as it is pretty skewed
          dat$comlslope_log <- log(dat$comlslope)
          dat$comlslope_stan <- scale(dat$comlslope_log)
          dat$comlslope_g <- inla.group(dat$comlslope_stan,n=100)
          # There is the odd point in the complexity data that is also causing some grief, looks much nicer log transformed
          # even though it is simply bounded by 0,1
          dat$complexity_log <- log(dat$complexity)
          dat$complexity_stan <- scale(dat$complexity_log)
          dat$complexity_g <- inla.group(dat$complexity_stan,n=100)
          # The phosphate data need to be binned a bit more tightly
          dat$phos_avg96_g <- inla.group(dat$phos_avg96_stan,n=80)
      } # end if(surveys[i] != 'scallop' && grepl("PA",species[s]))
      # First we need to make the model matrix, this needs to align with our formula below, this doesn't strictly need to be 
      # done unless we have a categorical covariate
      options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
      # The nice thing here is that we can make this a complex as we want and just run submodels from within this model
      # structure.
      # For the groundfish tihs is our matrix
      if(surveys[i] != 'scallop' && grepl("PA",species[s]))
      {
        # This gets rid of the intercept in the model matrix, which we want as we specify the intercept seperately in the model.
        X.matrix <- model.matrix(~  0 +PC1_g + PC2_g + PC3_g + PC4_g + depth_cen_g + botstr_wt_g + 
                                       complexity_g + comlaspect_g+ Mud_g+ sal_avg96_g + sal_rg96_g+ phos_avg96_g+ comlslope_g +sand_g+sst_avg_g+SEDNUM_g+strat96_g +    
                                       FID_GMAINE_median_g +sil_avg96_g+ chl_rg_g, data = dat)
        # And then make a covariate matrix
        X <- data.frame(PC1_g =              X.matrix[,1],
                        PC2_g =              X.matrix[,2],
                        PC3_g =              X.matrix[,3],
                        PC4_g =              X.matrix[,4],
                        depth_cen_g =        X.matrix[,5],
                        botstr_wt_g =        X.matrix[,6],
                        complexity_g =       X.matrix[,7],
                        comlaspect_g=        X.matrix[,8],
                        Mud_g=               X.matrix[,9],
                        sal_avg96_g =        X.matrix[,10],
                        sal_rg96_g=          X.matrix[,11],
                        phos_avg96_g=        X.matrix[,12],
                        comlslope_g =        X.matrix[,13],
                        sand_g=              X.matrix[,14],
                        sst_avg_g=           X.matrix[,15],
                        SEDNUM_g=            X.matrix[,16],
                        strat96_g =          X.matrix[,17],
                        FID_GMAINE_median_g =X.matrix[,18],
                        sil_avg96_g=         X.matrix[,19],
                        chl_rg_g =           X.matrix[,20])
      }
      
      # For the scallop survey do this...
      if(surveys[i] == 'scallop' && !grepl("PA",species[s]))
      {
          X.matrix <- model.matrix(~  0 + depth_cen_g , data = dat)
          # And then make a covariate matrix
          X <- data.frame(depth_cen_g = X.matrix[,1])

      } # end  if(surveys[i] == 'scallop' && !grepl("PA",species[s]))
      
      # Make the stack for the spatial models without spatio-temporal correlation
      stk = inla.stack(tag="est",
                           data=list(y = dat$response, link=1L),
                           effects=list(intercept = rep(1, nrow(dat)), 
                                        X = X,
                                        w = w.index),
                           A = list(1,1,A))

      # Now let's make our formula, intercept with each observation set as a random variable, this makes 
      # As soon as you make a spatial model make your own intercept.  Here is an initial suite of models I like...
      intercept <- 1 # intercept
      # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
      # overfit less, to do this I need to bin the covariates using the inla.group function below, problem is 
      # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
      # and he recommends these priors to make sure it doesn't get too funky
      U <- 0.5
      hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
      
      # every time I want to remove all model names so these are unique for each run
      rm(list = ls()[grep("model.",ls())])
      # for convience I'm going to call these formulas models..
      model.intercept <- y ~ 0 + intercept + f(w,model=spde)
      model.depth <-     y ~ 0 + intercept + f(depth_cen_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
      if(surveys[i] != 'scallop' && grepl("PA",species[s]))
      {
        # Now the models
        model.pc1 <-              y ~ 0 + intercept + f(PC1_g, model = "rw2", hyper = hyp.rw2) + f(w,model=spde)
        model.pc2 <-              y ~ 0 + intercept + f(PC2_g,       model = "rw2", hyper = hyp.rw2) + f(w,model=spde)
        model.pc3 <-              y ~ 0 + intercept + f(PC3_g,       model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.pc4 <-              y ~ 0 + intercept + f(PC4_g,       model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.complexity <-       y ~ 0 + intercept + f(complexity_g,model = "rw2", hyper = hyp.rw2) + f(w,model=spde)
        model.comlaspect <-       y ~ 0 + intercept + f(comlaspect_g,model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.Mud <-              y ~ 0 + intercept + f(Mud_g,       model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.sal.avg <-          y ~ 0 + intercept + f(sal_avg96_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.sal.rg <-           y ~ 0 + intercept + f(sal_rg96_g,       model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.phos.avg <-         y ~ 0 + intercept + f(phos_avg96_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.slope <-            y ~ 0 + intercept + f(comlslope_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.sand <-             y ~ 0 + intercept + f(sand_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.sst <-              y ~ 0 + intercept + f(sst_avg_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.Sednum <-           y ~ 0 + intercept + f(SEDNUM_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.strat <-            y ~ 0 + intercept + f(strat96_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.gmaine <-           y ~ 0 + intercept + f(FID_GMAINE_median_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.sil <-              y ~ 0 + intercept + f(sil_avg96_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.chl.rg <-           y ~ 0 + intercept + f(chl_rg_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
      }
     
      # How many different models do we have
      n.mods <- length(grep("model.",ls()))
      mod.names <- ls()[grep("model.",ls())]

      # Let's giver, make the spatial model.
      # Now we can loop this to run our different PCA and depth models
      for(m in 1:n.mods)
      {
        run.name <- paste0(species[s]," ", surveys[i]," survey ",mod.names[m])
        r.out <- inla(get(mod.names[m]), family=fam, data = inla.stack.data(stk),
                      control.predictor=list(A=inla.stack.A(stk)),
                      #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                      #verbose=TRUE,
                      control.compute = list(dic=T,waic = T)) 
        #} # end  if(run.name !="rec_stan_ly scallop survey model.intercept")
          
          # This one recruit model is not converging, this fixed it, but will lead to some potential bias in the field.
          # if(run.name =="rec_stan_ly scallop survey model.intercept" )
          # {
          # r.out <- inla(get(mod.names[m]), family=fam, data = inla.stack.data(stk),
          #               control.predictor=list(A=inla.stack.A(stk)),
          #               # Added the cmin=0 based on Harvard Rue suggestion here... https://groups.google.com/forum/#!topic/r-inla-discussion-group/hDboQsJ1Mls
          #               # From the notes...  cmin is the minimum value for the negative Hessian from the likelihood. 
          #               #Increasing this value will stabalise the optimisation but can introduce bias in some estimates unless -Inf is used. 
          #               #control.inla=list(cmin = 0 ), 
          #               control.compute = list(dic=T,waic = T)) 
          # } # end if(run.name =="rec_stan_ly scallop survey model.intercept" )
          # 
        mo.out <- data.frame(fitted = r.out$summary.fitted.values[1:N,"mean"] , # The expected values can be found with this
                            resid = dat$response - r.out$summary.fitted.values[1:N,"mean"],
                            response = dat$response,
                            depth_cen = dat$depth_cen)
        
        # For the non-scallop data we add in the PC scores, these are the original values calculated, not the grouped values
        # we used in the model
        if(surveys[i] != 'scallop' && grepl("PA",species[s]))
        {
          mo.out$PC1              = dat$PC1
          mo.out$PC2              = dat$PC2 
          mo.out$PC3              = dat$PC3
          mo.out$PC4              = dat$PC4
          mo.out$depth_cen        = dat$depth_cen
          mo.out$botstr_wt        = dat$botstr_wt
          mo.out$complexity       = dat$complexity
          mo.out$comlaspect       = dat$comlaspect
          mo.out$Mud              = dat$Mud
          mo.out$sal_avg          = dat$sal_avg
          mo.out$sal_rg           = dat$sal_rg
          mo.out$phos_avg         = dat$phos_avg
          mo.out$comlslope        = dat$comlslope
          mo.out$sand             = dat$sand
          mo.out$sst_avg          = dat$sst_avg
          mo.out$SEDNUM           = dat$SEDNUM
          mo.out$strat96          = dat$strat96
          mo.out$FID_GMAINE_median= dat$FID_GMAINE_median
          mo.out$sil_avg96        = dat$sil_avg96
          mo.out$chl_rg           = dat$chl_rg
          
        }
        mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
        mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
        
        md.out <- data.frame(dic = r.out$dic$dic, 
                               dic.p.eff = r.out$dic$p.eff,
                               waic = r.out$waic$waic, 
                               waic.p.eff = r.out$waic$p.eff,
                               Dispersion = sum()) 
        md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
        
        
        res[[run.name]] <- r.out
        #res[[mod.names[m]]]$model <- mod.names[m]
        mod.output[[run.name]] <- mo.out
        mod.output[[run.name]]$model <- run.name
        mod.output[[run.name]]$species <- species[s]
        mod.output[[run.name]]$survey <- surveys[i]
        mod.output[[run.name]]$model.id <- mod.names[m]
        
        mod.diagnostics[[run.name]] <- md.out
        mod.diagnostics[[run.name]]$model <- run.name
        mod.diagnostics[[run.name]]$species <- species[s]
        mod.diagnostics[[run.name]]$survey <- surveys[i]
        mod.diagnostics[[run.name]]$model.id <- mod.names[m]
        
        rand.field[[run.name]] <- r.out$summary.random$w # THis will contain mutliple random fields for the spatio-temporal models.
        rand.field[[run.name]]$model <- run.name
        rand.field[[run.name]]$species <- species[s]
        rand.field[[run.name]]$surveys <- surveys[i]
        rand.field[[run.name]]$model.id <- mod.names[m]

        # Stick a print in here so we know this is moving forward
        messy <- print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
        # Write a message to the ESS so I can see progress remotely....
        fileConn<-file(paste0(direct.proj,"Results/status.txt"))
        writeLines(messy, fileConn)
        close(fileConn)
        
      } # end for(m in 1:n.mods)
      
      # I also want to save the dat object so I can use it below, and the w indices to make pulling out the data easier for the spatio-temporal models.
      dat.list[[paste(species[s],surveys[i],sep="_")]] <- dat
      w.index.list[[paste("Base",species[s],surveys[i],sep="_")]] <- w.index
      # Save the image on the way through just in case the computer decides to shut down...
      #save.image(paste0(direct.proj,"Results/INLA_output_",species[s],"_",surveys[i],".RData"))
    } #end the massive if statement to just run the model for scallop with scallop data, and survey with groundfish data.

  }# end for(i in 1:n.surveys)
  # Save the image on the way through just in case the computer decides to shut down, worst case here I lose about half a day of modelling.
  #save.image(paste0(direct.proj,"Results/INLA_output_spatial_",species[s],".RData"))
} # end for(s in 1:n.species)

# The results of the INLA fun
#save.image(paste0(direct.proj,"Results/INLA_spatial_output_scal_2016.RData"))
load("Y:/Projects/GB_time_area_closure_SPERA/Results/INLA_spatial_output.RData")


# Stitch together the model outputs
#mod.out <- do.call("rbind",mod.output) # This won't work as the PC's don't exist in the scallop data...
mod.diag <- do.call("rbind",mod.diagnostics)
# Now we actually want to melt these so that we have the data in long form for ggplot
mod.diag <- reshape2::melt(mod.diag,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
# What models do we have issues with...


# So now I can make ggplots of everything, probably could pull this off in some crazy nexted object, but I don't want too..
# I'm going to save the ggplots and plot them later
diag.plot <- NULL
#GOM.pec <- NULL
#GB.pec <- NULL
for(s in 1:num.species) 
{
  # Loop through each of the surveys
  for(i in 1:num.surveys) 
  {
    # now only run the rest of this script if we have data
    if((surveys[i] == 'scallop' && !grepl("PA",species[s])) || (surveys[i] != 'scallop' && grepl("PA",species[s])))
    {
      
      # Now we can make the figures we need for each model, first we want to get the names of each run we did and also the number of models this includes
      run.names <- names(mod.output)[grep(paste(species[s],surveys[i],sep=" "),names(mod.output))]
      n.mods <- length(run.names)
      # Now grab the model names, fortunately I made the model names with a '.' so this is pretty easy.
      #The first ^ matches the beginning of the string, but the next one in square brackets negates, so it matches all characters EXCEPT "." - 
      #finally the star means match that any number of times - so match everything from the start up until (but not including) the first dot. 
      #Second argument then replaces that match with an empty string
      mod.names <- sub("^[^.]*", "", run.names)
      # and drop the initial period...
      mod.names <- substr(mod.names,2,20)
      
      # First let's plot our model diagnostics between the models
      # I'd like to add an h-line for the intercept model, then a line at DIC/WAIC -2 and DIC/WAIC -10
      # to indicate which models appear to be an improvement over the intercept only model.
      int.res <- mod.diag[mod.diag$model.id == "model.intercept",]
      min.plus <- int.res %>%
        group_by(diag,species,survey) %>% summarise(min2 = data - 2, min10 = data - 10) 
      
      # The original way I did it which looked for min model and added 2 and 10 to that model.
      #min.plus <- mod.diag %>%
      #  group_by(diag,species,survey) %>% summarise(min2 = min(data) + 2, min10 = min(data) + 10) 
      
      
      # Don't want to do the above for the dispersion or dic's so makes those NA's
      min.plus[min.plus$diag %in% c("dic.p.eff","waic.p.eff","Dispersion"),c("min2","min10")] <- NA
      # Get the diagnostics data together...
      diag.dat <- mod.diag[mod.diag$species == species[s] & mod.diag$survey == surveys[i],]
      plt.min.plus <- min.plus[min.plus$species == species[s] & min.plus$survey == surveys[i],]
      # Get some much more simple model names
      diag.dat$plt.name <- substr(sub("^[^.]*", "", diag.dat$model),2,20)
      # First we can make our diagnostic summary plot
      pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Diagnositc_plots_spatial_",species[s],"_",surveys[i],".pdf"),width=14,height = 8)
      par(mfrow = c(1,1))
      diag.plot[[paste(species[s],surveys[i],sep="_")]] <- ggplot(diag.dat, aes(data,plt.name)) + geom_point() +  
        geom_vline(data = plt.min.plus, aes(xintercept = min2),color="blue",linetype = "dashed",size=1) + 
        geom_vline(data = plt.min.plus, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
        facet_wrap(~diag,scales = 'free_x') + xlab("") + ylab("") + ggtitle(paste("Diagnostics", species[s], surveys[i],sep=" "))+
        theme(axis.text=element_text(size=8),axis.title=element_text(size=14,face="bold")) 
      # Now pring the diagnostics for each survey/species combo
      #pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Diagnositc_spatial_plots.pdf"),width=14,height = 8)
      print(diag.plot[[paste(species[s],surveys[i],sep="_")]])
      dev.off()
      
     
        
      } #end for(m in 1:n.mods)
      dev.off()
      
    } # end if((surveys[i] == 'scallop' && !grepl("PA_Adu",species[s])) || (surveys[i] != 'scallop' && grepl("PA_Adu",species[s])))
  } # end for(i in 1:num.surveys) 
} # end for(s in 1:num.species) 




















