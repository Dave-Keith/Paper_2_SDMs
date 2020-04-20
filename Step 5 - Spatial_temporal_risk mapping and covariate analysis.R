# Here's what I hope is the script that contains the final spatio-temporal INLA models used for the analysis reside, here's the idea...

# 1:  Here I'm adding in a spatio-temporal field (AR1) to the mix.  To start I'm going to look at a 10 and 5 year field to see if they
#     are improvements on our initial spatial models
# 2:  Once we sort that out I'll look at the best "spatial" models and add in any environmental covariates that were identified there
#     and see how these improve the models
# 3:  The covariates selected for this analysis are the ones that improved both the WAIC and DIC by > 2 over the intercept only
#     spatial model in 2 or more of the datasets.  This model selection retained SST, depth, SEDNUM, and chl.rg only.  We do model selection
#     for all combinations on this subset of covaraites.
# 4:  For 1-2 I will only do this with the full data-sets for YT and COD.  Once we have full models chosen I'll also run the analysis
#     on the Canadian subset of data to overlap that with our scallop maps.
# 5:  This will also produce video for the entire region for the best models (Cod is 5 years, YT probably 3, but depends)
# 6:  I will also start to explore the covariate effects for the best models.
#############################################

# Aside:  Worth nothing that b/c we are using presence absence data we are really doing a Bernoulli (i.e. N trials = 1) model.
# Spatial-temporal model
# We will now apply the model:

# WP_ti ~ Bernoulli(Pi_ti)
# E(WP_ti)   = Pi_ti
# var(WP_ti) = P_ti * (1 - Pi_ti)
#
#           exp(Intercept + Covariates_ti + v_ti)
# Pi_ti = ----------------------------------------
#          1 + exp(Intercept + Covariates_ti + v_ti)

# Where:   v_ti = phi * v_t-1,i + u_ti
# 
# u_ti ~ N(0, SIGMA)
# Use the SPDE approach to estimate SIGMA.

#############################################

# To do list:
# 8:  Take a look at the rw smoothers results and see if they make sense


### ISSUES RESOLVED

# 1:  Add in an ar1 process to the spatial field (at least for the ones that are spatial)
# 2:  Check on what is going on with the scallop maps, did I mess up my link or mess up the calcs
#     In fact I did neither, the nature of the data is so skewed that it is just ugly ugly shit
#     I have made a revision to the data so that the top 90% percentile is all treated as having an equal
#     probability of being fished.  I think because we're using a beta distribution this is might just work
#     out, but I may have introduced some bad behaviour into the model
# 6:  Add in a new covariates, do some variance inflation factor work to sort out what to narrow the list
#     down to.  There are around 12 covariates that we can look at once we pair the list down by using variannce
#     inflation factors, which basicially looks for correlation between variables.
# 4:  Reverse the era's, start at most recent year and walk it backwards.
# 3:  Add in a couple of more era options, first add in every 5 years and see if this is an improvment over the decadal pattern.
#     if it is better then we can try even fewer years (say 3 years), but if we go there only do it on
#     our preferred model from the 5 year run to save grief.
# 5:  Don't bother re-running the ly models, the Canadian groundfish models or the all year scallop biomass models.
#     The Canadian groundfish models we can run once we have figured out what covariates and best spatial model we want
# 7:  Make sure I standardize covariates, depth needs to be log transformed and it should work better!

################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
#----
rm(list=ls())
direct.fun <- "Y:/Offshore/Assessment/"
#direct.fun <- "D:/R/"
direct.proj <- "Y:/Projects/GB_time_area_closure_SPERA/"
#direct.proj <- "d:/r"

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
#inla.upgrade(testing = F)

# Bring in the functions we'll need
source(paste(direct.fun,"Assessment_fns/Maps/pectinid_projector.R",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/convert_coords.R",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/add_alpha_function.r",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/combine_shapefile_layers.R",sep=""))


# Here is the data we need, this comes from Step 3 INLA_mesh_for_gb_surveys_and_scallop_survey.R
load(paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))
load(paste0(direct.proj,"Data/INLA_meshes.RData"))

##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########
##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########


##########################  Section 2     Run the analyses   ##########################  Section 2     Run the analyses ###########################
############  Here's where we do the INLA'ing and get our model results #####################
##########################  Section 2     Run the analyses   ##########################  Section 2     Run the analyses ###########################
############ Start here for analysis, load in the data we need and go nuts...
#----
# Now I need to stick my UTM coordinates into the data
dat.final$X <- loc.gf@coords[,1]
dat.final$Y <- loc.gf@coords[,2]
# At this point I really start the analysis, and what I want to do is split up each survey, I might as well loop this, or at least allow for a loop
# so we can move through each survey easily, this will get complicated with a number of models for each survey, especially when I split out the time...
surveys <- unique(dat.final$survey)
num.surveys <- length(surveys)

# We are going to need to specify our sigma for each model.  From Zuur chapter 19 pages 414-416 he does give some guidance of the highest this should 
# ever be, think about your random spatial field, if we were just mapping the spatial field that field could range between the maximum and
# minimum value in the data, for our data we are following the logit distribution, so we can play around a bit with this, think about how variable
# you expect the surface to be on it's own, for example if expecting values to be between 0.2 and 0.8 what would sigma be (around 0.75)
# sigma <- seq(0,4,by=0.25)
# # Assuming the field is 0 mean a 0.5 would let the field itself vary between 0.27 and 0.72
# CI <- cbind(exp(-1.96*sigma)/ (1+exp(-1.96*sigma)),exp(1.96*sigma)/ (1+exp(1.96*sigma)),sigma)
# CI
# Zuur tends to us sigma = 0.5 for these in the logit cases with a low probability that the sigma is above this value
# So this will tend to make the surface relatively smooth, but we will want to compare this with that the model estimates
sigma <- 0.5
s.alpha <- 0.05
# Similarly for the range, we've already settled on a value when making the mesh previously following these same sections of zuur
# But here for the range our alpha is the likelihood that the range is smaller than our set value, again Zuur tends
# to make this small to avoid overfitting
r.alpha <- 0.05

# So what we want to do 

# I need a loop for the number of "species" i'm looking at, Now all I care about are Cod and Yellowtail, everything 
# else is taken care of in Step 4.
species <- c("cod_PA","yt_PA") 
#species <- c("cod_PA_can","yt_PA_can") # These take the cod and YT data that are on the Canadian side of GB. 
num.species <- length(species)
# Need to initialize some objects for later
res <- NULL
#mod.diag <- NULL
mod.output <- NULL
rand.field <- NULL
mod.diagnostics <- NULL
dat.list <- NULL
w.index.list <- NULL


################  HEADS UP!!! Set the st.mods which is the numbers of years in each spatial field, for now 3,5, and 10 are supported.
#### If you are running all the models do this
#st.mods <- c(3,5,10)
### If you are running st.10 models do this first...
#st.mods <- 10
### If you are running the st.5 and/or st.3 models do this first
st.mods <- 3
#st.mods <- c(5)

for(st in 1:length(st.mods))
{
  for(s in 1:num.species) 
  {
    # Loop through each of the surveys
    for(i in 1:num.surveys) 
    {
        # Now lets get our input data sorted
        # Let's select the data for the particular survey of interest
        dat <- dat.final[dat.final$survey == surveys[i],]
        # Rename the varialbe of interest to "response"
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
        # If looking at all the data use the gf mesh
        if(!grepl("_can",species[s])) 
        {
          mesh <- mesh.gf
          range <- range.gf
        }
        # If just looking at the canadian data use the scallop mesh.
        if(grepl("_can",species[s])) 
        {
          mesh <- mesh.sc
          range <- range.sc
        } # end if(!grepl("_can",species[s])) 
        
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
        if(st.mods[st] == 10) eras <- as.numeric(dat$years_10)
        # Now the 3 and 5 year model
        if(st.mods[st] ==5) eras <- as.numeric(dat$years_5)
        if(st.mods[st] == 3) eras <- as.numeric(dat$years_3)
          
        era.names <- unique(eras)
        n.eras <- length(unique(eras))
        if(st.mods[st] != 3) A.era <- inla.spde.make.A(mesh, loc,repl = eras)
        if(st.mods[st] == 3) A.era <- inla.spde.make.A(mesh, loc,group = eras,n.groups =n.eras)
        
        # While I have my range for my spatial priors I don't have my sigma or the probabilites for the prirors
        # The priors here can be pretty informative, the SPDE approximation improves if the corrleation length (i.e. roughly the range) of the process
        # is similar, but larger, than the maximum edge length ofthe mesh
        # so here we define our spde
  
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
        
   
        # For all random walk models I use inla.group to group the data so that the minimum difference
        # between values is greater than the threshold for the random walk models, these groups are very
        # fine and really just bin things that are essentially the same/
        # First up we only need to do this with the variables that appear interesting from
        # the spatial model runs.  These likely differ by species so might need a YT and a Cod list
        # guess it depends if any of them come out from the modelling as significant?
        dat$depth_cen_g <- inla.group(dat$depth_cen,n=100)
        dat$chl_rg_cen_g <- inla.group(dat$chl_rg_cen,n=100)
        dat$sst_avg_cen_g <- inla.group(dat$sst_avg_cen,n=100)
        
        # Only do up the PC's for the Groundfish models, use 150 for these data.
        #dat$PC1_g <- inla.group(dat$PC1,n=150)
        
        
        
        # First we need to make the model matrix, this needs to align with our formula below, this doesn't strictly need to be 
        # done unless we have a categorical covariate
        options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
        # The nice thing here is that we can make this a complex as we want and just run submodels from within this model
        # structure.
        # Now we need to make a model matrix for the covariates that come out as useful
        # First I want to convert SEDNUM to a factor for this analysis.
        X.matrix <- model.matrix(~ 0+ depth_cen_g +  sst_avg_cen_g + chl_rg_cen_g +  fSEDNUM, data = dat)
        # And then make a covariate matrix
        X <- data.frame(depth =        X.matrix[,1],
                        sst   =        X.matrix[,2],
                        chl    =       X.matrix[,3],
                        fsed_3     =   X.matrix[,4],
                        fsed_4     =   X.matrix[,5])
  
        # Next up we make the stack...
        # I need a stack, probably should eventually split this into an estimation, validation and prediction stack, but for now
        # will stick with the one estimation stack....
        # Make the stack for the spatial models without spatio-temporal correlation
         stk = inla.stack(tag="est",
                         data=list(y = dat$response, link=1L),
                         effects=list(intercept = rep(1, nrow(dat)), 
                         X = X,
                         w = w.index),
                         A = list(1,1,A.era))
        
        
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
        
        # The model formulas are identical for the 5 and 10 models, this is just how I'm making the model run through the different era scenarios
        # I suggest I just start these by comparing an intercept only model with the "full" model which will have additive
        # terms for all the covariates that are deemed "interesting" by the spatial only model.  Once we
        # select the best spatio-tempral model from that then we can do some model selection on that model.  So if we have 4 variables
        # we start with 4 random walks, we'll then do some relatively painful (likely at least) model selection by
        # droppping 1 term at a time to see it's effect on the model.  Right now it looks like depth, sst, complexity, and strata may
        #
        
        # These are models we'll run in all scenarios for now.
        if(st.mods[st] != 3) model.int <-          y ~ 0 + intercept + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
        if(st.mods[st] == 3) model.int <-          y ~ 0 + intercept + f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The group is found inside w.index.X
        
        # For the canaidan subset of data I only run the intercept model so skip all this fun
        if(!grepl("_can",species[s])) 
        {
          # This is a really good model but not the best model for YT (SED just slightly improves things)
          # So we run this for the 5 and 10 sceanrios and for the cod 3 scenario (but that's done below)
          if(st.mods[st] !=3) model.depth.sst <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          # For the cod data give this model a whirl to see if it helps.
          if(species[s] == "cod_PA"&& st.mods[st] ==5) model.depth.sst.chl <-  y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                                                                     f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                                                                     f(chl , model = "rw1", hyper = hyp.rw2)  + 
                                                                                                     f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          
          # For yellowtail toss in these 2 models for comparison with 5.
          if(species[s] == "yt_PA" && st.mods[st] ==5)
          {
          model.depth.sed <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                      fsed_3 + fsed_4 +
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          # Add in the sst to this model, it might be useful.
          model.depth.sed.sst <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                      fsed_3 + fsed_4 +
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          } # end if(species[s] == "yt_PA" && st.mods[st] != 10)
          
          # Now instead of giving each "era" an independent random field, perhaps a correlated random field makes more sense
          # We likely only have enough data to do this with the 3 year random fields.
          if(species[s] == "cod_PA" && st.mods[st] == 3) 
          {
            model.depth.sst <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                        f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                        f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
            
            # model.depth.sst.ar1 <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            #                                           f(sst , model = "rw1", hyper = hyp.rw2)  + 
            #                                            f(w,model=spde,group = w.group,control.group = list(model = 'ar1')) # The w.repl is found inside w.index.X
          } # end  if(species[s] == "cod_PA" && st.mods[st] == 3) 
          
          if(species[s] == "yt_PA" && st.mods[st] == 3) 
          {
            model.depth.sed.sst <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                            f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                            fsed_3 + fsed_4 +
                                                            f(w,model=spde,group = w.group,control.group = list(model = 'iid')) # The w.repl is found inside w.index.X
            
            # model.depth.sed.sst.ar1 <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
            #                                         f(sst , model = "rw1", hyper = hyp.rw2)  + 
            #                                         fsed_3 + fsed_4 +
            #                                         f(w,model=spde,group = w.group,control.group = list(model = 'ar1')) # The w.repl is found inside w.index.X
          } # end if(species[s] == "yt_PA" && st.mods[st] == 3) 
          
          
          if(st.mods[st] == 10)
          {
          model.int <-          y ~ 0 + intercept + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          # The single models
          model.depth <-          y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          model.sst <-          y ~ 0 + intercept + f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          model.sed <-          y ~ 0 + intercept + fsed_3 + fsed_4 +
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          model.chl <-          y ~ 0 + intercept + f(chl , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          # Now all the possible 2 combo models, first the depths
          model.depth.sst <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          model.depth.sed <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                      fsed_3 + fsed_4 +
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          model.depth.chl <-      y ~ 0 + intercept + f(depth , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(chl , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          
          # Now the 2 sst without depth
          model.sst.sed <-      y ~ 0 + intercept + f(sst, model = "rw1", hyper = hyp.rw2)  + 
                                                      fsed_3 + fsed_4 +
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          model.sst.chl <-      y ~ 0 + intercept + f(sst , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(chl , model = "rw1", hyper = hyp.rw2)  + 
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          # Finally the sediment chl model
          model.sed.chl <-      y ~ 0 + intercept + f(chl , model = "rw1", hyper = hyp.rw2)  + 
                                                      fsed_3 + fsed_4 +
                                                      f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.X
          } # end if(st.mods[st] == 10)
        } # end if(!grepl("can",species[s])
        # How many different models do we have
        n.mods <- length(grep("model.",ls()))
        mod.names <- ls()[grep("model.",ls())]
  
        # Let's giver, make the spatial model.
        # Now we can loop this to run over each model and for each stack, 
        # I was going to run a stack loop, but that is amazingly slow, so instead I'm going to do the model selection with the
        # 10 year field, and using the best model from that I'll then run a one off script for the st.5
        # model and see which I prefer....
        
          #if(st == 2) {stk <- skt.5; st.mod <- "st.5"}
          for(m in 1:n.mods)
          {
            run.name <- paste0(species[s]," ", surveys[i]," survey ",mod.names[m],"_st_",st.mods[st])
            # Now we run the models
            r.out <- inla(get(mod.names[m]), family=fam, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk)),
                          #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                          #verbose=TRUE,
                          control.compute = list(dic=T,waic=T)) 
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
                                   Dispersion = sum()) 
            md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
            
            
            res[[run.name]] <- r.out
            #res[[mod.names[m]]]$model <- mod.names[m]
            mod.output[[run.name]] <- mo.out
            mod.output[[run.name]]$model <- run.name
            mod.output[[run.name]]$species <- species[s]
            mod.output[[run.name]]$survey <- surveys[i]
            mod.output[[run.name]]$model.id <- mod.names[m]
            mod.output[[run.name]]$st.era <- st.mods[st]
            
            mod.diagnostics[[run.name]] <- md.out
            mod.diagnostics[[run.name]]$model <- run.name
            mod.diagnostics[[run.name]]$species <- species[s]
            mod.diagnostics[[run.name]]$survey <- surveys[i]
            mod.diagnostics[[run.name]]$model.id <- mod.names[m]
            mod.diagnostics[[run.name]]$st.era <-  st.mods[st]
            
            rand.field[[run.name]] <- r.out$summary.random$w # THis will contain mutliple random fields for the spatio-temporal models.
            rand.field[[run.name]]$model <- run.name
            rand.field[[run.name]]$species <- species[s]
            rand.field[[run.name]]$surveys <- surveys[i]
            rand.field[[run.name]]$model.id <- mod.names[m]
            rand.field[[run.name]]$st.era <- st.mods[st]
    
            # Stick a print in here so we know this is moving forward
            messy <- print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
            # Write a message to the ESS so I can see progress remotely....
            fileConn<-file(paste0(direct.proj,"Results/status.txt"))
            writeLines(messy, fileConn)
            close(fileConn)
          } # end for(m in 1:n.mods)
            #save the w indices to make pulling out the data easier for the spatio-temporal models.
            w.index.list[[paste("Spatio_temporal",species[s],surveys[i],st.mods[st],sep="_")]] <- w.index
            
          
        # I also want to save the dat object so I can use it later, just needs to be species and surveys, 
        dat.list[[paste(species[s],surveys[i],sep="_")]] <- dat
        # Save the image on the way through just in case the computer decides to shut down...
        #save.image(paste0(direct.proj,"Results/INLA_st_output_",species[s],"_",surveys[i],".RData"))
    }# end for(i in 1:n.surveys)
    # Save the image on the way through just in case the computer decides to shut down, worst case here I lose about half a day of modelling.
    # This thing is gonna be big...
    #save.image(paste0(direct.proj,"Results/INLA_output_spatial_temporal",species[s],".RData"))
  } # end for(s in 1:n.species)
} # for(st in 1:length(st.mods))
  
  
# The results of the INLA fun
#save.image(paste0(direct.proj,"Results/INLA_st_output.RData"))
# Use this for all the models
#save.image(paste0(direct.proj,"Results/INLA_st_",paste(st.mods,collapse = "_"),"_output.RData"))

# If just running the Canadian model save this smaller object
#save.image(paste0(direct.proj,"Results/INLA_Can_st_",paste(st.mods,collapse = "_"),"_output.RData"))
#load(paste0(direct.proj,"Results/INLA_st_output.RData"))

# And I want to make a mesh list so I have that easily accessed...
mesh.list <- mesh



# Stitch together the model outputs
#mod.out <- do.call("rbind",mod.output) # This won't work as the PC's don't exist in the scallop data...
mod.diag <- do.call("rbind",mod.diagnostics)
r.field <- do.call("rbind",rand.field)
# Now we actually want to melt these so that we have the data in long form for ggplot
mod.diag <- reshape2::melt(mod.diag,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
# What models do we have issues with...
mod.diag[mod.diag$data == Inf,]
# Output the mod.diag into a csv so I can tidy up any of the models that didn't converge the first time.  Most of the issues are with the depth models and
# the biggest problems being the YT and the data groundfish data restricted to last year
write.csv(mod.diag,paste0(direct.proj,"Results/INLA_st_models_that_did_not_converge_first_time.csv"))


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
      # First let's plot our model diagnostics between the models
      # I'd like to add an h-line at the min + 2 and min + 10 for the dic and waic's (I'll have to add it for all but whatever)
      min.plus <- mod.diag %>%
        group_by(diag,species,survey) %>% summarise(min2 = min(data) + 2, min10 = min(data) + 10) 
      # Don't want to do the above for the dispersion or dic's so makes those NA's
      min.plus[min.plus$diag %in% c("dic.p.eff","waic.p.eff","Dispersion"),c("min2","min10")] <- NA
      # Get the diagnostics data together...
      diag.dat <- mod.diag[mod.diag$species == species[s] & mod.diag$survey == surveys[i],]
      plt.min.plus <- min.plus[min.plus$species == species[s] & min.plus$survey == surveys[i],]
      # First we can make our diagnostic summary plot
      #pdf(file = paste0(direct.proj,"/Results/Diagnositc_plots",species[s],surveys[i],".pdf"),width=14,height = 8)
      par(mfrow = c(1,1))
      diag.plot[[paste(species[s],surveys[i],sep="_")]] <- ggplot(diag.dat, aes(data,model.id)) + geom_point() +  
        geom_vline(data = plt.min.plus, aes(xintercept = min2),color="blue",linetype = "dashed",size=1) + 
        geom_vline(data = plt.min.plus, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
        facet_wrap(~diag,scales = 'free_x') + xlab("") + ylab("") + ggtitle(paste("Diagnostics", species[s], surveys[i],sep=" "))+
        theme(axis.text=element_text(size=8),axis.title=element_text(size=14,face="bold")) 
      # Now pring the diagnostics for each survey/species combo
      pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Diagnositc_plots_st_",st.mods,".pdf"),width=14,height = 8)
      print(diag.plot)
      dev.off()
   
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
      # First we make some residual plots for each model.
      # First I make some spatial residual and fitted plots which are interesting, no obvious weird spatial patterns is nice.    
      pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Residuals/Resid_fitted_st_",st.mods,"_",species[s],"_",surveys[i],".pdf"),width=11,height = 11)
      par(mfrow = c(2,2),mar=c(4,4,2,5))  
      for(m in 1:n.mods)
      {
        run.name <- run.names[m]
        # If it is a spatio-temporal model get this w index
        w.ind <- w.index.list[[paste("Spatio_temporal",species[s],surveys[i],st.mods[st],sep="_")]]
        
        # Residual plots for each model, probably would be better to make this a 
        # Spatial fitted and residual plots
        quilt.plot(dat.list[[paste(species[s],surveys[i],sep="_")]]$X, 
                   dat.list[[paste(species[s],surveys[i],sep="_")]]$Y,mod.output[[run.name]]$resid.stan,
                   main=paste("Residuals",mod.names[m]))
        quilt.plot(dat.list[[paste(species[s],surveys[i],sep="_")]]$X, 
                   dat.list[[paste(species[s],surveys[i],sep="_")]]$Y,mod.output[[run.name]]$fitted,
                   main = paste("Fitted",mod.names[m]))
      } # end for(m in 1:n.mods)
      dev.off()
      
      # # Now make some residual plots, b/c it's binomial there aren't a lot of great ones, but residuals v.s. covariates can't hurt...
      # pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Residuals/Resid_vs_covariates_st_",species[s],"_",surveys[i],".pdf"),
      #     width=11,height = 11,onefile=T)
      # for(m in 1:n.mods)
      # {
      #   run.name <- run.names[m]
      #   r.vs.d <- ggplot(mod.output[[run.name]], aes(depth_cen,resid.stan)) + geom_point() + geom_smooth() + 
      #                    ggtitle(paste("Resid vs depth - Model",mod.names[m])) + 
      #                    theme(axis.text=element_text(size=10),axis.title=element_text(size=10),plot.title = element_text(size=14)) 
      #   r.vs.pc1 <- ggplot(mod.output[[run.name]], aes(PC1,resid.stan)) + geom_point() + geom_smooth() + 
      #                    ggtitle("Resid vs PC1") + 
      #                    theme(axis.text=element_text(size=10),axis.title=element_text(size=10),plot.title = element_text(size=14)) 
      #   grid.arrange(r.vs.d,r.vs.pc1)
      # } # end for(m in 1:n.mods)
      # dev.off()
      
      
      # Now for the spatial and spatio-temporal plots...
      pdf(paste0(direct.proj,"Results/Figures/INLA/GOM_and_GB_spatial_fields_st_",st.mods[st],"_",species[s],"_",surveys[i],".pdf"), width = 22, height = 11,onefile = T)
      # plot 2 per page
      par(mfrow = c(1,2),mar=c(4,1,2,6))      
      for(m in 1:n.mods)
      {
        run.name <- run.names[m]
        print(run.name) # Just want to make sure it runs through all the models we want...
        # If it is a spatio-temporal model get this w index
        w.ind <- w.index.list[[paste("Spatio_temporal",species[s],surveys[i],st.mods[st],sep="_")]]
        # Here is what ya ne1ed to do to get the spatio-temporal field working.
        eras <- unique(w.ind$w.repl)
        if(st.mods[st] == 3)  eras <- unique(w.ind$w.group)
        n.eras <- length(eras)
        dat.eras <- eras
        # For the RV surve we want to add 2 to this as the eras go from 3-5 in the years_10 grouping...
        if(surveys[i] == "RV" && st.mods[st] == 10) dat.eras <- eras +2
        if(surveys[i] == "RV" && st.mods[st] == 5) dat.eras <- eras +4
        if(surveys[i] == "RV"&& st.mods[st] == 3) dat.eras <- eras + 6
 
        for (p in 1:n.eras)
        {
          # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
          # is always just our intercept.
          if(st.mods !=3) tmp.field <- data.frame(r.field.link = rand.field[[run.name]]$mean[w.ind$w.repl == eras[p]] + res[[run.name]]$summary.fixed$mean, 
                                  r.field.response = inv.logit(rand.field[[run.name]]$mean+ res[[run.name]]$summary.fixed$mean)[w.ind$w.repl == eras[p]],
                                  r.field.sd = rand.field[[run.name]]$sd[w.ind$w.repl == eras[p]],
                                  era = eras[p])
          
          if(st.mods ==3) tmp.field <- data.frame(r.field.link = rand.field[[run.name]]$mean[w.ind$w.group == eras[p]] + res[[run.name]]$summary.fixed$mean, 
                                                  r.field.response = inv.logit(rand.field[[run.name]]$mean+ 
                                                                              res[[run.name]]$summary.fixed$mean)[w.ind$w.group == eras[p]],
                                                  r.field.sd = rand.field[[run.name]]$sd[w.ind$w.group == eras[p]],
                                                  era = eras[p])
          
          # Get the points used for each of the models.
          if(st.mods[st] ==5)  dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_5 == dat.eras[p],]
          if(st.mods[st] ==10)  dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
          if(st.mods[st] ==3)  dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_3 == dat.eras[p],]
          name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
          loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
          coordinates(loc) <- ~X+Y
          proj4string(loc) <- proj4string(loc.sc)
          
          
          # Make sure we have the right mesh and locations for the model
          mesh.plt <- mesh.list
          # Let's clip the area, this really doesn't do anything as we clip to the mesh...
          con.hull <- chull(loc@coords)
          clp <- loc@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon
          # Make the plot of the whole GOM region
          pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                    dims=c(400,400), clip = clp,add_land =T,add_EEZ = "sounds grand",
                    colors = rev(magma(10)),add_obj = cod.closed.cells, c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                    repo='github')
          points(loc,cex=0.05,col=addalpha("black",0.25),pch=19)
    
          if(n.eras > 1) title(paste("Encounter Probability - ",name.era,mod.names[m]))
          
          # Same thing for GB
          # Make the plot of the whole GB region
          pecjector(area = "GB", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                    dims=c(400,400), clip = clp,add_land =T,add_EEZ = "sounds grand",
                    colors = rev(magma(10)),add_obj = cod.closed.cells, c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                    repo='github')
          points(loc@coords,cex=0.5,col=addalpha("black",0.25),pch=19)
          if(n.eras == 1) title(paste("Encounter Probability - All years",mod.names[m]))
          if(n.eras > 1) title(paste("Encounter Probability - ",name.era,mod.names[m]))
          
        } # end  for (p in 1:n.eras)
        
      } #end for(m in 1:n.mods)
      dev.off()
    } # end for(i in 1:num.surveys) 
} # end for(s in 1:num.species) 
  

# Now here are the subsets of data I want for the closure related side of things

cod.RV <- NULL
cod.RV <- list(res = res[["cod_PA RV survey model.int st.10"]],
               rand.field = rand.field[["cod_PA RV survey model.int st.10"]],
               mod.output = mod.output[["cod_PA RV survey model.int st.10"]],
               rand.field = mod.diagnostics[["cod_PA RV survey model.int st.10"]],
               mesh = mesh.gf)

yt.spring <- NULL
yt.spring <- list(res= res[["yt_PA nmfs-spring survey model.intercept st.10"]],
                  rand.field = rand.field[["yt_PA nmfs-spring survey model.intercept st.10"]],
                  mod.output = mod.output[["yt_PA nmfs-spring survey model.intercept st.10"]],
                  mod.diagnostics = mod.diagnostics[["yt_PA nmfs-spring survey model.intercept st.10"]],
                  mesh = mesh.gf)

cod.fall <- NULL
cod.fall <- list(res = res[["cod_PA nmfs-fall survey model.intercept st.10"]],
                 rand.field =rand.field[["cod_PA nmfs-fall survey model.intercept st.10"]],
                 mod.output= mod.output[["cod_PA nmfs-fall survey model.intercept st.10"]],
                 mod.diagnostics= mod.diagnostics[["cod_PA nmfs-fall survey model.intercept st.10"]],
                 mesh = mesh.gf)

yt.fall <- NULL
yt.fall <- list(res = res[["yt_PA nmfs-fall survey model.intercept st.10"]],
                rand.field = rand.field[["yt_PA nmfs-fall survey model.intercept st.10"]],
                mod.output = mod.output[["yt_PA nmfs-fall survey model.intercept st.10"]],
                mod.diagnostics = mod.diagnostics[["yt_PA nmfs-fall survey model.intercept st.10"]],
                mesh = mesh.gf)


#save(cod.RV,yt.spring,cod.fall,yt.fall,file = paste0(direct.proj,"Results/select_st_10_res.RData"))
# So this is used to get the combined random field, groundfish field vs scallop field.
#load(paste0(direct.proj,"Results/INLA_Can_st_",paste(10,collapse = "_"),"_output.RData"))
load(paste0(direct.proj,"Results/INLA_Can_st_",paste(5,collapse = "_"),"_output.RData"))
#load("Y:/Projects/GB_time_area_closure_SPERA/Results/select_st_10_res.RData")
load("Y:/Projects/GB_time_area_closure_SPERA/Results/Scal_ly_results.RData")
# Note that this is missing the mean field, for the most recent era of the cod and yellowtail data.


######### Specific spatial plots
# Now I need to transform the yt and cod cells to the proper UTM to plot!
yt.closed.cells.utm <- spTransform(yt.closed.cells,proj4string(loc.gf))
yt.centroids <- gCentroid(yt.closed.cells.utm,byid=T)
cod.closed.cells.utm <- spTransform(cod.closed.cells,proj4string(loc.gf))
cod.centroids <- gCentroid(cod.closed.cells.utm,byid=T)
# First lets get the scallop field in a nice little plot
# Just the scallop plot so we have something nice and similar...
tmp.field <- data.frame(r.field.response = inv.logit(com.ly$rand.field$mean + com.ly$res$summary.fixed$mean))
mesh.plt <- mesh.sc
con.hull <- chull(loc.sc@coords)
clp <- loc.sc@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon


png(paste0(direct.proj,"Results/Figures/INLA/scallop_distribution_map.png"), width = 11, height = 11,units = "in",res=800)
pecjector(area = "GB", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
          dims=c(400,400), clip = clp,add_land =T,add_EEZ =NULL,
          colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = 20, # loc.sc and loc.gf have same proj.
          repo='github',add_nafo = "sub")
# Add the UTM version of yt and cod closed cells.
plot(yt.centroids,pch=1, cex=6,add=T,col = "blue",lwd=2)
plot(cod.centroids,pch=7, cex=6,add=T)
title(paste("Scallop Relative Biomass Index"),cex.main=2,line=-2)
dev.off()

############### FOR the Canadian side we do these pieces!  

# Now we get the cod RV survey data...
# For the st 10 random field we need to pick out the random fiels associated with the most recent era...
dat <- dat.final[dat.final$survey == "RV",]
resp <- "cod_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
dat$years_5 <- as.factor(dat$years_5)

eras <- as.numeric(dat$years_5)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names +4 # For RV survey only, nmfs doesn't need the + 2
p <- max(era.names)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)

tmp.field <- data.frame(r.field.response = (inv.logit(rand.field[['cod_PA_can RV survey model.int_st_5']]$mean[w.index$w.repl == era.names[p]] + 
                                                     res[['cod_PA_can RV survey model.int_st_5']]$summary.fixed$mean) +
                                           inv.logit(com.ly$rand.field$mean + com.ly$res$summary.fixed$mean))/2)
con.hull <- chull(loc.sc@coords)
clp <- loc.sc@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon


# Here's the figure I've been waiting 3 years for... for cod
windows(11,11)
png(paste0(direct.proj,"Results/Figures/INLA/Cod_co-occurence_map.png"), width = 11, height = 11,units = "in",res=200)
pecjector(area = "GB", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
          dims=c(400,400), clip = clp,add_land =T,
          colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
          repo='github',add_nafo = "sub")
plot(cod.centroids,pch=7, cex=6,add=T)
title("Cod Co-occurence Probability",cex.main=2,line=-2)
dev.off()


saveVideo(
  {
    ani.options(interval = 2, nmax = n.eras)
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['cod_PA_can RV survey model.int_st_5']]$mean[w.index$w.repl == era.names[p]] + 
                                                           res[['cod_PA_can RV survey model.int_st_5']]$summary.fixed$mean), era = era.names[p])
      dat.era <- dat[dat$years_5 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GB", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                dims=c(400,400), clip = clp,add_land =T,
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='github',add_nafo = "sub")
      plot(cod.centroids,pch=7, cex=6,add=T)
      title(paste("Cod Occurence Probability -",name.era),cex.main=2,line=-2)
      ani.pause()
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/Cod_spatial_field_5.mp4'), ani.width = 800, ani.height = 800)



dat <- dat.final[dat.final$survey == "RV",]
resp <- "cod_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
dat$years_3 <- as.factor(dat$years_3)

eras <- as.numeric(dat$years_3)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names +6 # For RV survey only, nmfs doesn't need this + 2
p <- max(era.names)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)


saveVideo(
  {
    ani.options(interval = 2, nmax = n.eras)
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['cod_PA RV survey model.depth.sst_st_3']]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['cod_PA RV survey model.depth.sst_st_3']]$summary.fixed$mean), era = era.names[p])
      dat.era <- dat[dat$years_3 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh.gf,lvls = c(0,0.05,0.1,0.15,0.2,0.3,0.4,0.6),
                dims=c(400,400), clip = clp,add_land =T,direct = "Y:/Offshore scallop/Assessment/",
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='local',add_nafo = "sub")
      #plot(cod.centroids,pch=7, cex=6,add=T)
      title(paste("Cod Spaital field -",name.era),cex.main=2,line=-2)
      ani.pause()
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/Cod_full_model_spatial_field_3.mp4'), ani.width = 800, ani.height = 800)



# Now do the same for yellowtail...
dat <- dat.final[dat.final$survey == "nmfs-spring",]
resp <- "yt_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
dat$years_5 <- as.factor(dat$years_5)
# For clipping the plot area.


eras <- as.numeric(dat$years_5)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names # This is just a crutch for the RV survey...
p <- max(dat.eras)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)

tmp.field <- data.frame(r.field.response = (inv.logit(rand.field[['yt_PA_can nmfs-spring survey model.int_st_5']]$mean[w.index$w.repl == era.names[p]] + 
                                                        res[['yt_PA_can nmfs-spring survey model.int_st_5']]$summary.fixed$mean) +
                                              inv.logit(com.ly$rand.field$mean + com.ly$res$summary.fixed$mean))/2)
png(paste0(direct.proj,"Results/Figures/INLA/YT_co-occurence_map.png"), width = 11, height = 11,units = "in",res=800)
pecjector(area = "GB", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
          dims=c(400,400), clip = clp,add_land =T,
          colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
          repo='github')
plot(yt.centroids,pch=1, cex=6,add=T,col = "blue",lwd=2)
title("Yellowtail Co-occurence Probability",cex.main=2,line=-2)
dev.off()

saveVideo(
  {
    ani.options(interval = 2, nmax = 50)
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['yt_PA_can nmfs-spring survey model.int_st_5']]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['yt_PA_can nmfs-spring survey model.int_st_5']]$summary.fixed$mean), era = era.names[p])
      dat.era <- dat[dat$years_5 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GB", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                dims=c(400,400), clip = clp,add_land =T,add_EEZ = "sounds grand",
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='github')
      plot(yt.centroids,pch=1, cex=6,add=T,col = "blue",lwd=2)
      title(paste("Yellowtail Occurence Probability -",name.era),cex.main=2,line=-2)
      ani.pause()
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/YT_spatial_field_5.mp4'), ani.width = 800, ani.height = 800)


#############  End of the Canadian side only video, now for the whole area...
## Now if instead we want all the region and not just the Canadian side we want to do this...
## For cod the 5 year field is best.
load("Y:/Projects/GB_time_area_closure_SPERA/Results/INLA_st_5_output.RData")

# Now we get the cod RV survey data...
dat <- dat.final[dat.final$survey == "RV",]
resp <- "cod_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
dat$years_5 <- as.factor(dat$years_5)

eras <- as.numeric(dat$years_5)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names +4 # For RV survey only, nmfs doesn't need the + 4
p <- max(era.names)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)

con.hull <- chull(loc.gf@coords)
clp <- loc.gf@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon


# Now the cod video for the entire region, not just canada...
saveVideo(
  {
    ani.options(interval = 2, nmax = 50)
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['cod_PA RV survey model.int_st_5']]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['cod_PA RV survey model.int_st_5']]$summary.fixed$mean), era = era.names[p])
      dat.era <- dat[dat$years_5 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                dims=c(400,400),add_land = T,clip= clp,add_EEZ = T,
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='github',add_nafo = "sub")
      #plot(cod.centroids,pch=7, cex=6,add=T)
      title(paste("Cod Occurence Probability -",name.era),cex.main=2,line=-2)
      ani.pause()

    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/cod_spatial_field_5_all.mp4'), ani.width = 800, ani.height = 800)


# Now we get the cod nmfs Fall survey data
dat <- dat.final[dat.final$survey == "nmfs-fall",]
resp <- "cod_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
dat$years_5 <- as.factor(dat$years_5)

eras <- as.numeric(dat$years_5)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names 
p <- max(era.names)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)

con.hull <- chull(loc.gf@coords)
clp <- loc.gf@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon


# Now the cod video for the entire region, not just canada...
saveVideo(
  {
    ani.options(interval = 2, nmax = 50)
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['cod_PA nmfs-fall survey model.int_st_5']]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['cod_PA nmfs-fall survey model.int_st_5']]$summary.fixed$mean), era = era.names[p])
      dat.era <- dat[dat$years_5 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                dims=c(400,400),add_land = T,clip= clp,add_EEZ = T,
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='github',add_nafo = "sub")
      #plot(cod.centroids,pch=7, cex=6,add=T)
      title(paste("Cod Occurence Probability -",name.era),cex.main=2,line=-2)
      ani.pause()
      
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/cod_spatial_field_5_all-nmfs_fall.mp4'), ani.width = 800, ani.height = 800)



# and now the yellowtail video for the whole region, note that the yellowtail is 3 year so I need to load that here...
load("Y:/Projects/GB_time_area_closure_SPERA/Results/INLA_st_3_output.RData")

dat <- dat.final[dat.final$survey == "nmfs-spring",]
resp <- "yt_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
dat$years_5 <- as.factor(dat$years_5)
# For clipping the plot area.


eras <- as.numeric(dat$years_3)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names # This is just a crutch for the RV survey...
p <- max(dat.eras)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)

con.hull <- chull(loc.gf@coords)
clp <- loc.gf@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon


# Now the yt video for the entire region, not just canada...
saveVideo(
  {
    ani.options(interval = 2, nmax = 50)
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['yt_PA nmfs-spring survey model.int_st_3']]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['yt_PA nmfs-spring survey model.int_st_3']]$summary.fixed$mean), era = era.names[p])
      dat.era <- dat[dat$years_3 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                dims=c(400,400),add_land = T,clip= clp,add_EEZ = T,
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='github',add_nafo = "sub")
      #plot(cod.centroids,pch=7, cex=6,add=T)
      title(paste("YT Occurence Probability -",name.era),cex.main=2,line=-2)
      ani.pause()
      
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/yt_spatial_field_3_all_nmfs_spring.mp4'), ani.width = 800, ani.height = 800)


# Now the yt video for the spatial field of the best model.
saveVideo(
  {
    ani.options(interval = 2, nmax = 50,ffmpeg = 'C:/Program Files/ImageMagick-7.0.9-Q16/ffmpeg.exe')
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['yt_PA nmfs-spring survey model.depth.sed.sst_st_3']]$summary.fixed$mean[1]), era = era.names[p])
      dat.era <- dat[dat$years_3 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh,lvls = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4),
                dims=c(400,400),add_land = T,clip= clp,add_EEZ = T,direct="Y:/Offshore scallop/Assessment/",
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='local',add_nafo = "sub")
      #plot(cod.centroids,pch=7, cex=6,add=T)
      title(paste("YT Occurence Probability -",name.era),cex.main=2,line=-2)
      ani.pause()
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/yt_spatial_field_3_all_nmfs_spring_best_model.mp4'), ani.width = 800, ani.height = 800)



# Now for the Fall survey

dat <- dat.final[dat.final$survey == "nmfs-fall",]
resp <- "yt_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
dat$years_5 <- as.factor(dat$years_5)
# For clipping the plot area.


eras <- as.numeric(dat$years_3)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names # This is just a crutch for the RV survey...
p <- max(dat.eras)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)

con.hull <- chull(loc.gf@coords)
clp <- loc.gf@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon


# Now the cod video for the entire region, not just canada...
saveVideo(
  {
    ani.options(interval = 2, nmax = 50)
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['yt_PA nmfs-fall survey model.int_st_3']]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['yt_PA nmfs-fall survey model.int_st_3']]$summary.fixed$mean), era = era.names[p])
      dat.era <- dat[dat$years_3 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      #dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$years_10 == dat.eras[p],]
      #name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      #loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
      #coordinates(loc) <- ~X+Y
      #proj4string(loc) <- proj4string(loc.sc)
      #windows(11,11)
      pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                dims=c(400,400),add_land = T,clip= clp,add_EEZ = T,
                colors = rev(magma(10)), c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                repo='github',add_nafo = "sub")
      #plot(cod.centroids,pch=7, cex=6,add=T)
      title(paste("YT Occurence Probability -",name.era),cex.main=2,line=-2)
      ani.pause()
      
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/yt_spatial_field_3_all_nmfs_fall.mp4'), ani.width = 800, ani.height = 800)





############################################################
## Now for some fun with covariates
## Note here that SEDNUM of 3 and 4 represent fine and very fine sand, you'll see the model results suggest YT are
## far more likely to be encountered on these 2 bottom type compared to other areas, but likelihood still low from this effect!

# First need to note that for depth and for SST these are centered by data set, so a number is specific
# for that covariate. First up I'll do yellowtail
dat$depth_log <- log(-dat$comldepth)
dat$depth_cen <-  dat$depth_log - mean(dat$depth_log) # Log transform should help with issues related to skew of the depth data.
dat$sst_avg_cen <- scale(dat$sst_avg)



# Model hyperparameters explained, onte that this is using the wrong YT data (sould be the 3 year model)
# The correlation distance here is 240 km, so fairly long distance, but I think
# this is mostly because there aren't any cod across much of the bank..
res[['cod_PA nmfs-fall survey model.int_st_5']]$summary.hyperpar[1,"mean"]/1000
# Can see distance is much smaller for yellowtail from same survey
res[['yt_PA nmfs-fall survey model.int_st_5']]$summary.hyperpar[1,"mean"]/1000
# During teh RV survey the distances are smaller than what we see in the fall
# so I think this suggests that there range contracts during the spring
res[['cod_PA RV survey model.int_st_5']]$summary.hyperpar[1,"mean"]/1000
# Can see distance is much smaller for yellowtail from same survey
res[['yt_PA RV survey model.int_st_5']]$summary.hyperpar[1,"mean"]/1000

# Finally for nmfs spring the range is larger for both, interesting.
res[['cod_PA nmfs-spring survey model.int_st_5']]$summary.hyperpar[1,"mean"]/1000
# Can see distance is much smaller for yellowtail from same survey
res[['yt_PA nmfs-spring survey model.int_st_5']]$summary.hyperpar[1,"mean"]/1000

# Now we set our sigma to be around 0.5 and the prior was fairly strongly suggesting 
# that is should remain below this (see not above), that would effectly limit our 
# field to range between 0.27 and 0.72, but our field is much broader than this
# As we see below it basically allows for a full range in our field, but the long
# range of the process (above) keeps our field fairly smooth.
# Fall wants to go effectively from 0 to 1 field
res[['yt_PA nmfs-fall survey model.int_st_5']]$summary.hyperpar[2,"mean"]
res[['cod_PA nmfs-fall survey model.int_st_5']]$summary.hyperpar[2,"mean"]
# RV is a bit less variable (and correlation distances shorter), whether that is a seasonal thing
# or an RV survey thing we'll never know, but YT still pretty variable likely because 0's are so common
res[['yt_PA RV survey model.int_st_5']]$summary.hyperpar[2,"mean"]
# Cod on the other hand a bit less so, 0.03-0.97 would be possible variability.
res[['cod_PA RV survey model.int_st_5']]$summary.hyperpar[2,"mean"]
# Both more or less ranging from 0.01-0.99
res[['yt_PA nmfs-spring survey model.int_st_5']]$summary.hyperpar[2,"mean"]
res[['cod_PA nmfs-spring survey model.int_st_5']]$summary.hyperpar[2,"mean"]


# So the likelihood of encountering YT in other sediment types is about 2% 
inv.logit(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1])
# So the likelihood of encountering YT in Sediment type 3 is 6% 
inv.logit(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[2,1])
# In sediment type for it increases to 7.5 %
inv.logit(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[3,1])

# So here are the depth and SST impacts for Yellowtail.  SST in this case is average SST across a number of years, so suggests 
# that the Yellowtail like warmer parts of the bank (if the effect is positistve!!)
# Note how likely it is to encoutner YT at certain depths, very small depth range is very common to see YT!
nmfs.spr  <-dat.final[dat.final$survey == surveys[1],]

# The ID column in these randome terms are the transformed covariate values, so easy peasy to plot these relationshipr up, thanks INLA'ers...
# Depth model
mod.res <- data.frame(resp = inv.logit(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + 
                                 res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.random$depth$mean),
                      covar = exp(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.random$depth$ID + 
                              mean(log(-nmfs.spr$comldepth))))

p <- ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("depth") + ylab("Encounter Probability") + scale_x_continuous(limits=c(0,200)) +
                                               theme_classic() + theme(text = element_text(size=30))
ggsave(plot= p,filename = paste0(direct.proj,"Results/Figures/INLA/yt_3_year_full_model_EP_vs_depth.png"),height=8,width=8,units='in')

# SST modeel
# Need to rescal the data...
sst.scale <- scale(nmfs.spr$sst_avg)
mod.res <- data.frame(resp = inv.logit(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + 
                                   res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.random$sst$mean),
                      covar = res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.random$sst$ID * attr(sst.scale,'scaled:scale')+
                                 attr(sst.scale,'scaled:center'))
p<- ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("SST") + ylab("Encounter Probability") + 
                                                              theme_classic() + theme(text = element_text(size=30))

ggsave(plot= p,filename = paste0(direct.proj,"Results/Figures/INLA/yt_3_year_full_model_EP_vs_SST.png"),height=8,width=8,units='in')

# Now we can make soome figure of the sediment effect..., type 3 type 4 vs the rest of the area...
mod.res <- data.frame(resp.link = c(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1],
                                   res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1]+ 
                                   res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[2,1],
                                   res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1]+ 
                                   res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[3,1]),
                      sd =    inv.logit(c(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,2],
                                 res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[2,2],
                                  res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[3,2])),
                      LCI.link =    c(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,3],
                                    res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[2,3],
                                    res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[3,3]),
                      UCI.link =    c(res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[1,5],
                                 res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[2,5],
                                 res[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$summary.fixed[3,5]),
                      covar = c("Sed X","Sed 3", "Sed 4"))
mod.res$LCI <- inv.logit(mod.res$LCI.link)
mod.res$UCI <- inv.logit(mod.res$UCI.link)
mod.res$resp <- inv.logit(mod.res$resp.link)
mod.res$LCI[2:3] <- inv.logit(mod.res$LCI.link + mod.res$resp.link[1])[2:3]
mod.res$UCI[2:3] <- inv.logit(mod.res$UCI.link + mod.res$resp.link[1])[2:3]

p<- ggplot(mod.res,aes(covar,resp)) + geom_point(size=4) + geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0,size=1) +
                                 xlab("Sediment type")+ylab("Encounter Probability") + theme_classic() + theme(text = element_text(size=30))
ggsave(plot= p,filename = paste0(direct.proj,"Results/Figures/INLA/yt_3_year_full_model_EP_vs_sediment_type.png"),height=8,width=8,units='in')



#######################################
# Now to yellwottail in thei fall, for curiosty

nmfs.fall  <-dat.final[dat.final$survey == surveys[2],]

# The ID column in these randome terms are the transformed covariate values, so easy peasy to plot these relationshipr up, thanks INLA'ers...
# Depth model
mod.res <- data.frame(resp = inv.logit(res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.random$depth$mean),
                      covar = exp(res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.random$depth$ID + 
                                    mean(log(-nmfs.fall$comldepth))))

ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("depth") + ylab("Encounter Probability") + scale_x_continuous(limits=c(0,200)) +
  theme_classic() + theme(text = element_text(size=20))

# SST modeel
# Need to rescal the data...
sst.scale <- scale(nmfs.fall$sst_avg)
mod.res <- data.frame(resp = inv.logit(res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.random$sst$mean),
                      covar = res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.random$sst$ID * attr(sst.scale,'scaled:scale')+
                        attr(sst.scale,'scaled:center'))
ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("SST") + ylab("Encounter Probability") + 
  theme_classic() + theme(text = element_text(size=20))

# Now we can make soome figure of the sediment effect..., type 3 type 4 vs the rest of the area...
mod.res <- data.frame(resp.link = c(res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1],
                                    res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1]+ 
                                      res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[2,1],
                                    res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1]+ 
                                      res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[3,1]),
                      sd =    inv.logit(c(res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,2],
                                          res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[2,2],
                                          res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[3,2])),
                      LCI.link =    c(res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,3],
                                      res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[2,3],
                                      res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[3,3]),
                      UCI.link =    c(res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[1,5],
                                      res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[2,5],
                                      res[["yt_PA nmfs-fall survey model.depth.sed.sst_st_3"]]$summary.fixed[3,5]),
                      covar = c("Sed X","Sed 3", "Sed 4"))
mod.res$LCI <- inv.logit(mod.res$LCI.link)
mod.res$UCI <- inv.logit(mod.res$UCI.link)
mod.res$resp <- inv.logit(mod.res$resp.link)
mod.res$LCI[2:3] <- inv.logit(mod.res$LCI.link + mod.res$resp.link[1])[2:3]
mod.res$UCI[2:3] <- inv.logit(mod.res$UCI.link + mod.res$resp.link[1])[2:3]

ggplot(mod.res,aes(covar,resp)) + geom_point(size=4) + geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0,size=1) +
  xlab("Sediment type")+ylab("Encounter Probability") + theme_classic() + theme(text = element_text(size=20))



#######################################
# Now to yellwottaill in the RV survey

RV  <-dat.final[dat.final$survey == surveys[3],]

# The ID column in these randome terms are the transformed covariate values, so easy peasy to plot these relationshipr up, thanks INLA'ers...
# Depth model
mod.res <- data.frame(resp = inv.logit(res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.random$depth$mean),
                      covar = exp(res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.random$depth$ID + 
                                    mean(log(-RV$comldepth))))

ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("depth") + ylab("Encounter Probability") + scale_x_continuous(limits=c(0,200)) +
  theme_classic() + theme(text = element_text(size=20))

# SST modeel
# Need to rescal the data...
sst.scale <- scale(RV$sst_avg)
mod.res <- data.frame(resp = inv.logit(res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.random$sst$mean),
                      covar = res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.random$sst$ID * attr(sst.scale,'scaled:scale')+
                        attr(sst.scale,'scaled:center'))
ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("SST") + ylab("Encounter Probability") + 
  theme_classic() + theme(text = element_text(size=20))

# Now we can make soome figure of the sediment effect..., type 3 type 4 vs the rest of the area...
mod.res <- data.frame(resp.link = c(res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1],
                                    res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1]+ 
                                      res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[2,1],
                                    res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,1]+ 
                                      res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[3,1]),
                      sd =    inv.logit(c(res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,2],
                                          res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[2,2],
                                          res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[3,2])),
                      LCI.link =    c(res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,3],
                                      res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[2,3],
                                      res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[3,3]),
                      UCI.link =    c(res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[1,5],
                                      res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[2,5],
                                      res[["yt_PA RV survey model.depth.sed.sst_st_3"]]$summary.fixed[3,5]),
                      covar = c("Sed X","Sed 3", "Sed 4"))
mod.res$LCI <- inv.logit(mod.res$LCI.link)
mod.res$UCI <- inv.logit(mod.res$UCI.link)
mod.res$resp <- inv.logit(mod.res$resp.link)
mod.res$LCI[2:3] <- inv.logit(mod.res$LCI.link + mod.res$resp.link[1])[2:3]
mod.res$UCI[2:3] <- inv.logit(mod.res$UCI.link + mod.res$resp.link[1])[2:3]

ggplot(mod.res,aes(covar,resp)) + geom_point(size=4) + geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0,size=1) +
  xlab("Sediment type")+ylab("Encounter Probability") + theme_classic() + theme(text = element_text(size=20))


######################################################################
# Same shit for COD

nmfs.spr  <-dat.final[dat.final$survey == surveys[1],]

# The ID column in these randome terms are the transformed covariate values, so easy peasy to plot these relationshipr up, thanks INLA'ers...
# Depth model
mod.res <- data.frame(resp = inv.logit(res[["cod_PA nmfs-spring survey model.depth.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["cod_PA nmfs-spring survey model.depth.sst_st_3"]]$summary.random$depth$mean),
                      covar = exp(res[["cod_PA nmfs-spring survey model.depth.sst_st_3"]]$summary.random$depth$ID + 
                                    mean(log(-nmfs.spr$comldepth))))

ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("depth") + ylab("Encounter Probability") + scale_x_continuous(limits=c(0,400)) +
  theme_classic() + theme(text = element_text(size=20))

# SST modeel
# Need to rescal the data...
sst.scale <- scale(nmfs.spr$sst_avg)
mod.res <- data.frame(resp = inv.logit(res[["cod_PA nmfs-spring survey model.depth.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["cod_PA nmfs-spring survey model.depth.sst_st_3"]]$summary.random$sst$mean),
                      covar = res[["cod_PA nmfs-spring survey model.depth.sst_st_3"]]$summary.random$sst$ID * attr(sst.scale,'scaled:scale')+
                        attr(sst.scale,'scaled:center'))
ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("SST") + ylab("Encounter Probability") + 
  theme_classic() + theme(text = element_text(size=20))

# What is relationship between those SST's and depth, nothing particular obvious, at higher SST's the depths tends to be a deeper.  Likely 
# a function of location on the bank.
plot(dat.final$comldepth ~ dat.final$sst_avg)

#######################################
# Now to yellwottail in thei fall, for curiosty

nmfs.fall  <-dat.final[dat.final$survey == surveys[2],]

# The ID column in these randome terms are the transformed covariate values, so easy peasy to plot these relationshipr up, thanks INLA'ers...
# Depth model
mod.res <- data.frame(resp = inv.logit(res[["cod_PA nmfs-fall survey model.depth.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["cod_PA nmfs-fall survey model.depth.sst_st_3"]]$summary.random$depth$mean),
                      covar = exp(res[["cod_PA nmfs-fall survey model.depth.sst_st_3"]]$summary.random$depth$ID + 
                                    mean(log(-nmfs.fall$comldepth))))

ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("depth") + ylab("Encounter Probability") + scale_x_continuous(limits=c(0,200)) +
  theme_classic() + theme(text = element_text(size=20))

# SST modeel
# Need to rescal the data...
sst.scale <- scale(nmfs.fall$sst_avg)
mod.res <- data.frame(resp = inv.logit(res[["cod_PA nmfs-fall survey model.depth.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["cod_PA nmfs-fall survey model.depth.sst_st_3"]]$summary.random$sst$mean),
                      covar = res[["cod_PA nmfs-fall survey model.depth.sst_st_3"]]$summary.random$sst$ID * attr(sst.scale,'scaled:scale')+
                        attr(sst.scale,'scaled:center'))
ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("SST") + ylab("Encounter Probability") + 
  theme_classic() + theme(text = element_text(size=20))

#######################################
# Now to yellwottaill in the RV survey

RV  <-dat.final[dat.final$survey == surveys[3],]

# The ID column in these randome terms are the transformed covariate values, so easy peasy to plot these relationshipr up, thanks INLA'ers...
# Depth model
mod.res <- data.frame(resp = inv.logit(res[["cod_PA RV survey model.depth.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["cod_PA RV survey model.depth.sst_st_3"]]$summary.random$depth$mean),
                      covar = exp(res[["cod_PA RV survey model.depth.sst_st_3"]]$summary.random$depth$ID + 
                                    mean(log(-RV$comldepth))))

ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("depth") + ylab("Encounter Probability") + scale_x_continuous(limits=c(0,200)) +
  theme_classic() + theme(text = element_text(size=20))

# SST modeel
# Need to rescal the data...
sst.scale <- scale(RV$sst_avg)
mod.res <- data.frame(resp = inv.logit(res[["cod_PA RV survey model.depth.sst_st_3"]]$summary.fixed[1,1] + 
                                         res[["cod_PA RV survey model.depth.sst_st_3"]]$summary.random$sst$mean),
                      covar = res[["cod_PA RV survey model.depth.sst_st_3"]]$summary.random$sst$ID * attr(sst.scale,'scaled:scale')+
                        attr(sst.scale,'scaled:center'))
ggplot(mod.res,aes(covar,resp)) + geom_line() + xlab("SST") + ylab("Encounter Probability") + 
  theme_classic() + theme(text = element_text(size=20))



#######################GIS GIS GIS ############################################################
# bring in some GIS layers I want
load("Y:/Projects/GB_time_area_closure_SPERA/Results/INLA_st_3_output.RData")

# This is the good part of US, with Mathas Vinyard and Nantucket.  
prj4s <- CRS("+init=epsg:4326") 
us <- maps::map("worldHires", "USA",fill=TRUE,
                col="transparent", plot=FALSE)
IDs <- sapply(strsplit(us$names, ":"), function(x) x[1])
us.sp <- map2SpatialPolygons(
  us, IDs=IDs, proj4string=prj4s)
us.i <- SpatialPolygons(list(Polygons(
  us.sp@polygons[[1]]@Polygons[c(1,53,90)],'0')), ## removing islands
  proj4string=us.sp@proj4string)
us.sf <- st_as_sf(us.i) 

# Now Canada
ca <- maps::map("worldHires", "Canada", fill=TRUE,
                col="transparent", plot=FALSE)
IDs <- sapply(strsplit(ca$names, ":"), function(x) x[1])

ca.sp <- map2SpatialPolygons(
  ca, IDs=IDs, proj4string=prj4s)
ca.ll <- ca.sp
ca.sf <- st_as_sf(ca.sp)



# For clipping the plot area.
temp <- tempfile()
# Download this to the temp directory you created above
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/EEZ/EEZ.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
eez.all <- readOGR(paste0(temp2, "/EEZ.shp"))
eez.all <- st_as_sf(eez.all)
# Now clip it...
#eez <- eez.all %>% filter(SRC_AGENCY == "DFO")

# Now the NAFO boundaries
temp <- tempfile()
# Download this to the temp directory you created above
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/NAFO/Divisions/Divisions.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
nafo.all <- readOGR(paste0(temp2, "/Divisions.shp"))
# Now convert to sf
nafo.all <- st_as_sf(nafo.all)


# Now the offshore boundaries
# temp <- tempfile()
# # Download this to the temp directory you created above
# download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore/offshore.zip", temp)
# # Figure out what this file was saved as
# temp2 <- tempfile()
# # Unzip it
# unzip(zipfile=temp, exdir=temp2)
# off.all <- all.layers(paste0(temp2))
# raster::union(off.all$Ban,off.all$BBn)
# n.shps <- length(names(off.all))
# 
# # This would work but kinda sucks due to GB overlaps, I need to clean up the shapefiles, probably the best thing to do will be to 
# # make a core set of 'area' shapeiles and put those in the offshore and inshore directories, then make a series of
# # shapefiles that go into an 'other directory.  Really just want the core stuff in the inshore and offshore, other crap can go in other spots.
# # If the layers all have unique names this will work, unfortuatenly my bevy of GB sublayers kinda suck at this...
# for(i in 1:(n.shps-1))
# {
#   if(i == 1) tmp <- raster::union(off.all[[i]],off.all[[i+1]])
#   if(i > 1) tmp <- raster::union(tmp,off.all[[i+1]])
# }
# # Now convert to sf
# off.all <- st_as_sf(off.all)



#######################
## Thinking about how to tie inla output into a GIS structure
rotate <- function(x) t(apply(x, 2, rev)) # This is needed to spin the field projection to be oriented how GIS wants them, unclear why it is weird.

dat <- dat.final %>% filter(survey=="nmfs-spring")
N.dat <- nrow(dat)
resp <- "yt_PA"
response <- which(names(dat) == resp)
names(dat)[response] <- "response"
eras <- as.numeric(dat$years_3)
dat$years_3 <- as.factor(dat$years_3)
era.names <- unique(eras)
n.eras <- length(unique(eras))
dat.eras <- era.names 
p <- max(dat.eras)
w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
p=13
tmp.field <- data.frame(r.field.response = inv.logit(rand.field[['yt_PA nmfs-spring survey model.depth.sed.sst_st_3']]$mean[w.index$w.repl == era.names[p]] + 
                                                       res[['yt_PA nmfs-spring survey model.depth.sed.sst_st_3']]$summary.fixed$mean[1]), era = era.names[p])

# Georges Bank extent...
gb.area.sf <- st_as_sf(loc.gf)
# To get a convex hull of GB...
tst <- st_centroid(gb.area.sf)
clp <- st_convex_hull(st_union(tst)) 

# Getting all the data proper...
c_sys = proj4string(loc.gf)
loc.sf <- st_as_sf(loc.gf)
nafo <- st_transform(nafo.all,crs=c_sys)
eez <- st_transform(eez.all,crs=c_sys)
us.sf <- st_transform(us.sf,crs=c_sys)
ca.sf <- st_transform(ca.sf,crs=c_sys)
coords <- convert.coords(plot.extent = "GOM",c_sys = c_sys)
coords <- st_as_sf(coords)
xlim <- st_bbox(loc.sf)[c(1,3)]
ylim <- st_bbox(loc.sf)[c(2,4)]
dims= c(100,100)
field <-  tmp.field$r.field.response
projec = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
field.projec = inla.mesh.project(projec, field)
# Now we need to make the NA's -9999 for happy conversion...
field.projec[is.na(field.projec)] <- -9999

# Now sure this is right, but looks like we have an sf object from our raster :-)
# Here's a 270 degree rotation of the matrix.
raster <- raster(rotate(rotate(rotate(field.projec))))
extent(raster) <- c(range(projec$x),range(projec$y))
#raster <- as.raster(raster)
# To convert a raster to a spatial polygon.is easy..
#tmp <- as(raster,"SpatialGridDataFrame")
tmp <- as(raster, "SpatialPolygonsDataFrame") 
proj4string(tmp) <- c_sys
# Make it an sf object
spd <- st_as_sf(tmp,as_points=F,merge=F)
#spd <- spd %>% filter(layer != -9999)

# Now I should be able to clip my spd to the gb.bound and get even nicer
spd.clip <- st_intersection(spd,clp)
x.lim <- c(300000,900000 )
y.lim <- c(4300000,5000000)

# get a colour scale.. first set a global colour ramp
myPalette <- colorRampPalette(rev(viridis(20)))

# Maximum value in data
max.ep <- max(inv.logit(rand.field[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$mean+res[['yt_PA nmfs-spring survey model.depth.sed.sst_st_3']]$summary.fixed$mean[1]))
max.ep <- round(max.ep,digits=1)
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max.ep),breaks=seq(0,max.ep,by=0.05))
sf <- scale_fill_gradientn(colours = myPalette(100), limits=c(0, max.ep),breaks=seq(0,max.ep,by=0.05))


#  Plot of the area
windows(11,11)
ggplot(spd.clip)   + geom_sf(aes(fill=layer,colour=layer)) + sf + 
  sc + geom_sf(data=ca.sf) + geom_sf(data=us.sf)+
  geom_sf(data=eez,size=1)+geom_sf(data=nafo,alpha=0,colour='grey',linetype = "11", size = 0.5) +
                coord_sf(xlim=x.lim,ylim=y.lim) +theme_bw()

#  Same thing but a bit more zoomed out
windows(11,11)
p<- ggplot(spd.clip)   + geom_sf(aes(fill=layer,colour=layer)) + sf + sc+
   geom_sf(data=ca.sf) + geom_sf(data=us.sf)+
  geom_sf(data=eez,size=1)+geom_sf(data=nafo,alpha=0,colour='grey',linetype = "11", size = 0.5) +
  coord_sf(xlim=c(100000,1200000 ),ylim= c(4300000,5500000)) +theme_bw() + 
  theme(text=element_text(size=30),legend.text=element_text(size=15)) + labs(fill = "Probability",colour="Probability")
print(p)
ggsave(plot= p,filename = paste0(direct.proj,"Results/Figures/INLA/area_overview.png"),width=10,height=8,units='in')

# The points from this survey...
windows(11,11)
p <- ggplot(loc.sf)   + geom_sf(alpha=0.2) +  geom_sf(data=ca.sf) + geom_sf(data=us.sf)+
  geom_sf(data=eez,size=1)+geom_sf(data=nafo,alpha=0,colour='grey',linetype = "11", size = 0.5) +
  coord_sf(xlim=x.lim,ylim= y.lim) +theme_bw() + theme(text=element_text(size=30))
print(p)
ggsave(plot= p,filename = paste0(direct.proj,"Results/Figures/INLA/Survey_location_example.png"),width=10,height=8,units='in')

# Let's try to make a video from these :-)
# Now the yt video for the spatial field of the best model.
saveVideo(
  {
    ani.options(interval = 2, nmax = 50,ffmpeg = 'C:/Program Files/ImageMagick-7.0.8-Q16/ffmpeg.exe')
    for (p in 1:n.eras)
    {
      # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
      # is always just our intercept.
      tmp.field <- data.frame(r.field.response = inv.logit(rand.field[["yt_PA nmfs-spring survey model.depth.sed.sst_st_3"]]$mean[w.index$w.repl == era.names[p]] + 
                                                             res[['yt_PA nmfs-spring survey model.depth.sed.sst_st_3']]$summary.fixed$mean[1]), era = era.names[p])
      dat.era <- dat[dat$years_3 == dat.eras[p],]
      name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
      # Get the points used for each of the models.
      
      projec = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
      field.projec = inla.mesh.project(projec, tmp.field$r.field.response)
      # Now we need to make the NA's -9999 for happy conversion...
      #field.projec[is.na(field.projec)] <- -9999
      
      # Now sure this is right, but looks like we have an sf object from our raster :-)
      # But we need to rotate this matrixx counter clockwise..
      # Here's a 270 degree rotation of the matrix.
      raster <- raster(rotate(rotate(rotate(field.projec))))
      extent(raster) <- c(range(projec$x),range(projec$y))
      # Take a couple steps to move from a raster to a spatial polygon...
      tmp <- as(raster,"SpatialGridDataFrame")
      tmp <- as(tmp, "SpatialPolygonsDataFrame") 
      proj4string(tmp) <- c_sys
      
      spd <- st_as_sf(tmp,as_points=F,merge=F)

      # Now I should be able to clip my spd to the gb.bound and get even nicer
      spd.clip <- st_intersection(spd,clp)

      # I'd add the US but the us.sf object is a mess to convert due to some error in the data...
      # windows(11,11)
      # ggplot(spd.clip) + geom_sf(aes(fill=layer,colour=layer)) + scale_fill_viridis_c(direction =-1,begin=1-max(spd$layer)) + 
      #   scale_colour_viridis_c(direction =-1,begin =1-max(spd$layer)) + geom_sf(data=ca.sf) + geom_sf(data=us.sf)+
      #   coord_sf(xlim=c(300000,900000 ),ylim=c(4300000,5000000)) +theme_bw() + ggtitle(paste("YT Occurence Probability -",name.era))
      # 
        
      p<- ggplot(spd.clip) +geom_sf(aes(fill=layer,colour=layer)) + sc+sf+geom_sf(data=ca.sf) + geom_sf(data=us.sf)+
        geom_sf(data=eez,size=1)+geom_sf(data=nafo,alpha=0,colour='grey',linetype = "11", size = 0.5) +
        coord_sf(xlim=x.lim,ylim=y.lim) +theme_bw() + ggtitle(paste("YT Occurence Probability -",name.era)) + 
        theme(text=element_text(size=30),legend.text=element_text(size=15)) + labs(fill = "Probability",colour="Probability")
      
      print(p)
      ani.pause()
    }
  }, video.name = paste0(direct.proj,'Results/Figures/INLA/yt_spatial_field_3_all_nmfs_spring_best_model_new.mp4'), ani.width = 800, ani.height = 800)

