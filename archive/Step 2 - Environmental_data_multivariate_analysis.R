#This script builds on the environmental covariate work that CWP performed, here we look for correlations between the variables to trim down the variable list further
# we will then look at multivariate approaches (assuming there are a lot of variables left in the analysis)

# Clean up the workspace and load packages I need
rm(list=ls())
require(tidyverse)
require(reshape2)
require(GGally)
require(boot)
require(bbmle)
require(MASS)
require(leaps)
require(COUNT)
require(cowplot)
#install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg) # Heads up that this package isn't on CRAN, the repo is above, this is needed for the rootogram to check our models.

#direct <- "/Users/user/DFO_2017/"
direct <- "Y:/Projects/GB_time_area_closure_SPERA/"

# Load in the data for each of the data sets, these summaries were created as part of the contract from CWP.
nmfs.fall <-read.csv(paste(direct,"Data/Summaries/NMFSfall.csv",sep=""),stringsAsFactors = F)
nmfs.spr <-read.csv(paste(direct,"Data/Summaries/NMFSspring.csv",sep=""),stringsAsFactors = F)
RV.surv <-read.csv(paste(direct,"Data/Summaries/RVSurvey.csv",sep=""),stringsAsFactors = F)
obs.gf <-read.csv(paste(direct,"Data/Summaries/Observers_Groundfish_Set.csv",sep=""),stringsAsFactors = F)
fish.gf <-read.csv(paste(direct,"Data/Summaries/Groundfish_Sets.csv",sep=""),stringsAsFactors = F)
obs.scal <-read.csv(paste(direct,"Data/Summaries/Observers_Scallop_TripSet.csv",sep=""),stringsAsFactors = F)

# there is something screwy (the depth is 168 meters which isn't in line with any of the other data so I'm removing it...
obs.scal <- obs.scal[obs.scal$tripset !=  "J14-0442_2",]


# Now let's look at some pairs plots of the data and see where we are finding redundancies in these environmental covariates...

# These are the potential variables that had some association with the response variables

variables <- c("botstr_wt",
               "chl_avg",
               "complexity",
               "comlaspect",
               "comldepth",
               "k490_avg",
               'Mud',
               "nit_avg96",
               "sal_avg96",
               'sal_rg96',
               'phos_avg96',
               'comlslope',
               'sand',
               'sst_avg',
               'sst_rg',
               'SEDNUM',
               'FID_GMAINE_median',
               'Year_median',
               'strat96',
               'sil_avg96',
               'chl_rg',
               'growth')
variables.obs <- variables[variables != "SEDNUM" & variables != "growth"]
variables.gf <- variables[variables != "SEDNUM"]
# Now subset each data set to the 22 variables that seem potentially interesting...
nmfs.fall.enviro <- nmfs.fall[,variables]
nmfs.spr.enviro <- nmfs.spr[,variables]
RV.surv.enviro <-RV.surv[,variables]
obs.gf.enviro <- obs.gf[,variables.obs]
fish.gf.enviro <- fish.gf[,variables.gf]
obs.scal.eviro <-obs.scal[,variables.obs]



# Next up lets make some pairs plots of these data, in theory these should all look more or less the same, i.e. the relationship between
# different variables shouldn't change based on whehter we are looking at survey data or observer data, but ya never know...
#windows(11,11)
ggpairs(nmfs.spr.enviro)
ggsave(paste0(direct,"Results/Figures/Covariates/nmfs_spring_pairs_plot.png"), height=30,width=30,dpi = 600)
# nmfs fall
ggpairs(nmfs.fall.enviro)
ggsave(paste0(direct,"Results/Figures/Covariates/nmfs_fall_pairs_plot.png"), height=30,width=30,dpi=600)
# RV survey
ggpairs(RV.surv.enviro)
ggsave(paste0(direct,"Results/Figures/Covariates/RV_survey_pairs_plot.png"), height=30,width=30,dpi=600)
# Observer GF
ggpairs(obs.gf.enviro)
ggsave(paste0(direct,"Results/Figures/Covariates/Obs_gf_pairs_plot.png"), height=30,width=30,dpi=600)
# GF fishery
ggpairs(fish.gf.enviro)
ggsave(paste0(direct,"Results/Figures/Covariates/fish_gf_pairs_plot.png"), height=30,width=30,dpi=600)
# Observer scallop
ggpairs(obs.scal.eviro)
ggsave(paste0(direct,"Results/Figures/Covariates/obs_scallop_pairs_plot.png"), height=30,width=30,dpi=600)


######################## Multivariate Analyses ################################################################# Multivariate Analyses ###########################
######################## Multivariate Analyses ################################################################# Multivariate Analyses ###########################
# So what we are doing with this PCA is summarizing the environment of each location sampled which explains the overall variation we are seeing in the environment.
# So if there are spatial environmental patterns in the environment we should see these show up in maps of each PC shouldn't we.  PC1 for each location
# is 1 value that summarizes the environment in that location and explains the most variation in the data that one linear predictor can (around 40% in most of these cases)
# What I'm going to hope is that there is some spatial patterning the principle components and they aren't just random patterns and also that they
# are able to explain some of the variation in either presence/absence or overall abundance...
# The variables removed showed no evidence for correlation with other variables, or had very non-linear correlation patterns which would limit
# their ability to be included in a PCA...
var.rm <- c("botstr_wt","complexity", "k490_avg",'comlslope','sst_rg','Year_median','chl_rg','growth',"SEDNUM","sal_rg96",'comlaspect')
#var.rm <- c("growth")
# Get the variables we want to keep, this includes positional data along with abudnace/Presense Absense data for cod and yellowtail...
non.enviro.surv.dat <- names(nmfs.fall)[1:20] 
non.enviro.gf.dat <- names(fish.gf)[1:11]
non.enviro.gf.obs.dat <- names(obs.gf)[1:24]
non.enviro.scal.obs.dat <- names(obs.scal)[1:24]

enviro.survey.dat <- variables[!is.element(variables,var.rm)]
enviro.obs.dat <- variables.obs[!is.element(variables.obs,var.rm)]
enviro.gf.dat <- variables.gf[!is.element(variables.obs,c("botstr_wt","complexity", "k490_avg",'comlslope','sst_rg','Year_median','chl_rg',"SEDNUM"))]
# Now subset the survey dataset to those we have complete data for.
nmfs.spr.4pca <- na.omit(nmfs.spr[,c(non.enviro.surv.dat,enviro.survey.dat)])
nmfs.fall.4pca <- na.omit(nmfs.fall[,c(non.enviro.surv.dat,enviro.survey.dat)])
RV.surv.4pca <- na.omit(RV.surv[,c(non.enviro.surv.dat,enviro.survey.dat)])

# Now for the observer data we subset to this...
obs.gf.4pca <- na.omit(obs.gf[,c(non.enviro.gf.obs.dat,enviro.obs.dat)])
obs.scal.4pca <-na.omit(obs.scal[,c(non.enviro.scal.obs.dat,enviro.obs.dat)])
fish.gf.4pca <- na.omit(fish.gf[,c(non.enviro.gf.dat,enviro.gf.dat)])


# Now I think we are ready to do some PCA'ing, first up the surveys
nmfs.spr.pca.mod <- prcomp(nmfs.spr.4pca[enviro.survey.dat],scale=T)
summary(nmfs.spr.pca.mod)
nmfs.fall.pca.mod <- prcomp(nmfs.fall.4pca[enviro.survey.dat],scale=T)
summary(nmfs.fall.pca.mod)
RV.pca.mod <- prcomp(RV.surv.4pca[enviro.survey.dat],scale=T)
summary(RV.pca.mod)
# Now the observer models
obs.gf.pca.mod <- prcomp(obs.gf.4pca[enviro.obs.dat],scale=T)
summary(obs.gf.pca.mod)
obs.scal.pca.mod <- prcomp(obs.scal.4pca[enviro.obs.dat],scale=T)
summary(obs.scal.pca.mod)
# and finally the groundfish fishery data...
fish.gf.pca.mod <- prcomp(fish.gf.4pca[enviro.gf.dat],scale=T)
summary(fish.gf.pca.mod)

windows(11,11)
par(mfrow=c(2,3))
plot(nmfs.spr.pca.mod,main="NMFS Spring")
plot(nmfs.fall.pca.mod,main="NMFS Fall")
plot(RV.pca.mod,main = "RV Survey")
plot(obs.gf.pca.mod, main = "Observer Groundfish")
plot(obs.scal.pca.mod,main = "Observer Scallop")
plot(fish.gf.pca.mod,main = "Groundfish fishery")

windows(11,11)
par(mfrow=c(2,3))
biplot(nmfs.spr.pca.mod,main="NMFS Spring",cex = c(0.1,1))
biplot(nmfs.fall.pca.mod,main="NMFS Fall",cex = c(0.1,1))
biplot(RV.pca.mod,main = "RV Survey",cex = c(0.1,1))
biplot(obs.gf.pca.mod, main = "Observer Groundfish",cex = c(0.1,1))
biplot(obs.scal.pca.mod,main = "Observer Scallop",cex = c(0.1,1))
biplot(fish.gf.pca.mod,main = "Groundfish fishery",cex = c(0.1,1))

# Now we can extract as many of the PC's from the model as we want, for the moment I'm taking the top 4, but I need for formalize this
# somehow, is possible I only look at the first one depending on the selection algorithim I look at...
nmfs.spr.4pca$PC1 <- predict(nmfs.spr.pca.mod)[,1]
nmfs.spr.4pca$PC2 <- predict(nmfs.spr.pca.mod)[,2]
nmfs.spr.4pca$PC3 <- predict(nmfs.spr.pca.mod)[,3]
nmfs.spr.4pca$PC4 <- predict(nmfs.spr.pca.mod)[,4]
# NMFS Fall
nmfs.fall.4pca$PC1 <- predict(nmfs.fall.pca.mod)[,1]
nmfs.fall.4pca$PC2 <- predict(nmfs.fall.pca.mod)[,2]
nmfs.fall.4pca$PC3 <- predict(nmfs.fall.pca.mod)[,3]
nmfs.fall.4pca$PC4 <- predict(nmfs.fall.pca.mod)[,4]
# RV survey
RV.surv.4pca$PC1 <- predict(RV.pca.mod)[,1]
RV.surv.4pca$PC2 <- predict(RV.pca.mod)[,2]
RV.surv.4pca$PC3 <- predict(RV.pca.mod)[,3]
RV.surv.4pca$PC4 <- predict(RV.pca.mod)[,4]
# GF observer
obs.gf.4pca$PC1 <- predict(obs.gf.pca.mod)[,1]
obs.gf.4pca$PC2 <- predict(obs.gf.pca.mod)[,2]
obs.gf.4pca$PC3 <- predict(obs.gf.pca.mod)[,3]
obs.gf.4pca$PC4 <- predict(obs.gf.pca.mod)[,4]
# Scallop observer
obs.scal.4pca$PC1 <- predict(obs.scal.pca.mod)[,1]
obs.scal.4pca$PC2 <- predict(obs.scal.pca.mod)[,2]
obs.scal.4pca$PC3 <- predict(obs.scal.pca.mod)[,3]
obs.scal.4pca$PC4 <- predict(obs.scal.pca.mod)[,4]
# Groundfish fishery
fish.gf.4pca$PC1 <- predict(fish.gf.pca.mod)[,1]
fish.gf.4pca$PC2 <- predict(fish.gf.pca.mod)[,2]
fish.gf.4pca$PC3 <- predict(fish.gf.pca.mod)[,3]
fish.gf.4pca$PC4 <- predict(fish.gf.pca.mod)[,4]

# Let's tidy up the data so we only have the data we need for the rest of the analysis...
nmfs.spr.final <- nmfs.spr.4pca[,c("unique_set_ID","lat_dd","lon_dd","year","COD_number_Adu","COD_PA_Adu","YT_number_Adu","YT_PA_Adu",
                                   names(nmfs.spr.4pca)[grep("PC",names(nmfs.spr.4pca))],"strata")]

nmfs.fall.final <- nmfs.fall.4pca[,c("unique_set_ID","lat_dd","lon_dd","year","COD_number_Adu","COD_PA_Adu","YT_number_Adu","YT_PA_Adu",
                                     names(nmfs.fall.4pca)[grep("PC",names(nmfs.fall.4pca))],"strata")]

RV.surv.final <- RV.surv.4pca[,c("unique_set_ID","lat_dd","lon_dd","year","COD_number_Adu","COD_PA_Adu","YT_number_Adu","YT_PA_Adu",
                                 names(RV.surv.4pca)[grep("PC",names(RV.surv.4pca))],"strata")]
names(RV.surv.final) <- c("unique_set_ID","lat","lon",'year','cod',"cod_PA",'yt','yt_PA',names(RV.surv.final)[grep("PC",names(RV.surv.final))],'strata')
names(nmfs.spr.final) <- names(nmfs.fall.final) <- names(RV.surv.final)
# Observer datasets
obs.scal.final <- obs.scal.4pca[,c("tripset","lat.dd","lon.dd","year","cod_est_num_caught","cod_PA_caught","yt_est_num_caught","yt_PA_caught",
                                   names(obs.scal.4pca)[grep("PC",names(obs.scal.4pca))],'num_hook_haul','month')]
names(obs.scal.final) <- c("tripset","lat","lon",'year','cod',"cod_PA",'yt','yt_PA',names(obs.scal.final)[grep("PC",names(obs.scal.final))],'num_hook_haul','month')
obs.gf.final <- obs.gf.4pca[,c("tripset","lat.dd","lon.dd","year","cod_count_a","cod_PA_a","yt_count_a","yt_PA_a",
                               names(obs.gf.4pca)[grep("PC",names(obs.gf.4pca))],'gear','month')]
names(obs.gf.final) <- c("tripset","lat","lon",'year','cod',"cod_PA",'yt','yt_PA',names(obs.gf.final)[grep("PC",names(obs.gf.final))],'gear','month')

fish.gf.final <- fish.gf.4pca[,c('set_id',"lat_dd","lon_dd","year","cod_kg","cod_PA","yt_kg","yt_PA",
                                 names(fish.gf.4pca)[grep("PC",names(fish.gf.4pca))],'gear_code','trip_id','month')]
names(fish.gf.final) <- c("set_id","lat","lon",'year','cod',"cod_PA",'yt','yt_PA',names(fish.gf.4pca)[grep("PC",names(fish.gf.4pca))],'gear_code','trip_id','month')
# K now we have a large number of potential response variables and potential covariates with 4 Principle Components and 6 datasets, funnnnn!
# Let's focus on the number of adults, and PA, for Cod/YT, this makes it most consistent b/t analyses I hope...

# Put the data together and get set up so we can run a loop
final.dat <- list(spr.nmfs = nmfs.spr.final,fall.nmfs = nmfs.fall.final,RV = RV.surv.final,scal.obs = obs.scal.final,gf.obs = obs.gf.final,gf.fish = fish.gf.final)
dat.names <- names(final.dat)
num.dat <- length(final.dat)
response.variables <- c("cod_PA","cod","yt_PA","yt")
num.rvs <- length(response.variables)
mod <- NULL
mod.comp <- NULL
pred.data <- NULL
orig.dat <- NULL
# Now we will run a crap load of analyses on each dataset....
for(i in 1:num.dat)
{
  # Subset to the data we want for each loop, subset to just the data we need....
  dat <- final.dat[[dat.names[i]]]
  # This is for predictions, data is needed so we have predictions for all the possible combinations of the PCA variables for the dataset we have
  pred.dat <- expand.grid(PC1=  seq(min(floor(dat$PC1)),max(ceiling(dat$PC1)),by=0.25),
                          PC2 = seq(min(floor(dat$PC2)),max(ceiling(dat$PC2)),by=0.25),
                          PC3 = seq(min(floor(dat$PC3)),max(ceiling(dat$PC3)),by=0.25),
                          PC4=  seq(min(floor(dat$PC4)),max(ceiling(dat$PC4)),by=0.25))
  
  for(j in 1:num.rvs)
  {
    if((i == 6 && j ==4)==F) # For whatever reason this breaks just for the yellowtail using the fishery data and I can't figure out why...
    {
      mod.name <- response.variables[j]
      # Note the awesomeness of get(), if we are looking at a presence absence model do this
      if(length(grep("PA",mod.name))==1) 
      {
        mod.full <- glm(get(mod.name) ~ (PC1+PC2+PC3+PC4)^2, dat,family='binomial')
        mod.null <- glm(get(mod.name) ~ 1, dat,family='binomial')
      }
      # If we are looking at the abundace/biomass data do this...
      if(length(grep("PA",mod.name))==0) 
      {
        # The full model for this will just be all the 2 way interactions, past this is asking too much of our data....
        mod.full <- glm.nb(round(get(mod.name)) ~ (PC1+PC2+PC3+PC4)^2, dat)
        mod.null <- glm.nb(round(get(mod.name)) ~ 1, dat)
        #############  Now I also want to run diagnostics, I will run the diagnostics on the full models...
        ### This will produce rootograms for ZIP's, Poissions, Neg Bins, and Hurdle models for each response variable and plot them up
        # First fit the different models
        # This crashes if trying to fit the full fishery data so I'm going to skip doing that
        if(dat.names[i] != "gf.fish")
        {
          mod.poisson <- glm(get(mod.name) ~(PC1+PC2+PC3+PC4)^2 ,dat,family="poisson") # 
          mod.zinb <- zeroinfl(round(get(mod.name)) ~(PC1+PC2+PC3+PC4)^2 , dat,dist="negbin") # 
          mod.zip <- zeroinfl(round(get(mod.name)) ~(PC1+PC2+PC3+PC4)^2 , dat,dist="poisson") # 
          mod.hrd.nb <- hurdle(round(get(mod.name))~(PC1+PC2+PC3+PC4)^2,dat,dist="negbin")
          mod.hrd.p <- hurdle(round(get(mod.name))~(PC1+PC2+PC3+PC4)^2,dat,dist="poisson")
          # Now make the rootograms, the "hanging ones are similar to comparing fitted to observered 
          # While the suspended ones are like fitting residual vs fitted plots.
          ps.mod.h <- rootogram(mod.poisson,style="hanging",plot=F)
          ps.mod.s <- rootogram(mod.poisson,style="suspended",plot=F)
          nb.mod.h <- rootogram(mod.full,style="hanging",plot=T)
          nb.mod.s <- rootogram(mod.full,style="suspended",plot=F)
          zip.mod.h <- rootogram(mod.zip,style="hanging",plot =F)
          zip.mod.s <- rootogram(mod.zip,style="suspended",plot =F)
          zinb.mod.h <- rootogram(mod.zinb,style="hanging",plot =F)
          zinb.mod.s <- rootogram(mod.zinb,style="suspended",plot =F)
          hrd.nb.mod.h <- rootogram(mod.hrd.nb,style="hanging",plot =F)
          hrd.nb.mod.s <- rootogram(mod.hrd.nb,style="suspended",plot =F)
          hrd.p.mod.h <- rootogram(mod.hrd.p,style="hanging",plot =F)
          hrd.p.mod.s <- rootogram(mod.hrd.p,style="suspended",plot =F)
          
          # Now make a plot and save it so we can look at it later...
          #windows(16,11)
          p<- plot_grid(autoplot(c(ps.mod.h,nb.mod.h)),autoplot(c(zip.mod.h,zinb.mod.h)),autoplot(c(hrd.nb.mod.h,hrd.p.mod.h)),ncol=3)
          ggsave(plot = p,filename = paste0(direct,"Results/Figures/Environmental_models/Fitted_rootogram_",mod.name,"_",dat.names[i],".png"), 
                 width =16, height=11, units = 'in',dpi=600)
          #windows(16,11)
          p <- plot_grid(autoplot(c(ps.mod.s,nb.mod.s)),autoplot(c(zip.mod.s,zinb.mod.s)),autoplot(c(hrd.nb.mod.s,hrd.p.mod.s)),ncol=3)
          ggsave(plot=p,filename = paste0(direct,"Results/Figures/Environmental_models/Resid_rootogram_",mod.name,"_",dat.names[i],".png"), 
                 width =16, height=11, units = 'in',dpi=600)
        } # end if(dat.names[i] != "gf.fish")
       }# end if(length(grep("PA",mod.name))==0) 
      
      mod.backward <- stepAIC(mod.full,scope = list(upper = ~ (PC1+PC2+PC3+PC4)^2, lower = ~1),direction="both",trace=F)
      mod.forward <- stepAIC(mod.null,scope = list(upper = ~ (PC1+PC2+PC3+PC4)^2, lower = ~1),direction="both",trace=F)
      # Compare the models...
      mod.comparision <- AICctab(mod.backward,mod.forward,base=T,weights=T)
      
      # Pick one model, going with the most parsomonious if the AIC's are within 2 of each other....
      if(mod.comparision$dAICc[2] < 2 & mod.comparision$df[2] < mod.comparision$df[1]) mod.chosen <- mod.forward else mod.chosen <- mod.backward
      # Now make a huge list of all the model results and let's keep the model comparision as well...
      mod[[paste0(response.variables[j],"_",dat.names[i])]] <- mod.chosen
      mod.comp[[paste0(response.variables[j],"_",dat.names[i])]] <- mod.comparision
      # To add the residuals, this is currently just for the first model...
      
      if(length(grep("PA",mod.name))==0) 
      {
        if(length(grep("cod",mod.name))==1)
        {
          dat$resid_cod <- residuals(mod.chosen)
          dat$num_pred_cod <- predict(mod.chosen,newdata = dat,type="response")
          dat$se_cod <- predict(mod.chosen,newdata = dat,type="response",se.fit=T)$se.fit
          pred.dat$num_pred_cod <- predict(mod.chosen, newdata = pred.dat,type = "response")
          pred.dat$se_pred_cod <- predict(mod.chosen, newdata = pred.dat,type = "response",se.fit=T)$se.fit
        } # end if(length(grep("cod",mod.name))==1)
        # now do the yt
        if(length(grep("yt",mod.name))==1)
        {
          dat$resid_yt <- residuals(mod.chosen)
          dat$num_pred_yt <- predict(mod.chosen,newdata = dat,type="response")
          dat$se_yt <- predict(mod.chosen,newdata = dat,type="response",se.fit=T)$se.fit
          pred.dat$num_pred_yt <- predict(mod.chosen, newdata = pred.dat,type = "response")
          pred.dat$se_pred_yt <- predict(mod.chosen, newdata = pred.dat,type = "response",se.fit=T)$se.fit
        } # end if(length(grep("cod",mod.name))==1)
        
      }
      # Now do the presense/absence models.
      if(length(grep("PA",mod.name))==1) 
      {
        if(length(grep("cod",mod.name))==1)
        {
          dat$resid_PA_cod <- residuals(mod.chosen)
          dat$PA_pred_cod <- predict(mod.chosen,newdata = dat,type="response")
          dat$PA_se_cod <- predict(mod.chosen,newdata = dat,type="response",se.fit=T)$se.fit
          pred.dat$PA_pred_cod <- predict(mod.chosen, newdata = pred.dat,type = "response")
          pred.dat$PA_se_pred_cod <- predict(mod.chosen, newdata = pred.dat,type = "response",se.fit=T)$se.fit
        } # end if(ength(grep("cod",mod.name))==1)
        if(length(grep("yt",mod.name))==1)
        {
          dat$resid_PA_yt <- residuals(mod.chosen)
          dat$PA_pred_yt <- predict(mod.chosen,newdata = dat,type="response")
          dat$PA_se_yt <- predict(mod.chosen,newdata = dat,type="response",se.fit=T)$se.fit
          pred.dat$PA_pred_yt <- predict(mod.chosen, newdata = pred.dat,type = "response")
          pred.dat$PA_se_pred_yt <- predict(mod.chosen, newdata = pred.dat,type = "response",se.fit=T)$se.fit
        } # end if(ength(grep("cod",mod.name))==1)
      } # end if(length(grep("PA",mod.name))==0) 
      orig.dat[[paste0(response.variables[j],"_",dat.names[i])]] <- dat
      pred.data[[paste0(response.variables[j],"_",dat.names[i])]] <- pred.dat
    } # end if( i != 6 && j !=4)
  }# end for(j in 1:num.rvs
} # end for(i in 1:num.dat)

#save.image(paste0(direct,"Results/PCA_models.RData"))
#load(paste0(direct,"Results/PCA_models.RData"))


############ Diagnostics ############ Diagnostics  ############ Diagnostics  ############ Diagnostics ############ Diagnostics ############ Diagnostics 
# Now that I have all the model results (except the Groundfishery yt which I couldn't get to converge, I expect this is due to the massive number of 0's,
# Looking at the rootograms as a model diagnostic shows that the negative binomial is pretty good for most of the scenarios, but the RV survey data
# is terrible + groundfish observed isn't particularly great for any of the distributions, but in general the hurdle negative binomial
# produces the best fitting models.
#.  Not how terrible the Poisson models are for these...
# All the rootgrams are found here, note that we didn't do these for the GF data as it just blows up.  All the figures are in this folder.
# Y:\Projects\GB_time_area_closure_SPERA\Results\Figures\Environmental_models

# Next up I should do some diagnostics on the negative binomial models to make sure there isn't anything terrible in here.  
# I'm not doing any diagnostics on the logistic regressions because there really aren't any good diagnostics I know of.  
# For the negative bionmial models on first blush it looks like the models are having trouble predicting the rare large catches, 
#I don't think this should be a surprise given there would be repeated
# measures in areas with very similar PC scores, so the odd big catch isn't something the model should capture, but if we are able
# to capture the general trend of certain areas catching more individuals this is a good thing I think....


# Here is how one gets the log odds for a particular value, if there is just an intercept and slope model...
#inv.logit(mod.chosen$coefficients[1] + 4*mod.chosen$coefficients[2])

# Here is how you would take your predicted data from a logistic regression with multiple predictors and 
# plot it.  Becasue you have 4 covariates you can only really show this for one predictor while holding all
# the other predictors at a given value.  So the Below gives us the probability of seeing a yellowtail 
# flounder  in the spring nmfs survey across the range of PC1 values, 
# In general for each of these data sets...
# PC1 is mostly made up of positive loadings for the nit/sil/phos/sal/stratification and negative loadings for deth and chlorphyll 'a'.
# PC 2 is mostly sst/mud/sand as negative loadings, and FID GMAIN as a positive (some are exactly reversed, not sure if that means anything at all)
tst <- pred.data$yt_PA_spr.nmfs[order(pred.data$yt_PA_spr.nmfs$PC1),]
tst <- tst[tst$PC2 == 1 & tst$PC3 == 1 & tst$PC4 == 1,]
# And here's our model, which makes some sense I think....
#pc1.at.50 <- nmfs.fall.final$PC1[which(abs(nmfs.fall.final$pred-0.5)==min(abs(nmfs.fall.final$pred-0.5)))]
windows(11,11)
ggplot(tst) + geom_line(aes(PC1,PA_pred_yt)) + geom_point(data=dat,aes(PC1,yt_PA)) #+
#geom_line(data=tst,aes(PC1,cod_PA),colour="blue")
#a




