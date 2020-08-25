# Here's where I'm putting all the model diagnostic crap together

# 1:  This is going to be AIC/WAIC summaries from the models I need to summarize...
################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
#----
rm(list=ls())
direct.proj <- "d:/Github"
library(rlist)
library(tidyverse)
library(cowplot)

# So this is the big file that has all the initial model diagnostics in it. This 
# one wasn't included in the inital list of model diagnostics I develoepd because that is overwhelming for the dashboard
# grand model diagnostics summary file so we don't need to load everything all the time.
# load("D:/NAS/Projects/GB_time_area_closure_SPERA/Results/INLA_spatial_output.RData")
# mod.diag.initial <- mod.diag #<- mod.diag.step.1
# mod.diagnostics.initial <- mod.diagnostics #<- mod.diagnostics.step.1
# "Only" the first 120 models count here, here's a handy function from the rlist package.
#mod.diagnostics.fe <- list.remove(mod.diagnostics.initial,121:136)
#mod.diag.fe <- do.call("rbind",mod.diagnostics.fe)
#save(mod.diag.fe,file = "D:/NAS/Projects/GB_time_area_closure_SPERA/Results/FE_static_RF_model_selection.RData")

load("D:/Github/Paper_2_SDMs/Data/FE_static_RF_model_selection.RData")
load("D:/Github/Paper_2_SDMs/Data/All_model_diagnostics.RData")


# So we'll show model diagnostics for the single fixed effects and static random field as one figure with 6 panels. 3 for surveys
# and 3 for species
# Now we actually want to melt these so that we have the data in long form for ggplot
mod.diag.fe <- reshape2::melt(mod.diag.fe,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
mod.diag.fe$species[grepl("yt_PA",mod.diag.fe$species)] <- "Yellowtail"
mod.diag.fe$species[grepl("cod_PA",mod.diag.fe$species)] <- "Cod"
mod.diag.fe$model.id <- substr(mod.diag.fe$model.id,7,25) 

# Ready for some loop-d-loop
species <- unique(mod.diag.fe$species)
num.species <- length(species)
surveys <- unique(mod.diag.fe$survey)
num.surveys <- length(surveys)

# The lines for these figures...
plt.min.fe.waic <- mod.diag.fe %>% filter(diag == 'waic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.fe.dic <- mod.diag.fe %>% filter(diag == 'dic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)

# Now we can grab the model selection for the more complex models, first the st.10 comparisons
mod.diag.10 <- list.remove(all.mod.diag,!grepl('st.10',names(all.mod.diag)))
mod.diag.10 <- do.call('rbind',mod.diag.10)
mod.diag.10 <- reshape2::melt(mod.diag.10,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
mod.diag.10$species[grepl("yt_PA",mod.diag.10$species)] <- "Yellowtail"
mod.diag.10$species[grepl("cod_PA",mod.diag.10$species)] <- "Cod"
mod.diag.10$model.id[grepl("model.sst.sed",mod.diag.10$model.id)] <-   "SST + Sed"
mod.diag.10$model.id[grepl("model.sst.chl",mod.diag.10$model.id)] <-   "SST + Chl"
mod.diag.10$model.id[grepl("model.depth.chl",mod.diag.10$model.id)] <- "Dep + Chl"
mod.diag.10$model.id[grepl("model.depth.sst",mod.diag.10$model.id)] <- "Dep + SST"
mod.diag.10$model.id[grepl("model.depth.sed",mod.diag.10$model.id)] <- "Dep + Sed"
mod.diag.10$model.id[grepl("model.sed.chl",mod.diag.10$model.id)] <-   "Chl + Sed"
mod.diag.10$model.id[grepl("model.sst",mod.diag.10$model.id)] <- "SST"
mod.diag.10$model.id[grepl("model.depth",mod.diag.10$model.id)] <- "Dep"
mod.diag.10$model.id[grepl("model.chl",mod.diag.10$model.id)] <- "Chl"
mod.diag.10$model.id[grepl("model.sed",mod.diag.10$model.id)] <- "Sed"
mod.diag.10$model.id[grepl("model.int",mod.diag.10$model.id)] <- "Intercept"
mod.diag.10$era <- 10

plt.min.10.waic <- mod.diag.10 %>% filter(diag == 'waic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.10.dic <- mod.diag.10 %>% filter(diag == 'dic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)

mod.diag.5 <- list.remove(all.mod.diag,!grepl('st.5',names(all.mod.diag)))
mod.diag.5 <- do.call('rbind',mod.diag.5)
mod.diag.5 <- reshape2::melt(mod.diag.5,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
mod.diag.5$species[grepl("yt_PA",mod.diag.5$species)] <- "Yellowtail"
mod.diag.5$species[grepl("cod_PA",mod.diag.5$species)] <- "Cod"
mod.diag.5$model.id[grepl("model.depth.sst.chl",mod.diag.5$model.id)] <- "Dep + SST + Chl"
mod.diag.5$model.id[grepl("model.depth.sed.sst",mod.diag.5$model.id)] <- "Dep + SST + Sed"
mod.diag.5$model.id[grepl("model.depth.sst",mod.diag.5$model.id)] <- "Dep + SST"
mod.diag.5$model.id[grepl("model.depth.sed",mod.diag.5$model.id)] <- "Dep + Sed"
mod.diag.5$model.id[grepl("model.int",mod.diag.5$model.id)] <- "Intercept"
mod.diag.5$era <- 5

plt.min.5.waic <- mod.diag.5 %>% filter(diag == 'waic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.5.dic <- mod.diag.5 %>% filter(diag == 'dic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)

# These model 3's aren't necessary really as they are only comparing the covariate model to an intercept only model.
# But the intercept model is useful for us to compare the random fields later...
mod.diag.3 <- list.remove(all.mod.diag,!grepl('st.3',names(all.mod.diag)))
mod.diag.3 <- do.call('rbind',mod.diag.3)
mod.diag.3 <- reshape2::melt(mod.diag.3,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
mod.diag.3$species[grepl("yt_PA",mod.diag.3$species)] <- "Yellowtail"
mod.diag.3$species[grepl("cod_PA",mod.diag.3$species)] <- "Cod"
mod.diag.3$model.id[grepl("model.int",mod.diag.3$model.id)] <- "Intercept"
mod.diag.3$model.id[grepl("model.depth.sst",mod.diag.3$model.id)] <- "Dep + SST"
mod.diag.3$model.id[grepl("model.depth.sed.sst",mod.diag.3$model.id)] <- "Dep + SST + Sed"
mod.diag.3$era <- 3


plt.min.3.waic <- mod.diag.3 %>% filter(diag == 'waic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.3.dic <- mod.diag.3 %>% filter(diag == 'dic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)

# Finally we compare the random fields in one object, just comparing the intercept models for simplicity.
mod.diag.rf <- bind_rows(mod.diag.10 %>% filter(model.id == "Dep + SST"),
                         mod.diag.5 %>% filter(model.id == "Dep + SST"),
                         mod.diag.5 %>% filter(model.id == "Dep + SST + Sed"),
                         mod.diag.3 %>% filter(model.id == "Dep + SST"),
                         mod.diag.3 %>% filter(model.id == "Dep + SST + Sed"),)
mod.diag.rf$era <- as.factor(mod.diag.rf$era)
plt.min.rf.cod.waic <- mod.diag.rf %>% filter(diag == 'waic' & species== 'Cod') %>% group_by(survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.rf.cod.dic <- mod.diag.rf %>% filter(diag == 'dic'& species == 'Cod') %>% group_by(survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)

plt.min.rf.yt.3.5.waic <- mod.diag.rf %>% filter(diag == 'waic' & model.id == "Dep + SST + Sed") %>% group_by(survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.rf.yt.3.5.dic <- mod.diag.rf %>% filter(diag == 'dic'& model.id == "Dep + SST + Sed") %>% group_by(survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
# and yellowtail 5 and 10 year comparison
plt.min.rf.yt.5.10.waic <- mod.diag.rf %>% filter(diag == 'waic' & model.id == "Dep + SST" & species == "Yellowtail") %>% group_by(survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.rf.yt.5.10.dic <- mod.diag.rf %>% filter(diag == 'dic'& model.id == "Dep + SST" & species == "Yellowtail") %>% group_by(survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)


# The base models using a fixed random field and single covariates, WAIC then DIC
plt.waic.fe <- ggplot(mod.diag.fe %>% filter(diag == 'waic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free_x') + 
  geom_vline(data = plt.min.fe.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.fe.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.dic.fe <- ggplot(mod.diag.fe %>% filter(diag == 'dic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free_x') + 
  geom_vline(data = plt.min.fe.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.fe.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

# More complex models using the 10 year random field
plt.waic.10 <- ggplot(mod.diag.10 %>% filter(diag == 'waic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free_x') + xlab("WAIC") + ylab("") +
  geom_vline(data = plt.min.10.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.10.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.dic.10 <- ggplot(mod.diag.10 %>% filter(diag == 'dic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free_x') + xlab("DIC") + ylab("")+
  geom_vline(data = plt.min.10.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.10.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

# More complex models using the 5 year random field
plt.waic.5 <- ggplot(mod.diag.5 %>% filter(diag == 'waic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free') + 
  geom_vline(data = plt.min.5.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.5.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.dic.5 <- ggplot(mod.diag.5 %>% filter(diag == 'dic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free') + 
  geom_vline(data = plt.min.5.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.5.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  


# Comparing the random field intercept only models.
plt.cod.waic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'waic' & species == 'Cod')) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~survey ,scales = 'free') +  xlab("WAIC")+
  geom_vline(data = plt.min.rf.cod.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.cod.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.cod.dic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'dic' & species == 'Cod')) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~ survey,scales = 'free') +  xlab("DIC")+
  geom_vline(data = plt.min.rf.cod.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.cod.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

# Similar for yt but needs to be done in two pieces, compare 10 and 5 year models then 5 and 3 year models.
plt.yt.5.10.waic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'waic' & species == 'Yellowtail' & model.id == "Dep + SST")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~survey ,scales = 'free') + xlab("")+
  geom_vline(data = plt.min.rf.yt.5.10.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.5.10.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.yt.5.10.dic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'dic'& species == 'Yellowtail' & model.id == "Dep + SST")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~ survey,scales = 'free') + xlab("")+
  geom_vline(data = plt.min.rf.yt.5.10.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.5.10.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  


plt.yt.3.5.waic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'waic' & species == 'Yellowtail' & model.id == "Dep + SST + Sed")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~survey ,scales = 'free') + xlab("WAIC")+
  geom_vline(data = plt.min.rf.yt.3.5.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.3.5.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.yt.3.5.dic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'dic'& species == 'Yellowtail' & model.id == "Dep + SST + Sed")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~ survey,scales = 'free') + xlab("DIC")+
  geom_vline(data = plt.min.rf.yt.3.5.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.3.5.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.yt.rf.waic <- plot_grid(plt.yt.5.10.waic.rf,plt.yt.3.5.waic.rf,nrow=2)
plt.yt.rf.dic <- plot_grid(plt.yt.5.10.dic.rf,plt.yt.3.5.dic.rf,nrow=2)

#save.image("D:/Github/Paper_2_SDMs/Data/model_diagnostics_for_papers.RData")
load("D:/Github/Paper_2_SDMs/data/model_diagnostics_for_papers.RData")













