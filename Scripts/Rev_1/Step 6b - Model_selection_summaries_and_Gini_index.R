# Here's where I'm putting all the model diagnostic crap together

# 1:  This is going to be AIC/WAIC summaries from the models I need to summarize...
################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
#----
rm(list=ls())
direct.proj <- "d:/Github"
library(rlist)
library(tidyverse)
library(cowplot)
library(sf)


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
source("D:/Github/Assessment_fns/Other_functions/gini_function.R")
theme_set(theme_classic(base_size = 10))



############### Section 2  MODEL SELECTION SUMMARIES ###################################################################################################
######
######

# So we'll show model diagnostics for the single fixed effects and static random field as one figure with 6 panels. 3 for surveys
# and 3 for species
# Now we actually want to melt these so that we have the data in long form for ggplot
mod.diag.fe <- reshape2::melt(mod.diag.fe,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
mod.diag.fe$species[grepl("yt_PA",mod.diag.fe$species)] <- "Yellowtail"
mod.diag.fe$species[grepl("cod_PA",mod.diag.fe$species)] <- "Cod"
mod.diag.fe$model.id <- substr(mod.diag.fe$model.id,7,25) 
mod.diag.fe$model.id[grepl("sst",mod.diag.fe$model.id)] <- "SST"
mod.diag.fe$model.id[grepl("depth",mod.diag.fe$model.id)] <- "Dep"
mod.diag.fe$model.id[grepl("chl.rg",mod.diag.fe$model.id)] <- "Chl"
mod.diag.fe$model.id[grepl("Sednum",mod.diag.fe$model.id)] <- "Sed"
mod.diag.fe$model.id[grepl("int",mod.diag.fe$model.id)] <- "Intercept"
mod.diag.fe$survey[grepl("RV",mod.diag.fe$survey)] <- "Winter (DFO)"
mod.diag.fe$survey[grepl("nmfs-spring",mod.diag.fe$survey)] <- "Spring (NMFS)"
mod.diag.fe$survey[grepl("nmfs-fall",mod.diag.fe$survey)] <- "Fall (NMFS)"
mod.diag.fe$survey <- factor(mod.diag.fe$survey, levels = c("Winter (DFO)","Spring (NMFS)","Fall (NMFS)"))

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
mod.diag.10$survey[grepl("RV",mod.diag.10$survey)] <- "Winter (DFO)"
mod.diag.10$survey[grepl("nmfs-spring",mod.diag.10$survey)] <- "Spring (NMFS)"
mod.diag.10$survey[grepl("nmfs-fall",mod.diag.10$survey)] <- "Fall (NMFS)"
mod.diag.10$survey <- factor(mod.diag.10$survey, levels = c("Winter (DFO)","Spring (NMFS)","Fall (NMFS)"))


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
mod.diag.5$survey[grepl("RV",mod.diag.5$survey)] <- "Winter (DFO)"
mod.diag.5$survey[grepl("nmfs-spring",mod.diag.5$survey)] <- "Spring (NMFS)"
mod.diag.5$survey[grepl("nmfs-fall",mod.diag.5$survey)] <- "Fall (NMFS)"
mod.diag.5$survey <- factor(mod.diag.5$survey, levels = c("Winter (DFO)","Spring (NMFS)","Fall (NMFS)"))


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
mod.diag.3$survey[grepl("RV",mod.diag.3$survey)] <- "Winter (DFO)"
mod.diag.3$survey[grepl("nmfs-spring",mod.diag.3$survey)] <- "Spring (NMFS)"
mod.diag.3$survey[grepl("nmfs-fall",mod.diag.3$survey)] <- "Fall (NMFS)"
mod.diag.3$survey <- factor(mod.diag.3$survey, levels = c("Winter (DFO)","Spring (NMFS)","Fall (NMFS)"))


plt.min.3.waic <- mod.diag.3 %>% filter(diag == 'waic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)
plt.min.3.dic <- mod.diag.3 %>% filter(diag == 'dic') %>% group_by(species,survey) %>% summarise(min2= min(data)+2, min10 = min(data) + 10)

# Finally we compare the random fields in one object, just comparing the intercept models for simplicity.
mod.diag.rf <- bind_rows(mod.diag.10 %>% filter(model.id == "Dep + SST"),
                         mod.diag.5 %>% filter(model.id == "Dep + SST"),
                         mod.diag.5 %>% filter(model.id == "Dep + SST + Sed"),
                         mod.diag.3 %>% filter(model.id == "Dep + SST"),
                         mod.diag.3 %>% filter(model.id == "Dep + SST + Sed"))
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
  facet_wrap(~species + survey,scales = 'free_x') + xlab("WAIC") + ylab("") +
  geom_vline(data = plt.min.fe.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.fe.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.dic.fe <- ggplot(mod.diag.fe %>% filter(diag == 'dic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free_x') + xlab("DIC") + ylab("") +
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

# More complex models using the 5 year random field, because we have differing models 
# I need to do this a bit messier...
plt.waic.5.cod <- ggplot(mod.diag.5 %>% filter(diag == 'waic', species == "Cod")) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free_x') + xlab("WAIC") + ylab("")+
  geom_vline(data = plt.min.5.waic %>% filter(species == "Cod"), aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.5.waic %>% filter(species == "Cod"), aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.waic.5.yt <- ggplot(mod.diag.5 %>% filter(diag == 'waic', species == "Yellowtail")) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free_x') + xlab("WAIC") + ylab("")+
  geom_vline(data = plt.min.5.waic %>% filter(species == "Yellowtail"), aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.5.waic %>% filter(species == "Yellowtail"), aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.waic.5 <- plot_grid(plt.waic.5.cod,plt.waic.5.yt,nrow=2)

plt.dic.5 <- ggplot(mod.diag.5 %>% filter(diag == 'dic')) + geom_point(aes(y = model.id, x = data)) + 
  facet_wrap(~species + survey,scales = 'free') + xlab("DIC") + ylab("")+
  geom_vline(data = plt.min.5.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.5.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

# Comparing the random field intercept only models.
plt.cod.waic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'waic' & species == 'Cod')) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~survey ,scales = 'free_x') +  xlab("WAIC")+ ylab('Era (Cod)') +
  geom_vline(data = plt.min.rf.cod.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.cod.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  


plt.cod.dic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'dic' & species == 'Cod')) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~ survey,scales = 'free_x') +  xlab("DIC")+ ylab('Era (Cod)') +
  geom_vline(data = plt.min.rf.cod.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.cod.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

# Similar for yt but needs to be done in two pieces, compare 10 and 5 year models then 5 and 3 year models.
plt.yt.5.10.waic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'waic' & species == 'Yellowtail' & model.id == "Dep + SST")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~survey ,scales = 'free_x') + xlab("") + ylab('Era (Yellowtail)') +
  geom_vline(data = plt.min.rf.yt.5.10.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.5.10.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.yt.5.10.dic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'dic'& species == 'Yellowtail' & model.id == "Dep + SST")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~ survey,scales = 'free_x') + xlab("")+ ylab('Era (Yellowtail)') +
  geom_vline(data = plt.min.rf.yt.5.10.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.5.10.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  


plt.yt.3.5.waic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'waic' & species == 'Yellowtail' & model.id == "Dep + SST + Sed")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~survey ,scales = 'free_x') + xlab("WAIC")+ ylab('Era (Yellowtail)') +
  geom_vline(data = plt.min.rf.yt.3.5.waic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.3.5.waic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.yt.3.5.dic.rf <- ggplot(mod.diag.rf %>% filter(diag == 'dic'& species == 'Yellowtail' & model.id == "Dep + SST + Sed")) + geom_point(aes(y = era, x = data)) + 
  facet_wrap(~ survey,scales = 'free_x') + xlab("DIC")+ ylab('Era (Yellowtail)') +
  geom_vline(data = plt.min.rf.yt.3.5.dic, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
  geom_vline(data = plt.min.rf.yt.3.5.dic, aes(xintercept = min2),color="blue",linetype = "dashed",size=1)  

plt.rf.waic <- plot_grid(plt.cod.waic.rf,plt.yt.5.10.waic.rf,plt.yt.3.5.waic.rf,nrow=3)
plt.rf.dic <- plot_grid(plt.cod.waic.rf,plt.yt.5.10.dic.rf,plt.yt.3.5.dic.rf,nrow=3)

#save.image("D:/Github/Paper_2_SDMs/Data/model_diagnostics_for_papers.RData")
load("D:/Github/Paper_2_SDMs/data/model_diagnostics_for_papers.RData")

############### End Section 2  END MODEL SELECTION SUMMARIES ###################################################################################################
########


######################## Section 3 Gini Index Calcs #########################################################################################################
########
# Now we can do the Gini calculations here 

load("D:/Github/Paper_2_SDMs/Results/Data_for_Gini.RData" )

# 
# names(nmfs.surv.gb) <- tolower(names(nmfs.surv.gb))
# names(rv.surv.gb) <- tolower(names(rv.surv.gb))

#rv.surv.gb <- rv.surv.gb %>% dplyr::select(c("strataid","areakm","type",'geometry'))
#names(rv.surv.gb) <- c("strata","area",'type','geometry')
# Sadly some of the survey strata in the RV shapefile are finer scale than in our data.. (5Z3 and 5Z4 are the ones...)
#rv.surv.gb$strata <- substr(rv.surv.gb$strata,1,3)
# Now I am going to strip the geometry and merge the areas of same strata...
#st_geometry(rv.surv.gb) <- NULL
#rv.surv.gb <- rv.surv.gb %>% group_by(strata) %>% dplyr::summarise(area = sum(area))

#nmfs.surv.gb <- nmfs.surv.gb %>% dplyr::select(c("strata","areakm",'set_','geometry'))
#names(nmfs.surv.gb) <- c("strata","area",'set','geometry')
#nmfs.surv.gb$strata <- nmfs.surv.gb$strata

gini.nmfs <- left_join(data.frame(nmfs.gini),data.frame(nmfs.surv.gb),by = 'strata')
gini.rv <- left_join(data.frame(rv.gini),data.frame(rv.surv.gb),by = 'strata')
gini.rv$weight[gini.rv$number == 0] <- 0
head(gini.rv)
gini.rv <- gini.rv %>% filter(year >=1987)
gini.rv$strata <- as.factor(gini.rv$strata)

cod.bms <-  gini.rv %>% dplyr::filter(species == 'cod_PA') %>% group_by(year,strata,area,.drop=F) %>% dplyr::summarize(wt.mn.yr.strata = mean(weight),num.mn.yr.strta = mean(number))
cod.bms <- cod.bms[,!names(cod.bms) == "area"]
cod.bms$species <- 'Atlantic Cod'
cod.bms <- left_join(cod.bms,rv.surv.gb,by = "strata")
cod.bms$wt.mn.yr.strata[is.nan(cod.bms$wt.mn.yr.strata)] <- 0
cod.bms$num.mn.yr.strta[is.nan(cod.bms$num.mn.yr.strta)] <- 0
# Do the same for YT
yt.bms <-  gini.rv %>% dplyr::filter(species == 'yt_PA') %>% group_by(year,strata,area,.drop=F) %>% dplyr::summarize(wt.mn.yr.strata = mean(weight),num.mn.yr.strta = mean(number))
yt.bms <- yt.bms[,!names(yt.bms) == "area"]
yt.bms$species <- 'Yellowtail Flounder'
yt.bms <- left_join(yt.bms,rv.surv.gb,by = "strata")
yt.bms$wt.mn.yr.strata[is.nan(yt.bms$wt.mn.yr.strata)] <- 0
yt.bms$num.mn.yr.strta[is.nan(yt.bms$num.mn.yr.strta)] <- 0

rv.bms <- dplyr::bind_rows(cod.bms,yt.bms)

rv.bms$tot.area <- sum(rv.surv.gb$area)

rv.bms <- rv.bms %>% dplyr::mutate(wbn = (wt.mn.yr.strata*(area/tot.area)))
rv.bms <- rv.bms %>% group_by(year,species) %>% dplyr::mutate(wbd = sum(wbn))
rv.bms <- rv.bms %>% dplyr::mutate(wb = wbn/wbd)
rv.bms <- rv.bms %>% group_by(year,species) %>% arrange(wb,.by_group =T)
rv.bms <- rv.bms %>% group_by(year,species) %>% dplyr::mutate(cum.pbm = cumsum(wb))
rv.bms <- rv.bms %>% group_by(year,species) %>% dplyr::mutate(cum.area = cumsum(area))
rv.bms <- rv.bms %>% group_by(year,species) %>% dplyr::mutate(p.area = area/tot.area)
rv.bms <- rv.bms %>% group_by(year,species) %>% dplyr::mutate(cum.p.area = cumsum(p.area))
rv.bms$survey <- "Winter"
# Now the proportion of biomass by group 
rv.bms <- rv.bms %>% group_by(year,species) %>%  dplyr::mutate(Gini = gini(wb,p.area)) 



ggplot(rv.bms) + geom_line(aes(x = cum.area, y = wb, color = as.factor(year))) + facet_wrap(~species)
ggplot(rv.bms) + geom_line(aes(x = year, y = Gini,color=species)) #

#Now we do the same with NMFS...
# I need a survey object with all the levels in each year in it for this...
nmfs <- expand.grid(strata = nmfs.surv.gb$strata,year = unique(gini.nmfs$year))
nmfs <- left_join(nmfs,nmfs.surv.gb,by='strata')

cod.bms <-  gini.nmfs %>% dplyr::filter(species == 'cod_PA', survey == 'nmfs-spring') %>% group_by(year,strata,area,.drop=F) %>% dplyr::summarize(wt.mn.yr.strata = mean(weight),num.mn.yr.strta = mean(number))
cod.bms <- cod.bms[,!names(cod.bms) == "area"]
cod.bms <- right_join(cod.bms,nmfs,by = c("strata",'year'))
cod.bms$species <- 'Atlantic Cod'
cod.bms$wt.mn.yr.strata[is.nan(cod.bms$wt.mn.yr.strata)] <- 0
cod.bms$num.mn.yr.strta[is.nan(cod.bms$num.mn.yr.strta)] <- 0
cod.bms$wt.mn.yr.strata[is.na(cod.bms$wt.mn.yr.strata)] <- 0
cod.bms$num.mn.yr.strta[is.na(cod.bms$num.mn.yr.strta)] <- 0

# Do the same for YT
yt.bms <-  gini.nmfs %>% dplyr::filter(species == 'yt_PA',survey == 'nmfs-spring') %>% group_by(year,strata,area,.drop=F) %>% dplyr::summarize(wt.mn.yr.strata = mean(weight),num.mn.yr.strta = mean(number))
yt.bms <- yt.bms[,!names(yt.bms) == "area"]
yt.bms <- right_join(yt.bms,nmfs,by = c("strata",'year'))
yt.bms$species <- 'Yellowtail Flounder'
yt.bms$wt.mn.yr.strata[is.nan(yt.bms$wt.mn.yr.strata)] <- 0
yt.bms$wt.mn.yr.strata[is.na(yt.bms$wt.mn.yr.strata)] <- 0
yt.bms$num.mn.yr.strta[is.nan(yt.bms$num.mn.yr.strta)] <- 0
yt.bms$num.mn.yr.strta[is.na(yt.bms$num.mn.yr.strta)] <- 0

spring.bms <- dplyr::bind_rows(cod.bms,yt.bms)

spring.bms$tot.area <- sum(nmfs.surv.gb$area)
spring.bms <- spring.bms %>% dplyr::mutate(wbn = (wt.mn.yr.strata*(area/tot.area)))
spring.bms <- spring.bms %>% group_by(year,species) %>% dplyr::mutate(wbd = sum(wbn))
spring.bms <- spring.bms %>% dplyr::mutate(wb = wbn/wbd)
spring.bms <- spring.bms %>% group_by(year,species) %>% arrange(wb,.by_group =T)
spring.bms <- spring.bms %>% group_by(year,species) %>% dplyr::mutate(cum.pbm = cumsum(wb))
spring.bms <- spring.bms %>% group_by(year,species) %>% dplyr::mutate(cum.area = cumsum(area))
spring.bms <- spring.bms %>% group_by(year,species) %>% dplyr::mutate(p.area = area/tot.area)
spring.bms <- spring.bms %>% group_by(year,species) %>% dplyr::mutate(cum.p.area = cumsum(p.area))
# The Gini is calculating the area under from the proportion of biomass by (* I think) and proportion of area figure
spring.bms$survey <- "Spring"
spring.bms <- spring.bms %>% dplyr::select(-set,-geometry)
spring.bms$strata <- as.character(spring.bms$strata)
spring.bms <- spring.bms %>% group_by(year,species) %>%  dplyr::mutate(Gini = gini(wb,p.area)) 


ggplot(spring.bms) + geom_line(aes(x = cum.area, y = wb, color = as.factor(year))) + facet_wrap(~species)
ggplot(spring.bms) + geom_line(aes(x = year, y = Gini,color=species)) + ylim(c(0,1))


# Now we do the same for NMFS Fall

# Now for RV survey get this
cod.bms <-  gini.nmfs %>% dplyr::filter(species == 'cod_PA', survey == 'nmfs-fall') %>% group_by(year,strata,area,.drop=F) %>% dplyr::summarize(wt.mn.yr.strata = mean(weight),num.mn.yr.strta = mean(number))
cod.bms <- cod.bms[,!names(cod.bms) == "area"]
cod.bms <- right_join(cod.bms,nmfs,by = c("strata",'year'))
cod.bms$species <- 'Atlantic Cod'
cod.bms$wt.mn.yr.strata[is.nan(cod.bms$wt.mn.yr.strata)] <- 0
cod.bms$num.mn.yr.strta[is.nan(cod.bms$num.mn.yr.strta)] <- 0
cod.bms$wt.mn.yr.strata[is.na(cod.bms$wt.mn.yr.strata)] <- 0
cod.bms$num.mn.yr.strta[is.na(cod.bms$num.mn.yr.strta)] <- 0

# Do the same for YT
yt.bms <-  gini.nmfs %>% dplyr::filter(species == 'yt_PA',survey == 'nmfs-fall') %>% group_by(year,strata,area,.drop=F) %>% dplyr::summarize(wt.mn.yr.strata = mean(weight),num.mn.yr.strta = mean(number))
yt.bms <- yt.bms[,!names(yt.bms) == "area"]
yt.bms <- right_join(yt.bms,nmfs,by = c("strata",'year'))
yt.bms$species <- 'Yellowtail Flounder'
yt.bms$wt.mn.yr.strata[is.nan(yt.bms$wt.mn.yr.strata)] <- 0
yt.bms$wt.mn.yr.strata[is.na(yt.bms$wt.mn.yr.strata)] <- 0
yt.bms$num.mn.yr.strta[is.nan(yt.bms$num.mn.yr.strta)] <- 0
yt.bms$num.mn.yr.strta[is.na(yt.bms$num.mn.yr.strta)] <- 0


fall.bms <- dplyr::bind_rows(cod.bms,yt.bms)
# For some reason a 1990 strata pops in here in 2001 and 2004, removing that...
fall.bms <- fall.bms %>% dplyr::filter(strata != 1990)
fall.bms$tot.area <- sum(nmfs.surv.gb$area)
fall.bms <- fall.bms %>% dplyr::mutate(wbn = (wt.mn.yr.strata*(area/tot.area)))
fall.bms <- fall.bms %>% group_by(year,species) %>% dplyr::mutate(wbd = sum(wbn))
fall.bms <- fall.bms %>% dplyr::mutate(wb = wbn/wbd)
fall.bms <- fall.bms %>% group_by(year,species) %>% arrange(wb,.by_group =T)
fall.bms <- fall.bms %>% group_by(year,species) %>% dplyr::mutate(cum.pbm = cumsum(wb))
fall.bms <- fall.bms %>% group_by(year,species) %>% dplyr::mutate(cum.area = cumsum(area))
fall.bms <- fall.bms %>% group_by(year,species) %>% dplyr::mutate(p.area = area/tot.area)
fall.bms <- fall.bms %>% group_by(year,species) %>% dplyr::mutate(cum.p.area = cumsum(p.area))
# The Gini is calculating the area under from the proportion of biomass by (* I think) and proportion of area figure
fall.bms$survey <- "Fall"
fall.bms <- fall.bms %>% dplyr::select(-set,-geometry)
fall.bms$strata <- as.character(fall.bms$strata)
fall.bms <- fall.bms %>% group_by(year,species) %>%  dplyr::mutate(Gini = gini(wb,p.area)) 


# Now put them all together
gini.surveys <- dplyr::bind_rows(rv.bms,spring.bms,fall.bms)
gini.surveys$survey <- factor(gini.surveys$survey,levels = c("Winter","Spring","Fall"))
#save(gini.surveys,file = "D:/Github/Paper_2_SDMs/Results/Gini_results.RData")
load("D:/Github/Paper_2_SDMs/Results/Gini_results.RData")
# Now we do the same with NMFS...

ggplot(gini.surveys) + geom_line(aes(x = cum.p.area, y = cum.pbm, color = year,group = year)) + facet_wrap(~species+survey) + 
  geom_abline(slope=1,intercept =0) + xlim(c(0,1)) + ylim(c(0,1)) + 
  ylab("Cumlative Proportion of Biomass") + xlab("Cumulative Area")  + scale_color_viridis_c(option = "A")

ggplot(gini.surveys) + geom_line(aes(x = year, y = Gini,color=species))  + facet_wrap(~survey) + ylim(c(0,1)) + xlab("") + ylab("Gini Index")

tot.area.rv <- sum(rv.surv.gb$area)





