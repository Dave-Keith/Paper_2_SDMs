# Here's a script to pull out the key model outputs so that I'm not loading files that are 1 GB in size all the time...

#direct.proj <- "Y:/Projects/GB_time_area_closure_SPERA/"
direct.proj <- "D:/NAS/Projects/GB_time_area_closure_SPERA/"

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

eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combo_shp.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/centre_of_gravity.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Dave-Keith/Paper_2_SDMs/master/predict_fields.R",ssl.verifypeer = FALSE)))


############# Here we grab the random fields, the depth, sst, and
direct.tmp <- direct.proj
load(paste0(direct.proj,"Results/INLA_st_output.RData"))
direct.proj <- direct.tmp

st.10.rf <- NULL
run.names <- names(mod.output)
n.mods <- length(run.names)
for(m in 1:n.mods)
{
  run.name <- run.names[m]
  print(run.name) # Just want to make sure it runs through all the models we want...
  # If it is a spatio-temporal model get this w index
  species <- unique(mod.output[[run.name]]$species)
  survey <- unique(mod.output[[run.name]]$survey)
  # If it is a spatio-temporal model get this w index
  w.ind <- w.index.list[[paste0("Spatio_temporal_",species,"_",survey,"_st.10")]]
  # Here is what ya ne1ed to do to get the spatio-temporal field working.
  eras <- unique(w.ind$w.repl)
  #if(st.mods[st] == 3)  eras <- unique(w.ind$w.group)
  n.eras <- length(eras)
  dat.eras <- eras
  # For the RV surve we want to add 2 to this as the eras go from 3-5 in the years_10 grouping...
  if(survey == "RV" ) dat.eras <- eras +2
  #if(surveys[i] == "RV" && st.mods[st] == 5) dat.eras <- eras +4
  #if(surveys[i] == "RV"&& st.mods[st] == 3) dat.eras <- eras + 6
  tmp.field <- NULL
  for (p in 1:n.eras)
  {
    # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
    # is always just our intercept.
    tmp.field[[p]] <- data.frame(r.field.raw = rand.field[[run.name]]$mean[w.ind$w.repl == eras[p]],
                                 r.field.link = rand.field[[run.name]]$mean[w.ind$w.repl == eras[p]] + res[[run.name]]$summary.fixed$mean[1],
                                 r.field.response = inv.logit(rand.field[[run.name]]$mean+ res[[run.name]]$summary.fixed$mean[1])[w.ind$w.repl == eras[p]],
                                 r.field.sd = rand.field[[run.name]]$sd[w.ind$w.repl == eras[p]],
                                 era = eras[p])
  }
  tf <- do.call("rbind",tmp.field)
  st.10.rf[[run.names[m]]] <- tf
}

mod.10s <- names(st.10.rf)
# Get rid of these models for the random fields
best.10s <- mod.10s[mod.10s %in% c("cod_PA nmfs-fall survey model.int st.10",
                                "cod_PA nmfs-spring survey model.int st.10",
                                "cod_PA RV survey model.int st.10",
                                "yt_PA RV survey model.int st.10",
                                "yt_PA nmfs-spring survey model.int st.10",
                                "yt_PA nmfs-fall survey model.int st.10",
                                "cod_PA nmfs-fall survey model.depth.sst st.10" ,
                                "cod_PA nmfs-spring survey model.depth.sst st.10",
                                "cod_PA RV survey model.depth.sst st.10",
                                "yt_PA RV survey model.depth.sst st.10",
                                "yt_PA nmfs-spring survey model.depth.sst st.10",
                                "yt_PA nmfs-fall survey model.depth.sst st.10")]
best.10.rf <- NULL
for(i in 1:length(best.10s)) best.10.rf[[best.10s[i]]] <- st.10.rf[[best.10s[i]]]



st.10.diag <- mod.diagnostics


mods.10 <- names(res)
n.mods.10 <- length(mods.10)


st.10.depth <- NULL
st.10.sst <- NULL
st.10.fixed <- NULL
st.10.chl <- NULL
for(i in 1:n.mods.10)
{
  st.10.depth[[mods.10[i]]]<- res[[mods.10[[i]]]]$summary.random$depth
  st.10.sst[[mods.10[i]]]<- res[[mods.10[[i]]]]$summary.random$sst
  st.10.chl[[mods.10[i]]]<- res[[mods.10[[i]]]]$summary.random$chl
  st.10.fixed[[mods.10[i]]]<- res[[mods.10[[i]]]]$summary.fixed
}

load(paste0(direct.proj,"Results/INLA_st_5_output.RData"))
direct.proj <- direct.tmp


# So I want to pull out the correlation distances, posteriors and fun stuff like that from the models.
# So here are the 3 best models for cod..

full.mod.5s <- list(cod.winter = res$`cod_PA RV survey model.depth.sst_st_5`,
               cod.spring = res$`cod_PA nmfs-spring survey model.depth.sst_st_5`,
               cod.fall  = res$`cod_PA nmfs-fall survey model.depth.sst_st_5`)



st.5.rf <- NULL
run.names <- names(mod.output)
n.mods <- length(run.names)
for(m in 1:n.mods)
{
  run.name <- run.names[m]
  print(run.name) # Just want to make sure it runs through all the models we want...
  species <- unique(mod.output[[run.name]]$species)
  survey <- unique(mod.output[[run.name]]$survey)
  # If it is a spatio-temporal model get this w index
  w.ind <- w.index.list[[paste0("Spatio_temporal_",species,"_",survey,"_5")]]
  # Here is what ya ne1ed to do to get the spatio-temporal field working.
  eras <- unique(w.ind$w.repl)
  #if(st.mods[st] == 3)  eras <- unique(w.ind$w.group)
  n.eras <- length(eras)
  dat.eras <- eras
  # For the RV surve we want to add 2 to this as the eras go from 3-5 in the years_10 grouping...
  #if(surveys[i] == "RV" ) dat.eras <- eras +2
  if(survey == "RV") dat.eras <- eras +4
  #if(surveys[i] == "RV"&& st.mods[st] == 3) dat.eras <- eras + 6
  tmp.field <- NULL
  for (p in 1:n.eras)
  {
    # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
    # is always just our intercept.
    tmp.field[[p]] <- data.frame(r.field.raw = rand.field[[run.name]]$mean[w.ind$w.repl == eras[p]],
                                 r.field.link = rand.field[[run.name]]$mean[w.ind$w.repl == eras[p]] + res[[run.name]]$summary.fixed$mean[1],
                                 r.field.response = inv.logit(rand.field[[run.name]]$mean+ res[[run.name]]$summary.fixed$mean[1])[w.ind$w.repl == eras[p]],
                                 r.field.sd = rand.field[[run.name]]$sd[w.ind$w.repl == eras[p]],
                                 era = eras[p])
  }
  tf <- do.call("rbind",tmp.field)
  st.5.rf[[run.names[m]]] <- tf
}





mod.5s <- names(st.5.rf)
# Get rid of these models for the random fields
best.5s <- mod.5s[mod.5s %in% c("cod_PA nmfs-fall survey model.int_st_5",
                                "cod_PA nmfs-fall survey model.depth.sst_st_5" ,
                                "cod_PA nmfs-spring survey model.depth.sst_st_5",
                                "cod_PA nmfs-spring survey model.int_st_5",
                                "cod_PA RV survey model.depth.sst_st_5",
                                "cod_PA RV survey model.int_st_5",
                                "yt_PA RV survey model.int_st_5",
                                "yt_PA RV survey model.depth.sst_st_5",
                                "yt_PA nmfs-spring survey model.depth.sst_st_5",
                                "yt_PA nmfs-spring survey model.int_st_5",
                                "yt_PA nmfs-fall survey model.depth.sst_st_5",
                                "yt_PA nmfs-fall survey model.int_st_5")]
best.5.rf <- NULL
for(i in 1:length(best.5s)) best.5.rf[[best.5s[i]]] <- st.5.rf[[best.5s[i]]]

st.5.diag <- mod.diagnostics

mods.5 <- names(res)
n.mods.5 <- length(mods.5)
st.5.depth <- NULL
st.5.sst <- NULL
st.5.fixed <- NULL
st.5.chl <- NULL
for(i in 1:n.mods.5)
{
  st.5.depth[[mods.5[i]]]<- res[[mods.5[[i]]]]$summary.random$depth
  st.5.sst[[mods.5[i]]]<- res[[mods.5[[i]]]]$summary.random$sst
  st.5.chl[[mods.5[i]]]<- res[[mods.5[[i]]]]$summary.random$chl
  st.5.fixed[[mods.5[i]]]<- res[[mods.5[[i]]]]$summary.fixed
}

load(paste0(direct.proj,"Results/INLA_st_3_output.RData"))
direct.proj <- direct.tmp


# So I want to pull out the correlation distances, posteriors and fun stuff like that from the models.
# So here are the 5 best models for cod..

full.mod.3s <- list(yt.winter = res$`yt_PA RV survey model.depth.sed.sst_st_3`,
               yt.spring = res$`yt_PA nmfs-spring survey model.depth.sed.sst_st_3`,
               yt.fall  = res$`yt_PA nmfs-fall survey model.depth.sed.sst_st_3`)


st.3.rf <- NULL
run.names <- names(mod.output)
n.mods <- length(run.names)
for(m in 1:n.mods)
{
  run.name <- run.names[m]
  print(run.name) # Just want to make sure it runs through all the models we want...
  species <- unique(mod.output[[run.name]]$species)
  survey <- unique(mod.output[[run.name]]$survey)
  # If it is a spatio-temporal model get this w index
  w.ind <- w.index.list[[paste0("Spatio_temporal_",species,"_",survey,"_3")]]
  # Here is what ya need to do to get the spatio-temporal field working.
  eras <- unique(w.ind$w.group)
  n.eras <- length(eras)
  dat.eras <- eras
  # For the RV surve we want to add 2 to this as the eras go from 3-5 in the years_10 grouping...
  #if(surveys[i] == "RV" ) dat.eras <- eras +2
  #if(survey == "RV") dat.eras <- eras +4
  if(survey == "RV") dat.eras <- eras + 6
  tmp.field <- NULL
  for (p in 1:n.eras)
  {
    # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
    # is always just our intercept.
    tmp.field[[p]] <- data.frame(r.field.raw = rand.field[[run.name]]$mean[w.ind$w.group == eras[p]],
                                 r.field.link = rand.field[[run.name]]$mean[w.ind$w.group== eras[p]] + res[[run.name]]$summary.fixed$mean[1],
                                 r.field.response = inv.logit(rand.field[[run.name]]$mean+ res[[run.name]]$summary.fixed$mean[1])[w.ind$w.group == eras[p]],
                                 r.field.sd = rand.field[[run.name]]$sd[w.ind$w.group == eras[p]],
                                 era = eras[p])
  }
  tf <- do.call("rbind",tmp.field)
  st.3.rf[[run.names[m]]] <- tf
}

st.3.diag <- mod.diagnostics

mods.3 <- names(res)
n.mods.3 <- length(mods.3)
st.3.depth <- NULL
st.3.sst <- NULL
st.3.fixed <- NULL
st.3.chl <- NULL
for(i in 1:n.mods.3)
{
  st.3.depth[[mods.3[i]]]<- res[[mods.3[[i]]]]$summary.random$depth
  st.3.sst[[mods.3[i]]]<- res[[mods.3[[i]]]]$summary.random$sst
  st.3.chl[[mods.3[i]]]<- res[[mods.3[[i]]]]$summary.random$chl
  st.3.fixed[[mods.3[i]]]<- res[[mods.3[[i]]]]$summary.fixed
}


all.rand.fields <- c(st.10.rf,st.5.rf,st.3.rf) # Random fields
# Subset this to the most interesting random fields
select.rand.fields <- c(best.10.rf,best.5.rf,st.3.rf)




all.mod.diag <- c(st.10.diag,st.5.diag,st.3.diag) # Model diagnostics
# The best model covariates...
all.mod.depth <- c(st.10.depth,st.5.depth,st.3.depth)
all.mod.sst <- c(st.10.sst,st.5.sst,st.3.sst)
all.mod.chl <- c(st.10.chl,st.5.chl,st.3.chl)
all.mod.fixed <- c(st.10.fixed,st.5.fixed,st.3.fixed)

## Here we get teh full model results from the top 6 models toegether
best.full.mods <- list(yt.mods = full.mod.3s,cod.mods = full.mod.5s)

# Now I save all these seperately into relatively nice sized object for further analysis
#save(all.rand.fields, file = paste0(direct.proj,"/Results/All_random_fields.RData"))
# The smaller and more reasonable best random fields...
#save(select.rand.fields, file = paste0(direct.proj,"/Results/random_fields_from_top_models.RData"))

# All the model diagnostics
#save(all.mod.diag, file = paste0(direct.proj,"/Results/All_model_diagnostics.RData"))
# All the model covariates, well the fixed effects, depth, chl, and sst.
#save(all.mod.depth,all.mod.sst,all.mod.chl,all.mod.fixed, file = paste0(direct.proj,"/Results/All_model_covariate_fits.RData"))

#save(best.full.mods,file = paste0(direct.proj,"/Results/top_yt_cod_models_full_results.RData"))

load(paste0(direct.proj,"/Results/top_yt_cod_models_full_results.RData"))
# To get the posterior marginals
pm.dep.prec <- as.data.frame(best.full.mods$cod.mods$cod.winter$marginals.hyperpar$`Precision for depth`)
pm.sst.prec <- as.data.frame(best.full.mods$cod.mods$cod.winter$marginals.hyperpar$`Precision for sst`)
pm.range.field <- as.data.frame(best.full.mods$cod.mods$cod.winter$marginals.hyperpar$`Range for w`)
pm.sd.field <- as.data.frame(best.full.mods$cod.mods$cod.winter$marginals.hyperpar$`Stdev for w`)

# Here's what we really want to save, the hyper posteriors and the estimates
# for each model, we'll wanna plot all of these and I think that's enough figures
# For one lifetime.
hyper.p.est <- best.full.mods$cod.mods$cod.winter$summary.hyperpar
hyper.p.post <- best.full.mods$cod.mods$cod.winter$marginals.hyperpar

# Now extract the hyperparameter estimates and posteriors from the model
hyper.mod.est <- NULL
hyper.mod.post <- NULL
# Sorry me this is really lazy coding...
for(i in 1:2)
{
  tmp <- best.full.mods[[i]]
  mods <- names(tmp)
  for(j in 1:3)
  {
    tmp2 <- tmp[[j]]
    model <- mods[j]
    hyper.mod.est[[model]] <- tmp2$summary.hyperpar
    hyper.mod.est[[model]]$model <- model
    hyper.mod.est[[model]]$hyper <- rownames(hyper.mod.est[[model]])
    if(i == 1) hyper.mod.est[[model]]$species <- "Yellowtail"
    if(i == 2) hyper.mod.est[[model]]$species <- "Cod"
    if(j == 1) hyper.mod.est[[model]]$survey <- "Winter"
    if(j == 2) hyper.mod.est[[model]]$survey <- "Spring"
    if(j == 3) hyper.mod.est[[model]]$survey <- "Fall"

    dep <- as.data.frame(tmp2$marginals.hyperpar$`Precision for depth`)
    dep$x <- sqrt(1/dep$x)
    dep$hyper <- "\u03C3 (Dep)"
    sst <- as.data.frame(tmp2$marginals.hyperpar$`Precision for sst`)
    sst$x <- sqrt(1/sst$x)
    sst$hyper <-"\u03C3 (SST)"
    range <- as.data.frame(tmp2$marginals.hyperpar$`Range for w`)
    range$x <- range$x/1000
    range$hyper <- "Range of Field (km)"
    sd <- as.data.frame(tmp2$marginals.hyperpar$`Stdev for w`)
    sd$hyper <- "SD of Field"
    tmp3 <- rbind(dep,sst,range,sd)
    if(i == 1) tmp3$species <- "Yellowtail"
    if(i == 2) tmp3$species <- "Cod"
    if(j == 1) tmp3$survey <- "Winter"
    if(j == 2) tmp3$survey <- "Spring"
    if(j == 3) tmp3$survey <- "Fall"
    hyper.mod.post[[model]] <- tmp3
    hyper.mod.post[[model]]$model <- model
  }
}

hyper.mod.post <- do.call('rbind',hyper.mod.post)
hyper.mod.post$hyper <- factor(hyper.mod.post$hyper,levels = c("\u03C3 (Dep)",
                                                               '\u03C3 (SST)',
                                                               'Range of Field (km)',
                                                               'SD of Field'))

hyper.mod.est <- do.call('rbind',hyper.mod.est)
hyper.mod.est$survey <- factor(hyper.mod.est$survey, levels = c("Winter","Spring","Fall"))
hyper.mod.post$survey<- factor(hyper.mod.post$survey, levels = c("Winter","Spring","Fall"))

#save(hyper.mod.est,hyper.mod.post,file = paste0(direct.proj,"/Results/hyper_parameters.RData"))
# The marginals for the random effects are for each RE (so each node in the w field, and each know in the RW)
# So those are way too intense to show anywhere.  I'll take the intercept and the
# hyperparmeters.















