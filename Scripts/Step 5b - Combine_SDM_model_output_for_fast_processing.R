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

# Now I save all these seperately into relatively nice sized object for further analysis
save(all.rand.fields, file = paste0(direct.proj,"/Results/All_random_fields.RData"))
# The smaller and more reasonable best random fields...
save(select.rand.fields, file = paste0(direct.proj,"/Results/random_fields_from_top_models.RData"))

# All the model diagnostics
save(all.mod.diag, file = paste0(direct.proj,"/Results/All_model_diagnostics.RData"))
# All the model covariates, well the fixed effects, depth, chl, and sst.
save(all.mod.depth,all.mod.sst,all.mod.chl,all.mod.fixed, file = paste0(direct.proj,"/Results/All_model_covariate_fits.RData"))

# Now let's make some figures from these... all this silly loads above will have messed up our functions....

eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combo_shp.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",ssl.verifypeer = FALSE)))
#source("D:/Github/Offshore/Assessment_fns/DK/Maps/pectinid_projector_sf.R")
eval(parse(text =getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/centre_of_gravity.R",ssl.verifypeer = FALSE)))
eval(parse(text =getURL("https://raw.githubusercontent.com/Dave-Keith/Paper_2_SDMs/master/predict_fields.R",ssl.verifypeer = FALSE)))

# Our basemap
bp <- pecjector(area="GOM",plot=F,repo = 'github',add_layer = list(eez = 'eez',nafo = 'main',scale.bar = 'tl'),c_sys = 32619)
# The convex hull around our points...
clp <- st_convex_hull(st_union(st_as_sf(loc.gf)))
clp.poly <- st_as_sf(data.frame(X = c(508000,508000,900000,650000,600000,550000),
                                Y=c(4540000,4350000,4674000,4674000,4661000,4622000),ID=1),coords = c("X","Y"),crs= 32619)
# Now make this a polygon
clp.poly <- st_cast(st_combine(clp.poly),"POLYGON")
clp.pred <- st_intersection(clp,clp.poly)
# The random field for model X
fld <- select.rand.fields$`cod_PA nmfs-spring survey model.depth.sst st.10` 
brk <- pretty(fld$r.field.response,n=10)
range(fld$r.field.response)
eras <- unique(fld$era)
n.eras <- length(eras)

for(i in 1:n.eras)
{
  tmp.fld <- fld %>% dplyr::filter(era ==eras[i])
  tst2 <- pecjector(gg.obj = bp,c_sys = 32619,area = "GOM",
                    add_inla = list(field = tmp.fld$r.field.response,mesh = mesh.list,dims=c(50,50),clip = clp,
                                    scale= list(breaks = brk,limits = range(brk),alpha = 0.8)))
}

tst2 <- pecjector(gg.obj = bp,c_sys = 32619,area = "GOM",
                  add_inla = list(field = tmp.fld$r.field.response,mesh = mesh.list,dims=c(50,50),clip = clp,
                                  scale= list(breaks = brk,limits = range(brk),alpha = 0.8)))

mesh.list$crs <- CRS("+init=epsg:32619")

col <- addalpha(pals::viridis(101),1)
if(length(brk) <= 6) hgt <- unit(0.5,'cm')
if(length(brk) > 6 & length(brk) <= 12) hgt <- unit(1.75,'cm')
if(length(brk) > 12) hgt <- unit(2.5,'cm')
lims <- range(brk)

sf <- scale_fill_gradientn(colours = col, limits=lims,breaks=brk,name="Probability")
sc <- scale_colour_gradientn(colours = col, limits=lims,breaks=brk,name="Probability")

mesh.sf <- st_as_sf(data.frame(x = mesh.list$loc[,1], y = mesh.list$loc[,2]), coords = c('x','y'),crs = 32619)

st_geometry(fld) <- rep(st_geometry(mesh.sf),n.eras)
fld.clp <- st_intersection(fld,clp.pred)
# Facet version of the above NOTE HOW THE YEARS ARE F'd up now...
plt<- bp + geom_sf(data = fld.clp  ,aes(fill = r.field.response,colour=r.field.response))+
  facet_wrap(~as.factor(era)) +
  coord_sf(datum=32619) + sf + sc + #theme_map() +
  theme(legend.key.height =hgt,text = element_text(size=22)) + theme_map()

plt





