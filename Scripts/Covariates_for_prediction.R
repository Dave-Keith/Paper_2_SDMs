# Here I want to make a prediction grid that includes the spatial covariates which we are obtaining from the the SST and Depth Rasters
# So I think all I need to do is add a prediction stack to the input with NA's as resspone and covariate values within each mesh node...
# And predictions for each era too...
# Re-run the model and it will predict at these locations (in time/space).  That makes loads of sense... now how to make that matrix based on our mesh...


# Bring in the two rasters, which I've obtained from the Geodatabase found here... 
#.../NAS/Projects/GB_time_area_closure_SPERA/Data/CoML_layers.gdb
#I first converted these using the function Convert_raster_geodatabase_to_raster.R

library(INLA)
library(readxl)
library(xtable)
library(pander)
library(png)
library(PBSmapping)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(betareg)
library(MASS)
library(tidyverse)
library(mgcv)
library(boot)
library(cowplot)
library(sf)
library(raster)
library(stars)
library(inlabru)

direct.proj <- "D:/NAS/Projects/GB_time_area_closure_SPERA/" 
direct_fns <- "D:/Github/Offshore/Assessment_fns/DK/"

# Here are the rasters
load(paste0(direct.proj,"Data/Depth_and_sst_rasters.RData"))
# Here is the mesh and the data (which we don't really need right now...)
load(paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))
load(paste0(direct.proj,"Data/INLA_meshes.RData"))
# Pick our mesh...
mesh <- mesh.gf
# Give the mesh a crs, in this case 32619 is best
mesh$crs <- crs("+init=epsg:32619")

# Now I need to stick my UTM coordinates into the data
dat.final$X <- loc.gf@coords[,1]
dat.final$Y <- loc.gf@coords[,2]
# I need a loop for the number of "species" i'm looking at, Now all I care about are Cod and Yellowtail, everything 
# else is taken care of in Step 4.
species <- c("cod_PA","yt_PA") 
#species <- c("cod_PA_can","yt_PA_can") # These take the cod and YT data that are on the Canadian side of GB. 
num.species <- length(species)

# Convert the rasters into sf polygon data frames.
sst.sf <- st_as_sf(as(sst, "SpatialPolygonsDataFrame"))
depth.sf <- st_as_sf(as(depth, "SpatialPolygonsDataFrame"))


# To get a convex hull of GB...
gb.area.sf <- st_as_sf(loc.gf)
cent <- st_centroid(gb.area.sf)
clp <- st_convex_hull(st_union(cent)) 

save(sst.sf,depth.sf,gb.area.sf,clp,file=paste0(direct.proj,"Data/SST_and_Depth_covariates_and_boundary_for_prediction.RData"))

