# Note that you need a 32 bit version of R for this to work...
library(arcgisbinding)
library(sf)
library(raster)
library(stars)
arc.check_product() 
depth <- as.raster(arc.raster(arc.open("D:/CoML_layers.gdb/depth")))
sst <- as.raster(arc.raster(arc.open("D:/CoML_layers.gdb/sst_avg")))

save.image("D:/Dpeth_and_sst_rasters.RData")
