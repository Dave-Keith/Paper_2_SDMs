####Build a mesh for the combined surveysthis has been updated to include methods suggested by Zuur and INLA teams.
# DK Aug 2021

rm(list=ls())
direct.proj <- "D:/Github/Paper_2_SDMs/"

#direct.fun <- "e:/R/"
#direct.proj <- "d:/Projects/GB_time_area_closure_SPERA/"


library(INLA)
library(boot)
library(fields)
library(PBSmapping)
require(tidyverse)
require(reshape2)
require(GGally)
require(bbmle)
require(MASS)
require(leaps)
require(COUNT)
require(cowplot)
require(viridis)
require(maptools)
require(gridExtra)
require(rgeos)
require(raster)
library(mapdata)
library(sf)


funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/convert_coords.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/combo_shp.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/centre_of_gravity.R",
          "https://raw.githubusercontent.com/Dave-Keith/Paper_2_SDMs/master/Scripts/predict_fields.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_inla_mesh_to_sf.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(un in funs)

#################### Section 1 Load in process data#################### Section 1 Load in process data#################### Section 1

# First we  we want to bring in the locations of the yellowtail and cod closures, we'll deal with these at the end of the section
new.dat.final <- readRDS(paste0(direct.proj,"data/Rev_1/new.dat.final.RDS"))



###################  Section 2 Preliminary Analysis for Mesh###################  Section 2 Preliminary Analysis for Mesh
###################  Section 2 Preliminary Analysis for Mesh###################  Section 2 Preliminary Analysis for Mesh
#----

# Frist thing we want to do is plot the data and see what the different survey data look like.  This is based on Zuur 
# and it is used to give us a sense of how far apart the points are for the different surveys.  



# Now I want to get the distances by survey and by year, this is a bit more involved than I first thought
# I don't want to make a bunch of different objects so run this through a loop
dis.by.surv <- NULL
surveys <- unique(new.dat.final$survey)
num.surveys <- length(surveys)

# These distance coefficient calculations are gigantic (69 GB, which maybe someone in 10 years will say you think that's big...)
# Anyways, to save on memory and speed I'm not going to save the object but will just plot sequentially and save the output file 
# for our enjoyment later...
count <- 0 ; j <- 0 ;k <- 0
p <- NULL;q <- NULL

# I can make the object but I can't get the figure to save more than the first images, wth???
#pdf(paste0(direct.proj,"Results/Figures/distance_plots_for_surveys.pdf"),onefile=T,width=8.5,height=11)
for(i in 1:num.surveys)
{
  pdf(paste0(direct.proj,"Results/Figures/distance_plots_for_",surveys[i],".pdf"),onefile=T,width=8.5,height=11)
  count = count + 1
  #subset to each survey
  dat.tmp <- data.frame(new.dat.final[new.dat.final$survey == surveys[i],])
  locs <-dat.tmp[,c("lat","lon")]
  # Get the distance coefficent for the combined surveys
  dis.tmp <- data.frame(D = as.vector(dist(locs)))
  cum.tmp <- data.frame(D = sort(dis.tmp$D),cum.prop = 1:nrow(dis.tmp)/nrow(dis.tmp))
  p[[count]] <- ggplot(dis.tmp,aes(D)) + geom_histogram()+ ggtitle(paste(surveys[i],"All years"))
  q[[count]] <- ggplot(cum.tmp,aes(D,cum.prop)) + geom_line() + ggtitle(paste(surveys[i],"All years"))
  # Now make the plot, only do this for every 9th figure.
  if(count == 5)
  {
    grid.arrange(p[[1]],q[[1]],p[[2]],q[[2]], p[[3]],q[[3]],p[[4]],q[[4]],p[[5]],q[[5]],nrow = 5, ncol = 2)
    # reset everything and start again.
    count = 0; p <- NULL ; q <- NULL
    #dev.off()
  }
  # Now how many years of data is there for this survey
  years <- sort(unique(dat.tmp$year))
  num.years <- length(years)
  # Now do this for every year we have a survey...
  for(j in 1:num.years) 
  {
    locs <-dat.tmp[dat.tmp$year == years[j],c("lat","lon")]
    count = count +1
    dis.tmp <- data.frame(D = as.vector(dist(locs)))
    cum.tmp <- data.frame(D = sort(dis.tmp$D),cum.prop = 1:nrow(dis.tmp)/nrow(dis.tmp))
    p[[count]] <- ggplot(dis.tmp,aes(D)) + geom_histogram()+ ggtitle(paste(surveys[i],"Year",years[j]))
    q[[count]] <- ggplot(cum.tmp,aes(D,cum.prop)) + geom_line()+ ggtitle(paste(surveys[i],"Year",years[j]))
    # Now make the plot, only do this for every 5th figure.
    if(count == 5)
    {
      grid.arrange(p[[1]],q[[1]],p[[2]],q[[2]], p[[3]],q[[3]],p[[4]],q[[4]],p[[5]],q[[5]],nrow = 5, ncol = 2)
      # reset everything and start again.
      count = 0; p <- NULL ; q <- NULL
      #dev.off()
      #windows(11,11)
    } # end if(count == 5)
  } # end for(j in 1:num.years) 
  dev.off()  
} # end for(i in 1:num.surveys)


###################  End Section 2 ###################  End Section 2 ###################  End Section 2 ###################  End Section 2 
###################  End Section 2 ###################  End Section 2 ###################  End Section 2 ###################  End Section 2 


##################  Section 3 - Now we make the meshes##################  Section 3 - Now we make the meshes##################  Section 3 -############
##################  Section 3 - Now we make the meshes##################  Section 3 - Now we make the meshes##################  Section 3 - ###########
#----
##################  Section 3 - Now we make the meshes##################  Section 3 - Now we make the meshes##################  Section 3 -############
##################  Section 3 - Now we make the meshes##################  Section 3 - Now we make the meshes##################  Section 3 - ###########
#----

# First we want to convert all of our coordinates to UTM, Zuur does this b/c of projection issues, UTM coordinates, for everything we are doing
# This is what are data are currently...
# epsg:4326 is Lat/Long and WGS84 specification, the EPSG thing is the "European Petroleum Survey Group" who put together a database of all the
# coordinate referece systems.  
# Other important ones Lat/Lon with NAD 83 = EPSG:4269, NAD 27 = EPSG:4267
# THe UTM zone we will mostly be dealing with is UTM 19 EPSG:32619 (Basically GOM and GB, also SPA3,6 and most of SFA 29) 
# or 20 EPSG:32620 (Most of Scotian shelf and most of the BoF)
prj4s <- CRS("+init=epsg:4326") 

utm.prj4s <- CRS("+init=epsg:32619")
# The location of the groundfish tows, make this a proper sf object...
loc.ll <- new.dat.final
loc.utm <- st_transform(loc.ll,crs=utm.prj4s) 
loc.sp.utm <- as_Spatial(loc.utm) # Needs to be sp Spatial Points object for INLA later

p <- ggplot(loc.utm) + geom_sf(aes(colour = survey),size=1)
windows(11,11);p

### define a non-convex hull boundary for the tow locations
# The convex and concave controls how much distance to add between the sampling points and the boundary, Zuur uses about
# 2/3's of the range or uses the default settings for this, doesn't explain why, but that's what he does.
# The default setting give something very similar to bound, while bounds gives a bit more distance b/t the points.
# I think bound is plenty conservative, especially for the piece of GB that we are really interested in for the Canadian side
# so I'll go with bound for this.
bound.inla <- inla.nonconvex.hull(loc.sp.utm,70000,70000) # That's about a 70 km border
#bound2 <- inla.nonconvex.hull(loc, 1, 1)
bound.sp.utm <- SpatialPolygons(list(Polygons(
  list(Polygon(bound.inla$loc, FALSE)), '0')), proj4=utm.prj4s)
#bound.sp.ll <- spTransform(bound.sp,prj4s)
bound.utm <- st_as_sf(bound.sp.utm)

sf_use_s2(FALSE) # Issues with these maps means I need to turn off the spherical on sf() until we clip the map to the bits we want...
### select a region from US and CA maps, this is so we can add in any land barriers to the mesh if needed (Cape Cod, Nantuck, Marthas V essentially)
us <- maps::map("worldHires", "USA",fill=TRUE,
                col="transparent", plot=FALSE)
IDs <- sapply(strsplit(us$names, ":"), function(x) x[1])
us.sp.ll <- map2SpatialPolygons(
  us, IDs=IDs, proj4string=prj4s)
# We can't transform because extent of US blows up our UTM projection
#us.sp <- spTransform(us.ll,utm.prj4s)
# Convert back to nicer sf object
us.ll <- st_as_sf(us.sp.ll)
# Now we'd need to cut out Hawaii and other far away places to make this utm...  and really shouldn't be using UTM for continental US, but it's fine...
clp = st_sfc(st_polygon(list(cbind(c(-90,-40,-40,-90,-90),c(30,30,70,70,30)))))
st_crs(clp) <- prj4s
# Clip to our our little world view.
us.clp.ll <- st_intersection(us.ll,clp)
# The UTM doesn't really cover all this area, but it's fine for our purposes...
us.utm <- st_transform(us.clp.ll,crs = utm.prj4s)

# now Canada
ca <- maps::map("worldHires", "Canada", fill=TRUE,
                col="transparent", plot=FALSE)
IDs <- sapply(strsplit(ca$names, ":"), function(x) x[1])
ca.sp.ll <- map2SpatialPolygons(
  ca, IDs=IDs, proj4string=prj4s)
ca.ll <- st_as_sf(ca.sp.ll)
# There are issues with the polygons coming from the maps this takes care of any "bad" polygons
ca.ll <- st_simplify(ca.ll,dTolerance = 0.00001)
# Clip to our our little world view.
ca.clp.ll <- st_intersection(ca.ll,clp)
# The UTM doesn't really cover all this area, but it's fine...
ca.utm <- st_transform(ca.clp.ll,crs = utm.prj4s)

sf_use_s2(TRUE) # Now we can turn the spherical geometry back on.


# A nice pretty plot of the area and tows...
ggplot(bound.utm) + geom_sf() + geom_sf(data = loc.utm) + 
  geom_sf(data=us.utm,fill='light grey')+ geom_sf(data=ca.utm,fill='grey') + coord_sf(xlim = c(330000,900000), ylim = c(4300000,4900000))

# Get sf object with island and land forms identified by number, this is so we can remove these from our boundary and eventually the mesh itself.
can.lab <- data.frame(X = NA, Y = NA, pnt=NA)
for (j in 1:length(ca.sp.ll@polygons[[1]]@Polygons))
{
  can.lab[j,] <- data.frame(X = ca.sp.ll@polygons[[1]]@Polygons[[j]]@labpt[1],Y = ca.sp.ll@polygons[[1]]@Polygons[[j]]@labpt[2], pnt = paste(j))
}
can.lab.ll <- st_as_sf(can.lab,coords = c("X","Y"),crs = prj4s)
can.lab.utm <- st_transform(can.lab.ll,crs =utm.prj4s)

us.lab <- data.frame(X = NA, Y = NA, pnt=NA)
for (j in 1:length(us.sp.ll@polygons[[1]]@Polygons))
{
  us.lab[j,] <- data.frame(X = us.sp.ll@polygons[[1]]@Polygons[[j]]@labpt[1],Y = us.sp.ll@polygons[[1]]@Polygons[[j]]@labpt[2], pnt = paste(j))
}  
us.lab.ll <- st_as_sf(us.lab,coords = c("X","Y"),crs = prj4s)
us.lab.utm <- st_transform(us.lab.ll,crs =utm.prj4s)


### which polygons are Islands that overlap with our mesh boundary, just have a couple along with the US mainland
ggplot(bound.utm) + geom_sf() + 
  geom_sf(data = loc.utm,color =grey(0.3,alpha=0.2) ) + 
  geom_sf(data=ca.utm,fill ='grey') + geom_sf(data=us.utm,fill = 'light grey') + 
  geom_sf_label(data = can.lab.utm, aes(label = pnt)) + 
  geom_sf_label(data = us.lab.utm, aes(label = pnt)) + 
  coord_sf(xlim = c(330000,900000), ylim = c(4300000,4900000))


### Remove islands from map + US mainland, nothing on Canadian side to worry about, but Nantucket and Martha's Vineyard gotta go.
### Plus a couple smaller islands, the US continent is 1 (this changes sometimes so check that...)
us.i.sp.ll <- SpatialPolygons(list(Polygons(
  #us.ll@polygons[[1]]@Polygons[c(46,47,48,49,59)],'0')), ## removing islands
  us.sp.ll@polygons[[1]]@Polygons[c(53,90,1)],'0')), ## removing islands
  proj4string=us.sp.ll@proj4string)
# Check that these make sense
us.i.ll <- st_as_sf(us.i.sp.ll)
us.i.utm <- st_transform(us.i.ll,crs = utm.prj4s)
plot(us.i.ll)
### make that as holes in the boundary
bound.i.utm <- st_difference(bound.utm, us.i.utm)
#bound.ll <- st_transform(bound,utm.prj4s)
bound.i.sp.utm <- as_Spatial(bound.i.utm)

### visualize it, can see US is now removed from the boundary for the mesh. Can add other layer if you'd like..
ggplot(bound.i.utm) + geom_sf(color = 3,fill = gray(.7)) #+ 
#geom_sf(data = ca.sf) + geom_sf(data = us.sf) + 
#coord_sf(xlim=c(-72,-60),ylim = c(39,46))
#geom_sf(data = loc, col=rainbow(1,alpha=0.1), cex=0.1, pch=19)

### build the mesh, a general reasonable statement I found

# Now we take a guess at an appropriate mesh range, this won't be the same for scallop, cod, and yellowtail.
# The RV survey spacing is also a bit different from the NMFS surveys, based on the distance between sampling
# locations we see that within a year there aren't a lot of stations within 0.5 minutes for NMFS (more like 0.25 for RV survey), but
# when we look at the aggregated data across all years there is a fair bit of data at small scales.  That said
# probably the correlation distance for cod and yellowtail will be in the 10's of kms, 
#has to do with how they move and whether there is preferred "habitat" I think...
#whereas for scallop it could be much smaller; I suspect this would be around the size of a bed, 
#
rangy <- 100*1000 # based on our initial work (see Res-Doc someday) it' looks like a range of around 100 is reasonable, that really helps with our mesh size too!
# be reasonable for the flat fish, I think we'll need something tighter for the scallop though
# The max edge should be 1/5 of the range, this defines the size of the boudary layer, this is courtesy Zuur chapter 19 and originates with 
# INLA guru Haakon Bakka.
# The true marginal variance should be equal to 1 and the SPDE approximation gets more accurate 
# when the maximum edge lenghth is close to and smaller than the correlation length of the process.
#We may conclude that the best resolution of the mesh would be the one with triangle length approximately equal to 
# or no larger than the process correlation length.
# We go with 1/5 of the range for the max.edge based on Zuur and 
max.edge <- rangy/5 

mesh <- inla.mesh.2d(loc.sp.utm, 
                        max.edge=c(1,5)*max.edge, cutoff=max.edge/5,
                        boundary=inla.sp2segment(bound.i.sp.utm))

mesh$n # This is a pretty big mesh and has will certainly not have any problematic edge effects!

# Now convert the mesh to an sf object, note this will contain the vertices and triangles
mesh.sf <- inla.mesh2sf(mesh)
# Add the projection since we didn't have one in the mesh object
st_crs(mesh.sf$triangles) <- utm.prj4s
st_crs(mesh.sf$vertices) <- utm.prj4s

### visualize the mesh and sample locations for the Groundfish surveys.
ggplot(mesh.sf$triangles) + geom_sf(size=0.2,fill= NA) + 
  geom_sf(data = bound.i.utm ,size=2,colour='blue',fill=NA) + 
  geom_sf(data=ca.utm) + geom_sf(data=us.utm) + 
  geom_sf(data = loc.utm,fill=rainbow(1,alpha=0.2),color=rainbow(1,alpha=0.2),size=1) + 
  coord_sf(xlim = c(330000,900000), ylim = c(4300000,4900000))



### save as R object
save(mesh,mesh.sf,rangy,bound.sp.utm,bound.utm,max.edge,loc.utm,file = paste0(direct.proj,"Data/Rev_1/INLA_meshes.RData"))



