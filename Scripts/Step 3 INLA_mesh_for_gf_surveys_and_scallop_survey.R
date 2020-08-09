####Build a mesh for the combined surveys and for the scallop survey, this has been updated to include methods suggested by Zuur and INLA teams.
# DK edits in Fall 2018 and Winter 2019

rm(list=ls())
direct.fun <- "Y:/Offshore scallop/Assessment/"
direct.proj <- "Y:/Projects/GB_time_area_closure_SPERA/"
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
source(paste(direct.fun,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep=""))

#################### Section 1 Load in process data#################### Section 1 Load in process data#################### Section 1
#################### Section 1 Load in process data#################### Section 1 Load in process data
#----
# First we  we want to bring in the locations of the yellowtail and cod closures, we'll deal with these at the end of the section
load(paste(direct.proj,"Results/closure.locations.RData",sep=""))
# Next I want to bring in the results of the PCA, this has all the spatial information we'd ever want and has the data in nice shape.
load(paste0(direct.proj,"Results/PCA_and_vif_enviro_data.RData"))
nmfs.spr.final$survey <- "nmfs-spring"
nmfs.fall.final$survey <- "nmfs-fall"
RV.surv.final$survey <- "RV"

# Next up I want the scallop survey data so I can map the scallop abundance.  I need to go somewhere different to get this info...
load(paste(direct.fun,"Data/Survey_data/2018/Survey_summary_output/Survey_all_results.Rdata",sep=""))

# Not the concious decision to use all the survey data, i.e. including the industry stations, I might subset later, but interested to see
# how that looks...
scal.surv.dat <- rbind(surv.Live$GBa,surv.Live$GBb)
# I'd like to "standardize" the scallop data in a couple ways as well.  Standardize the FR, Rec, and 3 size classes of PR's to be bound by
# 0 and 1.  To do this we'll wanna do a beta regression, which is possible happily in INLA>
# So first we want to get the maximum tow for each of the respective bins of interest by year.
# To make this easier I need to tweak the column names for bin 50-70 as R sees the - as a negative.
colnames(scal.surv.dat)[which(names(scal.surv.dat) %in% c("bin_50-70","bin_70-85"))] <- c("bin_50_70","bin_70_85")
maxs <- aggregate(cbind(bin_lt_50,bin_50_70,bin_70_85,rec,com)~year,max,data=scal.surv.dat)
years <- maxs$year
num.years <- nrow(maxs)
stan.names <- paste(names(maxs[-1]),"stan",sep="_")
new.dat <- as.data.frame(matrix(NA,ncol=length(stan.names),nrow = nrow(scal.surv.dat)))
names(new.dat) <- stan.names
scal.surv.dat <- cbind(scal.surv.dat,new.dat)
# So this gets me the standardized biomass for each year, doesn't account for potential impact of
# more effort in high biomass years, which we could account for as well, might be easier to account for it as 
# another term in the model really rather than making another set of response variables that are scaled to maximum ever observed
# biomass, I don't mind that idea if it comes to it (not sure it will mind you)
# Now I need to revise this because the highest abundance tows completely swamp the analysis, we really don't 
# care how high the high areas are, we just want to identify them, but when you get one tow that 
# is far larger than the rest it entirely overpowers the rest of the analysis, the data is just
# ridiculously right skewed, we know the top 10% of the tows are going to be areas of high interest,
# So can we justify treating the top 10% of the tows as areas of high interest, these tows
# will all recieve a value near 1 as they are highly probably regions in which 
# the fishery will occur.
for(i in 1:num.years)
{
  # So we want to identify the top 10% of the tows in each year, all tows above 
  # this are treated as having a similar probability of fishing occuring in them.
  # The -1 excludes the year from the maxs object...
  tmp2 <- scal.surv.dat[scal.surv.dat$year == years[i],names(scal.surv.dat) %in% names(maxs)[-1]]
  perct.max <- apply(tmp2,2, FUN = function(x) quantile(x,probs=0.9))
  # Now replace any value above this percentil with this percentile, I think this will be fine for a
  # a beta regression since the distribution is so damn flexible between 0 and 1

  for(j in 1:length(perct.max)) tmp2[which(tmp2[,j] > perct.max[j]),j] <- perct.max[j]
  res <- t(t(tmp2)  / apply(tmp2,2,max))
  # Now a beta regression (which is what we have coming) can't have 0's and 1's, based on Zuur the way to deal with this 
  # is to use the transformation (Obs * (N -1) + 0.5)/N, see Smithson and Verkuilen 2006
  # Doesn't screw up the regression to come...
  tst <- as.data.frame((res * (nrow(res)-1) + 0.5)/nrow(res))
  names(tst) <- paste(names(tst),"stan",sep="_")
  scal.surv.dat[scal.surv.dat$year == years[i],stan.names ] <- tst
}
# I also want to replace the survey column with "scallop" rather than 'summer' so as not to confuse things
scal.surv.dat$survey <- "scallop"
# So there are two pieces of data of interest, the data which is intersected with the PCA analysis and the data that isn't
# There was a small amount of data lost in the PCA analysis due to missing enivironmental data (somewhere around 5%)
# This is a list with the complete PCA analysis data in it.  The data retained are for the adults in the population only, we 
# could get the whole population or just the juvenilles as well.  Given we are looking generally at spawning aggregations 
# we stick with the adults for now, not a problem to include other ages tho.
#dat.pca <- final.dat
dat.final<- rbind(nmfs.spr.final,nmfs.fall.final,RV.surv.final)
# This includes every tow, but doesn't have the PCA tied to it, it does have depth so I would like to try a smiple model with depth included...
#cols <- c("unique_set_ID","lat_dd","lon_dd","year","strata","COD_number_Adu", "COD_PA_Adu" ,"YT_number_Adu" ,"YT_PA_Adu" , "comldepth","survey")

# So what I want to do is look at the spatial picture by survey, the DFO survey gives us a snapshot in Feb-March
# The NMFS-spring gives us the picture in April-May
# The NMFS-fall gives us the picture in the Fall, with these 3 surveys we really can get a picture of the seasonal movement of these
# species on the bank.  We probably could put this all into one wacky model, but for now let's just compare the survey's in individual models
# what I want to look at first is the overall patterns by survey, and then look at the patterns by year, 5 years, and decade
# After I look through that I think I toss in some of the PCA covariates and the more obvious covariates (e.g. depth)
# and see how these influence the model...
#dat.surv <- rbind(nmfs.spr[,cols],nmfs.fall[,cols],RV.surv[,cols])
# We should be able to combine the dat.surv with the dat.surv.pca to have the pca values tied to the entire set of survey data...
#dat.final <- left_join(dat.surv,dat.survey.pca)
head(dat.final)
# Now I want to chuck the duplicated data so I don't get confused later.  The PCA data contains NA's for about 5% of the data where we didn't have
# covariate data at a location.
#toss <- which(names(dat.final) %in% c("lat","lon","cod","cod_PA","yt","yt_PA"))
#dat.final <- dat.final[,-toss]

# If we want to compare the patterns across decades we'll need a grouping vector on the year.
# How much data do we have by year, notice that the amount of data improves dramaticaly in the mid-1980's when the RV survey starts up on GB
table(dat.final$year)

# Set the eras to 1, saves me doing some fancy code stick handling later.  I want the eras to count backwards, so the 
# most recent year of data is the most recent era.
dat.final$years_10 <- 1
dat.final$years_5 <- 1
dat.final$years_3 <- 1

eras.10 <- c(seq(max(dat.final$year),min(dat.final$year), by = -10),min(dat.final$year))
eras.5 <- c(seq(max(dat.final$year),min(dat.final$year), by = -5),min(dat.final$year))
eras.3 <- c(seq(max(dat.final$year),min(dat.final$year), by = -3),min(dat.final$year))
n.eras.10 <- length(eras.10)
n.eras.5 <- length(eras.5)
n.eras.3 <- length(eras.3)
# Order the year groups, I know this works if numbered, not sure how it works if they were factors, so sticking with what I know works..
for(i in 1:(n.eras.10-1)) dat.final$years_10[dat.final$year <= eras.10[i] & dat.final$year > eras.10[i+1]] <- n.eras.10-i
for(i in 1:(n.eras.5-1)) dat.final$years_5[dat.final$year <= eras.5[i] & dat.final$year > eras.5[i+1]] <- n.eras.5-i
for(i in 1:(n.eras.3-1)) dat.final$years_3[dat.final$year <= eras.3[i] & dat.final$year > eras.3[i+1]] <- n.eras.3-i

                                             
# Now do the same for the scallop, I again want to start in the most recent year and go backwards, so code is the same as above...
scal.surv.dat$years_10 <- 1
scal.surv.dat$years_5 <- 1
scal.surv.dat$years_3 <- 1

sc.eras.10 <- c(seq(max(scal.surv.dat$year),min(scal.surv.dat$year), by = -10),min(scal.surv.dat$year))
sc.eras.5 <- c(seq(max(scal.surv.dat$year),min(scal.surv.dat$year), by = -5),min(scal.surv.dat$year))
sc.eras.3 <- c(seq(max(scal.surv.dat$year),min(scal.surv.dat$year), by = -3),min(scal.surv.dat$year))
n.sc.eras.10 <- length(sc.eras.10)
n.sc.eras.5 <- length(sc.eras.5)
n.sc.eras.3 <- length(sc.eras.3)
for(i in 1:(n.sc.eras.10-1)) scal.surv.dat$years_10[scal.surv.dat$year <= sc.eras.10[i] & scal.surv.dat$year > sc.eras.10[i+1]] <- n.sc.eras.10-i
for(i in 1:(n.sc.eras.5-1)) scal.surv.dat$years_5[scal.surv.dat$year <= sc.eras.5[i] & scal.surv.dat$year > sc.eras.5[i+1]] <- n.sc.eras.5-i
for(i in 1:(n.sc.eras.3-1)) scal.surv.dat$years_3[scal.surv.dat$year <= sc.eras.3[i] & scal.surv.dat$year > sc.eras.3[i+1]] <- n.sc.eras.3-i



# Finally we get the unique set of closed cells for the bank 
yt.unique.closures <- findPolys(cells,subset(closure, Species == "Flounder"))
cod.unique.closures <- findPolys(cells,subset(closure, Species == "Cod"))
yt.closed.cells <- as.PolySet(subset(GBa.grid,PID %in% unique(yt.unique.closures$EID)),projection="LL")
cod.closed.cells <- as.PolySet(subset(GBa.grid,PID %in% unique(cod.unique.closures$EID)),projection="LL")

# This gets the cell ID from the GBa grid in which closures occured
unique.closures <- findPolys(cells,closure)
# This gets the actual closed cells polygon for each unique closed cell.  These become WGS84, which is what we want
# based on discussions with Alan Reeves (email saved here Y:\Projects\GB_time_area_closure_SPERA\Emails\RE  GB closures and Datums.msg)
closed.cells <- as.PolySet(subset(GBa.grid,PID %in% unique(unique.closures$EID)),projection="LL")
closed.cells <- PolySet2SpatialPolygons(closed.cells)
yt.closed.cells <- PolySet2SpatialPolygons(yt.closed.cells)
cod.closed.cells <- PolySet2SpatialPolygons(cod.closed.cells)


save(dat.final,scal.surv.dat,yt.closed.cells,cod.closed.cells,vif.variables,file = paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))


#################### End Section 1 get data #################### End Section 1 get data #################### End Section 1 get data ###
#################### End Section 1 get data ################### End Section 1 get data #################### End Section 1 get data ###

###################  Section 2 Preliminary Analaysis for Mesh###################  Section 2 Preliminary Analaysis for Mesh
###################  Section 2 Preliminary Analaysis for Mesh###################  Section 2 Preliminary Analaysis for Mesh
#----
load(file = paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))

# Frist thing we want to do is plot the data and see what the different survey data look like.  This is based on Zuur 
# and it is used to give us a sense of how far apart the points are for the different surveys.  You shouldn't need to run this
# as the pdf output already exists (it takes a couple minute to run)

# Now I want to get the distances by survey and by year, this is a bit more involved than I first thought
# I don't want to make a bunch of different objects so run this through a loop
dis.by.surv <- NULL
surveys <- c(unique(dat.final$survey),unique(scal.surv.dat$survey))
num.surveys <- length(surveys)

# These distance coefficient calculations are gigantic (69 GB, which maybe someone in 10 years will say you think that's big...)
# Anyways, to save on memory and speed I'm not going to save the object but will just plot sequentially and save the output file 
# for our enjoyment later...
count <- 0 ; j <- 0 ;k <- 0
p <- NULL;q <- NULL

# I can make the object but I can't get the figure to save more than the first images, wth???
pdf(paste0(direct.proj,"Results/Figures/distance_plots_for_gf_and_scallop_surveys.pdf"),onefile=T,width=8.5,height=11)
for(i in 1:num.surveys)
{
  count = count + 1
  #subset to each survey
  if(surveys[i] != "scallop") dat.tmp <- dat.final[dat.final$survey == surveys[i],]
  if(surveys[i] == "scallop") dat.tmp <- scal.surv.dat
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
  years <- unique(dat.tmp$year)
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
  # And we can also get this for our "year grouping"
  year.groups <- unique(dat.tmp$year.group)
  num.year.groups <- length(year.groups)
  for(k in 1:num.year.groups) 
  {  
    locs <-dat.tmp[dat.tmp$year.group == year.groups[k],c("lat","lon")]
    dis.tmp <- data.frame(D = as.vector(dist(locs)))
    cum.tmp <- data.frame(D = sort(dis.tmp$D),cum.prop = 1:nrow(dis.tmp)/nrow(dis.tmp))
    p[[count]] <- ggplot(dis.tmp,aes(D)) + geom_histogram() + ggtitle(paste(surveys[i],"Year cluster",year.groups[k]))
    q[[count]] <- ggplot(cum.tmp,aes(D,cum.prop)) + geom_line() + ggtitle(paste(surveys[i],"Year cluster",year.groups[k]))
    # Now make the plot, only do this for every 9th figure.
    if(count == 5)
    {
      grid.arrange(p[[1]],q[[1]],p[[2]],q[[2]], p[[3]],q[[3]],p[[4]],q[[4]],p[[5]],q[[5]],nrow = 5, ncol = 2)
      # reset everything and start again.
      count = 0; p <- NULL ; q <- NULL
      #dev.off()
    }
  }
} # end fo(i in 1:num.surveys)
dev.off()
###################  End Section 2 ###################  End Section 2 ###################  End Section 2 ###################  End Section 2 
###################  End Section 2 ###################  End Section 2 ###################  End Section 2 ###################  End Section 2 


##################  Section 3 - Now we make the meshes##################  Section 3 - Now we make the meshes##################  Section 3 -############
##################  Section 3 - Now we make the meshes##################  Section 3 - Now we make the meshes##################  Section 3 - ###########
#----

# First we want to convert all of our coordinates to UTM, Zuur does this b/c of projection issues, UTM coordinates, for everything we are doing
# we are in UTM zone 20 I believe, the sp package figures this all out itself happily.
# This is what are data are currently...
# epsg:4326 is Lat/Long and WGS84 specification, the EPSG thing is the "European Petroleum Survey Group" who put together a database of all the
# coordinate referece systems.  
# Other important ones Lat/Lon with NAD 83 = EPSG:4269, NAD 27 = EPSG:4267
# THe UTM zone we will mostly be dealing with is UTM 19 EPSG:32619 (Basically GOM and GB, also SPA3,6 and most of SFA 29) 
# or 20 EPSG:32620 (Most of Scotian shelf and most of the BoF)
prj4s <- CRS("+init=epsg:4326") 

utm.prj4s <- CRS("+init=epsg:32619")
# The location of the groundfish tows, make this a proper spatial point object
loc.gf.ll <- SpatialPoints(data.frame(X=dat.final$lon, Y = dat.final$lat),proj4string = prj4s)
# Now we can convert these to UTM
loc.gf <- spTransform(loc.gf.ll,utm.prj4s)
log.gf.sf.ll <- st_as_sf(loc.gf.ll)
  
  
plot(loc.gf,cex=0.1,pch=19)
# The location of the scallop survey tows
loc.sc.ll <- SpatialPoints(data.frame(X = scal.surv.dat$lon,Y = scal.surv.dat$lat),proj4string = prj4s)
# Now we can convert these to UTM
loc.sc <- spTransform(loc.sc.ll,utm.prj4s)

plot(loc.sc,cex=0.1,pch=19)

### define a non-convex hull boundary for the tow locations
# The convex and concave controls how much distance to add between the sampling points and the boundary, Zuur uses about
# 2/3's of the range or uses the default settings for this, doesn't explain why, but that's what he does.
# The default setting give something very similar to bound, while bounds gives a bit more distance b/t the points.
# I think bound is plenty conservative, especially for the piece of GB that we are really interested in for the Canadian side
# so I'll go with bound for this.
bound.gf <- inla.nonconvex.hull(loc.gf,70000,70000) # That's about a 70 km border
#bound2.gf <- inla.nonconvex.hull(loc.gf, 1, 1)
bound.gf.sp <- SpatialPolygons(list(Polygons(
  list(Polygon(bound.gf$loc, FALSE)), '0')), proj4=utm.prj4s)
bound.gf.sp.ll <- spTransform(bound.gf.sp,prj4s)
bound.gf.sf.ll <- st_as_sf(bound.gf.sp.ll)

### select a region from US and CA maps, this is so we can add in any land barriers to the mesh if needed (Cape Cod, Nantuck, Marthas V essentially)
us <- maps::map("worldHires", "USA",fill=TRUE,
                col="transparent", plot=FALSE)
IDs <- sapply(strsplit(us$names, ":"), function(x) x[1])
us.sp <- map2SpatialPolygons(
  us, IDs=IDs, proj4string=prj4s)
# Doesn't work as utm blows up given size of world US has colonized...
#us.sp <- spTransform(us.sp,utm.prj4s)
us.sf <- st_as_sf(us.sp)
#box = c(xmin = -70, ymin = 35, xmax = -50, ymax = 48)
#us.sf <- st_crop(us.sf,box)

# now canada
ca <- maps::map("worldHires", "Canada", fill=TRUE,
                col="transparent", plot=FALSE)
IDs <- sapply(strsplit(ca$names, ":"), function(x) x[1])

ca.sp <- map2SpatialPolygons(
  ca, IDs=IDs, proj4string=prj4s)
ca.ll <- ca.sp
ca.sp <- spTransform(ca.sp,utm.prj4s)
ca.sf <- st_as_sf(ca.sp)

# A nice pretty plot of the area and tows...
ggplot(bound.gf.sf.ll) + geom_sf() + geom_sf(data = log.gf.sf.ll) + 
                         geom_sf(data=us.sf,fill='light grey')+ geom_sf(data=ca.sf,fill='grey') + coord_sf(xlim = c(-72,-60), ylim = c(39,46))

### which polygons from CA map are the islands?
plot(bound.gf.sp.ll,xlim=c(-70,-60),ylim = c(39,43))
#plot(bound2.gf.sp, add=TRUE)
points(loc.gf.ll, pch=19, col=grey(0.3,alpha=0.2), cex=0.2)
# Plot the maps and add points for any land
plot(ca.ll, add=T, border=2,col='grey')
for (j in 1:length(ca.sp@polygons[[1]]@Polygons))
  text(ca.ll@polygons[[1]]@Polygons[[j]]@labpt[1],
       ca.ll@polygons[[1]]@Polygons[[j]]@labpt[2], paste(j))

plot(us.sp, add=T, border=2,col='grey')
for (j in 1:length(us.sp@polygons[[1]]@Polygons))
  text(us.sp@polygons[[1]]@Polygons[[j]]@labpt[1],
       us.sp@polygons[[1]]@Polygons[[j]]@labpt[2], paste(j))


### Remove islands from map + US mainland, nothing on Canadian side to worry about, but Nantucket and Martha's Vineyard gotta go.
### Plus a couple smaller islands, the US contentent is 59
us.i <- SpatialPolygons(list(Polygons(
  us.sp@polygons[[1]]@Polygons[c(46,47,48,49,59)],'0')), ## removing islands
  proj4string=us.sp@proj4string)
### make that as holes in the boundary
bound.gf.sp <- gDifference(bound.gf.sp, us.i)
#bound2.gf.sp <- gDifference(bound2.gf.sp, us.i)

### visualize it
plot(bound.gf.sp, border=3, col=gray(.7), asp=1)
plot(ca.sp, add=TRUE, border=5, lty=2, lwd=2)
points(loc.gf, col=rainbow(1,alpha=0.1), cex=0.1, pch=19)

# For the scallop we know we don't have any land barriers to worry about so the boundary is a bit simplier to deal with
# The boundary here can be much tigher than we had with the whole GB area, Later you'll see I'm guessing 
# the range to be about 2% of the groundfish range, so start with the convex/concave to be about 2% of what we had for gf
bound.sc <- inla.nonconvex.hull(loc.sc,14000,14000) # 14 km boundary
#bound2.sc <- inla.nonconvex.hull(loc.sc, 0.07, 0.07)# Bit too tight I think...
bound.sc.sp <- SpatialPolygons(list(Polygons(
  list(Polygon(bound.sc$loc, FALSE)), '0')), proj4=utm.prj4s)
#bound2.gf.sp <- SpatialPolygons(list(Polygons(

plot(bound.sc.sp, border=3, col=gray(.7), asp=1)
points(loc.sc, col=rainbow(1,alpha=0.1), cex=0.1, pch=19)

### build the mesh, a general reasonable statement I found

# Now we take a guess at an appropriate mesh range, this won't be the same for scallop, cod, and yellowtail.
# The RV survey spacing is also a bit different from the NMFS surveys, based on the distance between sampling
# locations we see that within a year there aren't a lot of stations within 0.5 minutes for NMFS (more like 0.25 for RV survey), but
# when we look at the aggregated data across all years there is a fair bit of data at small scales.  That said
# probably the correlation distance for cod and yellowtail will be in the 10's of kms, 
#has to do with how they move and whether there is preferred "habitat" I think...
#whereas for scallop it could be much smaller; I suspect this would be around the size of a bed, 
#
range.gf <- 50*1000 # really a guess. data are in UTM so we start with 50 km so let's start with 50 (50/110 km), should 
# be reasonable for the flat fish, I think we'll need something tighter for the scallop though
# The max edge should be 1/5 of the range, this defines the size of the boudary layer, this is courtesy Zuur chapter 19 and originates with 
# INLA guru Haakon Bakka.
range.sc <- 10*1000 # For the scallop I think the range will be far smaller, here we are really identifying the scale of the beds, I tried 1 km
# but that made far too fine of a mesh, 5 km with some tweaks to the mesh parameters works and gives me a mesh I can live with I think.
# But what I went for after looking at our strata was 10 km, most features the strata identify are at least 10 km so we try 10
# with Zuur recommendations
# The true marginal variance should be equal to 1 and the SPDE approximation gets more accurate 
# when the maximum edge lenghth is close to and smaller than the correlation length of the process.
#We may conclude that the best resolution of the mesh would be the one with triangle length approximately equal to 
# or no larger than the process correlation length.
# We go with 1/5 of the range for the max.edge based on Zuur and 
max.edge.gf <- range.gf/5 
# But that doesn't work for the Scallop data, far to fine scale and I'm going for far too small a correlation distance
max.edge.sc <- range.sc/5

mesh.gf <- inla.mesh.2d(loc.gf, 
                     max.edge=c(1,5)*max.edge.gf, cutoff=max.edge.gf/5,
                     boundary=inla.sp2segment(bound.gf.sp))
#plot(mesh.gf)
mesh.gf$n # This is a pretty big mesh and has will certainly not have any problematic edge effects!

# Because I expect the correlation range to be pretty low the Zuur defaults are leading to unreasonably small triangles here
# I think the cutoff I'm using is the big issue here, the cutoff is the minimum allowed distance between points.  We end up making
# very fine mesh pieces because we have data very close together and are allowing for very small triangles.  
# So what I've done is to use the maximum edge as suggested by Zuur, but for the cutoff (the smallest size for a triangle essentially)
# I had to make it larger than what Zuur suggests (which is 1/25 of the range), I could only get it down to about 13% of the range, was
#hoping I could get down to 10% but it just blows up.  Still think that is reasonable (basically data seperated by < 1.3/110 km are aggregated within
# a vertex.
mesh.sc <- inla.mesh.2d(loc.sc, 
                        max.edge=c(1,5)*max.edge.sc, cutoff=max.edge.sc/1.5,
                        boundary=inla.sp2segment(bound.sc.sp))
#plot(mesh.sc)
mesh.sc$n # It's a big mesh, but if I'm looking for correlation this fine it is what it is!

# mesh2 <- inla.mesh.2d(loc, 
#                       max.edge=0.25, cutoff=0.05,
#                       boundary=inla.sp2segment(bound2.gf.sp))
# plot(mesh2)

# (w <- rbind(x=range(mesh$loc[,1]),
#             y=range(mesh$loc[,2])))
# (r <- c(diff(w[1,]), diff(w[2,])))

### visualize the mesh and sample locations for the Groundfish surveys.
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=2:0)
plot(bound.gf.sp, lwd=3, border=0, asp=1)
plot(ca.sp, lwd=2, border=5, add=TRUE, col=gray(0.9))
plot(us.sp, lwd=2, border=5, add=TRUE, col=gray(0.9))
plot(bound.gf.sp, add=TRUE, border=4, lwd=3)
plot(mesh.gf, add=TRUE)
points(loc.gf, col=rainbow(1,alpha=0.2), cex=0.2, pch=19)

# Now the same for the scallop survey
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=2:0)
plot(bound.sc.sp, lwd=3, border=0, asp=1)
plot(bound.sc.sp, add=TRUE, border=4, lwd=3)
plot(mesh.sc, add=TRUE)
points(loc.sc, col=rainbow(1,alpha=0.5), cex=0.2, pch=19)


### save as R object
save(mesh.sc,mesh.gf,range.gf,range.sc,bound.gf.sp,bound.sc.sp,max.edge.gf,max.edge.sc,loc.sc,loc.gf,file = paste0(direct.proj,"Data/INLA_meshes.RData"))


