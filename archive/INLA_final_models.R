#USE INLA TO DESCRIBE THE SPATIAL, AND SPATIAL-TEMPORAL PATTERNS OF COD AND YELLOWTAIL ON GEORGES BANK

#Using the RVsurvey data, from DFO and NMFS, across all months and years, on George's Bank
#Using the 'mesh' created using the above data
#Summarize, query, clean, understand the data
#Run INLA to
# - describe spatial pattern across all years/months - where are the hotspots of each species? 
# - break down by month - is there a change in hotspots by month? - careful that there is more data in some months than others
# - break down by year - is there a change in hotspots by year? 

rm(list=ls())

# A way to plot our data spatially
# If you don't have access to ScallopMap use this function, plots are the same, just slightly more limited on what you can do with the image plot...
Generic.plot.field = function(field, mesh, xlim=c(-65.88, -60.13), ylim=c(45.8, 49.1), zlim = c(0,1), dims = c(50, 50), trans= "none",
                              clip= NULL,lvls = seq(0,1,by=0.01),add_boxes = NULL,colors = c("blue","white","yellow","darkred"),alpha = 0.8) 
{ 
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  #field.proj[is.na(field.proj)] <- 0
  if(!is.null(clip)) 
  {
    pred.in <- inout(proj$lattice$loc,clip) 
    field.proj[!pred.in] <- NA
  } # end if(!is.null(clip)) 
  #windows(11,11)
  if(trans== "exp") arg.list <- list(at=lvls,labels=round(exp(lvls)))
  if(trans == "none") arg.list <- list(at=lvls,labels=lvls)
  par(mar=c(4,4,1,1))
  image.plot(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, 
             axes=F,las=1,add=F, breaks=lvls, axis.args= arg.list,
             col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  if(!is.null(add_boxes)) plot(add_boxes,angle = 45,density=8,col="black",lwd=.5,add=T)
  lines(x=c(-67.4675,-65.6992),y=c(42.51889,40.45139),lwd=3,col=magma(1,alpha=0.3,begin=0)) # EEZ b/t Can and US
  # addPolys()
} # end function

# A way to add some transparancy to the colours if so desired.
addalpha <- function(colors, alpha=1.0) 
{
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


direct.fun <- "Y:/Offshore scallop/Assessment/"
direct.proj <- "Y:/Projects/GB_time_area_closure_SPERA/"


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
#inla.upgrade(testing = F)
source(paste(direct.fun,"Assessment_fns/Maps/ScallopMap.r",sep=""))

# The data to bring in , first we bring in some nicely formatted objects full of the closure locations
load(paste(direct.proj,"Results/closure.locations.RData",sep=""))
# Get the unique set of closed cells for the bank 
yt.unique.closures <- findPolys(cells,subset(closure, Species == "Flounder"))
cod.unique.closures <- findPolys(cells,subset(closure, Species == "Cod"))
yt.closed.cells <- as.PolySet(subset(GBa.grid,PID %in% unique(yt.unique.closures$EID)),projection="LL")
cod.closed.cells <- as.PolySet(subset(GBa.grid,PID %in% unique(cod.unique.closures$EID)),projection="LL")

# This gets the cell ID from the GBa grid in which closures occured
unique.closures <- findPolys(cells,closure)
# This gets the actual closed cells polygon for each unique closed cell.
closed.cells <- as.PolySet(subset(GBa.grid,PID %in% unique(unique.closures$EID)),projection="LL")
closed.cells <- PolySet2SpatialPolygons(closed.cells)
yt.closed.cells <- PolySet2SpatialPolygons(yt.closed.cells)
cod.closed.cells <- PolySet2SpatialPolygons(cod.closed.cells)

# Next I want to bring in the results of the PCA, this has all the spatial information we'd ever want and has the data in nice shape.
load(paste0(direct.proj,"Results/PCA_models.RData"))
nmfs.spr$survey <- "nmfs-spring"
nmfs.fall$survey <- "nmfs-fall"
RV.surv$survey <- "RV"
# So there are two pieces of data of interest, the data which is intersected with the PCA analysis and the data that isn't
# There was a small amount of data lost in the PCA analysis due to missing enivironmental data (somewhere are 5%)
# This is a list with the complete PCA analysis data in it.  The data retained are for the adults in the population only, we 
# could get the whole population or just the juvenilles as well.  Given we are looking generally at spawning aggregations 
# we stick with the adults for now, not a problem to include other ages tho.
dat.pca <- final.dat
dat.survey.pca <- rbind(nmfs.spr.final,nmfs.fall.final,RV.surv.final)
# This includes every tow, but doesn't have the PCA tied to it, it does have depth so I would like to try a smiple model with depth included...
cols <- c("unique_set_ID","lat_dd","lon_dd","year","strata","COD_number_Adu", "COD_PA_Adu" ,"YT_number_Adu" ,"YT_PA_Adu" , "comldepth","survey")

# So what I want to do is look at the spatial picture by survey, the DFO survey gives us a snapshot in Feb-March
# The NMFS-spring gives us the picture in April-May
# The NMFS-fall gives us the picture in the Fall, with these 3 surveys we really can get a picture of the seasonal movement of these
# species on the bank.  We probably could put this all into one wacky model, but for now let's just compare the survey's in individual models
# what I want to look at first is the overall patterns by survey, and then look at the patterns by year, 5 years, and decade
# After I look through that I think I toss in some of the PCA covariates and the more obvious covariates (e.g. depth)
# and see how these influence the model...

dat.surv <- rbind(nmfs.spr[,cols],nmfs.fall[,cols],RV.surv[,cols])

dat.surv <- dat.surv[!dat.surv$survey == "nmfs-fall" & dat.surv$year > 1900,]
# If I am going to look at the surveys integtrated I want to make a mesh built on the entire survey data 
load(paste(direct.proj,'Results/INLA_mesh_all_surveys.RData',sep=""))
# Load the mesh if just looking at RV survey
#load(paste(direct.proj,'Results/INLA_mesh_RVsurvey_Georges.RData',sep=""))
# Load the mesh if looking at NMFS Spring and Summer combined.
#load(paste(direct.proj,'Results/INLA_mesh_NMFS_Georges.RData',sep=""))

# If we want to compare the patterns across decades we'll need a grouping vector on the year.

# How much data do we have by year, notice that the amount of data improves dramaticaly in the mid-1980's when the RV survey starts up on GB
table(dat.surv$year)
dat.surv$year.group <- 1
eras <- unique(floor(dat.surv$year/10)*10)
n.eras <- length(eras)
# Order the year groups, I know this works if numbered, not sure how it works if they were factors, so sticking with what I know works..
dat.surv$year.group <- as.numeric(as.factor(floor(dat.surv$year/10)*10))

# Now make the A matrix, combination of your mesh and locations of our RV survey data
loc <- cbind(dat.surv$lon,dat.surv$lat)
A <- inla.spde.make.A(mesh, loc)
dim(A)

## 3. Observation Likelihood, 
family1 = "poisson"
control.family1 <- list(control.link = list(model="log"))
dat.surv$i <- 1:nrow(dat.surv)

# How about a biomonial..
family2 = "binomial"
control.family2 = list(control.link=list(model="logit"))
Ntrials = dat.surv$n

# Might like to get a negative binomial set up too...
family3 <- "nbinomial"
control.family3 = list(control.link=list(model="log"))
Ntrials = dat.surv$n



#######################################SPATIAL MODEL###############################################################
## Now let's try a spatial model.  This model is for yellowtail founder
### SPATIAL model
# The priors here can be pretty informative, the SPDE approximation improves if the corrleation length (i.e. roughly the range) of the process
# is similar, but larger, than the maximum edge length ofthe mesh,
spde <- inla.spde2.pcmatern(mesh,    
                            prior.sigma=c(2,0.5), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                            prior.range=c(0.5,0.5)) # The Meidan range and the probability that the range is less than this...
s <- 1:spde$n.spde # Size of our SPDE model (I think this is our sparse matrix but I might be dumb)
# 4 Now let's make our formula, intercept with each observation set as a random variable, this makes 
# As soon as you make a spatial model make your own intercept.  Here is
a0 <- 1 # intercept


formula1 <- y ~ 0 + a0 + f(s, model=spde)
formula2 <- y ~ 0 + a0 + comldepth +  f(s, model=spde)
#formula3 <- y ~ 0 + a0 + 

pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))

# I need a stack, probably should eventually split this into an estimation, validation and prediction stack, but for now
# will stick with the one estimation stack....
# We can mkae the stack overly complex so we don't have to keep making more stacks as we make the models increasingly complex.
stk.cod = inla.stack(tag="est",
                 data=list(y = dat.surv$COD_PA_Adu, link=1L),
                 effects=list(a0 = rep(1, nrow(dat.surv)), s = 1:spde$n.spde),
                 A = list(1, A))

stk.yt = inla.stack(tag="est",
                 data=list(y = dat.surv$YT_PA_Adu, link=1L),
                 effects=list(a0 = rep(1, nrow(dat.surv)), s = 1:spde$n.spde),
                 A = list(1, A))


# Let's giver, make the spatial model.
res.yt <- inla(formula1, family=family2, data = inla.stack.data(stk.yt),
             control.predictor=list(A=inla.stack.A(stk.yt), link=link, compute=F))

res.cod <- inla(formula1, family=family2, data = inla.stack.data(stk.cod),
             control.predictor=list(A=inla.stack.A(stk.cod),link=link, compute=F))

res.yt.nb <- inla(formula1, family=family3, data = inla.stack.data(stk.yt),
               control.predictor=list(A=inla.stack.A(stk.yt), link=link, compute=F))

res.cod.nb <- inla(formula1, family=family3, data = inla.stack.data(stk.cod),
                control.predictor=list(A=inla.stack.A(stk.cod),link=link, compute=F))

# THe model prediction fields
# Simple Binomial models
rands.cod <- inv.logit(res.cod$summary.random$s$mean + res.cod$summary.fixed$mean)
rands.yt <- inv.logit(res.yt$summary.random$s$mean+ res.cod$summary.fixed$mean)
# Negative Binomial models
rands.cod.nb <- exp(res.cod$summary.random$s$mean + res.cod$summary.fixed$mean)
rands.yt.nb <- exp(res.yt$summary.random$s$mean+ res.cod$summary.fixed$mean)






####################### Pretty plots!!!!!!!!
# The binomial model plots...
windows(12,8)
Generic.plot.field(rands.cod, mesh,xlim = range(dat.surv$lon_dd),ylim = range(dat.surv$lat_dd),dims=c(500,500),
                   lvls=c(0,0.1,0.2,0.5,0.7,0.8,0.9,0.95,1),colors = rev(magma(6)),add_boxes = cod.closed.cells)
# Add the survey locations
points(dat.surv$lon,dat.surv$lat,pch=19,cex=0.1,col=magma(1,alpha=0.3,begin=0))

# The binomial model plots...
windows(12,8)
Generic.plot.field(rands.yt, mesh,xlim = range(dat.surv$lon_dd),ylim = range(dat.surv$lat_dd),dims=c(500,500),
                   lvls=c(0,0.1,0.2,0.5,0.7,0.8,0.9,0.95,1),colors = rev(magma(6)),add_boxes = yt.closed.cells)
# Add the survey locations
points(dat.surv$lon,dat.surv$lat,pch=19,cex=0.1,col=magma(1,alpha=0.3,begin=0))



#############################################
# The negative binomials models...
windows(12,8)
Generic.plot.field(rands.cod.nb, mesh,xlim = range(dat.surv$lon_dd),ylim = range(dat.surv$lat_dd),dims=c(500,500),
                   lvls=c(0,1,2,3,4,5,8,10,20,25),colors = rev(magma(6)),add_boxes = cod.closed.cells)
# Add the survey locations
points(dat.surv$lon,dat.surv$lat,pch=19,cex=0.1,col=magma(1,alpha=0.3,begin=0))

# The binomial model plots...
windows(12,8)
Generic.plot.field(rands.yt.nb, mesh,xlim = range(dat.surv$lon_dd),ylim = range(dat.surv$lat_dd),dims=c(500,500),
                   lvls=c(0,10,20,50,100,200,500),colors = rev(magma(6)),add_boxes = yt.closed.cells)
# Add the survey locations
points(dat.surv$lon,dat.surv$lat,pch=19,cex=0.1,col=magma(1,alpha=0.3,begin=0))

# The negative binomials models...


