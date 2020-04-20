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
local.plot.field = function(field, mesh, xlim=c(-65.88, -60.13), ylim=c(45.8, 49.1), zlim = NULL, dims = c(300, 300)) 
{ # todo later: add convex hull, remove land from plot
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  windows(11,11)
  par(mar=c(4,4,1,1))
  image.plot(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, axes=T,las=1)
}


direct.fun <- "d:/r/"
direct.proj <- "d:/Projects/GB_time_area_closure_SPERA/"


library(INLA)
library(boot)
library(fields)
library(PBSmapping)
inla.upgrade(testing = F)
source(paste(direct.fun,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep=""))

## 1. Load data, rename, rescale
dat.surv <- read.csv(paste(direct.proj,"Data/RV_survey_CA&US/DFO_GB_spring+summer_tows_cod_ytf.csv",sep=""))
#dat.surv <- read.csv(paste(direct.proj,"Data/STRANAL_CodYTF_RVsurvey/NMFS_Spring_cod_yellowtail.csv",sep=""))
# If the data is from NMFS I need to do this....
#dat.surv$SLO <- -convert.dd.dddd(dat.surv$SLONG)
#dat.surv$SLA <- convert.dd.dddd(dat.surv$SLAT)
#if rescaling - to centre - do so here
#dat.surv$y = dat.surv$r

# Load the mesh
load(paste(direct.proj,'Results/INLA_mesh_RVsurvey_Georges.RData',sep=""))
# The mesh for NFMS
#load(paste(direct.proj,'Results/INLA_mesh_NMFS_Georges.RData',sep=""))


## 2. Explore data
summary(dat.surv)

hist(dat.surv$Cod_TOTNO,breaks=1000,xlim=c(0,10))
length(dat.surv$Cod_TOTNO[dat.surv$Cod_TOTNO>3.5])
length(dat.surv$Cod_TOTNO[dat.surv$Cod_TOTNO==0])
dat.surv$bino_cod <- dat.surv$Cod_TOTNO
#dat.surv$bino_cod <- dat.surv$COD
dat.surv$bino_cod[dat.surv$bino_cod > 0] <- 1
dat.surv$bino_yt <- dat.surv$Ytf_TOTNO
#dat.surv$bino_yt <- dat.surv$YELLOWTAIL
dat.surv$bino_yt[dat.surv$bino_yt > 0] <- 1
# Now make the A matrix, combination of your mesh and locations of our RV survey data
loc <- cbind(dat.surv$SLO,dat.surv$SLA)
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


# 4 Now let's make our formula, intercept with each observation set as a random variable, this makes 
# our model an ove-dispersed poisson
formula1 <- Cod_TOTNO ~ 1 + f(i, model='iid')
formula2 <- bino_cod ~ 1 + f(i, model='iid')

#formula3 <- bino_cod ~ 1 #
formula3 <- bino_yt ~ 1 

# Now the call to inla
res1 <- inla(formula1, family=family1, control.family=control.family1,data=dat.surv)
summary(res1)
# Mean count of cod
cod.mean.count <- exp(res1$summary.fixed[1])

# Now a binomial model...
res2 <- inla(formula2, family=family2, control.family=control.family2,data=dat.surv)
summary(res2)
# Probability of encountering cod.
prob.cod <- inv.logit(as.numeric(res2$summary.fixed[1]))


#######################################SPATIAL MODEL###############################################################
## Now let's try a spatial model.  This model is for yellowtail founder
### SPATIAL model
spde <- inla.spde2.pcmatern(mesh,    
                            prior.sigma=c(1,0.5),
                            prior.range=c(1,0.5))

a0 <- 1 # intercept
s <- 1:spde$n.spde # Size of our SPDE model (I think this is our sparse matrix but I might be dumb)


pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))

# I need a stack I guess
stk.cod = inla.stack(tag="est",
                 data=list(y = dat.surv$bino_cod, link=1L),
                 effects=list(a0 = rep(1, nrow(dat.surv)), s = 1:spde$n.spde),
                 A = list(1, A))

stk.yt = inla.stack(tag="est",
                 data=list(y = dat.surv$bino_yt, link=1L),
                 effects=list(a0 = rep(1, nrow(dat.surv)), s = 1:spde$n.spde),
                 A = list(1, A))
# As soon as you make a spatial model make your own intercept.  Here is
formula3 <- y ~ 0 + a0 + f(s, model=spde)

# Let's giver, make the spatial model.
res.yt <- inla(formula3, family=family2, data = inla.stack.data(stk.yt),
             control.predictor=list(A=inla.stack.A(stk.yt), link=link, compute=TRUE))

res.cod <- inla(formula3, family=family2, data = inla.stack.data(stk.cod),
             control.predictor=list(A=inla.stack.A(stk.cod),link=link, compute=TRUE))


## Now make a nice spatial plot of everything...
# Get the unique set of closed cells for the bank 
load(paste(direct.proj,"Results/Closure_biomass_VMS.RData",sep=""))
cells <- centres
names(cells) <- c("PID","X","Y","EID")
cells <- as.EventData(cells)
yt.unique.closures <- findPolys(cells,subset(closure, Species == "Flounder"))
cod.unique.closures <- findPolys(cells,subset(closure, Species == "Cod"))
yt.closed.cells <- subset(GBa.grid,PID %in% unique(yt.unique.closures$EID))
cod.closed.cells <- subset(GBa.grid,PID %in% unique(cod.unique.closures$EID))

# This gets the cell ID from the GBa grid in which closures occured
unique.closures <- findPolys(cells,closure)
# This gets the actual closed cells polygon for each unique closed cell.
closed.cells <- subset(GBa.grid,PID %in% unique(unique.closures$EID))

rands.cod <- inv.logit(res.cod$summary.random$s$mean)
rands.yt <- inv.logit(res.yt$summary.random$s$mean)

local.plot.field(rands.cod, mesh,c(0,1),xlim = range(dat.surv$SLO),ylim = range(dat.surv$SLA))
local.plot.field(rands.yt, mesh,c(0,1),xlim = range(dat.surv$SLO),ylim = range(dat.surv$SLA))

local.plot.field(rands.yt, mesh,zlim = range(rands.yt),xlim =c(-67,-65.5),ylim = c(41,42.3))
lines(x = c(-67.26,-66.18), y = c(42.3,41),lwd=2)
# Add the survey locations
points(loc,pch=19,cex=0.01)
# Add the cod closed cells
addPolys(yt.closed.cells,angle = 45,density=8,col="black",lwd=.5)
#addPolys(GB.poly,lwd=2,col="darkgrey")

local.plot.field(rands.cod, mesh,zlim =range(rands.yt),xlim =c(-67,-65.5),ylim = c(41,42.3))
#ScallopMap("GB",direct="d:/r/")
addPolys(cod.closed.cells,angle = 45,density=8,col="black",lwd=.5)
lines(x = c(-67.26,-66.18), y = c(42.3,41),lwd=2)
# Add the survey locations
points(loc,pch=19,cex=0.01)
# Add the cod closed cells