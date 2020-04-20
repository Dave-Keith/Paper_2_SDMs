# Here's what I hope is the script that contains the final INLA models used for the analysis reside, here's the idea...

# 1:  Each survey is it's own beast, from each of these surveys we create presence absence maps for each species, these are
#     considered the presence absence maps by species for each "season".  RV survey season is Feb-March (Winter, best data for Cod)
#     NFMS spring is April-May (Spring and best data for YT), and NMFS Fall is looking to see how different they distributions are in the fall
# 2:  We include the top 4 PCA scores as covariates, due to sanity limitations we only include 1 PCA score and see if any of these 
#     improve the model fit over the raw presence absence data alone.
# 3:  Next we develop INLA maps of scallop biomass from the survey (scaled to 0 - 1? and I think scaled by year, but that is up for debate) and show
#     where the scallop are.  Areas of high scallop biomass that overlap with an area of likely encouters with industry.  We also could
#     include predictive maps of scallop biomass based scallop size classes observed in the survey to give an idea of future hotspots, 
#     that's a very interesting idea that I'm digging a lot as I type this...
# 4:  We could also include a map of effort, given the first paper we see this would be very similar to the biomass one, and I'm not quite
#     as sure how we map the effort into a 0-1 risk framework like we do with biomass?
#############################################


################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
#----
rm(list=ls())
direct.fun <- "Y:/Offshore scallop/Assessment/"
direct.fun <- "D:/R/"
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
library(maps)
require(gridExtra)
library(sp)
library(rgeos)
library(splancs)
library(marmap)
library(dplyr)
library(spatstat)
library(gridBase)
#inla.upgrade(testing = F)

# Bring in the functions we'll need
source(paste(direct.fun,"Assessment_fns/Maps/pectinid_projector.R",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/convert_coords.R",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/add_alpha_function.r",sep=""))
source(paste(direct.fun,"Assessment_fns/Maps/combine_shapefile_layers.R",sep=""))


# Here is the data we need, this comes from Step 3 INLA_mesh_for_gb_surveys_and_scallop_survey.R
load(paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))
load(paste0(direct.proj,"Data/INLA_meshes.RData"))

# I tried to make a little function (o.k. stole...) to add the minimum + X to the diagnostic plots, intent of this is for comparison of wAIC and DIC for models
# You give your stat a name, the second is the "type" of geom, here we are making a stat (could be geom, position, scale)
# the compute_group is returned once per group (we have a faceted plot so have several groups, I think that's what that does)
# stat.min.plus <- ggproto("stat.min.plus", Stat,
#                         compute_group = function(data, scales) 
#                         {
#                           transform(data, yintercept=min(y))
#                         },
#                         required_aes = c("x", "y"))
# 
# # And this function actually draws the line there for us
# stat_min_line <- function(mapping = NULL, data = NULL, geom = "hline",position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) 
# {
#   layer(stat = stat.min.plus, data = data, mapping = mapping, geom = geom, 
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#         params = list(na.rm = na.rm, ...))
#}

##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########
##########################  END Section 1    Load data and functions ##########################  END Section 1    Load data and functions ###########


##########################  Section 2     Run the analyses   ##########################  Section 2     Run the analyses ###########################
############  Here's where we do the INLA'ing and get our model results #####################
##########################  Section 2     Run the analyses   ##########################  Section 2     Run the analyses ###########################
############ Start here for analysis, load in the data we need and go nuts...
#----
# Now I need to stick my UTM coordinates into the two data files...
scal.surv.dat$X <- loc.sc@coords[,1]
scal.surv.dat$Y <- loc.sc@coords[,2]
dat.final$X <- loc.gf@coords[,1]
dat.final$Y <- loc.gf@coords[,2]
# At this point I really start the analysis, and what I want to do is split up each survey, I might as well loop this, or at least allow for a loop
# so we can move through each survey easily, this will get complicated with a number of models for each survey, especially when I split out the time...
surveys <- c(unique(dat.final$survey),unique(scal.surv.dat$survey))
num.surveys <- length(surveys)

# Lets center our depth so 0 represents the mean depth of the region of study
dat.final$depth_cen <- dat.final$comldepth - mean(dat.final$comldepth)

# We are going to need to specify our sigma for each model.  From Zuur chapter 19 pages 414-416 he does give some guidance of the highest this should 
# ever be, think about your random spatial field, if we were just mapping the spatial field that field could range between the maximum and
# minimum value in the data, for our data we are following the logit distribution, so we can play around a bit with this, think about how variable
# you expect the surface to be on it's own, for example if expecting values to be between 0.2 and 0.8 what would sigma be (around 0.75)
#sigma <- seq(0,4,by=0.25)
# Assuming the field is 0 mean a 0.5 would let the field itself vary between 0.27 and 0.72
#CI <- cbind(exp(-1.96*sigma)/ (1+exp(-1.96*sigma)),exp(1.96*sigma)/ (1+exp(1.96*sigma)),sigma)
#CI
# Zuur tends to us sigma = 0.5 for these in the logit cases with a low probability that the sigma is above this value
# So this will tend to make the surface relatively smooth, but we will want to compare this with that the model estimates
sigma <- 0.5
s.alpha <- 0.05
# Similarly for the range, we've already settled on a value when making the mesh previously following these same sections of zuur
# But here for the range our alpha is the likelihood that the range is smaller than our set value, again Zuur tends
# to make this small to avoid overfitting
r.alpha <- 0.05

# Now we can run the analyses through each survey, the next question to sort out is how to build this to account for the different types of analyses
# Currently I want 1 for each PCA, I need one for the scallop biomass, I want something acounting for year (maybe the era analysis), 
# also I want an analysis without the PCA's in it, I need one for cod and one for yt don't forget, 
# do I want to set these up explicitly or toss into a loop... hmm 
# Might also be cool to do a "last 10 year" average scallop biomass as something that could be use as a longer term predicitve surface and
# One with all the scallop biomasses as well.

# I need a loop for the number of "species" i'm looking at, YT, cod, and up to 4 scallop size classes will all be informative
# Here I take "species" as the head in the file so it's easy to select which is being done
# To minimize code tweaking I also add in a model for the scallop that only looks at the field using the most recent year of data
species <- c("COD_PA_Adu","YT_PA_Adu","com_stan","rec_stan","bin_lt_50_stan","bin_50_70_stan", # These are the full suite of models
             "COD_PA_Adu_ly","YT_PA_Adu_ly","com_stan_ly","rec_stan_ly","bin_lt_50_stan_ly","bin_50_70_stan_ly", # These are the distributions for most recent year
             "COD_PA_Adu_can","YT_PA_Adu_can") # These take the cod and YT data that are on the Canadian side of GB.  These have same mesh as the scallop data so can be overlain easily!
num.species <- length(species)
# Need to initialize some objects for later
res <- NULL
#mod.diag <- NULL
mod.output <- NULL
rand.field <- NULL
mod.diagnostics <- NULL
dat.list <- NULL
w.index.list <- NULL
for(s in 1:num.species) 
{
  # Loop through each of the surveys
  for(i in 1:num.surveys) 
  {
    # and here we go into the scallop dat and extract the data we want...
    if(surveys[i] == 'scallop' && !grepl("PA_Adu",species[s])) 
    {
      # Let's select the data for the particular survey of interest
      dat <- scal.surv.dat
      # If we are just looking at the last year of data we subset it as so...
      if(grepl("_ly",species[s])) dat <- dat[dat$year == max(dat$year),]
      # Rename the varialbe of interest to "response", this doesn't quite work so nicely since I hacked the species to be everything...
      if(grepl("com", species[s])) resp <- "com_stan"
      if(grepl("rec", species[s])) resp <- "rec_stan"
      if(grepl("bin_lt_50", species[s])) resp <- "bin_lt_50_stan"      
      if(grepl("bin_50_70", species[s])) resp <- "bin_50_70_stan"
      response <- which(names(dat) == resp)
      names(dat)[response] <- "response"
      # Lets center our depth so 0 represents the median depth of the region of study, using median b/c of skew in data (deep tows)
      med.surv <- median(dat$depth,na.rm=T)
      dat$depth_cen <- dat$depth - med.surv # We should note these medians by area
      # I wan the year group to be a categorical variable.
      dat$year.group <- as.factor(dat$year.group)
      loc <- cbind(dat$X,dat$Y)
      # now the mesh
      mesh <-mesh.sc 
      range <- range.sc
      # We also need to decide Observation Likelihood, given these will be specific to the survey we can start that here...
      fam <- "beta"
      control.fam <- list(control.link = list(model="log"))
    } # end if(surveys[i] == 'scallop' && !species[s] %in%  c("COD_PA_Adu","YT_PA_Adu")) 
    
    # If we have the other surveys and aren't into the scallop
    if(surveys[i] != 'scallop' && grepl("PA_Adu",species[s])) 
    {
      # Now lets get our input data sorted
      # Let's select the data for the particular survey of interest
      dat <- dat.final[dat.final$survey == surveys[i],]
      # If we are just looking at the last year of data we subset it as so...
      if(grepl("_ly",species[s])) dat <- dat[dat$year == max(dat$year),]
      # For the NMFS-spring survey last year we need to drop a few tows with PC3 scores and depths that are problematic for the INLA models
      # These are basically outliers that really mess about with the smoothers, this gets rid of the 4 problematic observations
      if(grepl("_ly",species[s]) && surveys[i] =="nmfs-spring") 
      {
        # The PC3's are a bit funny due to NA's so I need to be careful I don't turf them all
        dat <- dat[dat$comldepth > -250,]
        if(species[s] == "YT_PA_Adu_ly") dat <- dat[dat$comldepth > -125,]
        dat <- dat[unique(c(which(dat$PC3 > -6), which(is.na(dat$PC3)))),]
      } # end if(grepl("_ly",species[s]) && surveys[i] =="nmfs-spring") 
      # Here this slices out 3 problematic observations
      if(grepl("_ly",species[s]) && surveys[i] =="nmfs-fall") dat <- dat[dat$comldepth > -245 ,]
      # For the Yellowtail RV survey we run into problems with the depth, so remove these from the YT model removing 16 points
      # YT likely bad because there are only 0 YT above and below these depths
      if(grepl("YT_PA_Adu_ly",species[s]) && surveys[i] =="RV") dat <- dat[dat$comldepth > -116.43 & dat$comldepth < -40 ,]
      if(grepl("YT_PA_Adu_ly",species[s]) && surveys[i] =="nmfs-fall") dat <- dat[dat$comldepth > -100  & dat$comldepth < -33,]
      # For yellowtail on the nmfs fall sruvey there are an additional 10 or so points we need to chuck out...
      if(species[s] == "YT_PA_Adu" && surveys[i] =="nmfs-fall") dat <- dat[dat$comldepth > -235 ,]

      # Now if I just want to use the Canadian data...
      if(grepl("_can",species[s])) 
      {
        dat.tmp <- dat
        coordinates(dat.tmp) <- ~ X + Y
        # Now make this a proper spatial coordinate system..
        proj4string(dat.tmp) <-  proj4string(loc.sc)
        # Subset the dat.tmp to be the data found within the Canadian mesh, this ain't half bad as US data will still be included in fringes...
        bnd <- chull(mesh.sc$loc)# this gets the location in mesh.sc$loc of the edges of the convex polygon 
        boundry <- mesh.sc$loc[c(bnd,bnd[1]),] # ANd this makes a spatial points object
        boundry <- as.data.frame(boundry[,1:2])
        names(boundry) <- c("X","Y")
        # Now make this a proper spatial coordinate system..
        coordinates(boundry) <- ~X + Y
        # and now we get a spatial polygon...
        bnd.poly <- SpatialPolygons(list(Polygons(list(Polygon(boundry)), ID=1)))
        # and project it...
        proj4string(bnd.poly) <- proj4string(loc.sc)
        # Now subset the Dat object to be the points found within this boundry
        keep <- gIntersects(bnd.poly,dat.tmp,byid=T)
        dat <- dat[keep,]
      } # end if(grepl("_can",species[s])) 

      # for YT nmfs-survey we need to toss everything deeper than 300 meters
      if(grepl("YT_PA_Adu_can",species[s]) && surveys[i] =="nmfs-fall") dat <- dat[dat$comldepth > -300 ,]  
      # And the RV survey version needs to remove everything below 240
      if(grepl("YT_PA_Adu_can",species[s]) && surveys[i] =="RV") dat <- dat[dat$comldepth > -240 ,]  
      # We need to get rid of the deep tows as they are too infrequent and can cause issues with the INLA convergence, < 1% of the data
      if(surveys[i] != "RV") dat <- dat[dat$comldepth > -330,]
      # For the RV survey the tows > 250 meters in depth can cause some issues so we need to remove those, only 2 stations...
      if(surveys[i] == "RV") dat <- dat[dat$comldepth > -250,]
        
      # Rename the varialbe of interest to "response"
      if(grepl("COD", species[s])) resp <- "COD_PA_Adu"
      if(grepl("YT", species[s])) resp <- "YT_PA_Adu"
      response <- which(names(dat) == resp)
      names(dat)[response] <- "response"
      # Lets center our depth so 0 represents the median depth of the region of study, using median b/c of skew in data (deep tows)
      med.surv <- median(dat$comldepth,na.rm=T)
      dat$depth_cen <- dat$comldepth - med.surv # We should note these medians by area
      # I wan the year group to be a categorical variable.
      dat$year.group <- as.factor(dat$year.group)
      # Get the location of our data...
      loc <- cbind(dat$X,dat$Y)
      #loc <- loc.gf
      if(!grepl("_can",species[s])) 
      {
        mesh <- mesh.gf
        range <- range.gf
      } # end if(!grepl("_can",species[s])) 
        
      # If just looking at Canada use the scallop mesh...
      if(grepl("_can",species[s])) 
      {
        mesh <- mesh.sc
        range <- range.sc
      } # end if(!grepl("_can",species[s])) 
        
        #We also need to decide Observation Likelihood
        fam <- "binomial"
    } # end if(surveys[i] != 'scallop' && species[s] %in%  c("COD_PA_Adu","YT_PA_Adu")) 

    # now only run the rest of this script if we have data
    if((surveys[i] == 'scallop' && !grepl("PA_Adu",species[s])) ||
       (surveys[i] != 'scallop' && grepl("PA_Adu",species[s])))
    {
      # The amount of data we have
      N = nrow(dat)
      # For both the beta and binomial families we'll need to determine the number of trials.
      Ntrials <- 1 # For each record there is only 1 trial.
      
      # For both our scenarios we are going to be using the logit model (note that this isn't scrictly necessary to write as the logit is the
      # 'canonical' link (to likely mis-use stats terminology) for the beta and binomial distributions.
      control.fam = list(control.link=list(model="logit"))
      
      
      # Now make the A matrix, combination of your mesh and locations of our survey
      A <- inla.spde.make.A(mesh, loc)
      dim(A)
      # Now for the model with the spatio-temporal random field we need a different A matrix.
      eras <- as.numeric(dat$year.group)
      era.names <- unique(eras)
      n.eras <- length(unique(eras))
      A.era <- inla.spde.make.A(mesh, loc,repl = eras)
      #table(eras) we should have a decent number of samples for each era.
      # While I have my range for my spatial priors I don't have my sigma or the probabilites for the prirors
      # The priors here can be pretty informative, the SPDE approximation improves if the corrleation length (i.e. roughly the range) of the process
      # is similar, but larger, than the maximum edge length ofthe mesh
      # so here we define our spde

      spde <- inla.spde2.pcmatern(mesh,    
                                  prior.sigma=c(sigma,s.alpha), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                  prior.range=c(range,r.alpha)) # The Meidan range and the probability that the range is less than this...
      
      
      # and now we define the spatial random field.
      w.index <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde)
      w.index.rep <- inla.spde.make.index(name = 'w',n.spde = spde$n.spde,n.rep = n.eras)
      
      # Zuur never talks about this puppy I don't think, it is a penalised complexity prior but I'm not sure what for, Zuur only
      # discusses these in terms of the PCP's of the spatial field, this is a prior for precision, see inla.doc("pc.prec")
      # certainly isn't entirely clear to me!
      #pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
      
      # Next up we make the stack...
      # I need a stack, probably should eventually split this into an estimation, validation and prediction stack, but for now
      # will stick with the one estimation stack....
      
      
      # For all random walk models I use inla.group to group the data so that the minimum difference
      # between values is greater than the threshold for the random walk models, these groups are very
      # fine and really just bin things that are essentially the same, tested with 75 groups for depth and 
      # this doesn't really do anything to the data as the data are vitually identical but allows for 
      # the random walk smoother to do its thing
      dat$depth_cen_g <- inla.group(dat$depth_cen,n=75)
      # Only do up the PC's for the Groundfish models, use 150 for these data.
      if(surveys[i] != 'scallop' && grepl("PA_Adu",species[s]))
      {
        if(!grepl('ly',species[s]))
        {
          dat$PC1_g <- inla.group(dat$PC1,n=150)
          dat$PC2_g <- inla.group(dat$PC2,n=150)
          dat$PC3_g <- inla.group(dat$PC3,n=150)
          dat$PC4_g <- inla.group(dat$PC4,n=150)
        }
        # If looking at just one year of data the grouping needs to be much tighter since we have so much less data
        if(grepl('ly',species[s]))
        {
          dat$PC1_g <- inla.group(dat$PC1,n=40)
          dat$PC2_g <- inla.group(dat$PC2,n=40)
          dat$PC3_g <- inla.group(dat$PC3,n=40)
          dat$PC4_g <- inla.group(dat$PC4,n=40)
        }
        # For the canadian subset we need something inbetween
        if(grepl('_can',species[s]))
        {
          dat$PC1_g <- inla.group(dat$PC1,n=75)
          dat$PC2_g <- inla.group(dat$PC2,n=75)
          dat$PC3_g <- inla.group(dat$PC3,n=75)
          dat$PC4_g <- inla.group(dat$PC4,n=75)
        }
        
      }
      # First we need to make the model matrix, this needs to align with our formula below, this doesn't strictly need to be 
      # done unless we have a categorical covariate
      options(na.action='na.pass')# Need to do this so that the model matrix retains the NA's in it.
      # The nice thing here is that we can make this a complex as we want and just run submodels from within this model
      # structure.
      # For the groundfish tihs is our matrix
      if(surveys[i] != 'scallop' && grepl("PA_Adu",species[s]))
      {
      
      if(grepl("_ly",species[s]))  
      {
        X.matrix <- model.matrix(~ 0 + PC1_g + PC2_g + PC3_g + PC4_g + depth_cen_g , data = dat)
        # And then make a covariate matrix
        X <- data.frame(PC1_g = X.matrix[,1],
                        PC2_g = X.matrix[,2],
                        PC3_g = X.matrix[,3],
                        PC4_g = X.matrix[,4],
                        depth_cen_g = X.matrix[,5])
      }
      
      if(!grepl("_ly",species[s]))    
      {
        X.matrix <- model.matrix(~ 0 + PC1_g + PC2_g + PC3_g + PC4_g + depth_cen_g + year.group , data = dat)
        # And then make a covariate matrix
        X <- data.frame(PC1_g = X.matrix[,1],
                        PC2_g = X.matrix[,2],
                        PC3_g = X.matrix[,3],
                        PC4_g = X.matrix[,4],
                        depth_cen_g = X.matrix[,5],
                        year.group = X.matrix[,6])
      }
      }
      # For the scallop survey do this...
      if(surveys[i] == 'scallop' && !grepl("PA_Adu",species[s]))
      {
        # If we are just using 1 year of data we don't want year.group in the model
        if(grepl("_ly",species[s]))  
        {
          X.matrix <- model.matrix(~ 0 +  depth_cen_g , data = dat)
          # And then make a covariate matrix
          X <- data.frame(depth_cen_g = X.matrix[,1])
        } # end if(grepl("_ly",species[s]))
        
        
        if(!grepl("_ly",species[s]))    
        {
        X.matrix <- model.matrix(~ 0 +  depth_cen_g + year.group , data = dat)
        # And then make a covariate matrix
        X <- data.frame(depth_cen_g = X.matrix[,1],
                        year.group = X.matrix[,2])
        } # end  if(!grepl("_ly",species[s]))  
        
      } # end  if(surveys[i] == 'scallop' && !grepl("PA_Adu",species[s]))
      
      # Make the stack for the spatial models without spatio-temporal correlation
      stk = inla.stack(tag="est",
                           data=list(y = dat$response, link=1L),
                           effects=list(intercept = rep(1, nrow(dat)), 
                                        X = X,
                                        w = w.index),
                           A = list(1,1,A))
      # Now we need the stack for the data with the spatio-temporal correlation, a couple things change here..
      if(!grepl("_ly",species[s])) 
      {
      stk.st.mod = inla.stack(tag="est",
                       data=list(y = dat$response, link=1L),
                       effects=list(intercept = rep(1, nrow(dat)), 
                                    X = X,
                                    w = w.index.rep),
                       A = list(1,1,A.era))
      }
      # Now let's make our formula, intercept with each observation set as a random variable, this makes 
      # As soon as you make a spatial model make your own intercept.  Here is an initial suite of models I like...
      intercept <- 1 # intercept
      # For the random walk models we need to set the priors for the random walk, Zuur recommmends rw2 as it seems to 
      # overfit less, to do this I need to bin the covariates using the inla.group function below, problem is 
      # the rw struggles with covariate values that are very close to each other (rw1 has same issue)
      # and he recommends these priors to make sure it doesn't get too funky
      U <- 0.5
      hyp.rw2 <- list(theta=list(prior = "pc.prec", param = c(U,0.05)))
      
      # every time I want to remove all model names so these are unique for each run
      rm(list = ls()[grep("model.",ls())])
      # for convience I'm going to call these formulas models..
      model.intercept <- y ~ 0 + intercept + f(w,model=spde)
      if(surveys[i] != 'scallop' && grepl("PA_Adu",species[s]))
      {
        # Now the models
        model.pc1 <-       y ~ 0 + intercept + f(PC1_g, model = "rw2", hyper = hyp.rw2) + f(w,model=spde)
        model.pc2 <-       y ~ 0 + intercept + f(PC2_g, model = "rw2", hyper = hyp.rw2) + f(w,model=spde)
        model.pc3 <-       y ~ 0 + intercept + f(PC3_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
        model.pc4 <-       y ~ 0 + intercept + f(PC4_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
      }
     
      model.depth <-     y ~ 0 + intercept + f(depth_cen_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde)
      
      # Now the same suite of models but with a random field that can vary by era or the model with the "era" in it
      if(!grepl('ly',species[s]))
      {
      model.era <-       y ~ 0 + intercept + year.group + f(w,model=spde)
      model.intercept.st <- y ~ 0 + intercept + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.rep
      
      if(surveys[i] != 'scallop' && grepl("PA_Adu",species[s]))
      {
        model.pc1.st <-       y ~ 0 + intercept + f(PC1_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.rep
        model.pc2.st <-       y ~ 0 + intercept + f(PC2_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.rep
        model.pc3.st <-       y ~ 0 + intercept + f(PC3_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.rep
        model.pc4.st <-       y ~ 0 + intercept + f(PC4_g, model = "rw2", hyper = hyp.rw2)  + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.rep
      }
      
      model.depth.st <-     y ~ 0 + intercept + f(depth_cen_g , model = "rw1", hyper = hyp.rw2)  + f(w,model=spde,replicate = w.repl) # The w.repl is found inside w.index.rep
      }
      # How many different models do we have
      n.mods <- length(grep("model.",ls()))
      mod.names <- ls()[grep("model.",ls())]

      # Let's giver, make the spatial model.
      # Now we can loop this to run our different PCA and depth models
      for(m in 1:n.mods)
      {
        run.name <- paste0(species[s]," ", surveys[i]," survey ",mod.names[m])
        #If we aren't dealing with a spatio-temporal model we do this
        if(!grepl("st",mod.names[m]))
        {
          # For almost all the models we run this
          if(run.name !="rec_stan_ly scallop survey model.intercept")
          {
            r.out <- inla(get(mod.names[m]), family=fam, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk)),
                          #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                          #verbose=TRUE,
                          control.compute = list(dic=T,waic = T)) 
          } # end  if(run.name !="rec_stan_ly scallop survey model.intercept")
          
          # This one recruit model is not converging, this fixed it, but will lead to some potential bias in the field.
          if(run.name =="rec_stan_ly scallop survey model.intercept" )
          {
          r.out <- inla(get(mod.names[m]), family=fam, data = inla.stack.data(stk),
                        control.predictor=list(A=inla.stack.A(stk)),
                        # Added the cmin=0 based on Harvard Rue suggestion here... https://groups.google.com/forum/#!topic/r-inla-discussion-group/hDboQsJ1Mls
                        # From the notes...  cmin is the minimum value for the negative Hessian from the likelihood. 
                        #Increasing this value will stabalise the optimisation but can introduce bias in some estimates unless -Inf is used. 
                        control.inla=list(cmin = 0 ), 
                        control.compute = list(dic=T,waic = T)) 
          } # end if(run.name =="rec_stan_ly scallop survey model.intercept" )
          
        } else 
          {
            #If we are dealing with a spatio-temporal model we do this
            r.out <- inla(get(mod.names[m]), family=fam, data = inla.stack.data(stk.st.mod),
                              control.predictor=list(A=inla.stack.A(stk.st.mod)),
                              #control.inla=list(int.strategy='eb'), ## do not integrate over theta, makes the calculation quicker but not to be used for a final model run
                              control.compute = list(dic=T,waic = T))
          } # end the if else statement for controlling the model run
        
        mo.out <- data.frame(fitted = r.out$summary.fitted.values[1:N,"mean"] , # The expected values can be found with this
                            resid = dat$response - r.out$summary.fitted.values[1:N,"mean"],
                            response = dat$response,
                            depth_cen = dat$depth_cen, year.group = dat$year.group)
        
        # For the non-scallop data we add in the PC scores, these are the original values calculated, not the grouped values
        # we used in the model
        if(surveys[i] != 'scallop' && grepl("PA_Adu",species[s]))
        {
          mo.out$PC1 = dat$PC1
          mo.out$PC2 = dat$PC2 
          mo.out$PC3 = dat$PC3
          mo.out$PC4 = dat$PC4
        }
        mo.out$var.Y <- 1* mo.out$fitted * (1-mo.out$fitted) # Get the variance, for a Bernoulli it is n*p*(1-p), where n = 1 for a Bernoulli
        mo.out$resid.stan <- mo.out$resid / sqrt(mo.out$var.Y) # Now we can get Pearson residuals
        
        md.out <- data.frame(dic = r.out$dic$dic, 
                               dic.p.eff = r.out$dic$p.eff,
                               waic = r.out$waic$waic, 
                               waic.p.eff = r.out$waic$p.eff,
                               Dispersion = sum()) 
        md.out$Dispersion <- sum(mo.out$resid.stan^2)/ (N-md.out$waic.p.eff)
        
        
        res[[run.name]] <- r.out
        #res[[mod.names[m]]]$model <- mod.names[m]
        mod.output[[run.name]] <- mo.out
        mod.output[[run.name]]$model <- run.name
        mod.output[[run.name]]$species <- species[s]
        mod.output[[run.name]]$survey <- surveys[i]
        mod.output[[run.name]]$model.id <- mod.names[m]
        
        mod.diagnostics[[run.name]] <- md.out
        mod.diagnostics[[run.name]]$model <- run.name
        mod.diagnostics[[run.name]]$species <- species[s]
        mod.diagnostics[[run.name]]$survey <- surveys[i]
        mod.diagnostics[[run.name]]$model.id <- mod.names[m]
        
        rand.field[[run.name]] <- r.out$summary.random$w # THis will contain mutliple random fields for the spatio-temporal models.
        rand.field[[run.name]]$model <- run.name
        rand.field[[run.name]]$species <- species[s]
        rand.field[[run.name]]$surveys <- surveys[i]
        rand.field[[run.name]]$model.id <- mod.names[m]

        # Stick a print in here so we know this is moving forward
        messy <- print(paste(run.name,"finished up at", format(Sys.time(),"%H:%M")))
        # Write a message to the ESS so I can see progress remotely....
        fileConn<-file(paste0(direct.proj,"Results/status.txt"))
        writeLines(messy, fileConn)
        close(fileConn)
        
      } # end for(m in 1:n.mods)
      
      # I also want to save the dat object so I can use it below, and the w indices to make pulling out the data easier for the spatio-temporal models.
      dat.list[[paste(species[s],surveys[i],sep="_")]] <- dat
      w.index.list[[paste("Base",species[s],surveys[i],sep="_")]] <- w.index
      w.index.list[[paste("Spatio_temporal",species[s],surveys[i],sep="_")]] <- w.index.rep
      # Save the image on the way through just in case the computer decides to shut down...
      #save.image(paste0(direct.proj,"Results/INLA_output_",species[s],"_",surveys[i],".RData"))
    } #end the massive if statement to just run the model for scallop with scallop data, and survey with groundfish data.

  }# end for(i in 1:n.surveys)
  # Save the image on the way through just in case the computer decides to shut down, worst case here I lose about half a day of modelling.
  save.image(paste0(direct.proj,"Results/INLA_output_",species[s],".RData"))
} # end for(s in 1:n.species)

# The results of the INLA fun
#save.image(paste0(direct.proj,"Results/INLA_output.RData"))
#load(paste0(direct.proj,"Results/INLA_output.RData"))

# And I want to make a mesh list so I have that easily accessed...
mesh.list <- NULL
mesh.list[["scallop"]] <- mesh.sc; mesh.list[["gf"]] <- mesh.gf



# Stitch together the model outputs
#mod.out <- do.call("rbind",mod.output) # This won't work as the PC's don't exist in the scallop data...
mod.diag <- do.call("rbind",mod.diagnostics)
r.field <- do.call("rbind",rand.field)
# Now we actually want to melt these so that we have the data in long form for ggplot
mod.diag <- reshape2::melt(mod.diag,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
# What models do we have issues with...
mod.diag[mod.diag$data == Inf,]
# Output the mod.diag into a csv so I can tidy up any of the models that didn't converge the first time.  Most of the issues are with the depth models and
# the biggest problems being the YT and the data groundfish data restricted to last year
write.csv(mod.diag,paste0(direct.proj,"Results/INLA_models_that_did_not_converge_first_time.csv"))


# So now I can make ggplots of everything, probably could pull this off in some crazy nexted object, but I don't want too..
# I'm going to save the ggplots and plot them later
diag.plot <- NULL
#GOM.pec <- NULL
#GB.pec <- NULL
for(s in 1:num.species) 
{
  # Loop through each of the surveys
  for(i in 1:num.surveys) 
  {
    # now only run the rest of this script if we have data
    if((surveys[i] == 'scallop' && !grepl("PA_Adu",species[s])) || (surveys[i] != 'scallop' && grepl("PA_Adu",species[s])))
    {
      # First let's plot our model diagnostics between the models
      # I'd like to add an h-line at the min + 2 and min + 10 for the dic and waic's (I'll have to add it for all but whatever)
      min.plus <- mod.diag %>%
        group_by(diag,species,survey) %>% summarise(min2 = min(data) + 2, min10 = min(data) + 10) 
      # Don't want to do the above for the dispersion or dic's so makes those NA's
      min.plus[min.plus$diag %in% c("dic.p.eff","waic.p.eff","Dispersion"),c("min2","min10")] <- NA
      # Get the diagnostics data together...
      diag.dat <- mod.diag[mod.diag$species == species[s] & mod.diag$survey == surveys[i],]
      plt.min.plus <- min.plus[min.plus$species == species[s] & min.plus$survey == surveys[i],]
      # First we can make our diagnostic summary plot
      pdf(file = paste0(direct.proj,"/Results/Diagnositc_plots",species[s],surveys[i],".pdf"),width=14,height = 8)
      par(mfrow = c(1,1))
      diag.plot[[paste(species[s],surveys[i],sep="_")]] <- ggplot(diag.dat, aes(model.id,data)) + geom_point() +  
        geom_hline(data = plt.min.plus, aes(yintercept = min2),color="blue",linetype = "dashed",size=1) + 
        geom_hline(data = plt.min.plus, aes(yintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
        facet_wrap(~diag,scales = 'free_y') + xlab("") + ylab("") + ggtitle(paste("Diagnostics", species[s], surveys[i],sep=" "))+
        theme(axis.text=element_text(size=8),axis.title=element_text(size=14,face="bold")) 
      # Now pring the diagnostics for each survey/species combo
      pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Diagnositc_plots.pdf"),width=14,height = 8)
      print(diag.plot)
      dev.off()
   
      # Now we can make the figures we need for each model, first we want to get the names of each run we did and also the number of models this includes
      run.names <- names(mod.output)[grep(paste(species[s],surveys[i],sep=" "),names(mod.output))]
      n.mods <- length(run.names)
      # Now grab the model names, fortunately I made the model names with a '.' so this is pretty easy.
      #The first ^ matches the beginning of the string, but the next one in square brackets negates, so it matches all characters EXCEPT "." - 
      #finally the star means match that any number of times - so match everything from the start up until (but not including) the first dot. 
      #Second argument then replaces that match with an empty string
      mod.names <- sub("^[^.]*", "", run.names)
      # and drop the initial period...
      mod.names <- substr(mod.names,2,20)
      # First we make some residual plots for each model.
      # First I make some spatial residual and fitted plots which are interesting, no obvious weird spatial patterns is nice.    
      pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Residuals/Resid_fitted_spatial_",species[s],"_",surveys[i],".pdf"),width=11,height = 11)
      par(mfrow = c(2,2),mar=c(4,4,2,5))  
      for(m in 1:n.mods)
      {
        run.name <- run.names[m]
        # If it is a spatio-temporal model get this w index
        if(grepl(".st",run.name)) w.ind <- w.index.list[[paste("Spatio_temporal",species[s],surveys[i],sep="_")]]
        # If it is not a spatio-temporal model get this w index
        if(!grepl(".st",run.name)) w.ind <- w.index.list[[paste("Base",species[s],surveys[i],sep="_")]]
   
        # Residual plots for each model, probably would be better to make this a 
        # Spatial fitted and residual plots
        quilt.plot(dat.list[[paste(species[s],surveys[i],sep="_")]]$X, 
                   dat.list[[paste(species[s],surveys[i],sep="_")]]$Y,mod.output[[run.name]]$resid.stan,
                   main=paste("Residuals",mod.names[m]))
        quilt.plot(dat.list[[paste(species[s],surveys[i],sep="_")]]$X, 
                   dat.list[[paste(species[s],surveys[i],sep="_")]]$Y,mod.output[[run.name]]$fitted,
                   main = paste("Fitted",mod.names[m]))
      } # end for(m in 1:n.mods)
      dev.off()
      
      # Now make some residual plots, b/c it's binomial there aren't a lot of great ones, but residuals v.s. covariates can't hurt...
      pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Residuals/Resid_vs_covariates_",species[s],"_",surveys[i],".pdf"),
          width=11,height = 11,onefile=T)
      for(m in 1:n.mods)
      {
        run.name <- run.names[m]
        r.vs.d <- ggplot(mod.output[[run.name]], aes(depth_cen,resid.stan)) + geom_point() + geom_smooth() + 
                         ggtitle(paste("Resid vs depth - Model",mod.names[m])) + 
                         theme(axis.text=element_text(size=10),axis.title=element_text(size=10),plot.title = element_text(size=14)) 
        if(surveys[i] != "scallop")
        {
        r.vs.pc1 <- ggplot(mod.output[[run.name]], aes(PC1,resid.stan)) + geom_point() + geom_smooth() + 
                         ggtitle("Resid vs PC1") + 
                         theme(axis.text=element_text(size=10),axis.title=element_text(size=10),plot.title = element_text(size=14)) 
        r.vs.pc2 <- ggplot(mod.output[[run.name]], aes(PC2,resid.stan)) + geom_point() + geom_smooth() + 
                         ggtitle("Resid vs PC2") + 
                         theme(axis.text=element_text(size=10),axis.title=element_text(size=10),plot.title = element_text(size=14)) 
        r.vs.pc3 <- ggplot(mod.output[[run.name]], aes(PC3,resid.stan)) + geom_point() + geom_smooth() + 
                         ggtitle("Resid vs PC3") + 
                         theme(axis.text=element_text(size=10),axis.title=element_text(size=10),plot.title = element_text(size=14)) 
        r.vs.pc4 <- ggplot(mod.output[[run.name]], aes(PC4,resid.stan)) + geom_point() + geom_smooth() + 
                         ggtitle("Resid vs PC4") + 
                         theme(axis.text=element_text(size=10),axis.title=element_text(size=10),plot.title = element_text(size=14)) 
        grid.arrange(r.vs.d,r.vs.pc1,r.vs.pc2,r.vs.pc3,r.vs.pc4)
        } # end if(surveys[i] != "scallop")
        if(surveys[i] == "scallop") print(r.vs.d)
        
      } # end for(m in 1:n.mods)
      dev.off()
      
      
      # Now for the spatial and spatio-temporal plots...
      pdf(paste0(direct.proj,"Results/Figures/INLA/GOM_and_GB_spatial_fields_",species[s],"_",surveys[i],".pdf"), width = 14, height = 11,onefile = T)
      # plot 2 per page
      par(mfrow = c(1,2),mar=c(4,1,2,6))      
      for(m in 1:n.mods)
      {
        run.name <- run.names[m]
        print(run.name) # Just want to make sure it runs through all the models we want...
        # If it is a spatio-temporal model get this w index
        if(grepl(".st",run.name)) w.ind <- w.index.list[[paste("Spatio_temporal",species[s],surveys[i],sep="_")]]
        # If it is not a spatio-temporal model get this w index
        if(!grepl(".st",run.name)) w.ind <- w.index.list[[paste("Base",species[s],surveys[i],sep="_")]]
        # Here is what ya need to do to get the spatio-temporal field working.
        eras <- unique(w.ind$w.repl)
        n.eras <- length(eras)
        # For the RV survey I kept the eras as 2-5 to keep era 2 as the 1980's, so I need to add 1 to these
        dat.eras <- eras
        if(surveys[i] %in% c("RV","scallop") && n.eras > 1) dat.eras <- eras+1
        
  
        for (p in 1:n.eras)
        {
          # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
          # is always just our intercept.
          tmp.field <- data.frame(r.field.link = rand.field[[run.name]]$mean[w.ind$w.repl == eras[p]] + res[[run.name]]$summary.fixed$mean, 
                                  r.field.response = inv.logit(rand.field[[run.name]]$mean + + res[[run.name]]$summary.fixed$mean)[w.ind$w.repl == eras[p]],
                                  r.field.sd = rand.field[[run.name]]$sd[w.ind$w.repl == eras[p]],
                                  era = eras[p])
          # Get the points used for each of the models.
          if(n.eras == 1) loc <- data.frame( X = dat.list[[paste(species[s],surveys[i],sep="_")]]$X, Y = dat.list[[paste(species[s],surveys[i],sep="_")]]$Y)
          if(n.eras > 1)
          {
            dat.era <- dat.list[[paste(species[s],surveys[i],sep="_")]][dat.list[[paste(species[s],surveys[i],sep="_")]]$year.group == dat.eras[p],]
            name.era <- paste0(min(dat.era$year),"-",max(dat.era$year))
            loc <- data.frame( X = dat.era$X, Y = dat.era$Y)
          } # end if(n.eras > 1)
          coordinates(loc) <- ~X+Y
          proj4string(loc) <- proj4string(loc.sc)
          
          
          # Make sure we have the right mesh and locations for the model
          ifelse(grepl('stan', species[s]) || grepl('_can', species[s]),mesh.plt <- mesh.list$sc,mesh.plt <- mesh.list$gf)
          # Let's clip the area, this really doesn't do anything as we clip to the mesh...
          con.hull <- chull(loc@coords)
          clp <- loc@coords[c(con.hull,con.hull[1]),] # extract those points, take the first one twice to close the polygon
          # Make the plot of the whole GOM region
          pecjector(area = "GOM", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                    dims=c(400,400), clip = clp,add_land =T,add_EEZ = "sounds grand",
                    colors = rev(magma(10)),add_obj = cod.closed.cells, c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                    repo='github')
          points(loc,cex=0.05,col=addalpha("black",0.25),pch=19)
          
          if(n.eras == 1) title(paste("Encounter Probability - All years",mod.names[m]))
          if(n.eras > 1) title(paste("Encounter Probability - ",name.era,mod.names[m]))
          
          # Same thing for GB
          # Make the plot of the whole GB region
          pecjector(area = "GB", field = tmp.field$r.field.response,mesh=mesh.plt,lvls = c(0,0.2,0.4,0.6,0.8,0.85,0.9,0.95,1),
                    dims=c(400,400), clip = clp,add_land =T,add_EEZ = "sounds grand",
                    colors = rev(magma(10)),add_obj = cod.closed.cells, c_sys = proj4string(loc.gf), add_bathy = NULL, # loc.sc and loc.gf have same proj.
                    repo='github')
          points(loc@coords,cex=0.5,col=addalpha("black",0.25),pch=19)
          if(n.eras == 1) title(paste("Encounter Probability - All years",mod.names[m]))
          if(n.eras > 1) title(paste("Encounter Probability - ",name.era,mod.names[m]))
          
        } # end  for (p in 1:n.eras)
        
      } #end for(m in 1:n.mods)
      dev.off()
      
    } # end if((surveys[i] == 'scallop' && !grepl("PA_Adu",species[s])) || (surveys[i] != 'scallop' && grepl("PA_Adu",species[s])))
  } # end for(i in 1:num.surveys) 
} # end for(s in 1:num.species) 
  





















