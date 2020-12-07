#############
###Query the DFO and NMFS RV survey data from spring and summer (DFO), and summer and fall (NMFS). 
###Grab all tows from each of the 4 surveys, and the tows with standandardized weight, for atlantic cod (spec=10) and yellowtail flounder (spec = 42)
###For all of Georges Bank (strata 500+ and 5Z+)
###Join all together, and plot to ensure locations are sensible. 
# Updated in 2020 to pull in all the survey data!


library(tidyverse)
library(lubridate)
library(Mar.datawrangling)
library(sf)
#chan <- odbcConnect(dsn="ptran", uid=un.ID, pwd = pwd.ID)
#get_data('rv', data.dir ="D:/to_sky/RV/",usepkg = 'roracle') 
#load("D:/to_sky/RV/RV_Survey_length_frequencies.RData") BIO
# Once data is downloaded you can load it from local directory
get_data('rv', data.dir ="D:/NAS/Projects/GB_time_area_closure_SPERA/Data/2017_2020_data/") 
# Run this to quickly subset the data fro summarize catches, Cod = 10, YT = 42.
# Because of the 0's I don't want to do cod and yt at the same time, 
get_survey(survey = "GEORGES")
data_filter() # In here I subset to 2017-2019 data, cod, and stratified survey stations (i.e. RV survey).
# Then you can run this on just the cod and yellowtail data, much quicker.
sets.cod <- summarize_catches(valid.coords = T) 
sets.cod$species <- "cod_PA"
names(sets.cod) <- tolower(names(sets.cod))
# Then do YT seperately, then I'll take care of merging these...
get_data('rv', data.dir ="D:/NAS/Projects/GB_time_area_closure_SPERA/Data/2017_2020_data/") 
get_survey(survey = "GEORGES")
data_filter()
sets.yt <- summarize_catches(valid.coords = T) 
sets.yt$species <- "yt_PA"
names(sets.yt) <- tolower(names(sets.yt))


# Now combine these 
sets.new <- rbind(sets.cod,sets.yt)

sets.new$spec.gsspecies <- tolower(sets.new$spec.gsspecies)
sets.new$totno[which(is.na(sets.new$totno))] <- 0
sets.new$PA <- 0
sets.new$PA[which(sets.new$totno > 0)] <- 1
#specs <- sort(unique(sets$spec.gsspecies))
#cod.yt.rv <- sets %>% filter(spec %in% c(10,42))
ggplot(sets.new) + geom_histogram(aes(x= PA)) + xlim(c(-1,5))


# This combines the catch table for cod and yt with the tow details. This is the same as Mike's summarize catches above (though his allows bad Lat/Lon to get removed), so 
# the NA"s in the summarize_catches includes the sets with 0's!  Mike is so good!
# combo <- left_join(GSINF,GSCAT,by = c("MISSION","SETNO"))
# combo$date <- ymd_hms(combo$SDATE)
# combo$year <- year(combo$date)
# # How much data do we have by year
# table(combo$year)
# Now let's get it by survey season, need to grab that from the GSMISSIONS table
rv.new.dat <- left_join(sets.new,GSMISSIONS, by = c('mission' = "MISSION"))
# So these are all the sets that 
rv.new.dat <- rv.new.dat %>% dplyr::select(-YEAR,-SEASON,-VESEL,-CRUNO) # dupicated...
rv.new.dat$survey <- "RV"
head(rv.new.dat)
names(rv.new.dat) <- tolower(names(rv.new.dat))
#loading in the Monkfish RV survey data
#load("Y:/Projects/Monkfish/Data/RVSurvey/RV_Survey_length_frequencies.RData")

table(rv.new.dat$pa)


# For the NMFS survey cod is 73 and yellowtail is 105

spring.cruises <- read.csv("D:/NAS/Projects/GB_time_area_closure_SPERA/Data/Survey/nmfs_spring_cod.csv")
spring.cruise <- c(unique(substr(spring.cruises$set,4,9)),201702,201802,201902)
fall.cruises <- read.csv("D:/NAS/Projects/GB_time_area_closure_SPERA/Data/Survey/nmfs_fall_cod.csv")
fall.cruise <- c(unique(substr(fall.cruises$set,4,9)),201704,201804,201904)

# Therse are all the relevent stations, I want an object for cod and yt, for later :-)
stations <- read.csv("D:/NAS/Projects/GB_time_area_closure_SPERA/Data/2017_2020_data/nmfs_stations.csv")
# Now I need to double the size of station info because I need it for the cod
yt.dat <- read.csv("D:/NAS/Projects/GB_time_area_closure_SPERA/Data/2017_2020_data/NMFS_YT_data.csv")
cod.dat <- read.csv("D:/NAS/Projects/GB_time_area_closure_SPERA/Data/2017_2020_data/NMFS_cod_data.csv")
yt.dat$species <- "yt_PA"
cod.dat$species <- 'cod_PA'
#nmfs.dat <- rbind(yt.dat,cod.dat)
# Now we  also just want to include Station type 1 tows (Survey tows) and 
# tows with a "Haul" of 1-2 which are either "good" tows or "representative" tows
# The gear condition codes feed back to the Haul codes so we dont need to specify those.
spring.station.list <- stations[stations$STATYPE == 1 & stations$HAUL %in% 1:2 & stations$CRUISE6 %in% spring.cruise,]
fall.station.list <- stations[stations$STATYPE == 1 & stations$HAUL %in% 1:2 & stations$CRUISE6 %in% fall.cruise,]

tst <- fall.station.list %>% group_by(CRUISE6) %>% summarise(n = n())
summary(tst$n)


# Now we can merge the catch table and the length frequency table together and will be able to spatially plot our data
# For the catch data we want to have all the tow locations, here tows without any Monkfish will be "NA's" so will replace with 0's.
nmfs.cod.dat <- left_join(stations,cod.dat,by = c("CRUISE6","CRUISE","STRATUM","TOW","STATION","STATUS_CODE","ID"))
nmfs.cod.dat$species <- "cod_PA"
nmfs.yt.dat <- left_join(stations,yt.dat,by = c("CRUISE6","CRUISE","STRATUM","TOW","STATION","STATUS_CODE","ID"))
nmfs.yt.dat$species <- "yt_PA"
nmfs.catch.dat <- rbind(nmfs.cod.dat,nmfs.yt.dat)
# To avoid a life time of all caps...
names(nmfs.catch.dat) <- tolower(names(nmfs.catch.dat))
# Now make any NA catches 0's
nmfs.catch.dat$expcatchnum[which(is.na(nmfs.catch.dat$expcatchnum))] <- 0
nmfs.catch.dat$expcatchwt[which(is.na(nmfs.catch.dat$expcatchwt))] <- 0
nmfs.catch.dat$PA <- 0
nmfs.catch.dat$PA[which(nmfs.catch.dat$expcatchnum > 0)] <- 1
# Subsets of just the spring and fall data.
nmfs.spring.catch.dat <- nmfs.catch.dat[nmfs.catch.dat$id %in% unique(spring.station.list$ID), ]
nmfs.spring.catch.dat$survey <- 'nmfs-spring'
nmfs.fall.catch.dat <- nmfs.catch.dat[nmfs.catch.dat$id %in% unique(fall.station.list$ID), ]
nmfs.fall.catch.dat$survey <- 'nmfs-fall'

nmfs.final <- rbind(nmfs.spring.catch.dat,nmfs.fall.catch.dat)
nmfs.final.new <- nmfs.final %>% filter(est_year  > 2016)

table(nmfs.final.new$PA)

# Now we can work to combine the 3 surveys into one object that has what we need...
nmfs.key.dat <- nmfs.final.new %>% select(tow,station,setdepth,decdeg_beglat,decdeg_beglon,species,PA,survey,est_year)
names(nmfs.key.dat) <- c("tow","station","comldepth","lat","lon","species","PA",'survey',"year")
rv.key.dat <- rv.new.dat %>% select(setno,station,start_depth,latitude,longitude,species,pa,survey,year)
names(rv.key.dat) <- c("tow","station","comldepth","lat","lon","species","PA",'survey',"year")
# Oops, the RV data is in fathoms not in meters, convert the depths...
rv.key.dat$comldepth <- 1.8288 * rv.key.dat$comldepth
# Now save this objects for use elsewhere...
#
nmfs.key.dat.sf <- st_as_sf(nmfs.key.dat, coords = c("lon","lat"),crs = 4326)
rv.key.dat.sf <- st_as_sf(rv.key.dat, coords = c("lon","lat"),crs = 4326)

#save(rv.key.dat,nmfs.key.dat,file = "D:/to_sky/2017_2020/RV_and_NMFS_survey_dat.RData" )
#load(file = "D:/to_sky/2017_2020/RV_and_NMFS_survey_dat.RData" )

# Lots of extra's we need to remove for the nmfs data
ggplot(nmfs.key.dat.sf) + geom_sf() + facet_wrap(~year)
# DFO coverage less than spectacular on US side in 2017-2018, but data ready to be 'predicted on.
ggplot(rv.key.dat.sf) + geom_sf() + facet_wrap(~year)


######################################### Now combine the new data into the INLA mesh input...

# Bring in the 
direct.proj <- "d:/NAS/Projects/GB_time_area_closure_SPERA/"; dir.tmp <- direct.proj
#load(paste0(direct.proj,"Data/2017_2020_data/RV_and_NMFS_survey_dat.RData"))
load(paste0("D:/Github/Paper_2_SDMs/data/Prediction_mesh.RData"))
load(paste0(direct.proj,"Data/INLA_mesh_input_data.RData"))
load(paste0(direct.proj,"Data/SST_and_Depth_covariates_and_boundary_for_prediction.RData"))
load(paste0(direct.proj,"Data/Depth_SST_and_Sed.RData"))


new.dat <- rbind(nmfs.key.dat,rv.key.dat)
new.dat$unique_set_ID <- paste(new.dat$survey,new.dat$year,new.dat$tow,new.dat$station,sep="_")
# This will split out the cod and yellowtail data from long to wide format
new.dat <- new.dat %>% pivot_wider(names_from = species,values_from = PA)
new.dat.sf <- st_as_sf(new.dat,coords = c("lon","lat"),crs = 4326,remove = F)
# SO I new to get the SST covariate into these data and clean up the nmfs so we don't have the whole eastern seaboard in here...
# First I clip the data to our prediction "mesh" so we only have valuees within the prediction grid.
new.dat.clp <- st_intersection(new.dat.sf,st_transform(mesh.grid,crs = 4326))
new.dat.final <- st_transform(new.dat.clp,crs=32619)
sst.sf <- st_transform(sst.sf,crs=32619)
depth.sf <- st_transform(depth.sf,crs= 32619)
sed.sf <- st_transform(sed,crs=32619)
sed.sf <- sed.sf %>% dplyr::select(-AREA,-PERIMETER)
names(sed.sf) <- tolower(names(sed.sf))
locs <- st_coordinates(new.dat.final)
# Need this for INLA...
new.dat.final$X <- locs[,1]
new.dat.final$Y <- locs[,2]

# Now we need to add in the sst covariate data and we're ready to rock.

#ggplot(sst.sf) + geom_sf()

new.dat.final <- st_intersection(new.dat.final,sst.sf)
new.dat.final$sst_avg <- new.dat.final$Band_1
new.dat.final <- st_intersection(new.dat.final,depth.sf)
new.dat.final$comldepth <- new.dat.final$Band_1.1
new.dat.final <- st_intersection(new.dat.final,sed.sf)
new.dat.final$SEDNUM <- new.dat.final$sednum
# Now do the same with depth
# These are the maximum years_3 and years_5 era's, recall these are the same for RV and NMFS as it was easier if the "Era" was the same for NMFS and RV so RV picks it up
# From the NMFS numbering.
new.dat.final$years_3 <- 16
new.dat.final$years_5 <- 10


# And now save this final object which we can use directly in our INLA modelling (Step 6, Section 7)
#save(new.dat.final,file = "D:/to_sky/Survey_data_with_ALL_covars_2017_2020.RData" )


########################## SECTION GINI SECTION GINI SECTION GINI SECTION GINI SECTION GINI SECTION GINI SECTION GINI SECTION GINI ########################
#########
#########

########## New step to put everything in its correct strata and get a strata estimate for all these...

temp <- tempfile()
# Download this to there
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/other_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
# Now grab the individual shape files I want.
rv <- st_read(dsn = paste0(temp2,"/MaritimesRegionEcosystemAssessmentStrata(2014-).shp"))
nmfs <- st_read(dsn = paste0(temp2,"/newUSstrata_UTM19.shp"))


load(paste0("D:/Github/Closure-Framework/data/Prediction_mesh.RData"))

clp.poly <- st_as_sf(data.frame(X = c(508000,508000,900000,650000,600000,550000),
                                Y=c(4540000,4350000,4674000,4674000,4661000,4622000),ID=1),coords = c("X","Y"),crs= 32619)
clp.poly <- st_cast(st_combine(clp.poly),"POLYGON")
# # Now use the bigger clp with this other clip to get a nice clipped GB area...
clp.pred <- st_intersection(clp,clp.poly)

# Now we can work to combine the 3 surveys into one object that has what we need...
nmfs.gini.dat <- nmfs.final %>% select(tow,station,setdepth,decdeg_beglat,decdeg_beglon,species,PA,survey,est_year,expcatchwt,expcatchnum,stratum)
names(nmfs.gini.dat) <- c("tow","station","comldepth","lat","lon","species","PA",'survey',"year","weight","number","strata")
nmfs.gini$strata <- as.numeric(substr(nmfs.gini$strata,2,5))
rv.gini.dat <- rv.new.dat %>% select(setno,station,start_depth,latitude,longitude,species,pa,survey,year,totwgt,totno,strat)
names(rv.gini.dat) <- c("tow","station","comldepth","lat","lon","species","PA",'survey',"year","weight","number","strata")

# Now subset to our areas of interest....
nmfs.gini.sf <- st_as_sf(nmfs.gini.dat,coords = c('lon','lat'),crs = 4326)
rv.gini.sf <- st_as_sf(rv.gini.dat,coords = c('lon','lat'),crs=4326)

nmfs.gini <- st_intersection(nmfs.gini.sf,st_transform(mesh.grid,crs = 4326))
nmfs.gini <- st_transform(nmfs.gini,crs = 32619)

rv.gini <- st_intersection(rv.gini.sf,st_transform(mesh.grid,crs = 4326))
rv.gini <- st_transform(rv.gini,crs = 32619)

ggplot(nmfs.gini) + geom_sf()

# Now we need to clip the various survey strata to our GB world...
# Now need an outline of mesh.grid

nmfs.surv.gb <- st_intersection(nmfs,clp.pred)
rv.surv.gb <- st_intersection(rv,clp.pred)
ggplot(nmfs.surv.gb) + geom_sf()

rv.surv.gb$Areakm <- rv.surv.gb %>% st_area() %>% units::set_units("km^2") %>% as.numeric()
nmfs.surv.gb$Areakm <- nmfs.surv.gb %>% st_area() %>% units::set_units("km^2") %>% as.numeric()

# So now I think I have everythign I need to do the Gini calculations, areas and biomasses for each survey strata.. shouldn't take long now..
# HA!! Let's save all the bits from the above...
save(nmfs.gini,rv.gini,nmfs.gini.sf,rv.gini.sf,nmfs.surv.gb,rv.surv.gb,clp.pred,file = "D:/Github/Paper_2_SDMs/Results/Data_for_Gini.RData" )
