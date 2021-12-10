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
direct.proj <- "D:/Github/Paper_2_SDMs/"

#chan <- odbcConnect(dsn="ptran", uid=un.ID, pwd = pwd.ID)
#get_data('rv', data.dir ="D:/to_sky/RV/",usepkg = 'roracle') 
#load("D:/to_sky/RV/RV_Survey_length_frequencies.RData") BIO
# Once data is downloaded you can load it from local directory
get_data('rv', data.dir =paste0(direct.proj,"Data/all_RV_data/"),usepkg = "roracle") 
# Run this to quickly subset the data fro summarize catches, Cod = 10, YT = 42.
# Because of the 0's I don't want to do cod and yt at the same time, 
#get_survey(data.dir ="D:/Github/Paper_2_SDMs/Data/all_RV_data/")
data_filter() # In here I subset to cod, and stratified survey stations (i.e. RV survey).
# Then you can run this on just the cod and yellowtail data, much quicker.
sets.cod <- summarize_catches(valid.coords = T) 
sets.cod$species <- "cod_PA"
names(sets.cod) <- tolower(names(sets.cod))
sets.cod$sciname <- "Gadus morhua"

# Then do YT seperately, then I'll take care of merging these...
get_data('rv', data.dir =paste0(direct.proj,"Data/all_RV_data/")) 
#get_survey(data.dir ="D:/Github/Paper_2_SDMs/Data/all_RV_data/")
data_filter()  # In here I subset to cod, and stratified survey stations (i.e. RV survey).
sets.yt <- summarize_catches(valid.coords = T) 
sets.yt$species <- "yt_PA"
names(sets.yt) <- tolower(names(sets.yt))
sets.yt$sciname <- "Limanda ferruginea"

# Now combine these 
sets.new <- rbind(sets.cod,sets.yt)

# Keep only the stratified random survey sets, bit harsh but should avoid people complaining... you can do this in the data_filter() step as well...
sets.new <- sets.new[sets.new$xtype ==1,]
# Now tidy up to make the NA's into 0's
sets.new$totno[which(is.na(sets.new$totno))] <- 0
sets.new$sampwgt[which(is.na(sets.new$sampwgt ))] <- 0
sets.new$totwgt[which(is.na(sets.new$totwgt ))] <- 0
sets.new$PA <- 0
sets.new$PA[which(sets.new$totno > 0)] <- 1
sets.new$survey <- "RV"
#specs <- sort(unique(sets$spec.gsspecies))
#cod.yt.rv <- sets %>% filter(spec %in% c(10,42))


# Now try to line up everything we have in nmfs and RV survey and subset the data to these.  Wonder if I need to reorder the columns or if rbind is smart enough

# Let's get these to match in what we are keeping so I can do everything consistent with both surveys.
common.names <- c('mission','station','tow','area','lat','lon','strata',
                  'num','wgt','PA','date','year',
                  'spec','sciname','survey','tow_info','ves','crew',
                  'dur_tow','dist','speed','wind_dir','weather',
                  'depth_min','depth_max','depth','temp_ss','temp_btm','sal_btm')
             

rv.new.dat <- sets.new %>% dplyr::select(mission,station,setno,area,latitude,longitude,strat,
                                           totno,totwgt,PA,sdate,year,
                                           species,sciname,survey,xtypedesc,vesel,cruno,
                                           dur,dist,speed,wind,force, # I believe weather in US is on that 1-9 scale, which is force in RV survey
                                           dmin,dmax,depth,  # depths are in fathoms
                                           surface_temperature,bottom_temperature,bottom_salinity)

names(rv.new.dat) <- common.names

# Make into a sf() object.
rv.sf <- st_as_sf(rv.new.dat,coords = c("lon","lat"),remove =F,crs = 4326)
#st_geometry(rv.new.dat) <- NULL

ggplot(rv.sf) + geom_sf()
rv.new.dat$station <- as.character(rv.new.dat$station)
rv.new.dat$tow <- as.character(rv.new.dat$tow)

#saveRDS(rv.new.dat,paste0(direct.proj,"data/Rev_1/New_RV_data.RDS"))
#rv.new.dat <- readRDS(paste0(direct.proj,"data/Rev_1/New_RV_data.RDS")

# Revise to use this to get NMFS data, select
get_data(data.dir =paste0(direct.proj,"Data/all_NMFS_data/"),usepkg = "roracle") 

#sixties.cruise <- c(196305, 196307,196401,196410,196413,196502,196510,196514,196601,196613,196614,196721,196803,196817,196902,196908,196911)
#sixities.station.list <- USS_STATION[USS_STATION$STATYPE == 1 & USS_STATION$HAUL %in% 1:2 & USS_STATION$CRUISE6 %in% sixties.cruise,]
# All sixities stations including different station types and haul codes...
#all.sixities.station.list <- USS_STATION[USS_STATION$CRUISE6 %in% sixties.cruise,]
#table(all.sixities.station.list$CRUISE6,all.sixities.station.list$EST_MONTH)
# This indicates that the spring cruises start annually in 1968, with cruises 196803 and 196902 begin spring
# This indicates that the fall cruises start annually in 1963, with cruises 196307, 196413, 196514,  196614, 196721, 196817, 196911 being fall

# These cruise files are based on the original data I received, cleaned them up a bit to be more general for needs and added in the stations for all years of interest now.
# spring.cruises <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Survey/nmfs_spring_cod.csv")
# spring.cruise <- c(unique(substr(spring.cruises$set,4,9)),196803,196902,201702,201802,201902)
# fall.cruises <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Survey/nmfs_fall_cod.csv")
# fall.cruise <- c(unique(substr(fall.cruises$set,4,9)),201704,201804,201904,196911,196817,196721,196614,196514,196413,196307)
#saveRDS(spring.cruise,paste0(direct.proj,"Data/Rev_1/spring_cruise_list.Rds"))
#saveRDS(fall.cruise,paste0(direct.proj,"Data/Rev_1/fall_cruise_list.Rds"))
spring.cruise <- readRDS(paste0(direct.proj,"Data/Rev_1/spring_cruise_list.Rds"))
fall.cruise <- readRDS(paste0(direct.proj,"Data/Rev_1/fall_cruise_list.Rds"))

# Now we  also just want to include Station type 1 tows (Survey tows) and 
# tows with a "Haul" of 1-2 which are either "good" tows or "representative" tows
# The gear condition codes feed back to the Haul codes so we don't need to specify those.
# Making a 'station type 0' and haul type 0 which will replace the NA's for these.  Using all data in which we don't have STATYPE info. This effects only the 1972-1981 data for some reason.
# Checked right in the DB and as of Aug 11, 2021 the 1972-1981 statype and Haul fields were largerly NULLs (so NA's in R).  Don't think it used to be like that....
USS_STATION$STATYPE[is.na(USS_STATION$STATYPE)] <- 0
USS_STATION$HAUL[is.na(USS_STATION$HAUL)] <- 0
spring.station.list <- USS_STATION[USS_STATION$STATYPE < 2  & USS_STATION$HAUL <3 & USS_STATION$CRUISE6 %in% spring.cruise,]
fall.station.list <- USS_STATION[USS_STATION$STATYPE < 2 & USS_STATION$HAUL <3 & USS_STATION$CRUISE6 %in% fall.cruise,]

# For the NMFS survey cod is 73 and yellowtail is 105
cod.catch <- USS_CATCH %>% dplyr::filter(SVSPP == '073')
yt.catch <- USS_CATCH %>% dplyr::filter(SVSPP == '105')
yt.catch$species <- "yt_PA"
cod.catch$species <- 'cod_PA'

# Now subset to the good stations in the spring and summer...

cod.spring.dat <- left_join(spring.station.list,cod.catch,by = c("CRUISE6","STATION","CRUISE","STRATUM","TOW","STATUS_CODE","ID"))
cod.spring.dat$survey <- "Spring"
yt.spring.dat <- left_join(spring.station.list,yt.catch,by = c("CRUISE6","STATION","CRUISE","STRATUM","TOW","STATUS_CODE","ID"))
yt.spring.dat$survey <- "Spring"
cod.fall.dat <- left_join(fall.station.list,cod.catch,by = c("CRUISE6","STATION","CRUISE","STRATUM","TOW","STATUS_CODE","ID"))
cod.fall.dat$survey <- "Fall"
yt.fall.dat <- left_join(fall.station.list,yt.catch,by = c("CRUISE6","STATION","CRUISE","STRATUM","TOW","STATUS_CODE","ID"))
yt.fall.dat$survey <- "Fall"




#Clean up the data
nmfs.cod.dat <- bind_rows(cod.spring.dat,cod.fall.dat)
names(nmfs.cod.dat) <- tolower(names(nmfs.cod.dat))
# Trim down the fields at least a little bit...
# Fill in the NA's
#nmfs.cod.dat$species_code <- "073"
nmfs.cod.dat$expcatchnum[is.na(nmfs.cod.dat$expcatchnum)] <- 0
nmfs.cod.dat$expcatchwt[is.na(nmfs.cod.dat$expcatchwt)] <- 0
nmfs.cod.dat$PA <- 0
nmfs.cod.dat$PA[nmfs.cod.dat$expcatchnum > 0] <- 1
nmfs.cod.dat$species <- 'cod_PA'
nmfs.cod.dat$sciname <- 'Gadus morhua'
# Subset to common fields
nmfs.cod.dat <- nmfs.cod.dat %>% dplyr::select(cruise6,station,tow,area,latitude,longitude,stratum,
                                               expcatchnum,expcatchwt,PA,begin_est_towdate,est_year,
                                               species,sciname,survey,statype,svvessel,crunum,
                                               towdur,dopdistb,desspeed,winddir,weather,  # I believe weather in US is on that 1-9 scale, which is force in RV survey
                                               mindepth,maxdepth,avgdepth,
                                               surftemp,bottemp,botsalin)
# Rename the fields
names(nmfs.cod.dat) <- common.names


# Now do the same for the yellowtail data
nmfs.yt.dat <- bind_rows(yt.spring.dat,yt.fall.dat)
names(nmfs.yt.dat) <- tolower(names(nmfs.yt.dat))
# Labels
#nmfs.yt.dat$species_code <- "105"
nmfs.yt.dat$expcatchnum[is.na(nmfs.yt.dat$expcatchnum)] <- 0
nmfs.yt.dat$expcatchwt[is.na(nmfs.yt.dat$expcatchwt)] <- 0
nmfs.yt.dat$species <- 'yt_PA'
nmfs.yt.dat$sciname <- "Limanda ferruginea"
nmfs.yt.dat$PA <- 0
nmfs.yt.dat$PA[nmfs.yt.dat$expcatchnum > 0] <- 1
nmfs.yt.dat <- nmfs.yt.dat %>% dplyr::select(cruise6,station,tow,area,latitude,longitude,stratum,
                                             expcatchnum,expcatchwt,PA,begin_est_towdate,est_year,
                                             species,sciname,survey,statype,svvessel,crunum,
                                             towdur,dopdistb,desspeed,winddir,weather,  # I believe weather in US is on that 1-9 scale, which is force in RV survey
                                             mindepth,maxdepth,avgdepth,
                                             surftemp,bottemp,botsalin)

names(nmfs.yt.dat) <- common.names


nmfs.final <- bind_rows(nmfs.cod.dat,nmfs.yt.dat)
nmfs.final$year <- as.numeric(nmfs.final$year)
nmfs.final$crew <- as.numeric(nmfs.final$crew)
nmfs.final$weather <- as.numeric(nmfs.final$weather)
#saveRDS(nmfs.final,paste0(direct.proj,"/data/Rev_1/New_NMFS_data.RDS"))
# nmfs.final <- readRDS("D:/Github/Paper_2_SDMs/data/Rev_1/New_NMFS_data.RDS")
new.dat.all <- bind_rows(nmfs.final,rv.new.dat)
new.dat.all <- st_as_sf(new.dat.all,coords= c('lon','lat'),crs=4326,remove=F)
#saveRDS(new.dat.all,paste0(direct.proj,"/data/Rev_1/new.dat.all.RDS"))
#new.dat.all <- readRDS(paste0(direct.proj,"/data/Rev_1/new.dat.all.RDS"))
# Now I want to subset to a region of interest, which is an interesting question, I'm aiming for GB and overlap between the surveys, so I'm thinking GB with a buffer around that so we can
# see movement off the bank. Also clipped close to GB southern border which will be useful later when making the mesh 

clp.region <- st_as_sf(data.frame(X = c(-70.0,  -70.0, -65.0, -65.0,  -65.6, -65.7,  -67.0 , -70.0),
                                  Y=c(   39.75,  42.85, 42.85, 42.3,   42.05, 41.55,  40.45,  39.75),ID=1),coords = c("X","Y"),crs= 4326)
# Now make this a polygon
clp.region <- st_cast(st_combine(clp.region),"POLYGON")
#saveRDS(clp.region,paste0(direct.proj,"data/Rev_1/clp.region.RDS"))
# check out the clip region
ggplot(new.dat.all) + geom_sf() + geom_sf(data=clp.region,fill=NA,size=1,color ='blue') + coord_sf(xlim = c(-71,-64),ylim=c(39,43))

# Plot it on pecjector
base.map <- pecjector(area = list(y = c(39.5,45.5),x = c(-71,-63),crs = 4326),add_layer = list(land = 'grey',eez = 'eez', bathy = 50),plot=F)
tst <- pecjector(gg.obj = base.map, area = list(y = c(39.5,45.5),x = c(-71,-63),crs = 4326),
                 add_layer = list(land = 'grey',nafo = 'sub'),
                 add_custom = list(obj=new.dat.all,facet = 'survey',color='green',size=0.2),
                 plot=F)
# Add the clp region 
tst2 <- pecjector(gg.obj = tst, area = list(y = c(39.5,45.5),x = c(-71,-63),crs = 4326),
                  add_custom = list(obj = clp.region,size=2,color='firebrick'))

new.dat.final <- st_intersection(new.dat.all,clp.region)
#saveRDS(new.dat.final,"D:/Github/Paper_2_SDMs/data/Rev_1/new.dat.final.RDS")
#new.dat.final <- readRDS("D:/Github/Paper_2_SDMs/data/Rev_1/new.dat.final.RDS")
