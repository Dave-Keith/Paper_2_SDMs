------
title: "Groundfish observer data - Import and QA/QC"
------

####################################################################
######### Can skip to line 324 if data is already saved.
######### Master cleaned tripset list is gf.obs.trips.sets (saved as MASTER_CLEAN_gf.obs.trips.sets.csv above)
######### Master cleaned tripset species list is gf.observer.final (saved as MASTER_CLEAN_gf.obs.trips.sets.species.csv above)
####################################################################

### OUTPUTS:
### Groundfish_observer_data_withgear.csv
### Groundfish_observer_length_data.csv
### MASTER_CLEAN_gf.obs.trips.sets.csv
### MASTER_CLEAN_gf.obs.trips.sets.species.csv
### gf.observer.ytf.csv
### gf.observer.cod.csv
####### length_long_cod_FINAL_FULL.csv
####### length_long_cod_FINAL_FULL.csv
####### length_wide_cod_FULL_FINAL.csv
####### length_wide_ytf_FULL_FINAL.csv

### This script will pull in the groundfish observer data, plus observer length frequency data
### Runs spatial, temporal and keypunching QA/QC on observer data (incl length frequency)
  
direct <- "d:/local_backup/r/"
direct <- "Y:/Offshore scallop/Assessment/"
require(ROracle)
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) #Source3 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source3 


############## PART 1: READ IN THE DATA USING RORACLE ###################

### Read in the trip list 
obs.ids <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/Trip_lists/Observer_trip_IDs_2002_2016.csv",
                    header=F,stringsAsFactors = F)

# Remove any duplicate trip ID (occur when fishing in multiple banks)
ids <- unique(obs.ids$V1)

# Open an RODBC channel, using 32 bit so make sure you are running 32 bit R
chan <- dbConnect(dbDriver("Oracle"), un.ID, pwd.ID, "PTRAN", believeNRows=FALSE)

# The query to grab log data

tmp <- NULL # define a temporary variable
# This is a slow and painful way to do this, but I was getting a big old error trying it how I did above, I believe due to
# it tripping on the quotation marks for the ID's (whereas above it was a number, here we have a character), so pulling multiples trips 
# was leading to R exploding.  This will do the trick just rather slow way to go about it since I'm querying the database 4900 times, yuk!
# But it only takes about 5 minutes tops so not terrible

for(i in 1:length(ids))
{
  #if(i > 1) count <- count+1000 # We are using the 1000 limit, so for each loop increase the index by 1000.
  #if(i < num.loop) id <- paste(ids[count:(count+999)],collapse = ",")
  #if(i == num.loop) id <- paste(ids[count:length(ids)],collapse = ",")
  # DK NOTE:  Added b.gear_id, Oct 16th 2017 to get the gear info in here.
  # FK NOTE: Removed b.gear_id, Oct 17 2017 with IA because it is just a unique identifier for the gear table. We need to know what the gear actually is.
  # So IA added "i.description as gear", "observer.isgears h, observer.isgearcodes i"AND b.gear_id=h.gear_id and h.gearcd_id=i.gearcd_id" to get us the right info.
  query <-   paste("SELECT g.cfv, g.vessel_name, g.license_no, b.num_hook_haul, b.source, y.board_date, y.landing_date, y.trip, y.trip_id, x.set_no,",
                   "x.speccd_id, d.common, x.est_num_caught, x.est_kept_wt, x.est_discard_wt, f.latitude, f.longitude, f.depth,  f.pntcd_id, f.setdate, f.settime, ",
                   "b.comarea_id, i.description as gear FROM observer.istrips y, observer.isfishsets b, observer.iscatches x, observer.isspeciescodes d,",
                   "observer.issetprofile f, observer.isvessels g, observer.isgears h, observer.isgearcodes i WHERE y.trip_id = b.trip_id AND g.vess_id = y.vess_id AND b.fishset_id = x.fishset_id",
                   "AND x.speccd_id = d.speccd_id AND b.fishset_id = f.fishset_id AND x.fishset_id = f.fishset_id ",
                   "AND y.trip = (",paste("'",ids[i],"'",sep=""),") AND f.pntcd_id = 2",
                   "AND b.gear_id = h.gear_id AND h.gearcd_id = i.gearcd_id AND h.trip_id = b.trip_id;")
  
  tmp[[i]] <- dbGetQuery(chan, query)
} # End the for loop

observer.data <- do.call("rbind",tmp)
# Make the column names lower case..
names(observer.data) <- tolower(names(observer.data))
write.csv(observer.data,"D:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/Groundfish_observer_data_withgear.csv")



################ PULL IN THE GF OBSERVER LENGTH FREQUENCY DATA USING THE QUERY IN ISDB LENGTH FREQUENCIES.SQL
# Open an RODBC channel, using 32 bit so make sure you are running 32 bit R
chan <- dbConnect(dbDriver("Oracle"), un.ID, pwd.ID, "PTRAN", believeNRows=FALSE)

tmp <- NULL # define a temporary variable
# This is a slow and painful way to do this, but I was getting a big old error trying it how I did above, I believe due to
# it tripping on the quotation marks for the ID's (whereas above it was a number, here we have a character), so pulling multiples trips 
# was leading to R exploding.  This will do the trick just rather slow way to go about it since I'm querying the database 4900 times, yuk!
# But it only takes about 5 minutes tops so not terrible

for(i in 1:length(ids))
{
  #if(i > 1) count <- count+1000 # We are using the 1000 limit, so for each loop increase the index by 1000.
  #if(i < num.loop) id <- paste(ids[count:(count+999)],collapse = ",")
  #if(i == num.loop) id <- paste(ids[count:length(ids)],collapse = ",")
  # FK note: isfishlengths has full data for length frequencies taken during observed trips. isfish contains extra data from keen observers. USE ISFISHLENGTHS! 
  query <-   paste("SELECT t.trip, g.gearcd_id, s.set_no, p.setdate, p.settime, p.latitude, p.longitude, sc.common, t.tripcd_id, p.pntcd_id, ",
                   "l.fish_length FL_fish_length, l.num_at_length FL_num_at_length",
                   "FROM observer.iscatches ca, ",
                   "observer.issamples sa, ",
                   "observer.isfishsets s, ",
                   "observer.istrips t, ",
                   "observer.isspeciescodes sc, ",
                   "observer.issetprofile p, ",
                   "observer.isfishlengths l, ",
                   "observer.isgears g ",
                   "WHERE s.fishset_id  = ca.fishset_id ", #### catches to fishsets via fishset_id (top level)
                   "AND s.fishset_id = p.fishset_id ", #### setprofile to fishset via fishset_id
                   "AND ca.catch_id = sa.catch_id ", #### samples to catches via catch_id (level 2)
                   "AND sa.smpl_id = l.smpl_id ", #### fishlengths to samples via smpl_id (lowest link)
                   "AND g.gear_id = s.gear_id ", #### gears to sets via fishset_id 
                   "AND t.trip_id = g.trip_id ", #### gears to sets via fishset_id 
                   "AND ca.speccd_id = sc.speccd_id ", #### speciescodes to catches via speccd_id
                   "AND substr(s.nafarea_id,1,3) in ('5ZJ','5ZM') ",
                   "and t.trip in ",
                   "(",paste("'",ids[i],"'",sep=""),") ",
                   "AND p.setdate between to_date(20040101,'YYYYMMDD') and to_date(20161231,'YYYYMMDD') ",
                   "and p.pntcd_id = 2 ", 
                   "and sc.speccd_id in (10, 42) -- cod = 10, ytf = 42 ",
                   "order by s.SET_NO;")
  
  tmp[[i]] <- dbGetQuery(chan, query)
} # End the for loop

observerlength.data <- do.call("rbind",tmp)
# Make the column names lower case..
names(observerlength.data) <- tolower(names(observerlength.data))
write.csv(observerlength.data,"Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/Groundfish_observer_length_data.csv")

###### End section 1 Pulling the data###### End section 1 Pulling the data###### End section 1 Pulling the data###### End section 1 Pulling the data


############################  Section 2 QA/QC############################  Section 2 QA/QC############################  Section 2 QA/QC

# If we have done the above now we can simply read in the data and skip Section 1

observer.data <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/Groundfish_observer_data_withgear.csv",stringsAsFactors = F)
fishlengths_gf <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/Groundfish_observer_length_data.csv")
offshore.banks <- read.csv(paste(direct,"/Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""))
GB.bounds <- offshore.banks[offshore.banks$label == "SFA27",]

names(observer.data) <- tolower(names(observer.data))
names(fishlengths_gf) <- tolower(names(fishlengths_gf))

# First we need to convert the Lat and longs to decimal degrees.
observer.data$lat <- convert.dd.dddd(observer.data$latitude)
observer.data$lon <- -convert.dd.dddd(observer.data$longitude)

# Lets start by doing some basics on the groundfish data..
windows(11,11)
ScallopMap("SS",xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct="Y:/Offshore scallop/Assessment/" ,cex.mn=2,dec.deg = F)
points(observer.data$lon,observer.data$lat,pch=19,cex=0.2,col="blue")

# SO as a first filter on the data I suggest we remove everything that isn't on Georges Bank from both sets of data
# First make a dataframe of the points and remove all NA's from the data
obs.dat <- data.frame(EID = 1:nrow(observer.data),X = observer.data$lon,Y = observer.data$lat)
obs.dat <- na.omit(obs.dat)
# Now identified the rows (EID's) with data inside of Georges Bank
key <-findPolys(obs.dat, GB.bounds,maxRows=3e5)
# Now subset the observer data to exclude all data outside of GB.
observer.GB <- observer.data[1:nrow(observer.data) %in% key$EID,]

# Now we can plot these to make sure we did this correctly, which we did...
windows(11,11)
ScallopMap("SS",xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
points(observer.GB$lon,observer.GB$lat,pch=19,cex=0.2,col="blue")

# get the dates sorted out for the observer data
observer.GB$board_date <- strptime(observer.GB$board_date, format = "%Y-%m-%d")
observer.GB$landing_date <- strptime(observer.GB$landing_date, format = "%Y-%m-%d")
observer.GB$setdate <- strptime(observer.GB$setdate, format = "%Y-%m-%d")
observer.GB$settime <- stringr::str_pad(observer.GB$settime,4,pad="0")
observer.GB$settime <- strptime(paste(observer.GB$setdate,observer.GB$settime), format = "%Y-%m-%d %H%M")
observer.GB$year <- format(observer.GB$setdate, "%Y")

############################## Now lets do a simlar set of QA/QC checks on the Observer data ##########################
# Now some quick summaries of these data...
num.obs.trips <- length(unique(observer.GB$trip_id)) # so we have 2737 observed trips
# Question, Do we know if we can trust that the "yellowtail flounder" are all the yellowtial flounder
# any thoughts about Flatfish and Flounder Unidentifed or potential other columns that could include Yellowtails?
species.caught <- sort(unique(observer.GB$common)) 
num.species.caught <- length(species.caught) # records for 231 unique species, again we are only really interested in 2 of these species.


# Trying to get at the number of sets recorded, The log_efrt_std_info_id will for this.
#groundfish.GB$loc <- paste0(groundfish.GB$latitude,groundfish.GB$longitude)
num.obs.sets <- aggregate(set_no~trip, observer.GB,function(x) length(unique(x)))
names(num.obs.sets)[2] <- "Num_sets"
num.sets <- sum(num.obs.sets$Num_sets) # The 2737 trips have 38382 sets
#Question:  This range is larger than the range for the groundfish data, is that a concern at all?
range(num.obs.sets$Num_sets) # The number of sets per trip range between 1 and 75. 


# Look first at the estimated kept weight, this is for any species at the moment
obs.catch.per.set <- aggregate(cbind(est_kept_wt,est_num_caught)~trip+set_no, observer.GB,sum) # Question:  How many tonnes of "kept" is max for a set?
obs.catch.per.set$trip <- as.factor(obs.catch.per.set$trip)
# This is a pretty aggresive boxplot, so just plot the data as points to look for analonomies...
windows(11,11)
p <- ggplot(obs.catch.per.set, aes(trip,est_kept_wt)) + geom_point() #Question  How much is too much landings for a groundfish set (7,000 kg???)
p
# Now look at the number caught
p <- ggplot(obs.catch.per.set, aes(trip,est_num_caught)) + geom_point() # Question: How many is too many for a set
p
# Question, what is difference between rnd weight and rpt weight
# Question, what is the proration ratio?
range(obs.catch.per.set$est_num_caught) # Lower end seems reasonable...
range(obs.catch.per.set$est_kept_wt) # Lower end seems reasonable...

# Question could we use the observer information to get an estimate of effort, 
# number of hooks x amount of time gear was in the water or something like that?

# How about our dates, do they all look reasonable?  
#Question, For the outliers any idea why they might exists outside key punch errors? This assumes that trip_id would be a sequential number, 
# which seems to be roughly what we are seeing.
windows(11,11)
p <- ggplot(observer.GB, aes(settime,trip_id)) + geom_point() + facet_wrap(~year, scales="free")
p

# Now what if we just look at the observer data for cod and yellowtail...
observer.GB.yt.cod <- observer.GB[observer.GB$speccd_id %in% c(10,42),]

length(unique(observer.GB.yt.cod$settime)) # So there are 34139 unique sets in which cod or yellowtail were caught.

# Question, is 15 tonnes of cod in a set reasonable, and was 7 tonnes of YT reasonable back in the early days
windows(11,11)
p <- ggplot(observer.GB.yt.cod, aes(settime,est_kept_wt)) + geom_point() + facet_wrap(~common) 
p


##### Spatial/temporal checks are done. Now we'll look at source code
observer.GB <- observer.GB[,-1]

## check the number of trips
length(unique(observer.GB$trip)) # 2737
length(unique(fishlengths_gf$trip)) # 2544
## we have 2737 observed gf trips, and 2544 gf trips with fish lengths

# check source 0/1. If source = 1, then we need to check these sets in the OTIS reports
unique(as.character(observer.GB$source))

################# fixing source = 0/1
source0 <- observer.GB[observer.GB$source ==0,]
source0_bycatch_count <- aggregate(speccd_id ~ trip + set_no, data=source0, function(x) length(unique(x)))
source1 <- observer.GB[observer.GB$source ==1,]
source1_bycatch_count <- aggregate(speccd_id ~ trip + set_no, data=source1, function(x) length(unique(x)))
source1_bycatch_count_multi <- source1_bycatch_count[source1_bycatch_count$speccd_id >1,]

# take a look:
require(ggplot2)
ggplot() + geom_histogram(data=source1_bycatch_count, aes(speccd_id), binwidth=1) # most source 1 trips have less than 6 species
summary(source1_bycatch_count$speccd_id) # # most source 1 trips have less than 6 species
ggplot() + geom_histogram(data=source0_bycatch_count, aes(speccd_id), binwidth=1) # most source 0 trips have less than 6 species
summary(source0_bycatch_count$speccd_id) # # most source 0 trips have less than 6 species

# looking at some random examples: 
# J13-0536, J13-0050, J13-0254
unique(observer.GB[observer.GB$trip=="J13-0536",]$source)
unique(observer.GB[observer.GB$trip=="J13-0536",]$common)
unique(observer.GB[observer.GB$trip=="J13-0050",]$source)
unique(observer.GB[observer.GB$trip=="J13-0050",]$common)
unique(observer.GB[observer.GB$trip=="J13-0254",]$source)
unique(observer.GB[observer.GB$trip=="J13-0254",]$common)
# so it seems like some trips just label all sets as unobserved (1) even though all sets were actually observed (0)

source_count <- aggregate(source ~ trip , data=observer.GB, function(x) length(unique(x)))
table(source_count$source)
# so it seems like MOST trips just label all sets as unobserved (1)
# so I think that if a trip has all sets as "unobserved", then we will replace the 1 with 0 (since they were likely all observed)
# but if a trip has some observed sets (0) and some unobserved sets, then we will trust those trips and leave them alone. 
# this means that we can't use the same source check process as we did for scallop. 
# this make sense as well since the SABS GF group explained that it IS possible for all sets to be observed on a GF trip, and that this commonly happens. 

all_sets_same <- observer.GB[observer.GB$trip %in% source_count[source_count$source==1,]$trip,] # 186214 rows, 2664 trips
# source_count_test <- aggregate(source ~ trip , data=all_sets_same, function(x) length(unique(x)))
# table(source_count_test$source)
all_sets_0 <- all_sets_same[all_sets_same$source==0,] # 184777 rows, 2646 trips
all_sets_1 <- all_sets_same[all_sets_same$source==1,] # 1437 rows, 18 trips

# so if all sets are 0, leave them as is. 
# if all sets are 1, switch them to 0.
all_sets_1$source <- 0
sets_mixed <- observer.GB[observer.GB$trip %in% source_count[source_count$source==2,]$trip,] # 5021 rows, 73 trips
# 5021 + 184777 + 1437 = 191235 # OK!
# 73 + 18 + 2646 = 2737 

source_fixed <- rbind(sets_mixed, all_sets_0, all_sets_1)
# > dim(source_fixed)
# [1] 191235     27 #OK!

table(source_fixed$source)
table(observer.GB$source)
# this looks better. # 99.72% of sets are source 0

source_fixed$tripset <- paste0(source_fixed$trip, "_", source_fixed$set_no)
observer.GB$tripset <- paste0(observer.GB$trip, "_", observer.GB$set_no)

gf.observer.final <- source_fixed
write.csv(gf.observer.final, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/gf.obs.trips.sets.species.csv")
###############################################
### THIS IS OUR MASTER CLEAN TRIP SET WITH SPECIES, OBSERVED ONLY. SPATIALLY FILTERED VIA GB POLYGON

# remove any sets with missing lats or longs
summary(gf.observer.final$latitude)
summary(gf.observer.final$longitude)

length(gf.observer.final[is.na(gf.observer.final$latitude)|is.na(gf.observer.final$longitude),]$trip)

# what about comarea_id for split trips or other wonky coords?
# there are no comarea_id's listed

# take a look at everything
ggplot() + geom_point(data=gf.observer.final, aes(lon,lat)) + 
  theme_bw() + theme(panel.grid=element_blank())

length(unique(gf.observer.final$tripset))
# 38382 tripsets

# these are our final GB trip sets shown spatially:
ggplot() + geom_point(data=gf.observer.final, aes(lon, lat)) + 
  theme_bw() + theme(panel.grid=element_blank())

#### MAKE SURE NUM_HOOK_HAUL IS HERE!! 
require(dplyr)
gf.obs.trips.sets <- unique(select(gf.observer.final, tripset, trip, set_no, vessel_name, latitude, longitude,
                                   setdate, settime, num_hook_haul, source, pntcd_id, comarea_id))

# write the full dataset (trips, sets, species). contains 38382 rows
write.csv(gf.obs.trips.sets, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/MASTER_CLEAN_gf.obs.trips.sets.csv")

# write the trip-set dataset (no species). contains 191235 rows
write.csv(gf.observer.final, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/MASTER_CLEAN_gf.obs.trips.sets.species.csv")
####################################################################
######### Master cleaned tripset list is gf.obs.trips.sets (saved as MASTER_CLEAN_gf.obs.trips.sets.csv above)
######### Master cleaned tripset species list is gf.observer.final (saved as MASTER_CLEAN_gf.obs.trips.sets.species.csv above)
####################################################################

# read in the clean full dataset if not already in environment:
gf.observer.final <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/MASTER_CLEAN_gf.obs.trips.sets.species.csv", stringsAsFactors = F, header=T)
if(dim(gf.observer.final)[2]==29) {
  gf.observer.final <- gf.observer.final[,2:29]
}
# get rid of silly row index column
gf.observer.final <- dplyr::select(gf.observer.final, -x)

# read in the clean trip-sets dataset if not already in environment:
gf.obs.trips.sets <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/MASTER_CLEAN_gf.obs.trips.sets.csv", stringsAsFactors = F, header=T)
if(dim(gf.obs.trips.sets)[2]==13) {
  gf.obs.trips.sets <- gf.obs.trips.sets[,2:13]
}

### now we're subsetting this final full dataset for each species, so that we have tripset rows where cod and ytf were caught
gf.observer.cod <- gf.observer.final[gf.observer.final$common=="COD(ATLANTIC)",] # 34356 rows
gf.observer.ytf <- gf.observer.final[gf.observer.final$common=="YELLOWTAIL FLOUNDER",] # 5177 rows

### plot spatially for fun
ggplot() + geom_point(data=gf.observer.cod, aes(lon,lat)) + 
  theme_bw() + theme(panel.grid=element_blank()) + ggtitle("cod sets")
ggplot() + geom_point(data=gf.observer.ytf, aes(lon,lat)) + 
  theme_bw() + theme(panel.grid=element_blank()) + ggtitle("ytf sets")

### check kept/discarded
summary(gf.observer.cod$est_kept_wt) # may contain values since gf trips may keep cod
summary(gf.observer.cod$est_discard_wt) # may contain values since gf trips may keep cod
ggplot() + geom_point(data = gf.observer.cod, aes(trip, est_kept_wt)) + theme_bw() + theme(panel.grid=element_blank())
ggplot() + geom_point(data = gf.observer.cod, aes(trip, est_discard_wt)) + theme_bw() + theme(panel.grid=element_blank())

summary(gf.observer.ytf$est_kept_wt) # should be mostly NA's, except in one year?
unique(gf.observer.ytf[is.na(gf.observer.ytf$est_kept_wt)==FALSE,]$trip) # why are so many ytf kept? 
summary(gf.observer.ytf$est_discard_wt) # contains mostly NA's?? 
ggplot() + geom_point(data = gf.observer.ytf, aes(trip, est_kept_wt)) + theme_bw() + theme(panel.grid=element_blank())
ggplot() + geom_point(data = gf.observer.ytf, aes(trip, est_discard_wt)) + theme_bw() + theme(panel.grid=element_blank())

length(unique(gf.observer.ytf[is.na(gf.observer.ytf$est_kept_wt)==FALSE,]$tripset))
length(unique(gf.observer.ytf[is.na(gf.observer.ytf$est_kept_wt)==TRUE,]$tripset))

# sum the est_kept_wt and est_discard_wt since bycatch MUST be kept on groundfish trips
gf.observer.cod[is.na(gf.observer.cod$est_discard_wt)=="TRUE",]$est_discard_wt <- 0
gf.observer.cod[is.na(gf.observer.cod$est_kept_wt)=="TRUE",]$est_kept_wt <- 0
gf.observer.cod$est_bycatch_wt <- gf.observer.cod$est_kept_wt + gf.observer.cod$est_discard_wt
gf.observer.ytf[is.na(gf.observer.ytf$est_discard_wt)=="TRUE",]$est_discard_wt <- 0
gf.observer.ytf[is.na(gf.observer.ytf$est_kept_wt)=="TRUE",]$est_kept_wt <- 0
gf.observer.ytf$est_bycatch_wt <- gf.observer.ytf$est_kept_wt + gf.observer.ytf$est_discard_wt

# remove sets with source = 1.
gf.observer.cod <- gf.observer.cod[gf.observer.cod$source==0,]
gf.observer.ytf <- gf.observer.ytf[gf.observer.ytf$source==0,]

write.csv(gf.observer.ytf, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/gf.observer.ytf.csv")
write.csv(gf.observer.cod, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/gf.observer.cod.csv")

gf.observer.ytf  <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/gf.observer.ytf.csv", header=T, stringsAsFactors = F)
gf.observer.cod  <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/gf.observer.cod.csv", header=T, stringsAsFactors = F)

if(dim(gf.observer.ytf)[2]==29) {
  gf.observer.ytf <- gf.observer.ytf[,2:29]
}
if(dim(gf.observer.cod)[2]==29) {
  gf.observer.cod <- gf.observer.cod[,2:29]
}

gf.obs.trips.sets <- gf.obs.trips.sets[gf.obs.trips.sets$source==0,]

# now we need to left-join the trip-sets to our ytf and cod dataframe so that we have 2 dataframes, each with one species' data for every single observed set (even if not detected)
gf.observer.cod <- join(gf.obs.trips.sets, gf.observer.cod[,c(27, 4, 11, 12, 23, 28)], type="left") # should have 38241 rows. this includes sets where cod=0
gf.observer.ytf <- join(gf.obs.trips.sets, gf.observer.ytf[,c(27, 4, 11, 12, 23, 28)], type="left") # should have 38241 rows. this includes sets where ytf=0

### next, onto the length frequency tidy up
### we're going to join these in wide format to the cod and ytf separately.
head(fishlengths_gf)

# check out the length bins
fishlengths_gf$tripset <- paste0(fishlengths_gf$trip, "_", fishlengths_gf$set_no)
fishlengths_gf <- fishlengths_gf[,-1]

# pdf("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length distribution_cod.pdf", height=5, width=7)
# ggplot() + geom_point(data=fishlengths_gf[fishlengths_gf$common=="COD(ATLANTIC)",], aes(fl_fish_length, fl_num_at_length)) +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   ggtitle("COD") + xlab("Fish length bin") + ylab("number at length")
# # ggplot() + geom_point(data=fishlengths_gf[fishlengths_gf$common=="COD(ATLANTIC)" & year(ymd(fishlengths_gf$setdate)) == 2006,], aes(fl_fish_length, fl_num_at_length)) + 
# #   theme_bw() + theme(panel.grid=element_blank()) + 
# #   ggtitle("COD - 2006") + xlab("Fish length bin") + ylab("number at length")
# # ggplot() + geom_point(data=fishlengths_gf[fishlengths_gf$common=="COD(ATLANTIC)" & year(ymd(fishlengths_gf$setdate)) == 2015,], aes(fl_fish_length, fl_num_at_length)) +
# #   theme_bw() + theme(panel.grid=element_blank()) +
# #   ggtitle("COD - 2015") + xlab("Fish length bin") + ylab("number at length")
# dev.off()
# 
# ytf_check <- fishlengths_gf[fishlengths_gf$common=="YELLOWTAIL FLOUNDER",]
# ytf_check$meanlength <- ytf_check$fl_fish_length * ytf_check$fl_num_at_length
# require(lubridate)
# ytf_check$year <- year(ymd_hms(ytf_check$setdate))
# ytf_means <- ddply(.data=ytf_check, .(year),
#                    summarize,
#                    annualmean = (sum(meanlength)/sum(fl_num_at_length)))
# ytf_means$sp <- "ytf"
# cod_check <- fishlengths_gf[fishlengths_gf$common=="COD(ATLANTIC)",]
# cod_check$meanlength <- cod_check$fl_fish_length * cod_check$fl_num_at_length
# cod_check$year <- year(ymd_hms(cod_check$setdate))
# 
# sp_lengths <- data.frame(lengths = c(rep(cod_check$fl_fish_length, cod_check$fl_num_at_length),
#                                      rep(ytf_check$fl_fish_length, ytf_check$fl_num_at_length)),
#                          years = c(rep(cod_check$year, cod_check$fl_num_at_length),
#                                    rep(ytf_check$year, ytf_check$fl_num_at_length)),
#                          sp = c(rep("cod", sum(cod_check$fl_num_at_length)),
#                                 rep("ytf", sum(ytf_check$fl_num_at_length))))
# 
# pdf("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_timeseries.pdf", onefile=T, width=8, height=5)
# ggplot() + geom_boxplot(data=sp_lengths, aes(x=years, y=lengths, group=years)) +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   facet_wrap(~sp)
# ggplot() + geom_point(data=sp_lengths, aes(x=years, y=lengths, colour=sp)) + 
#   geom_smooth(data=sp_lengths, aes(x=years, y=lengths, colour=sp), method="lm") +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   facet_wrap(~sp)
# dev.off()
# 
# pdf("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length distribution_ytf.pdf", height=5, width=7)
# ggplot() + geom_point(data=fishlengths_gf[fishlengths_gf$common=="YELLOWTAIL FLOUNDER",], aes(fl_fish_length, fl_num_at_length)) + 
#   theme_bw() + theme(panel.grid=element_blank()) +
#   ggtitle("YELLOWTAIL FLOUNDER") + xlab("Fish length bin") + ylab("number at length")
# dev.off()

#### sought feedback on lengths from Irene (on cod) and Heath (on ytf)
#### heath: ytf > 55 are suspect. Need to check datasheets
#### irene: cod > 100 are suspect in recent years. So for scallop, all ok, but groundfish need to double check.
#### removing all bins above these lengths at the end. 

# are the bins static? I.e. is each bin the same size for each species?
# diff(sort(unique(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="COD(ATLANTIC)"]))) 
# diff(sort(unique(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="YELLOWTAIL FLOUNDER"]))) 
# unfortunately, the bins are totally random. I think we'll just use bins of 1 cm for cod (since there are no 0.5's), and 0.5 for ytf, since they are much smaller in size 

# there are 0.5 size bins for YTF. Check these out...
# sort(unique(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="YELLOWTAIL FLOUNDER"]))
# fishlengths_gf[fishlengths_gf$common=="YELLOWTAIL FLOUNDER" & 
#                  fishlengths_gf$fl_fish_length %in% c("32.5", "33.5", "34.5", "35.5", "36.5", 
#                                                       "37.5", "41.5"),]
# check tripsets J04-0146_12 and J04-0187_8 eventually. In the meantime, let's examine these sets, and deal with them manually
# fishlengths_gf[fishlengths_gf$tripset=="J04-0146_12",]
# let's round down and aggregate later
fishlengths_gf[fishlengths_gf$tripset=="J04-0146_12",]$fl_fish_length <- floor(fishlengths_gf[fishlengths_gf$tripset=="J04-0146_12",]$fl_fish_length)
# fishlengths_gf[fishlengths_gf$tripset=="J04-0187_8",]
# let's round down and aggregate later. 
fishlengths_gf[fishlengths_gf$tripset=="J04-0187_8",]$fl_fish_length <- floor(fishlengths_gf[fishlengths_gf$tripset=="J04-0187_8",]$fl_fish_length)

# while were on this, let's look to see if other length bins have multiple records per trip (as happened with some scallop sets)
# this next one shows the number of records for each bin/set
# multi <- ddply(.data=fishlengths_gf, .(tripset, common, fl_fish_length),
#                summarize,
#                nrecords = length(fl_num_at_length))
# multi_len <- subset(multi, nrecords > 1)
# 
# # whereas this one shows the number of DIFFERENT records for each bin/set
# multi_num <- ddply(.data=fishlengths_gf, .(tripset, common, fl_fish_length),
#                    summarize,
#                    nrecords = length(unique(fl_num_at_length)))
# multi_num_sum <- subset(multi_num, nrecords > 1)

##### check the following trip datasheets:
# J16-0071_10 and J16-0071_11 # CONFIRMED. COD SEPARATED BY SEX
# J12-0559_30 - need to dig up hard copy
# J12-0512_5 - need to dig up hard copy

# table(multi[multi$common=="YELLOWTAIL FLOUNDER",]$nrecords)
# table(multi[multi$common=="COD(ATLANTIC)",]$nrecords)
# 
# table(multi_num[multi_num$common=="YELLOWTAIL FLOUNDER",]$nrecords)
# table(multi_num[multi_num$common=="COD(ATLANTIC)",]$nrecords)
# 
# length(unique(multi[multi$common=="YELLOWTAIL FLOUNDER",]$tripset)) == length(unique(fishlengths_gf$tripset[fishlengths_gf$common=="YELLOWTAIL FLOUNDER"]))
# length(unique(multi[multi$common=="COD(ATLANTIC)",]$tripset)) == length(unique(fishlengths_gf$tripset[fishlengths_gf$common=="COD(ATLANTIC)"]))
# 
# length(unique(multi[multi$common=="YELLOWTAIL FLOUNDER",]$fl_fish_length)) == length(unique(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="YELLOWTAIL FLOUNDER"]))
# length(unique(multi[multi$common=="COD(ATLANTIC)",]$fl_fish_length)) == length(unique(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="COD(ATLANTIC)"]))

########### WE ARE ASSUMING THAT FOR CASES WITH 2 RECORDS FOR ONE TRIPSET BIN, THE OBSERVER RECORDED THE LENGTHS BY SEX (AS OCCURRED IN SCALLOP OCCASIONALLY)

# we need to add extra rows so that we will have empty bins as well. 
fishlength_bins_cod <- expand.grid(tripset = unique(gf.observer.cod$tripset), fl_fish_length = seq(min(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="COD(ATLANTIC)"]), max(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="COD(ATLANTIC)"]), by=1))
fishlength_bins_ytf <- expand.grid(tripset = unique(gf.observer.ytf$tripset), fl_fish_length = seq(min(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="YELLOWTAIL FLOUNDER"]), max(fishlengths_gf$fl_fish_length[fishlengths_gf$common=="YELLOWTAIL FLOUNDER"]), by=1))

fishlength_bins_cod$tripset <- as.character(fishlength_bins_cod$tripset)
fishlength_bins_ytf$tripset <- as.character(fishlength_bins_ytf$tripset)

# fishlength_bins_cod rows = 4359018 ; fishlength_bins_ytf rows = 1682428

# rows should equal rows of gf.observer.cod * n bins cod, OR same for ytf
# 38237 * 114 = 4359018 for cod
# 38237 * 44 = 1682428 for YTF

# now join the tripsets to the lengthbins (in long)
fishlength_bins_cod <- join(gf.observer.cod, fishlength_bins_cod, type="left") # 4359018 x 17
fishlength_bins_ytf <- join(gf.observer.ytf, fishlength_bins_ytf, type="left") # 1682428 x 17

# join the actual length data to the lengthbins (in long)
fishlength_bins_cod <- join(fishlength_bins_cod, fishlengths_gf[fishlengths_gf$common =="COD(ATLANTIC)",c("tripset", "fl_fish_length", "fl_num_at_length")], type="left") 
fishlength_bins_ytf <- join(fishlength_bins_ytf, fishlengths_gf[fishlengths_gf$common =="YELLOWTAIL FLOUNDER",c("tripset", "fl_fish_length", "fl_num_at_length")], type="left")
# row numbers increase because of bins broken out by sex 

# dim(fishlength_bins_cod)
# [1] 4359029      18
# dim(fishlength_bins_ytf)
# [1] 1682934      18

# empty the bins that are above heath and irene's advice. still need to doublecheck the datasheets though
#### heath: ytf > 55 are suspect. Need to check datasheets
#### irene: cod > 100 are suspect in recent years. So for scallop, all ok, but groundfish need to double check.
fishlength_bins_cod <- fishlength_bins_cod[fishlength_bins_cod$fl_fish_length < 101,]
fishlength_bins_ytf <- fishlength_bins_ytf[fishlength_bins_ytf$fl_fish_length < 56,]

# dim(fishlength_bins_cod)
# 3058971      18
# dim(fishlength_bins_ytf)
# 1300564      18

# aggregate to clean up those sex-separated bins
fishlength_bins_cod[is.na(fishlength_bins_cod$fl_num_at_length),]$fl_num_at_length <-0
fishlength_bins_ytf[is.na(fishlength_bins_ytf$fl_num_at_length),]$fl_num_at_length <-0

length_long_cod <- aggregate(data=fishlength_bins_cod, fl_num_at_length ~ fl_fish_length + tripset, function(x) sum(x))
length_long_ytf<- aggregate(data=fishlength_bins_ytf, fl_num_at_length ~ fl_fish_length + tripset, function(x) sum(x))

# rejoin the rest of the data
length_long_cod <- join(length_long_cod, unique(fishlength_bins_cod[,1:16]), type="left")
length_long_ytf <- join(length_long_ytf, unique(fishlength_bins_ytf[,1:16]), type="left")

length_long_cod <- length_long_cod[,c(names(fishlength_bins_cod[,1:16]), "fl_fish_length", "fl_num_at_length")]
length_long_ytf <- length_long_ytf[,c(names(fishlength_bins_ytf[,1:16]), "fl_fish_length", "fl_num_at_length")]

# dim(length_long_cod)
# 3058960      18
# dim(length_long_ytf)
# 1300058      18

write.csv(length_long_cod, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_long_cod_FINAL_FULL.csv")
write.csv(length_long_ytf, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_long_ytf_FINAL_FULL.csv")

## checking out num_hook_haul
table(length_long_cod[length_long_cod$fl_fish_length==21,]$num_hook_haul)


# relabel bins before reshaping
fishlength_bins_cod$fl_fish_length <- stringr::str_pad(fishlength_bins_cod$fl_fish_length, 3, "left", pad="0")
fishlength_bins_ytf$fl_fish_length <- stringr::str_pad(fishlength_bins_ytf$fl_fish_length, 3, "left", pad="0")
fishlength_bins_cod$fl_fish_length <- paste0("bin_", fishlength_bins_cod$fl_fish_length)
fishlength_bins_ytf$fl_fish_length <- paste0("bin_", fishlength_bins_ytf$fl_fish_length)

# OK - proceeding with reshaping, with sum aggregation within each bin.
fishlength_bins_cod_sub <- fishlength_bins_cod[,c(1,17,18)]

fishlength_bins_ytf_sub <- fishlength_bins_ytf[,c(1,17,18)]

require(reshape2)
cod_length_wide <- dcast(fishlength_bins_cod_sub, tripset ~ fl_fish_length, sum)
ytf_length_wide <- dcast(fishlength_bins_ytf_sub, tripset ~ fl_fish_length, sum)

######## LENGTH FREQUENCIES FOR COD AND YTF ###########
write.csv(cod_length_wide, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_cod_FINAL.csv")
write.csv(ytf_length_wide, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_ytf_FINAL.csv")

### join the other metadata back in
cod_length_wide_full <- join(cod_length_wide, gf.observer.cod, type="left") 
ytf_length_wide_full <- join(ytf_length_wide, gf.observer.ytf, type="left") 

### rearrange columns
cod_length_wide_full <- cod_length_wide_full[,c('trip', 'set_no', 'tripset', 'vessel_name', 'latitude', 'longitude', 
                                                'setdate','settime', 'source', 'pntcd_id', 'comarea_id', 'num_hook_haul', 
                                                'speccd_id', 'gear', 'common','est_bycatch_wt',
                                                colnames(cod_length_wide_full[,c(2:81)]))]

ytf_length_wide_full <- ytf_length_wide_full[,c('trip', 'set_no', 'tripset', 'vessel_name', 'latitude', 'longitude', 'setdate',
                                                'settime', 'source', 'pntcd_id', 'comarea_id', 'num_hook_haul', 'speccd_id', 'gear',
                                                'common', 'est_bycatch_wt', 
                                                colnames(ytf_length_wide_full[2:35]))]
# tail(ytf_length_wide_full)

# sort rows by trip and set
cod_length_wide_full <- arrange(cod_length_wide_full, trip, set_no)
ytf_length_wide_full <- arrange(ytf_length_wide_full, trip, set_no)

######## LENGTH FREQUENCIES FOR COD AND YTF WITH METADATA ###########
write.csv(cod_length_wide_full, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_cod_FULL_FINAL.csv")
write.csv(ytf_length_wide_full, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_ytf_FULL_FINAL.csv")

######## fixing coords
cod_length_wide_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_cod_FULL_FINAL.csv")
ytf_length_wide_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_ytf_FULL_FINAL.csv")

names(cod_length_wide_full)



######### Doing some data summaries on these data..
#cod_length_wide_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_cod_FULL_FINAL.csv")
#ytf_length_wide_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_wide_ytf_FULL_FINAL.csv")
#ytf_length_long_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_long_ytf_FINAL_FULL.csv")
#cod_length_long_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Groundfish/length_long_cod_FINAL_FULL.csv")
#
library(lubridate)
summary(cod_length_wide_full$set_no)
summary(cod_length_wide_full$lat)
summary(cod_length_wide_full$lon)
summary(as.Date(cod_length_wide_full$setdate))
summary(as.Date(cod_length_wide_full$settime))
summary(cod_length_wide_full$source)
summary(cod_length_wide_full$pntcd_id)
unique(cod_length_wide_full$comarea_id)
summary(cod_length_wide_full$num_hook_haul,na.rm=T)
unique(ytf_length_wide_full$speccd_id)
unique(cod_length_wide_full$common)

ytf_length_wide_full$est_bycatch_wt[is.na(ytf_length_wide_full$est_bycatch_wt)] <- 0
cod_length_wide_full$est_bycatch_wt[is.na(cod_length_wide_full$est_bycatch_wt)] <- 0
range(cod_length_wide_full[,grep("bin",names(cod_length_wide_full))])

summary(cod_length_wide_full$est_bycatch_wt)
summary(ytf_length_wide_full$est_bycatch_wt)

summary(ytf_length_long_full$fl_fish_length)
summary(ytf_length_long_full$fl_num_at_length)
summary(cod_length_long_full$fl_fish_length)
summary(cod_length_long_full$fl_num_at_length)

dim(cod_length_wide_full)
dim(ytf_length_wide_full)
cod_length_wide_full$month <- month(cod_length_wide_full$setdate)
cod_length_wide_full$year <- year(cod_length_wide_full$setdate)

sets.per.month <- aggregate(tripset~month,cod_length_wide_full,FUN = function(x) mean(length(x)))
sets.per.year <- aggregate(tripset~year,cod_length_wide_full,FUN = function(x) length(x))

length(cod_length_wide_full[is.na(cod_length_wide_full$num_hook_haul),]$num_hook_haul) # 31338
length(cod_length_wide_full[!is.na(cod_length_wide_full$num_hook_haul),]$num_hook_haul) # 6899
length(cod_length_wide_full$num_hook_haul) # 38237
#31338 + 6899

length(ytf_length_wide_full[is.na(ytf_length_wide_full$num_hook_haul),]$num_hook_haul) # 31338
length(ytf_length_wide_full[!is.na(ytf_length_wide_full$num_hook_haul),]$num_hook_haul) # 6899
length(ytf_length_wide_full$num_hook_haul) # 38237
#31338 + 6899

table(ytf_length_wide_full[is.na(ytf_length_wide_full$num_hook_haul),]$gear)
