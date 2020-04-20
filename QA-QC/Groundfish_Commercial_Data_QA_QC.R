---
title: "Groundfish commercial data - Import and QA/QC"
---
## DK Oct 6th, 2017. This is a wacky script to pull in the groundfishery data by set for any trips that were identified as
## being either MG (Mobile Gear) or FG (Fixed Gear).  We then go on and do some basic QA/QC and summaries of the data to see what we actually have...
# We use Round weight not reported weighted

# Question:  What are the differnces between  the fixed gear and mobile gear fleets?
# Question:  Do we need to look at the fleets seperately
# Question:  Have these fleets changed fishing practices  over the years?
# Question:  Have these fleets had changes to license/reporting conditions over the years?
# Question:  What is the marfissci.pro_spc_info, is there another similar table, what are the differences with these.
# Question:  How likely is it that we are missing data given they don't report null sets and we are pulling all species caught
# Question:  
## By DK, with loads of help from JS, and IA (SABS).

direct <- "d:/local_backup/r/"
direct <- "Y:/Offshore scallop/Assessment/"
require(ROracle)
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) #Source3 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source3 



######################  Log effort discussion with Irene #########################
# There are 3 table, the easiest of those table is LOG_EFFORT.  There also are these 2 tables which seem to have a bunch of the data LOG_EFRT_ENTRD_DETS and LOG_EFRT_STD_INFO
# That we need.  
# LOG EFRT STD INFO is moderately clean table, LOG EFRT ENTRD_DETS is basically the raw data entered into the database and is the hardest to deal with, the Column_def_id in 
# this table makes is complex as there are over 700 + different entries for effort.  THe Log_efrt_std_info uses this info and works if someone used the correct
# Column_def_id.  Looks like most of the trawl data is in log_efrt_std_info (so gear code).  The problem is the longlines and likely the gillnets 
# log efrt std info also doesn't have trip id, so would have to link through log_efrt_std_info which we have in our call to get the catch information
#  Will have to pull out the FV_Duration_In_HOURS field as well.
# Example is
Select * from marfissci.log_efrt_std_info where flog_efrt_std_info_id = 51627
# One minor issue is the values may be slightly different, but good to at least 1 decimal place.
# in the log efrt entrd data table the values are also rounded even more.
# Column Definitions is COLUMN_DEFNS in MARFIS it has some all the crazy codes in the log efrt entrd 
# We won't be able to get effort for long-lines and gill nets but we will need to go into the log efrt entrd dets table to pull these out and it won't be straightforward
# at all.

#################  Section 1 Pulling the data#################  Section 1 Pulling the data#################  Section 1 Pulling the data
# FK: on Oct 18 at SABS, JS/DK/IA found that we were missing catch data due to joins with bad effort data. New procedure is to read in 
# the catch CSV, and 3 effort CSVs separately (all pulled in SQLDeveloper using Y:\Projects\GB_time_area_closure_SPERA\Scripts\sql\MARFISSSCI_groundfish_trips_by_year.sql), 
# clean/QA/QC them separately, and then join together. This all occurs after the commented out section below.

# # Bring in the trip numbers
# all.ids <- read.csv("D:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Groundfish_trip_IDs_2002_2016.csv",header=F)
# # Remove any duplicate trip ID (occur when fishing in multiple banks)
# ids <- unique(all.ids$V1)
# # Because of SQL limitations we can only run these 1000 at a time, so we need to run a loop to get at all the data
# num.loop <- ceiling(length(ids)/1000)
# 
# # Open an RODBC channel, using 32 bit so make sure you are running 32 bit R
# chan <- odbcConnect("PTRAN",uid=un.ID,pwd=pwd.ID,believeNRows=FALSE)
# # The query to grab log data
# 
# # We have the limitiation of the lists only being 1000 units long, so I'm looping through each list 1000 at a time and combining everythign at the end.
# tmp <- NULL # define a temporary variable
# count <- 1 # I need a counter to make sure we are pulling the correct data from the ids.
# for(i in 1:num.loop)
# {
# if(i > 1) count <- count+1000 # We are using the 1000 limit, so for each loop increase the index by 1000.
# if(i < num.loop) id <- paste(ids[count:(count+999)],collapse = ",")
# if(i == num.loop) id <- paste(ids[count:length(ids)],collapse = ",")
# query <- paste("select a.SPECIES_CODE,  a.RND_WEIGHT_KGS, a.RPT_WEIGHT_KGS,  b.EFFORT_AMOUNT, a.DATE_FISHED,",  
#                "a.LATITUDE,  a.LONGITUDE, a.TRIP_ID, ",
#                "a.LANDED_FORM_CODE, a.FISHING_AREA_ID, a.GEAR_CODE,", 
#                "a.PRORATION_RATIO, a.LOG_EFRT_STD_INFO_ID, ",
#                "b.EFFORT_MEASURE_ID from marfissci.pro_spc_info a, marfissci.log_effort b where a.TRIP_ID IN  ",
#                " (select distinct trip_id from  (",
#                  "SELECT a.vr_number_fishing,a.trip_id,",
#                "B.AREA,TO_CHAR(a.LANDED_DATE,'YYYY')Year,",
#                "TO_CHAR(a.LANDED_DATE,'MM')month,",
#                "TO_CHAR(a.LANDED_DATE,'DD')day,",
#                "a.gear_code,c.gross_tonnage,",
#                "sum(a.rnd_weight_kgs)species_wt -- This pulls in all the weights for the species codes below",
#                "FROM MARFISsci.PRO_SPC_INFO A,MARFISsci.AREAS B,marfissci.vessels c",
#                "where a.vr_number_fishing=c.vr_number",
#                "and A.NAFO_UNIT_AREA_ID=B.AREA_ID",  
#                "and TO_CHAR(a.LANDED_DATE,'YYYY') IN (2016)",
#                "and a.species_code in(100)",
#                "and b.area in('5ZEJ','5ZEM','5ZEU')",
#                "GROUP BY TO_CHAR(a.LANDED_DATE,'MM'),",
#                "a.vr_number_fishing,a.trip_id,",
#                "a.gear_code,c.gross_tonnage,",
#                "TO_CHAR(a.LANDED_DATE,'YYYY'),",
#                "TO_CHAR(a.LANDED_DATE,'DD'),",
#                "b.area)) and a.LOG_EFRT_STD_INFO_ID=b.LOG_EFRT_STD_INFO_ID")
# 
# tmp[[i]] <- sqlQuery(chan, query)
# } # End the for loop
# 
# ground.fish.by.set <- do.call("rbind",tmp)
# # Make the column names lower case..
# names(ground.fish.by.set) <- tolower(names(ground.fish.by.set))
# 
# write.csv(ground.fish.by.set,"D:/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Groundfish_catch_and_effort_by_set.csv")


############################  Section 2 QA/QC############################  Section 2 QA/QC############################  Section 2 QA/QC

# If we have done the above now we can simply read in the data and skip Section 1 forever

ground.fish.by.set <- read.csv("D:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/GFCommCatchData2003to2016.csv",stringsAsFactors = F)
offshore.banks <- read.csv(paste(direct,"/Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""))
GB.bounds <- offshore.banks[offshore.banks$label == "SFA27",]

names(ground.fish.by.set) <- tolower(names(ground.fish.by.set))
names(ground.fish.logeff.by.set) <- tolower(names(ground.fish.logeff.by.set))

# First we need to convert the Lat and longs to decimal degrees.
ground.fish.by.set$lat <- convert.dd.dddd(ground.fish.by.set$latitude)
ground.fish.by.set$lon <- -convert.dd.dddd(ground.fish.by.set$longitude)

# Tidy up the dates.
ground.fish.by.set$date_fished <- strptime(ground.fish.by.set$date_fished, "%y-%m-%d")
ground.fish.by.set$month <- month(ground.fish.by.set$date_fished)
ground.fish.by.set$year <- year(ground.fish.by.set$date_fished)


#################  First lets do some QA/QC on the Groundfish data ############################
# spatial/temporal tidy-up first. Requires PBSmapping and polygons from ESS drive

windows(11,11)
ScallopMap("SS",xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct="Y:/Offshore scallop/Assessment/" ,cex.mn=2,dec.deg = F)
points(ground.fish.by.set[ground.fish.by.set$year==2016,]$lon,ground.fish.by.set[ground.fish.by.set$year==2016,]$lat,pch=19,cex=0.2)

# SO as a first filter on the data I suggest we remove everything that isn't on Georges Bank from both sets of data
# First make a dataframe of the points and remove all NA's from the data
gf.dat <- data.frame(EID = 1:nrow(ground.fish.by.set),X = ground.fish.by.set$lon,Y = ground.fish.by.set$lat)
gf.dat <- na.omit(gf.dat)
# Now identified the rows (EID's) with data inside of Georges Bank
key <-findPolys(gf.dat, GB.bounds,maxRows=1e6)
# Now subset the comm commercial data to exclude all data outside of GB.
groundfish.GB <- ground.fish.by.set[1:nrow(ground.fish.by.set) %in% key$EID,]

# Now we can plot these to make sure we did this correctly, which we did...
windows(11,11)
ScallopMap("SS",xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
points(groundfish.GB$lon,groundfish.GB$lat,pch=19,cex=0.2)

## Next up I want to make the dates all a date format
# Question, what is the time zone used for the fished date??
# Get the date into proper format, need above answer to know time zone.
groundfish.GB$date_fished <- strptime(groundfish.GB$date_fished, format = "%Y-%m-%d %H:%M:%S")
# Get the year alone as this will be useful for using as a factor in figures...
groundfish.GB$year <- format(groundfish.GB$date_fished, "%Y")


# How much of the fishery occurs in what month over time... Can see there is a winter fishery starting in 2005 in Jan/Feb, we need
# to look at the winter fishery seperately than the rest of the fishery.  
# The number of sets per month
aggregate(trip_id~ month+year,groundfish.GB,FUN=function(x) length(unique(x)))
aggregate(trip_id ~year , groundfish.GB,FUN=function(x) length(unique(x)))
aggregate(log_efrt_std_info_id ~year , groundfish.GB,FUN=function(x) length(unique(x)))
aggregate(log_efrt_std_info_id ~year + gear_code , groundfish.GB,FUN=function(x) length(unique(x)))
# Get the landings by year by species... The cod biomasses are now matching what the TRAC document says, life it good!
aggregate(rnd_weight_kgs ~year + gear_code + species_code , groundfish.GB,FUN=sum)

# Now we can plot these to make sure we did this correctly, which we did, I"ve changed this to just cover GB as I know I've done it correctly.
windows(11,11)
ScallopMap(xlim = c(-67.5,-65.5),ylim=c(40.75,42.5),xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
points(groundfish.GB$lon[which(groundfish.GB$gear_code==12)],groundfish.GB$lat[which(groundfish.GB$gear_code==12)],pch=19,cex=0.2,col="blue")
points(groundfish.GB$lon[which(groundfish.GB$gear_code==41)],groundfish.GB$lat[which(groundfish.GB$gear_code==41)],pch=19,cex=0.2,col="green")
points(groundfish.GB$lon[which(groundfish.GB$gear_code==51)],groundfish.GB$lat[which(groundfish.GB$gear_code==51)],pch=19,cex=0.2,col="black")

## Next up I want to make the dates all a date format
# Question, what is the time zone used for the fished date??
# Get the date into proper format, need above answer to know time zone.
groundfish.GB$date_fished <- strptime(groundfish.GB$date_fished, format = "%Y-%m-%d %H:%M:%S")
# Get the year alone as this will be useful for using as a factor in figures...
groundfish.GB$year <- format(groundfish.GB$date_fished, "%Y")




# Now some quick summaries of these data... Checking for species code errors, landed forms, total trips

num.gf.trips <- length(unique(groundfish.GB$trip_id)) # so we have 8357 groundfish trips
# Question, do we have the correct species codes
species.caught <- sort(unique(groundfish.GB$species_code))
num.species.caught <- length(species.caught) # and there are 39 unique species codes, of which we are really only interested in 2...
# Question for meeting, what are the landed_form_codes, how are they used, can we just sum the weights up across these codes?
landed.forms <- sort(unique(groundfish.GB$landed_form_code)) 
num.landed.forms <-length(landed.forms) # 10 different landed forms.

# Trying to get at the number of sets recorded, The log_efrt_std_info_id will for this.
#groundfish.GB$loc <- paste0(groundfish.GB$latitude,groundfish.GB$longitude)
num.gf.sets <- aggregate(log_efrt_std_info_id~trip_id, groundfish.GB,function(x) length(unique(x)))
names(num.gf.sets)[2] <- "Num_sets"
num.sets <- sum(num.gf.sets$Num_sets) # The 8357 trips have 95781 sets
range(num.gf.sets$Num_sets) # The number of sets per trip range between 1 and 90.

# Trying to get at the number of sets recorded, The log_efrt_std_info_id will for this.

#groundfish.GB$loc <- paste0(groundfish.GB$latitude,groundfish.GB$longitude)
num.gf.sets <- aggregate(log_efrt_std_info_id~trip_id, groundfish.GB,function(x) length(unique(x)))
names(num.gf.sets)[2] <- "Num_sets"
num.sets <- sum(num.gf.sets$Num_sets) # The 8357 trips have 95781 sets
range(num.gf.sets$Num_sets) # The number of sets per trip range between 1 and 90.



#### FK: effort data checks
# look at gear codes, identify which ones can be removed
# look at duplicated log_efrt_std_info_id. Can we use these as set unique ID?
# plot effort amount as a function of effort measure ID and gear code
# plot gear codes by effort measure ID in both directions too

# check it out
head(ground.fish.logeff.by.set)

# decode gear codes
ground.fish.logeff.by.set$gear <- ifelse(ground.fish.logeff.by.set$gear_code == "12", "otter trawl", 
                             ifelse(ground.fish.logeff.by.set$gear_code == "22", "scottish seine", 
                                    ifelse(ground.fish.logeff.by.set$gear_code == "41", "gill net", 
                                           ifelse(ground.fish.logeff.by.set$gear_code == "51", "longline",
                                                  ifelse(ground.fish.logeff.by.set$gear_code == "59", "handline", 
                                                         ifelse(ground.fish.logeff.by.set$gear_code == "71", "drag", 
                                                                ifelse(ground.fish.logeff.by.set$gear_code == "81", "harpoon", NA)))))))

# how many records per log_efrt_std_info_id
unique(table(ground.fish.logeff.by.set$log_efrt_std_info_id))
# ok so since above = 1, that means that each log_efrt_std_info_id corresponds to one row in the table. So this is a good unique ID for each set. 



# identify/remove the unnecessary gear codes
efftripsetsbygear <- aggregate(log_efrt_std_info_id ~ trip_id + gear_code + gear, ground.fish.logeff.by.set,function(x) length(unique(x)))
effsetsbygear <- aggregate(log_efrt_std_info_id ~ gear_code + gear, ground.fish.logeff.by.set,function(x) length(unique(x)))
effsetsbygear$proportional <- effsetsbygear$log_efrt_std_info_id/length(unique(ground.fish.logeff.by.set$log_efrt_std_info_id))
#   gear_code           gear log_efrt_std_info_id  proportional
# 1        71           drag                 1837 0.02010594751
# 2        41       gill net                  410 0.00448744610
# 3        59       handline                    6 0.00006566994
# 4        81        harpoon                   81 0.00088654423
# 5        51       longline                 1499 0.01640654073
# 6        12    otter trawl                87518 0.95788367664
# 7        22 scottish seine                   14 0.00015322987

# plot effort



### FK: gear types/gear codes for commercial data, and the dominant species plots. The SABS groundfish folks helped us realize
# that we have some fishy trips since they contain gear codes for handline, harpoon, spear, scottish seine, and drag.
# We need to investigate these records and determine whether they should stay or go. 

unique(groundfish.GB$gear_code)
# we have 7 gear codes: 12, 71, 81, 51, 41, 59, 22
require(plyr)

# these codes correspond to the following: otter trawl, scottish seine, gill net, longline, handline, drag, harpoon
groundfish.GB$gear <- ifelse(groundfish.GB$gear_code == "12", "otter trawl", 
                             ifelse(groundfish.GB$gear_code == "22", "scottish seine", 
                                    ifelse(groundfish.GB$gear_code == "41", "gill net", 
                                           ifelse(groundfish.GB$gear_code == "51", "longline",
                                                  ifelse(groundfish.GB$gear_code == "59", "handline", 
                                                         ifelse(groundfish.GB$gear_code == "71", "drag", 
                                                                ifelse(groundfish.GB$gear_code == "81", "harpoon", NA)))))))

## log_efrt_std_info_id is a proxy for unique set
## length(unique(groundfish.GB$log_efrt_std_info_id))
## total number of sets = 95781
tripsetsbygear <- aggregate(log_efrt_std_info_id ~ trip_id + gear_code + gear, groundfish.GB,function(x) length(unique(x)))
setsbygear <- aggregate(log_efrt_std_info_id ~ gear_code + gear, groundfish.GB,function(x) length(unique(x)))
setsbygear$proportional <- setsbygear$log_efrt_std_info_id/95781
## trawl = 96% of sets
## drag = 1.9% of sets
## gillnet = 0.3% of sets
## handline = 0% of sets
## harpoon = 0% of sets
## longline = 1.2% of sets
## scottish seine = 0% of sets

## why doesn't the sum below match up with the 2016 cod assessment landings, effort, etc. 
require(lubridate)
ground.fish.by.set$year <- year(ground.fish.by.set$date_fished)
sum(ground.fish.by.set$rpt_weight_kgs[ground.fish.by.set$year==2016 & 
                                        ground.fish.by.set$gear_code == 12 & 
                                        ground.fish.by.set$species_code==100])

tripsbygear <- aggregate(trip_id ~ gear_code + gear, groundfish.GB,function(x) length(unique(x)))
splitgears <- aggregate(gear_code ~ trip_id, groundfish.GB,function(x) length(unique(x)))

# what about days per year
require(lubridate)
groundfish.GB$date_fished <- ymd_hms(groundfish.GB$date_fished)
groundfish.GB$year <- year(groundfish.GB$date_fished)
groundfish.GB$date <- floor_date(groundfish.GB$date_fished, unit = "day")

daysgears <- aggregate(date ~ gear_code + gear + year, groundfish.GB, function (x) length(unique(x)))
daysyearsgears1 <- aggregate(date ~ gear_code + gear, groundfish.GB, function (x) length(unique(x)))
daysyearsgears2 <- aggregate(year ~ gear_code + gear, groundfish.GB, function (x) length(unique(x)))

daysyearsgears <- join(daysyearsgears1, daysyearsgears2, type="full")
daysyearsgears$daysperyear <- daysyearsgears$date/daysyearsgears$year

## trawl = 200 days per year avg
## drag = 87.5 days per year avg
## gillnet = 37.3 days per year avg
## handline = 3 days per year avg
## harpoon = 3 days per year avg
## longline = 62.2 days per year avg
## scottish seine = 3 days per year avg

# 3 weird trips where fishing with harpoon AND gillnet occurred:
splitgeartrips <- subset(tripsetsbygear, trip_id %in% splitgears$trip_id[splitgears$gear_code==2])
sum(splitgeartrips$log_efrt_std_info_id)
# these trips account for 15 sets, total. 

require(ggplot2)

pdf("E:/GB_time_area_closure_SPERA/Scripts/sql/gear plots.pdf", onefile = T, width = 7, height=5)
# number of sets per gear type
ggplot() + geom_point(data=setsbygear[setsbygear$gear_code >12,], aes(as.factor(gear), log_efrt_std_info_id), stat="identity") +
  theme_bw() + theme(panel.grid=element_blank()) +
  geom_text(data=setsbygear[setsbygear$gear_code >12,], aes(as.factor(gear), log_efrt_std_info_id+100, 
                                                            label=paste0(log_efrt_std_info_id, " sets")), stat="identity") +
  ylab("Number of sets") +
  xlab("Gear")

# number of trips per gear type
ggplot() + geom_point(data=tripsbygear[tripsbygear$gear_code >12,], aes(as.factor(gear), trip_id), stat="identity") +
  geom_text(data=tripsbygear[tripsbygear$gear_code >12,], aes(as.factor(gear), trip_id+10, 
                                                            label=paste0(trip_id, " trips")), stat="identity") +
  theme_bw() + theme(panel.grid=element_blank()) + xlab("Gear") + ylab("number of trips")

# number of days per year by gear type
ggplot() + geom_point(data=daysgears, aes(year, date)) + facet_wrap(~gear) +
  geom_text(data=daysgears, aes(year, date+25, label=date), size=2, stat="identity") +
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab("Year") +
  ylab("Number of days fished")

# number of days per year by gear type
ggplot() + geom_point(data=daysyearsgears, aes(gear, daysperyear))+
  geom_text(data=daysyearsgears, aes(gear, daysperyear+10, label=paste0(round(daysperyear, 1), " days/yr")), stat="identity") +
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab("Gear") +
  ylab("Average number of days fished each year")

dev.off()
# next step is to plot the gear types spatially

plot.list <- NULL
subset_gears <- NULL
for (i in unique(groundfish.GB$gear)){
  subset1 <- groundfish.GB[groundfish.GB$gear==i,]
  subset1 <- data.frame(gear = i, number_of_sets = length(unique(subset1$log_efrt_std_info_id)))
  subset_gears <- rbind(subset_gears, subset1)
  p <- ggplot() + geom_point(data=groundfish.GB[groundfish.GB$gear==i,], aes(lon, lat)) + coord_map() +
    ggtitle(i) +
    theme_bw() + theme(panel.grid=element_blank())
  
  pdf(paste0("E:/GB_time_area_closure_SPERA/Scripts/sql/Spatial gear QAQC", i,".pdf"), width=10, height=10)  
  print(p)
  dev.off()
  }

pdf("E:/GB_time_area_closure_SPERA/Scripts/sql/Spatial gear QAQC.pdf", width=10, height=10)
print(plot.list)
dev.off()


#### determine targeted fishing species (based on highest catch per trip)

# first, match the species code with the common name
common <- read.csv("E:/GB_time_area_closure_SPERA/Data/MARFISSCI_SPECIEStable_ieSpeciesCodes.csv")
common <- common[,c(1,3)]
names(common) <- tolower(names(common))

groundfish.GB <- join(groundfish.GB, common, type="left")

# determine the dominant species for each trip based on the rnd weight est and plot it.
dominantsp_rnd <- aggregate(rnd_weight_kgs ~ trip_id + species_code + desc_eng, groundfish.GB, function(x) sum(x))
dominantsp_rnd_max <- aggregate(rnd_weight_kgs ~ trip_id, dominantsp_rnd, function(x) max(x))
dominantsp_rnd <- join(dominantsp_rnd_max, dominantsp_rnd, type="left")

#ggplot() + geom_point(data=dominantsp_rnd, aes(trip_id, rnd_weight_kgs)) + facet_wrap(~species_code) +
#  theme_bw() + theme(panel.grid=element_blank())

dominantsp_trips_rnd <- ddply(.data=dominantsp_rnd, .(desc_eng),
                          summarize,
                          trips = length(unique(trip_id)))

ggplot() + geom_bar(data=dominantsp_trips_rnd, aes(as.character(desc_eng), trips), stat="identity") + 
  theme_bw() + theme(panel.grid=element_blank()) + 
  xlab("Species") + 
  ylab("Number of commercial trips (based on rnd_weight")

#table(as.character(dominantsp_rnd$desc_eng)) ## most popular "dominant" species is haddock
dominantsp_trips_rnd$proportional <- dominantsp_trips_rnd$trips/sum(dominantsp_trips_rnd$trips)

#           desc_eng trips proportional
# 1              COD   104 0.0124446572
# 2             CUSK    11 0.0013162618
# 3          HADDOCK  7239 0.8662199354
# 4          HALIBUT     2 0.0002393203
# 5         MONKFISH     3 0.0003589805
# 6          POLLOCK   334 0.0399664952
# 7          REDFISH     3 0.0003589805
# 8     SCALLOP, SEA   169 0.0202225679
# 9            SKATE     7 0.0008376212
# 10       SWORDFISH    15 0.0017949025
# 11      WHITE HAKE     2 0.0002393203
# 12 WINTER FLOUNDER    21 0.0025128635
# 13      YELLOWTAIL   447 0.0534880938
# we need to get rid of the scallop trips. 

# determine the dominant species for each trip based on the rpt weight est and plot it.
dominantsp_rpt <- aggregate(rpt_weight_kgs ~ trip_id + species_code + desc_eng, groundfish.GB, function(x) sum(x))
dominantsp_rpt_max <- aggregate(rpt_weight_kgs ~ trip_id, dominantsp_rpt, function(x) max(x))
dominantsp_rpt <- join(dominantsp_rpt_max, dominantsp_rpt, type="left")

#ggplot() + geom_point(data=dominantsp_rpt, aes(trip_id, rpt_weight_kgs)) + facet_wrap(~species_code) +
#  theme_bw() + theme(panel.grid=element_blank())

dominantsp_trips_rpt <- ddply(.data=dominantsp_rpt, .(desc_eng),
                          summarize,
                          trips = length(unique(trip_id)))

dominantsp_trips_rpt$proportional <- dominantsp_trips_rpt$trips/sum(dominantsp_trips_rpt$trips)
#           desc_eng trips proportional
# 1  AMERICAN PLAICE     1 0.0001196602
# 2              COD   104 0.0124446572
# 3             CUSK    11 0.0013162618
# 4          HADDOCK  7234 0.8656216346
# 5          HALIBUT     2 0.0002393203
# 6         MONKFISH     1 0.0001196602
# 7          POLLOCK   338 0.0404451358
# 8          REDFISH     3 0.0003589805
# 9     SCALLOP, SEA   169 0.0202225679
# 10           SKATE     4 0.0004786407
# 11       SWORDFISH    15 0.0017949025
# 12      WHITE HAKE     3 0.0003589805
# 13 WINTER FLOUNDER    24 0.0028718440
# 14      YELLOWTAIL   448 0.0536077540
# remove the scallop trips

ggplot() + geom_bar(data=dominantsp_trips_rpt, aes(desc_eng, trips), stat="identity") + 
  theme_bw() + theme(panel.grid=element_blank()) + 
  xlab("Species") + 
  ylab("Number of commercial trips (based on rpt_weight)")


#### End of FK QA/QC section for now... Next thing to do is winter fishery checks, and observer gear code/species code checks I think?
#### Back to much nicer DK code finally!


#### More summary checks by DK

# Now lets get the total catch/set and per trip to see if it makes sense.
# first the round weights
# Question, what is log_efrt_std_info_id and effort_mearsure_id
dat.catch.per.set <- aggregate(cbind(rnd_weight_kgs,rpt_weight_kgs)~trip_id+log_efrt_std_info_id, groundfish.GB,sum)
dat.catch.per.set$trip_id <- as.factor(dat.catch.per.set$trip_id)
# This is a pretty aggresive boxplot, so just plot the data as points to look for analonomies...
windows(11,11)
p <- ggplot(dat.catch.per.set, aes(trip_id,rnd_weight_kgs)) + geom_point() #Question  How much is too much landings for a groundfish set (30,000 kg???)
p
# Now add in the rpt_weight field
p <- p + geom_point(aes(trip_id,rpt_weight_kgs),colour="blue") 
p
# Question, what is difference between rnd weight and rpt weight
# Question, what is the proration ratio?
range(dat.catch.per.set$rnd_weight_kgs) # Lower end seems reasonable...
range(dat.catch.per.set$rpt_weight_kgs) # Lower end seems reasonable...

# Question - Explain what we are seeing in this plot, the 1:1 line
p <-  ggplot(dat.catch.per.set, aes(rpt_weight_kgs,rnd_weight_kgs)) + 
                                            geom_abline(intercept = 0, slope = 1,colour="blue",size=1.5,linetype="dashed") + geom_point()
p

# Let's now look by trip rather than by set...
dat.catch.per.trip <- aggregate(cbind(rnd_weight_kgs,rpt_weight_kgs)~trip_id, groundfish.GB,sum)
dat.catch.per.trip$trip_id <- as.factor(dat.catch.per.trip$trip_id)
# This is a pretty aggresive boxplot, so just plot the data as points to look for analonomies...
windows(11,11)
p <- ggplot(dat.catch.per.trip, aes(trip_id,rpt_weight_kgs)) + geom_point() # There is one triip here with 250 tonnes, is a 75 tonne trip reasonable?
p
p <- p + geom_point(aes(trip_id,rnd_weight_kgs),colour="blue") #Question  How much is too much landings for a groundfish trip (600 tonnes?!?!?)
p
# Now compare the rpt weight and rnd weight by trip rather than by set
# Question, what are these data on the non 1:1 line.  Note that that 250 tonne trip appears to be an outlier
p <-  ggplot(dat.catch.per.trip, aes(rpt_weight_kgs,rnd_weight_kgs)) + 
  geom_abline(intercept = 0, slope = 1,colour="blue",size=1.5,linetype="dashed") + geom_point()
p

# Now lets look at the effort metric and see if anyting is odd..
# Question, what is the effort measure ID?
unique(groundfish.GB$effort_measure_id) # 1,2,3,5 + some NA's, Question:  Should we discard the NA's in the ID if we have the effort information, I think yes
dat.effort.per.trip <- aggregate(effort_amount~trip_id+effort_measure_id, groundfish.GB,sum)
dat.effort.per.trip$trip_id <- as.factor(dat.effort.per.trip$trip_id)

# Question, what would be an unusual value for each effort measure ID?  Several points in ID = 5 look suspiciously elevated.  Possibly some in 1 and 2 as well.
# Question, For ID =2 there are a few questionable point, but what's up with the trend with trip ID in the middle of the trip ids (which should be)
# a surrogate for time.
p <- ggplot(dat.effort.per.trip, aes(trip_id,effort_amount)) + geom_point() + facet_wrap(~effort_measure_id, scales="free")
p

# How about CPUE, any obvious weirdness with this, just going to use rpt_weight_kgs for the moment, looking at the maximum (median also of interest)
dat.cpue.per.trip <- aggregate(rpt_weight_kgs/effort_amount~trip_id+effort_measure_id, groundfish.GB,max)
dat.cpue.per.trip$trip_id <- as.factor(dat.cpue.per.trip$trip_id)
names(dat.cpue.per.trip)[3] <- "cpue"

# Question, any idea what a reasonable CPUE is for these data?  Type 1 has one potential outlier, Type 2 is really variable, is > 10000 reasonable
# Type 3 has several points that are suspicious, type 5 seems reasonable for the most part, though again the last few points seem weird.
p <- ggplot(dat.cpue.per.trip, aes(trip_id,cpue)) + geom_point() + facet_wrap(~effort_measure_id, scales="free")
p

# How about our dates, do they all look reasonable?  
#Question, what is going in in 2002-2003, are trip ID's not tied to dates fished?
# Question, in later years any ideas what the outlier points might represent, are these error or could they be something else?
windows(11,11)
p <- ggplot(groundfish.GB, aes(date_fished,trip_id)) + geom_point() + facet_wrap(~year, scales="free")
p



