## DK Oct 18th, 2017. This script is used to QA/QC the groundfish logbook data, questions are embedded along with some of the answers...

# Question:  What are the differnces between  the fixed gear and mobile gear fleets?
# Question:  Do we need to look at the fleets seperately
# Question:  Have these fleets changed fishing practices  over the years?
# Question:  Have these fleets had changes to license/reporting conditions over the years?
# Question:  What is the marfissci.pro_spc_info, is there another similar table, what are the differences with these.
# Question:  How likely is it that we are missing data given they don't report null sets and we are pulling all species caught
# Question:  
## By DK, with loads of help from JS, and IA (SABS).

#################  Section 1 Pulling the data#################  Section 1 Pulling the data#################  Section 1 Pulling the data

direct <- "d:/local_backup/r/"
library(RODBC)
library(lubridate)
require(plyr)
require(ggplot2)
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) #Source3 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source3 

#ground.fish.by.set <- read.csv("D:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Groundfish_catch_and_effort_by_set.csv",stringsAsFactors = F)
ground.fish.by.set <- read.csv("D:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/GFCommCatchData2003to2016.csv",stringsAsFactors = F)
offshore.banks <- read.csv(paste(direct,"/Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""))
GB.bounds <- offshore.banks[offshore.banks$label == "SFA27",]

# Species codes in MARFIS for
common <- read.csv("D:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/MARFIS_species_codes.csv")
common <- common[,c(1,3)]
names(common) <- tolower(names(common))
names(ground.fish.by.set) <- tolower(names(ground.fish.by.set))

# First we need to convert the Lat and longs to decimal degrees.
ground.fish.by.set$lat <- convert.dd.dddd(ground.fish.by.set$latitude)
ground.fish.by.set$lon <- -convert.dd.dddd(ground.fish.by.set$longitude)
# Tidy up the dates.
ground.fish.by.set$date_fished <- strptime(ground.fish.by.set$date_fished, "%y-%m-%d") # MARFIS is recording local time, so will be Atlantic Time Zone
ground.fish.by.set$month <- month(ground.fish.by.set$date_fished)
ground.fish.by.set$year <- year(ground.fish.by.set$date_fished)

# Now identified the rows (EID's) with data inside of Georges Bank
gf.dat <- data.frame(EID = 1:nrow(ground.fish.by.set),X = ground.fish.by.set$lon,Y = ground.fish.by.set$lat)
gf.dat <- na.omit(gf.dat)
# Now identified the rows (EID's) with data inside of Georges Bank
key <-findPolys(gf.dat, GB.bounds,maxRows=1e6)
# Now subset the observer data to exclude all data outside of GB.
groundfish.GB <- ground.fish.by.set[1:nrow(ground.fish.by.set) %in% key$EID,]



############################  Section 2 QA/QC############################  Section 2 QA/QC############################  Section 2 QA/QC

# Lets start by doing some basics on the groundfish data..
windows(11,11)
ScallopMap("SS",xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
points(ground.fish.by.set$lon,ground.fish.by.set$lat,pch=19,cex=0.2)

# How much of the fishery occurs in what month over time... Can see there is a winter fishery starting in 2005 in Jan/Feb, we need
# to look at the winter fishery seperately than the rest of the fishery.  
# The number of sets per month
aggregate(trip_id~ month+year,groundfish.GB,FUN=function(x) length(unique(x)))
aggregate(trip_id ~year , groundfish.GB,FUN=function(x) length(unique(x)))
aggregate(log_efrt_std_info_id ~year , groundfish.GB,FUN=function(x) length(unique(x)))
aggregate(log_efrt_std_info_id ~year + gear_code , groundfish.GB,FUN=function(x) length(unique(x)))
# Get the landings by year by species... The cod biomasses are now closely matching what the TRAC document says, life it good!
aggregate(rnd_weight_kgs ~year + gear_code + species_code , groundfish.GB,FUN=sum)

# Now we can plot these to make sure we did this correctly, which we did, I"ve changed this to just cover GB as I know I've done it correctly.
windows(11,11)
ScallopMap(xlim = c(-67.5,-65.5),ylim=c(40.75,42.5),xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
points(groundfish.GB$lon[which(groundfish.GB$gear_code==12)],groundfish.GB$lat[which(groundfish.GB$gear_code==12)],pch=19,cex=0.2,col="blue")
points(groundfish.GB$lon[which(groundfish.GB$gear_code==41)],groundfish.GB$lat[which(groundfish.GB$gear_code==41)],pch=19,cex=0.2,col="green")
points(groundfish.GB$lon[which(groundfish.GB$gear_code==51)],groundfish.GB$lat[which(groundfish.GB$gear_code==51)],pch=19,cex=0.2,col="black")

#################  First lets do some QA/QC on the Groundfish data ############################
# Now some quick summaries of these data...
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
num.sets <- sum(num.gf.sets$Num_sets) # The 10602 trips have 102271 sets
range(num.gf.sets$Num_sets) # The number of sets per trip range between 1 and 90.



#### FK: this seems like a logical place to start to look at gear types/gear codes for commercial data, and the dominant species plots. The SABS groundfish folk helped us realize
#### that we have some fishy trips since they contain gear codes for handline, harpoon, spear, scottish seine, and drag.
#### I need to investigate these records and we will determine whether they should stay or go. 

unique(groundfish.GB$gear_code)

groundfish.GB$gear <- ifelse(groundfish.GB$gear_code == "12", "otter trawl", 
                             ifelse(groundfish.GB$gear_code == "22", "scottish seine", 
                                    ifelse(groundfish.GB$gear_code == "41", "gill net", 
                                           ifelse(groundfish.GB$gear_code == "51", "longline",
                                                  ifelse(groundfish.GB$gear_code == "59", "handline", 
                                                         ifelse(groundfish.GB$gear_code == "71", "drag", 
                                                                ifelse(groundfish.GB$gear_code == "81", "harpoon", NA)))))))

## log_efrt_std_info_id is a proxy for unique set
tripsetsbygear <- aggregate(log_efrt_std_info_id ~ trip_id + gear_code + gear, groundfish.GB,function(x) length(unique(x)))
setsbygear <- aggregate(log_efrt_std_info_id ~ gear_code + gear , groundfish.GB,function(x) length(unique(x)))
tripsbygear <- aggregate(trip_id ~ gear_code + gear, groundfish.GB,function(x) length(unique(x)))
splitgears <- aggregate(gear_code ~ trip_id, groundfish.GB,function(x) length(unique(x)))


daysgears <- aggregate(as.character(date_fished) ~ gear_code + gear + year, groundfish.GB, function (x) length(unique(x)))
names(daysgears)[4] <- "date"
daysyearsgears1 <- aggregate(as.character(date_fished) ~ gear_code + gear, groundfish.GB, function (x) length(unique(x)))
names(daysyearsgears1)[3] <- "date"
daysyearsgears2 <- aggregate(year ~ gear_code + gear, groundfish.GB, function (x) length(unique(x)))

daysyearsgears <- join(daysyearsgears1, daysyearsgears2, type="full")
daysyearsgears$daysperyear <- daysyearsgears$date/daysyearsgears$year

# 3 weird trips where fishing with harpoon AND gillnet occurred:
splitgeartrips <- subset(tripsetsbygear, trip_id %in% splitgears$trip_id[splitgears$gear_code==2])
sum(splitgeartrips$log_efrt_std_info_id)
# these trips account for 15 sets, total. 



# number of sets per gear type
ggplot() + geom_point(data=setsbygear[setsbygear$gear_code >12,], aes(as.factor(gear), log_efrt_std_info_id), stat="identity") +
  theme_bw() + theme(panel.grid=element_blank()) +
  geom_text(data=setsbygear[setsbygear$gear_code >12,], aes(as.factor(gear), log_efrt_std_info_id+100, 
                                                            label=paste0(log_efrt_std_info_id, " sets")), stat="identity") +
  ylab("Number of sets") +
  xlab("Gear")

# number of trips per gear type
ggplot() + geom_point(data=tripsbygear[tripsbygear$gear_code >12,], aes(as.factor(gear), trip_id), stat="identity") +
  theme_bw() + theme(panel.grid=element_blank()) + xlab("Gear") + ylab("number of trips")

# number of days per year by gear type
ggplot() + geom_point(data=daysgears, aes(year, date)) + facet_wrap(~gear) +
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab("Year") +
  ylab("Number of days fished")

# number of days per year by gear type
ggplot() + geom_point(data=daysyearsgears, aes(gear, daysperyear))+
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab("Gear") +
  ylab("Average number of days fished each year")


# determine targeted fishing species
# first, match the species code with the common name


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
windows(11,11)
ggplot() + geom_bar(data=dominantsp_trips_rnd, aes(desc_eng, trips), stat="identity") + 
  theme_bw() + theme(panel.grid=element_blank()) + 
  xlab("Species") + 
  ylab("Number of commercial trips (based on rnd_weight")

table(as.character(dominantsp_rnd$desc_eng)) ## most popular "dominant" species is haddock

# determine the dominant species for each trip based on the rpt weight est and plot it.
dominantsp_rpt <- aggregate(rpt_weight_kgs ~ trip_id + species_code + desc_eng, groundfish.GB, function(x) sum(x))
dominantsp_rpt_max <- aggregate(rpt_weight_kgs ~ trip_id, dominantsp_rpt, function(x) max(x))
dominantsp_rpt <- join(dominantsp_rpt_max, dominantsp_rpt, type="left")

#ggplot() + geom_point(data=dominantsp_rpt, aes(trip_id, rpt_weight_kgs)) + facet_wrap(~species_code) +
#  theme_bw() + theme(panel.grid=element_blank())

dominantsp_trips_rpt <- ddply(.data=dominantsp_rpt, .(desc_eng),
                          summarize,
                          trips = length(unique(trip_id)))

ggplot() + geom_bar(data=dominantsp_trips_rpt, aes(desc_eng, trips), stat="identity") + 
  theme_bw() + theme(panel.grid=element_blank()) + 
  xlab("Species") + 
  ylab("Number of commercial trips (based on rpt_weight)")

#### End of FK QA/QC section for now... Next thing to do is winter fishery checks, and observer gear code/species code checks I think?
#### Back to much nicer DK code finally!



# Now lets get the total catch/set and per trip to see if it makes sense.
# first the round weights
# Question, what is log_efrt_std_info_id and effort_mearsure_id
# Answer log_efrt_stnd_info_id is just an ID to link table.  Effort_measure_id is odd so don't use it!
dat.catch.per.set <- aggregate(cbind(rnd_weight_kgs,rpt_weight_kgs)~trip_id+log_efrt_std_info_id, groundfish.GB,sum)
dat.catch.per.set.by.species <- aggregate(cbind(rnd_weight_kgs,rpt_weight_kgs)~trip_id+log_efrt_std_info_id+species_code, groundfish.GB,sum)

dat.catch.per.set$trip_id <- as.factor(dat.catch.per.set$trip_id)
# This is a pretty aggresive boxplot, so just plot the data as points to look for analonomies...
windows(11,11)
p <- ggplot(dat.catch.per.set, aes(trip_id,rnd_weight_kgs)) + geom_point() #Question  How much is too much landings for a groundfish set (30,000 kg???)
# Answer:  Yes you could land that much, but it has to be haddock!
p
#Question, what is difference between rnd weight and rpt weight. Answer: Round weight is what we use, rpt weight is some weird number that we aren't to use
# Question, what is the proration ratio?
# Answer, it is the same proration scheme used by scallop.  Captian gives estimate for each set, this gets summed up for a trip total.  At the 
# wharf a trip total is calculated as well and this is the final catch used and each set gets prorated based on this "final catch" number.
range(dat.catch.per.set$rnd_weight_kgs) # Lower end seems reasonable...
range(dat.catch.per.set$rpt_weight_kgs) # Lower end seems reasonable...

# Question - Explain what we are seeing in this plot, the 1:1 line
# Answer - each species gets it's own conversion, see second plot below
p <-  ggplot(dat.catch.per.set, aes(rpt_weight_kgs,rnd_weight_kgs)) + 
                                            geom_abline(intercept = 0, slope = 1,colour="blue",size=1.5,linetype="dashed") + geom_point()
p
# Does each species get it's own line?  Yep that's what it is
p <-  ggplot(dat.catch.per.set.by.species, aes(rpt_weight_kgs,rnd_weight_kgs)) + facet_wrap(~species_code) +
  geom_abline(intercept = 0, slope = 1,colour="blue",size=1.5,linetype="dashed") + geom_point()
p
# Let's now look by trip rather than by set...
dat.catch.per.trip <- aggregate(cbind(rnd_weight_kgs,rpt_weight_kgs)~trip_id, groundfish.GB,sum)
dat.catch.per.trip$trip_id <- as.factor(dat.catch.per.trip$trip_id)
dat.catch.per.trip.by.species <- aggregate(cbind(rnd_weight_kgs,rpt_weight_kgs)~trip_id+log_efrt_std_info_id+species_code, groundfish.GB,sum)

# This is a pretty aggresive boxplot, so just plot the data as points to look for analonomies...
windows(11,11)
p <- ggplot(dat.catch.per.trip, aes(trip_id,rpt_weight_kgs)) + geom_point() # There is one triip here with 250 tonnes, is a 75 tonne trip reasonable?
p
p <- p + geom_point(aes(trip_id,rnd_weight_kgs),colour="blue") #Question  How much is too much landings for a groundfish trip (250 tonnes?!?!?)
# Answer:  Yes you could land that much, but it has to be haddock!
p
# Looking at this by species we see the species each come out nicely.
p <-  ggplot(dat.catch.per.trip.by.species, aes(rpt_weight_kgs,rnd_weight_kgs)) + facet_wrap(~species_code) +
  geom_abline(intercept = 0, slope = 1,colour="blue",size=1.5,linetype="dashed") + geom_point()
p

# # How about our dates, do they all look reasonable?  
# #Question, what is going in in 2002-2003, are trip ID's not tied to dates fished?
# Question, in later years any ideas what the outlier points might represent, are these error or could they be something else?
windows(11,11)
p <- ggplot(groundfish.GB, aes(date_fished,trip_id)) + geom_point() + facet_wrap(~year, scales="free")
p

# Now lets look at the effort metric and see if anyting is odd..
# Question, what is the effort measure ID?
# Answer, it's something stupid so don't worry about it
#unique(groundfish.GB$effort_measure_id) # 1,2,3,5 + some NA's, Question:  Should we discard the NA's in the ID if we have the effort information, Answer don't worry about it.
#dat.effort.per.trip <- aggregate(effort_amount~trip_id+effort_measure_id, groundfish.GB,sum)
#dat.effort.per.trip$trip_id <- as.factor(dat.effort.per.trip$trip_id)

# Question, what would be an unusual value for each effort measure ID?  Several points in ID = 5 look suspiciously elevated.  Possibly some in 1 and 2 as well.
# Answer, don't worry about it
# Question, For ID =2 there are a few questionable point, but what's up with the trend with trip ID in the middle of the trip ids (which should be)
# a surrogate for time.
# Answer, don't worry about it
#p <- ggplot(dat.effort.per.trip, aes(trip_id,effort_amount)) + geom_point() + facet_wrap(~effort_measure_id, scales="free")
#p

# How about CPUE, any obvious weirdness with this, just going to use rpt_weight_kgs for the moment, looking at the maximum (median also of interest)
#dat.cpue.per.trip <- aggregate(rpt_weight_kgs/effort_amount~trip_id+effort_measure_id, groundfish.GB,max)
#dat.cpue.per.trip$trip_id <- as.factor(dat.cpue.per.trip$trip_id)
#names(dat.cpue.per.trip)[3] <- "cpue"

# Question, any idea what a reasonable CPUE is for these data?  
# Answer:  Need to pull in better effort data so don't worry about this.  Also, effort isn't used in assessments so SABS won't be able to help much on what looks reasonable.
#Type 1 has one potential outlier, Type 2 is really variable, is > 10000 reasonable
# Type 3 has several points that are suspicious, type 5 seems reasonable for the most part, though again the last few points seem weird.
# p <- ggplot(dat.cpue.per.trip, aes(trip_id,cpue)) + geom_point() + facet_wrap(~effort_measure_id, scales="free")
# p
# 



