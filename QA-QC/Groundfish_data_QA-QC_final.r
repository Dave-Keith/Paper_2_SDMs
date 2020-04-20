## DK Oct 18th, 2017. This script is used to QA/QC the groundfish logbook data, questions are embedded along with some of the answers...

# Question:  What are the differnces between  the fixed gear and mobile gear fleets?
#            In terms of reporting very little, but a set for longline or gill net fleet would be much longer than a set for the otter trawls, at least typically.
# Question:  Do we need to look at the fleets seperately
# Answer:    We need to look at otter trawls, longline, and gillnet fisheries somewhat differently.  We don't have effort metrics for longline and gillnet so
#            we cannot combine the 3 fleets for any CPUE type analysis, for presence absence we can combine the fleets.
# Question:  Have these fleets changed fishing practices  over the years?
# Answer     Some gear changes very recently (2016?) but we won't be using these data so we're fine
# Question:  Have these fleets had changes to license/reporting conditions over the years?
# Answer:    Nothing to be concerned with
# Question:  What is the marfissci.pro_spc_info, is there another similar table, what are the differences with these.
# Answer:    This table is good and the one we want
# Question:  How likely is it that we are missing data given they don't report null sets and we are pulling all species caught
# Answer:    very low
## By DK, with loads of help from JS, and IA (SABS).

#################  Section 1 Pulling the data#################  Section 1 Pulling the data#################  Section 1 Pulling the data
rm(list=ls)
direct <- "d:/local_backup/r/"
library(RODBC)
library(lubridate)
require(plyr)
require(ggplot2)
require(PBSmapping)
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) #Source3 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source3 

#effort.by.set <- read.csv("D:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Groundfish_catch_and_effort_by_set.csv",stringsAsFactors = F)
effort.by.set <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Effort_2003_2016_needs_QAQC.csv",
                               stringsAsFactors = F)
groundfish.GB <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Catch_GB_clean.csv",stringsAsFactors = F)

offshore.banks <- read.csv(paste(direct,"/Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""))
GB.bounds <- offshore.banks[offshore.banks$label == "SFA27",]

# Species codes in MARFIS for
common <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Groundfish/MARFIS_species_codes.csv")
common <- common[,c(1,3)]
names(common) <- tolower(names(common))
names(effort.by.set) <- tolower(names(effort.by.set))

which(is.na(effort.by.set$ent_longitude))

# First we need to convert the Lat and longs to decimal degrees.
effort.by.set$lat <- convert.dd.dddd(effort.by.set$ent_latitude)
effort.by.set$lon <- -convert.dd.dddd(effort.by.set$ent_longitude)
# Tidy up the dates.
effort.by.set$date_fished <- strptime(effort.by.set$fv_fished_datetime, "%d/%m/%Y") # MARFIS is recording local time, so will be Atlantic Time Zone
effort.by.set$month <- month(effort.by.set$date_fished)
effort.by.set$year <- year(effort.by.set$date_fished)

# Now identified the rows (EID's) with data inside of Georges Bank for effort data.
gf.dat <- data.frame(EID = 1:nrow(effort.by.set),X = effort.by.set$lon,Y = effort.by.set$lat)
gf.dat <- na.omit(gf.dat)
# Now identified the rows (EID's) with data inside of Georges Bank
key <-findPolys(gf.dat, GB.bounds,maxRows=1e6)
# Now subset the effort data to exclude all data outside of GB.
effort.GB <- effort.by.set[1:nrow(effort.by.set) %in% key$EID,]


############################  Section 2 QA/QC############################  Section 2 QA/QC############################  Section 2 QA/QC

# Lets start by doing some basics on the groundfish effort data..
windows(11,11)
ScallopMap("SS",xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
points(effort.by.set$lon,effort.by.set$lat,pch=19,cex=0.2) # Can see problem data has been removed.
points(effort.GB$lon,effort.GB$lat,pch=19,cex=0.2,col="blue")



windows(11,11)
ScallopMap(xlim = c(-67.5,-65.5),ylim=c(40.75,42.5),xlab="",ylab="", title="", bathy.source="quick", plot.bathy = T,
           plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
# So the gear code is in hardly in any of these, ugh, but we do have fv_duration_in_hours which is useful at least...
points(effort.GB$lon[which(effort.GB$fv_gear_code==12)],effort.GB$lat[which(effort.GB$fv_gear_code==12)],pch=19,cex=0.2,col="blue")



# How much of the fishery occurs in what month over time... Can see there is a winter fishery starting in 2005 in Jan/Feb, we may need
# to look at the winter fishery seperately than the rest of the fishery.  
# The number of sets per month, this looks really good as we have almost identical numbers of records!
aggregate(log_efrt_std_info_id ~ month + year,effort.GB,FUN=function(x) length(unique(x)))
aggregate(log_efrt_std_info_id ~ year , effort.GB,FUN=function(x) length(unique(x)))


# Lets continue and see how much of these data are NA's
no.effort <- length(which(is.na(effort.GB$fv_duration_in_hours))) # only losing 4396 trips lost, this is very hopeful to be the data we want...

# S0 let's remove these data since they are useless to us...
effort.GB <- effort.GB[-which(is.na(effort.GB$fv_duration_in_hours)),]

# QUestion:  I don't believe a 300 hour set is quit posssible, what is too long of a set?
windows(11,11)
p <- ggplot(effort.GB, aes(as.factor(year),fv_duration_in_hours)) + geom_boxplot() + scale_y_log10()
p

# The fishery doesn't go for longer 
length(which(effort.GB$fv_duration_in_hours > 12)) # About 5000 greater than 12 hours, now these could be long-lines still possibly I guess, I won't kick these out until we merge
# There is it, there are about 5000 "sets" that are all essentially 24 hours.  
# These are gillnet trips and we can remove these
length(which(effort.GB$fv_duration_in_hours > 23))
length(which(effort.GB$fv_duration_in_hours > 24)) 
# There are also 3116 trips that have efforts of 0 hours so we'll need to take a look at those too...
length(which(effort.GB$fv_duration_in_hours ==0))

# I think I'm ready to merge the data and take a look at it....
# Now get ride of all the columns we don't really need from the effort data
effort.GB.small <- effort.GB[,names(effort.GB) %in% c("log_efrt_std_info_id","fv_duration_in_hours")]
catch.effort.GB <- merge(groundfish.GB,effort.GB.small,by="log_efrt_std_info_id",all.x=T)
names(catch.effort.GB)[ncol(catch.effort.GB)] <- "Effort_hours"


# Did some stuff with gill net fishery
gill.trips <- catch.effort.GB[catch.effort.GB$gear_code == 41,]
length(unique(gill.trips$trip_id)) # 235 trips
length(unique(gill.trips$log_efrt_std_info_id)) # 831 sets

gill.effort <- aggregate(Effort_hours ~ trip_id,gill.trips,function(x) mean(x,na.rm=T)) # It looks like the 24 hour efforts are gill net trips
nrow(gill.effort) # 205 rows, so from above of 235 trips we have data of some sort for 204 of them, note there are loads of 0's which are silly...
nrow(gill.effort[gill.effort$Effort_hours==0,]) # 39 of the trips are 0's which is silly, that takes us from 235 down to about 165 of 235 trips with info

### THIS IS THE IMPORTANT BIT, OTTER TRAWL DATA WITH EFFORT INFORMATION, THIS IS WHAT WE ARE GOING TO USE FOR THE CATCH-EFFORT DATA, all other
# groundfish data is getting the boot!
# How many of the otter trawl trips have na's associated with them
otter.trips <- catch.effort.GB[catch.effort.GB$gear_code == 12,]
length(unique(otter.trips$trip_id)) # 7517 trips
length(unique(otter.trips$log_efrt_std_info_id)) # 91584 sets
length(which(is.na(otter.trips$Effort_hours))) # 7246 datapoints are NA's, so around 0.2% of the data we'll lose

# How does this look by trip...
otter.effort <- aggregate(Effort_hours ~ trip_id,otter.trips,function(x) mean(x,na.rm=T))
nrow(otter.effort) # So 7498 of these have data, this is 99.7 % of the trips have at least some effort data (we may lose about 8% of our sets...)
nrow(otter.effort[otter.effort$Effort_hours==0,]) # Only 5 entries with 0's, this is looking good!!
median(otter.effort$Effort_hours,na.rm=T)
mean(otter.effort$Effort_hours,na.rm=T)

# What do the NA's look like by year
otter.nas <- otter.trips[which(is.na(otter.trips$Effort_hours)),]
table(otter.nas$year) # These are reasonably clean over time, a bit of a spike in 2006, but we have some in all years, I suggest I just toss these
# We will lose about 8% of our data, but I'm o.k. with that...
otter.clean <- otter.trips[-which(is.na(otter.trips$Effort_hours)),]

p <- ggplot(otter.clean, aes(as.factor(year),Effort_hours)) + geom_boxplot() + scale_y_log10()
p

# I'm making the executive decision that I'm removing trawls longer than 10 hours of effort and trawls of 0
# Irene confirmed with industry that the maximum anyone would tow for is about 5-9 hours, given that information  we remove all sets 10 hours or longer in duration
otter.clean.final <- otter.clean[otter.clean$Effort_hours > 0 & otter.clean$Effort_hours < 10,] # This removes about 2000 sets from the data, < 1%
# The average time of tows with the clean data is...
median(otter.clean.final$Effort_hours)
mean(otter.clean.final$Effort_hours)

p <- ggplot(otter.clean.final, aes(as.factor(year),Effort_hours)) + geom_boxplot() 
p

# Number of otter trawl trips
num.trawl.sets <- length(unique(otter.clean.final$log_efrt_std_info_id)) # So there are 89105 trawl sets
num.trawl.trips <- length(unique(otter.clean.final$trip_id)) # So there are 7477 trawl trips, we've hardly lost any!!
89105/7477

time.fishing <- aggregate(date_fished~trip_id,otter.clean.final,range)
time.fishing$start_fish <- as.Date(time.fishing$date_fished[,2])
time.fishing$end_fish <- as.Date(time.fishing$date_fished[,1])

# This isn't quite working, but not seriously important...
difference <- abs(diff.Date((time.fishing$end_fish - time.fishing$start_fish)))
mean(difference)

#write.csv(otter.clean.final,"D:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Otter_trawl_catch_and_effort_clean.csv")

### The last step that is needed is to get rid of all the non Cod/Yellowtail data, but making sure that we keep all the sets so that
### we have any null sets....
# First we make a data.frame that has 1 entry for every set.
all.otter.sets <- data.frame(log_efrt_std_info_id = unique(otter.clean.final$log_efrt_std_info_id))
dat.tmp <- NULL
for(i in 1:length(all.otter.sets$log_efrt_std_info_id))
{
  pick <- which(otter.clean.final$log_efrt_std_info_id == all.otter.sets$log_efrt_std_info_id[i])
  dat.tmp[[i]] <- otter.clean.final[pick,which(names(otter.clean.final) %in% 
                                                 c("log_efrt_std_info_id","date_fished","lat","lon","month","year","gear_code","trip_id"))][1,]
} # end for(i in 1:length(all.otter.sets$log_efrt_std_info_id))


dat.frame <- do.call("rbind",dat.tmp)
head(dat.frame)

# All cod and yt data
all.otter.cod.sets <- otter.clean.final[otter.clean.final$species_code %in% c(100),]
all.otter.yt.sets <- otter.clean.final[otter.clean.final$species_code %in% c(141),]

# Get the effort data into the dat.frame object
effort.otter.clean.final <- aggregate(Effort_hours ~ log_efrt_std_info_id,otter.clean.final,unique)

dat.frame.e <- merge(dat.frame,effort.otter.clean.final,all.x=T)

# Now we need to add up all the sets in which we have crap listed twice
unique.otter.cod.sets <- aggregate(cbind(rnd_weight_kgs)~log_efrt_std_info_id,all.otter.cod.sets,FUN= sum)
names(unique.otter.cod.sets)[2] <- "rnd_weight_kgs_cod"


unique.otter.yt.sets <- aggregate(rnd_weight_kgs~log_efrt_std_info_id,all.otter.yt.sets,FUN= sum)
names(unique.otter.yt.sets)[2] <- "rnd_weight_kgs_yt"


step.cod <- merge(dat.frame.e,unique.otter.cod.sets,all.x=T)

all.otter.cod.yt.all.sets <- merge(step.cod,unique.otter.yt.sets,all.x=T)
# Make the NA's 0's for the catch of cod and yellowtail + improve the names!
names(all.otter.cod.yt.all.sets) <- c("set_id","Effort_hours","date_fished","trip_id","gear_code","lat","lon","month","year","cod_kg","yt_kg")
all.otter.cod.yt.all.sets$cod_kg[which(is.na(all.otter.cod.yt.all.sets$cod_kg))] <- 0
all.otter.cod.yt.all.sets$yt_kg[which(is.na(all.otter.cod.yt.all.sets$yt_kg))] <- 0

write.csv(all.otter.cod.yt.all.sets,"Y:/Projects/GB_time_area_closure_SPERA/Data/Groundfish/Otter_trawl_catch_and_effort_clean_yt_and_cod.csv")


## I also want to make a dataset of just YT and cod for the entire groundfish dataset rather than the otter trawl subset, we need to make
## sure that we keep all records when doing this, including dual NA's

# First we need to make a data set which all all of the sets in it, see above for what I did with the otter trawl data...
all.sets <- data.frame(log_efrt_std_info_id = unique(groundfish.GB$log_efrt_std_info_id))
dat.tmp <- NULL
for(i in 1:length(all.sets$log_efrt_std_info_id))
{
  pick <- which(groundfish.GB$log_efrt_std_info_id == all.sets$log_efrt_std_info_id[i])
  dat.tmp[[i]] <- groundfish.GB[pick,which(names(groundfish.GB) %in% 
                                                 c("log_efrt_std_info_id","date_fished","lat","lon","month","year","gear_code","trip_id"))][1,]
} # end for(i in 1:length(all.otter.sets$log_efrt_std_info_id))
dat.frame2 <- do.call("rbind",dat.tmp)
head(dat.frame2)


# All cod and yt data
groundfish.cod.sets <- groundfish.GB[groundfish.GB$species_code %in% c(100),]
groundfish.yt.sets <- groundfish.GB[groundfish.GB$species_code %in% c(141),]

# Now we need to add up all the sets in which we have crap listed twice
unique.cod.sets <- aggregate(rnd_weight_kgs~log_efrt_std_info_id,groundfish.cod.sets,FUN= sum)
names(unique.cod.sets)[2] <- "cod_kg"

unique.yt.sets <- aggregate(rnd_weight_kgs~log_efrt_std_info_id,groundfish.yt.sets,FUN= sum)
names(unique.yt.sets)[2] <- "yt_kg"

step.cod <- merge(dat.frame2,unique.cod.sets,all.x=T)

all.cod.yt.sets <- merge(step.cod,unique.yt.sets,all.x=T)
# Make the NA's 0's for the catch of cod and yellowtail + improve the names!
names(all.cod.yt.sets) <- c("set_id","date_fished","trip_id","gear_code","lat","lon","month","year","cod_kg","yt_kg")
all.cod.yt.sets$cod_kg[which(is.na(all.cod.yt.sets$cod_kg))] <- 0
all.cod.yt.sets$yt_kg[which(is.na(all.cod.yt.sets$yt_kg))] <- 0

# Finally remove the weird gear types which will have 0 yt/cod landings or that we don't know anything about 81 = harpoon, 71 = scallop drag, 22 = Scottish Seine
range(all.cod.yt.sets$cod_kg[all.cod.yt.sets$gear_code == 12])
all.cod.yt.sets <- all.cod.yt.sets[all.cod.yt.sets$gear_code %in% c(12,41,51,59),]

write.csv(all.cod.yt.sets,"F:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Groundfish/groundfish_yt_cod_catch.csv")
#all.cod.yt.sets <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Groundfish/groundfish_yt_cod_catch.csv")

# Some data summaries
aggregate(set_id ~ month,all.cod.yt.sets,function(x) mean(length(x)))
aggregate(set_id ~ year,all.cod.yt.sets,function(x) length(x))

summary(aggregate(cod_kg~set_id,all.otter.cod.yt.all.sets,mean)$cod_kg)
summary(aggregate(cod_kg~set_id,all.cod.yt.sets,mean)$cod_kg)
summary(aggregate(yt_kg~set_id,all.cod.yt.sets,mean)$yt_kg)

table(all.cod.yt.sets$gear_code)
