## DK Oct 6th, 2017. This is a wacky script to pull in the groundfishery data by set and also to pull in the observer data for any trips that were identified as
## being either MG (Mobile Gear) or FG (Fixed Gear).  We then go on and do some basic QA/QC and summaries of the data to see what we actually have...

## By DK, with loads of help from JS, and IA (SABS).

### Pulls observer data
### Length data was pulled in SQLDeveloper

### OUTPUT:
### Data/Observer/Scallop/Scallop_observer_data.csv
### Data/Observer/Scallop/MASTER_CLEAN_scall.obs.trips.sets.csv
### Data/Observer/Scallop/MASTER_CLEAN_scall.obs.trips.sets.species.csv
### Data/Observer/Scallop/scall.obs.data.ytf.csv
### Data/Observer/Scallop/scall.obs.data.cod.csv
### Data/Observer/Scallop/length_long_cod_FINAL_FULL.csv
### Data/Observer/Scallop/length_long_ytf_FINAL_FULL.csv
### Data/Observer/Scallop/length_wide_cod_FULL_FINAL.csv
### Data/Observer/Scallop/length_wide_ytf_FULL_FINAL.csv


library(RODBC)

###############  Scallop Observer data...
obs.ids <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Scallop_fishery/Observed trips_2004-2016.csv",
                    header=T,stringsAsFactors = F)

# Remove any duplicate trip ID (occur when fishing in multiple banks)
ids <- unique(obs.ids$IOP.Trip)

# Open an RODBC channel, using 32 bit so make sure you are running 32 bit R
chan <- odbcConnect("PTRAN",uid="keyserf",pwd="Decade06",believeNRows=FALSE)
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
  query <-   paste("SELECT g.cfv, g.vessel_name, g.license_no, b.num_hook_haul, b.source, y.board_date, y.landing_date, y.trip, y.trip_id, x.set_no,",
                   "x.speccd_id, d.common, x.est_num_caught, x.est_kept_wt, x.est_discard_wt, f.latitude, f.longitude, f.depth,  f.pntcd_id, f.setdate, f.settime, ",
                   "b.comarea_id  FROM observer.istrips y, observer.isfishsets b, observer.iscatches x, observer.isspeciescodes d,",
                   "observer.issetprofile f, observer.isvessels g WHERE y.trip_id = b.trip_id AND g.vess_id = y.vess_id AND b.fishset_id = x.fishset_id",
                   "AND x.speccd_id = d.speccd_id AND b.fishset_id = f.fishset_id AND x.fishset_id = f.fishset_id ",
                   "AND y.trip = (",paste("'",ids[i],"'",sep=""),") AND f.pntcd_id = 2;")
  
  tmp[[i]] <- sqlQuery(chan, query)
} # End the for loop

observer.data <- do.call("rbind",tmp)
#write.csv(observer.data,"Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/Scallop_observer_data.csv")
#############################

## read in the fish length data retrieved via Y:\Projects\GB_time_area_closure_SPERA\Scripts\sql\ISDB length frequencies.sql
scall.obs.data <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/Scallop_observer_data.csv")
fishlengths <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/Scallop_ISFISHLENGTHS.csv", stringsAsFactors = F)

names(scall.obs.data) <- tolower(names(scall.obs.data))
names(fishlengths) <- tolower(names(fishlengths))
require(plyr)
require(dplyr)
require(ggplot2)


###### End section 1 Pulling the data###### End section 1 Pulling the data###### End section 1 Pulling the data###### End section 1 Pulling the data
###### End section 1 Pulling the data###### End section 1 Pulling the data###### End section 1 Pulling the data###### End section 1 Pulling the data

unique(fishlengths$common)


############################  Section 2 QA/QC############################  Section 2 QA/QC############################  Section 2 QA/QC
### first, some quick checks
# check specific trips from list

head(scall.obs.data)

j060558 <- subset(scall.obs.data, trip=="J16-0558" & common=="MONKFISH,GOOSEFISH,ANGLER" & is.na(est_kept_wt))
#dim(subset(observer.data, TRIP=="J16-0558" & COMMON=="MONKFISH,GOOSEFISH,ANGLER" & !is.na(EST_KEPT_WT)))
#dim(subset(observer.data, TRIP=="J16-0558" & COMMON=="MONKFISH,GOOSEFISH,ANGLER" & is.na(EST_KEPT_WT)))


## check the number of trips
length(unique(scall.obs.data$trip))
length(unique(fishlengths$trip))
## we have 233 observed scallop trips, and 232 scallop trips with fish lengths

# check source 0/1. If source = 1, then we need to check these sets in the OTIS reports
unique(as.character(scall.obs.data$source))

################# fixing source = 0/1
source0 <- scall.obs.data[scall.obs.data$source ==0,]
source1 <- scall.obs.data[scall.obs.data$source ==1,]
source1_bycatch_count <- aggregate(speccd_id ~ trip + set_no, data=source1, function(x) length(unique(x)))
source1_bycatch_count_multi <- source1_bycatch_count[source1_bycatch_count$speccd_id >1,]

source1_change <- source1_bycatch_count_multi[source1_bycatch_count_multi$speccd_id >6,]
source1_change$source <- 0
source1_change <- source1_change[,c(1,2,4)]

source1_changed <- join(source1_change, source1[,-6], type="left")
# 1351 rows

source1_manual <- source1_bycatch_count[source1_bycatch_count$speccd_id <7 & source1_bycatch_count$speccd_id >2,]

# QA/QC the trips with >2 and <7 species (not incl. monkfish or scallops) manually, based on this list of potential errors
# See notes below on which trips to keep/remove/double check OTIS report
# 14 trips need manual checking:
# subset(scall.obs.data, trip=="J10-0631" & set_no==34)
#        trip set_no speccd_id
# 1  J05-0198     63         4  change to 0
# 2  J06-0119      2         6  change to 0
# 3  J09-0497     46         3  change to 0
# 4  J10-0631     34         6  change to 0
# 5  J10-0631     35         5  change to 0
# 6  J10-0631     36         6  change to 0
# 7  J11-0353     31         5  change to 0
# 8  J11-0353     33         5  change to 0
# 9  J11-0353     38         5  change to 0
# 10 J11-0353     42         6  change to 0
# 11 J12-0475     32         3  change to 0
# 12 J12-0475     35         3  change to 0
# 13 J12-0494     50         4  change to 0
# 14 J15-0205     50         4  change to 0

source1_manual$source <- 0
source1_manual <- source1_manual[,c(1,2,4)]
source1_manual <- join(source1_manual, source1[,-6], type="left")
# 65 rows

# any source 1's with <2 species, are removed. 
# source1_remove <- source1_bycatch_count[source1_bycatch_count$speccd_id <3,]
# source1_remove$source <- 1 # keep as 1
# source1_remove <- source1_remove[,c(1,2,4)]
# source1_remove <- join(source1_remove, source1[,-6], type="left")
# 7746 rows

# 7746 + 65 + 1351 = 9162

source0 <- source0[,c(9,11,6,1,2,3,4,5,7,8,10,12,13,14,15,16,17,18,19,20,21,22,23)]

source_fixed <- rbind(source0, source1_changed, source1_manual)

table(source_fixed$source)
table(scall.obs.data$source)

source_fixed$tripset <- paste0(source_fixed$trip, "_", source_fixed$set_no)
scall.obs.data$tripset <- paste0(scall.obs.data$trip, "_", scall.obs.data$set_no)

length(unique(source_fixed$tripset))
length(unique(scall.obs.data$tripset))
# 59.3% of sets are source 0

scall.obs.data.final <- source_fixed
write.csv(scall.obs.data.final, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/scall.obs.trips.sets.species.csv")
###############################################
### THIS IS OUR MASTER CLEAN TRIP SET WITH SPECIES, OBSERVED ONLY. NOT YET SPATIALLY FILTERED.

# remove any sets with missing lats or longs
summary(scall.obs.data.final$latitude)
summary(scall.obs.data.final$longitude)

length(scall.obs.data.final[is.na(scall.obs.data.final$latitude)|is.na(scall.obs.data.final$longitude),]$trip)

# so now we'll remove 282 records
test1 <- scall.obs.data.final[!is.na(scall.obs.data.final$latitude),]
scall.obs.data.final <- test1[!is.na(test1$longitude),]

summary(scall.obs.data.final$latitude)
summary(scall.obs.data.final$longitude)

unique(scall.obs.data.final[scall.obs.data.final$latitude > 45,])
unique(scall.obs.data.final[scall.obs.data.final$longitude > 70,])
# so there are three Mersey sets with crappy lat/longs. remove them.

scall.obs.data.final <- scall.obs.data.final[scall.obs.data.final$latitude <45 & scall.obs.data.final$longitude <70,]

# what about comarea_id for split trips or other wonky coords?
ggplot() + geom_point(data=scall.obs.data.final[scall.obs.data.final$comarea_id %in% c("SF27A", "SF27B"),], aes(-longitude, latitude)) + theme_bw() + theme(panel.grid=element_blank())
ggplot() + geom_point(data=scall.obs.data.final[scall.obs.data.final$comarea_id %in% c("SF25"),], aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank())
ggplot() + geom_point(data=scall.obs.data.final[scall.obs.data.final$comarea_id %in% c("SF26N"),], aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank())
ggplot() + geom_point(data=scall.obs.data.final[scall.obs.data.final$comarea_id %in% c("SF26"),], aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank())

table(scall.obs.data.final$comarea_id)

length(unique(scall.obs.data.final[scall.obs.data.final$comarea_id %in% c("SF25", "SF26", "SF26N"),]$trip))
# we're removing wonky comarea_ids but JS sleuthing discovered that some of these trips are NOT split trips. 
# We need to tidy these records up at a later date.

# remove anything not in SF27A or SF27B
scall.obs.data.final <- scall.obs.data.final[scall.obs.data.final$comarea_id %in% c("SF27A", "SF27B"),]
ggplot() + geom_point(data=scall.obs.data.final, aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank())

# narrow these down further to Georges
scall.obs.data.final <- scall.obs.data.final[scall.obs.data.final$latitude < 42.25 & scall.obs.data.final$longitude >64.5,]
ggplot() + geom_point(data=scall.obs.data.final, aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank())

scall.obs.data.final <- scall.obs.data.final[scall.obs.data.final$longitude < 67.5,]
ggplot() + geom_point(data=scall.obs.data.final, aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank())

source(paste("Y:/Projects/Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source3 
scall.obs.data.final$lat <- convert.dd.dddd(scall.obs.data.final$latitude)
scall.obs.data.final$lon <- convert.dd.dddd(-scall.obs.data.final$longitude)

ggplot() + geom_point(data=scall.obs.data.final, aes(lon, lat)) + 
  theme_bw() + theme(panel.grid=element_blank())

# which ones are split trips? 
# J13-0282 - not split
# J13-0336 - split
# J14-0013 - split
# J14-0097 - split
# J14-0442 - split
# J14-0522 - split
# J14-0587 - split (Ger & BB)
# J15-0013 - split
# J15-0669 - split


# write these to CSV so jessica can quickly remove the lat/long errors in ArcMap
# write.csv(file = "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/scall.obs.trips.sets.csv", scall.obs.trips.sets)

# read them back into R now that Jessica removed the "bad" trips (including the few trips on the US side of Georges)
# she removed 8 sets on the US side, as well as 6 sets that were too deep and too far off the bank. 

scall.obs.trips.sets.cleaned <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/scall.obs.trips.sets.csv", header=T, stringsAsFactors = F)

scall.obs.trips.sets.cleaned$tripset <- paste0(scall.obs.trips.sets.cleaned$trip, "_", scall.obs.trips.sets.cleaned$set_no)

tripset_clean <- unique(scall.obs.trips.sets.cleaned$tripset)
tripset_list <- unique(scall.obs.data.final$tripset)

tripset_list[!is.element( tripset_list, tripset_clean)]

scall.obs.data.final <- scall.obs.data.final[!scall.obs.data.final$tripset %in% 
                                               c("J05-0235_1",   "J06-0486_15",  "J07-0321_104", "J07-0512_3",   
                                                 "J09-0007_17",  "J10-0325_82",  "J10-0434_55",  "J11-0465_24"),]

length(unique(scall.obs.data.final$tripset))
# 11002 trips

# these are our final GB trip sets shown spatially:
ggplot() + geom_point(data=scall.obs.data.final, aes(lon, lat)) + 
  theme_bw() + theme(panel.grid=element_blank())

#### MAKE SURE NUM_HOOK_HAUL IS HERE!! 
scall.obs.trips.sets <- unique(select(scall.obs.data.final, tripset, trip, set_no, vessel_name, latitude, longitude,
                                                            setdate, settime, num_hook_haul, source, pntcd_id, comarea_id))

# write the trip-set dataset (no species). contains 11002 rows
write.csv(scall.obs.trips.sets, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/MASTER_CLEAN_scall.obs.trips.sets.csv")

# write the full dataset (trips, sets, species). contains 153326 rows
write.csv(scall.obs.data.final, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/MASTER_CLEAN_scall.obs.trips.sets.species.csv")
####################################################################
######### Master cleaned tripset list is scall.obs.trips.sets (saved as MASTER_CLEAN_scall.obs.trips.sets.csv above)
######### Master cleaned tripset species list is scall.obs.trips.sets.species (saved as MASTER_CLEAN_scall.obs.trips.sets.species.csv above)
####################################################################

# read in the clean full dataset if not already in environment:
scall.obs.data.final <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/MASTER_CLEAN_scall.obs.trips.sets.species.csv", stringsAsFactors = F, header=T)
if(dim(scall.obs.data.final)[2]==27) {
  scall.obs.data.final <- scall.obs.data.final[,2:27]
}
# get rid of silly row index column
scall.obs.data.final <- select(scall.obs.data.final, -x)

# read in the clean trip-sets dataset if not already in environment:
scall.obs.trips.sets <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/MASTER_CLEAN_scall.obs.trips.sets.csv", stringsAsFactors = F, header=T)
if(dim(scall.obs.trips.sets)[2]==13) {
  scall.obs.trips.sets <- scall.obs.trips.sets[,2:13]
}

# remove the following trips from all data due to species identification errors of flounder noted by Observer
# J05-0004, J05-0014
scall.obs.data.final <- subset(scall.obs.data.final, !trip %in% c("J05-0004", "J05-0014")) # 152955 rows
scall.obs.trips.sets <- subset(scall.obs.trips.sets, !trip %in% c("J05-0004", "J05-0014")) # 10936 rows


### now we're subsetting this final full dataset for each species, so that we have tripset rows where cod and ytf were caught
scall.obs.data.cod <- scall.obs.data.final[scall.obs.data.final$common=="COD(ATLANTIC)",] # 5273 rows
scall.obs.data.ytf <- scall.obs.data.final[scall.obs.data.final$common=="YELLOWTAIL FLOUNDER",] # 6722 rows

### plot spatially for fun
ggplot() + geom_point(data=scall.obs.data.cod, aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank()) + ggtitle("cod sets")
ggplot() + geom_point(data=scall.obs.data.ytf, aes(-longitude, latitude)) + 
  theme_bw() + theme(panel.grid=element_blank()) + ggtitle("ytf sets")

### check kept/discarded
summary(scall.obs.data.cod$est_kept_wt) # should be entirely NA's
summary(scall.obs.data.cod$est_discard_wt) # should contain values, no NA's, no 0's
ggplot() + geom_point(data = scall.obs.data.cod, aes(trip, est_kept_wt)) + theme_bw() + theme(panel.grid=element_blank())
ggplot() + geom_point(data = scall.obs.data.cod, aes(trip, est_discard_wt)) + theme_bw() + theme(panel.grid=element_blank())

summary(scall.obs.data.ytf$est_kept_wt) # should be entirely NA's
summary(scall.obs.data.ytf$est_discard_wt) # should contain values, no NA's, no 0's
ggplot() + geom_point(data = scall.obs.data.ytf, aes(trip, est_kept_wt)) + theme_bw() + theme(panel.grid=element_blank())
ggplot() + geom_point(data = scall.obs.data.ytf, aes(trip, est_discard_wt)) + theme_bw() + theme(panel.grid=element_blank())

write.csv(scall.obs.data.ytf, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/scall.obs.data.ytf.csv")
write.csv(scall.obs.data.cod, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/scall.obs.data.cod.csv")

scall.obs.data.ytf  <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/scall.obs.data.ytf.csv", header=T, stringsAsFactors = F)
scall.obs.data.cod  <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/scall.obs.data.cod.csv", header=T, stringsAsFactors = F)

if(dim(scall.obs.data.ytf)[2]==26) {
  scall.obs.data.ytf <- scall.obs.data.ytf[,2:26]
}
if(dim(scall.obs.data.cod)[2]==26) {
  scall.obs.data.cod <- scall.obs.data.cod[,2:26]
}

# now we need to left-join the trip-sets to our ytf and cod dataframe so that we have 2 dataframes, each with one species' data for every single observed set (even if not detected)
scall.obs.data.cod <- join(scall.obs.trips.sets, scall.obs.data.cod[,c(23,7,11,12,13,14,15)], type="left") # should have 10936 rows. this includes sets where cod=0
scall.obs.data.ytf <- join(scall.obs.trips.sets, scall.obs.data.ytf[,c(23,7,11,12,13,14,15)], type="left") # should have 10936 rows. this includes sets where ytf=0

### next, onto the length frequency tidy up
### we're going to join these in wide format to the cod and ytf separately.
head(fishlengths)

# check out the length bins
fishlengths$tripset <- paste0(fishlengths$trip, "_", fishlengths$set_no)

pdf("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length distribution_cod_2015.pdf", height=5, width=7)
# ggplot() + geom_point(data=fishlengths[fishlengths$common=="COD(ATLANTIC)",], aes(fl_fish_length, fl_num_at_length)) + 
#   theme_bw() + theme(panel.grid=element_blank()) + 
#   ggtitle("COD") + xlab("Fish length bin") + ylab("number at length")
# ggplot() + geom_point(data=fishlengths[fishlengths$common=="COD(ATLANTIC)" & year(ymd(fishlengths$setdate)) == 2006,], aes(fl_fish_length, fl_num_at_length)) + 
#   theme_bw() + theme(panel.grid=element_blank()) + 
#   ggtitle("COD - 2006") + xlab("Fish length bin") + ylab("number at length")
ggplot() + geom_point(data=fishlengths[fishlengths$common=="COD(ATLANTIC)" & year(ymd(fishlengths$setdate)) == 2015,], aes(fl_fish_length, fl_num_at_length)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  ggtitle("COD - 2015") + xlab("Fish length bin") + ylab("number at length")
dev.off()

ytf_check <- fishlengths[fishlengths$common=="YELLOWTAIL FLOUNDER",]
ytf_check$meanlength <- ytf_check$fl_fish_length * ytf_check$fl_num_at_length
ytf_check$year <- year(ymd(ytf_check$setdate))
require(plyr)
ytf_means <- ddply(.data=ytf_check, .(year),
                   summarize,
                   annualmean = (sum(meanlength)/sum(fl_num_at_length)))
ytf_means$sp <- "ytf"
cod_check <- fishlengths[fishlengths$common=="COD(ATLANTIC)",]
cod_check$meanlength <- cod_check$fl_fish_length * cod_check$fl_num_at_length
cod_check$year <- year(ymd(cod_check$setdate))

sp_lengths <- data.frame(lengths = c(rep(cod_check$fl_fish_length, cod_check$fl_num_at_length),
                                      rep(ytf_check$fl_fish_length, ytf_check$fl_num_at_length)),
                         years = c(rep(cod_check$year, cod_check$fl_num_at_length),
                                   rep(ytf_check$year, ytf_check$fl_num_at_length)),
                          sp = c(rep("cod", sum(cod_check$fl_num_at_length)),
                                 rep("ytf", sum(ytf_check$fl_num_at_length))))

pdf("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_timeseries.pdf", onefile=T, width=8, height=5)
ggplot() + geom_boxplot(data=sp_lengths, aes(x=years, y=lengths, group=years)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~sp)

ggplot() + geom_point(data=sp_lengths, aes(x=years, y=lengths, colour=sp)) + 
  geom_smooth(data=sp_lengths, aes(x=years, y=lengths, colour=sp), method="lm") +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~sp)
dev.off()

pdf("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length distribution_ytf.pdf", height=5, width=7)
ggplot() + geom_point(data=fishlengths[fishlengths$common=="YELLOWTAIL FLOUNDER",], aes(fl_fish_length, fl_num_at_length)) + 
  theme_bw() + theme(panel.grid=element_blank()) +
  ggtitle("YELLOWTAIL FLOUNDER") + xlab("Fish length bin") + ylab("number at length")
dev.off()

#### sought feedback on lengths from Irene (on cod) and Heath (on ytf)
#### heath: ytf > 55 are suspect. F to check datasheets
#### irene: cod > 100 are suspect in recent years. All ok.

# are the bins static? I.e. is each bin the same size for each species?
diff(sort(unique(fishlengths$fl_fish_length[fishlengths$common=="COD(ATLANTIC)"]))) 
diff(sort(unique(fishlengths$fl_fish_length[fishlengths$common=="YELLOWTAIL FLOUNDER"]))) 
# unfortunately, the bins are totally random. I think we'll just use bins of 1 cm for cod (since there are no 0.5's), and 0.5 for ytf, since they are much smaller in size 

#### the trip we manually corrected in excel. must check data sheet from greg croft
#### fishlengths[fishlengths$common=="YELLOWTAIL FLOUNDER" & fishlengths$tripset=="J10-0139_32",]


# we need to add extra rows so that we will have empty bins as well. 
fishlength_bins_cod <- expand.grid(tripset = unique(scall.obs.data.cod$tripset), fl_fish_length = seq(min(fishlengths$fl_fish_length[fishlengths$common=="COD(ATLANTIC)"]), max(fishlengths$fl_fish_length[fishlengths$common=="COD(ATLANTIC)"]), by=1))
fishlength_bins_ytf <- expand.grid(tripset = unique(scall.obs.data.ytf$tripset), fl_fish_length = seq(min(fishlengths$fl_fish_length[fishlengths$common=="YELLOWTAIL FLOUNDER"]), max(fishlengths$fl_fish_length[fishlengths$common=="YELLOWTAIL FLOUNDER"]), by=1))

fishlength_bins_cod$tripset <- as.character(fishlength_bins_cod$tripset)
fishlength_bins_ytf$tripset <- as.character(fishlength_bins_ytf$tripset)

# fishlength_bins_cod rows = 1082664; fishlength_bins_ytf rows = 623352

# rows should equal rows of scall.obs.data.cod * n bins cod, OR same for ytf
# 11002 * 99 = 1082664 for cod
# 10936 * 57 = 623352 for YTF

# now join the tripsets to the lengthbins (in long)
fishlength_bins_cod <- join(scall.obs.data.cod, fishlength_bins_cod, type="left") # 1082664 x 18
fishlength_bins_ytf <- join(scall.obs.data.ytf, fishlength_bins_ytf, type="left") # 623352 x 18

# join the actual length data to the lengthbins (in long)
fishlength_bins_cod <- join(fishlength_bins_cod, fishlengths[fishlengths$common =="COD(ATLANTIC)",], type="left") 
# this added 6 rows. This means that fishlengths has multiple num_at_length for the same bin
fishlength_bins_ytf <- join(fishlength_bins_ytf, fishlengths[fishlengths$common =="YELLOWTAIL FLOUNDER",], type="left")
# this added 973 rows. This means that fishlengths has multiple num_at_length for the same bin
View(fishlength_bins_cod)

length(unique(fishlengths[fishlengths$common =="COD(ATLANTIC)",]$tripset))
#[1] 2023
length(unique(fishlengths[fishlengths$common =="YELLOWTAIL FLOUNDER",]$tripset))
#[1] 2874
# why are there multiple records for the same bin in the same tripset
table(fishlengths[fishlengths$common =="COD(ATLANTIC)",]$fl_fish_length)
multi_cod <- ddply(.data=fishlengths[fishlengths$common =="COD(ATLANTIC)",], .(tripset, fl_fish_length),
      summarize,
      nrecords = length(fl_num_at_length))
subset(multi_cod, nrecords > 1)
multi_cod_num <- ddply(.data=fishlengths[fishlengths$common =="COD(ATLANTIC)",], .(tripset, fl_fish_length),
                   summarize,
                   nrecords = length(unique(fl_num_at_length)))
subset(multi_cod_num, nrecords > 1)

# Manual checks in excel and below. Cod errors are all in the same trip. Should I just scrap this trip?:
#fishlengths[fishlengths$common =="COD(ATLANTIC)" & fishlengths$tripset =="J04-0354_11" & fishlengths$fl_fish_length==46,]
# true - sum
#fishlengths[fishlengths$common =="COD(ATLANTIC)" & fishlengths$tripset =="J04-0354_11" & fishlengths$fl_fish_length==53,]
# true - sum
#fishlengths[fishlengths$common =="COD(ATLANTIC)" & fishlengths$tripset =="J04-0354_30" & fishlengths$fl_fish_length==24,]
# true - sum

multi_ytf <- ddply(.data=fishlengths[fishlengths$common =="YELLOWTAIL FLOUNDER",], .(tripset, fl_fish_length),
                   summarize,
                   nrecords = length(fl_num_at_length))
dim(subset(multi_ytf, nrecords > 1))
multi_ytf_num <- ddply(.data=fishlengths[fishlengths$common =="YELLOWTAIL FLOUNDER",], .(tripset, fl_fish_length),
                   summarize,
                   nrecords = length(unique(fl_num_at_length)))
dim(subset(multi_ytf, nrecords > 1))
#fishlengths[fishlengths$common =="YELLOWTAIL FLOUNDER" & fishlengths$tripset =="J04-0354_11" & fishlengths$fl_fish_length==46,]
# true - sum
# based on datasheets for J13-0336, J14-0346 and J14-0215 (both by the same observer), it looks like this occurs when the observer decides to sex the individuals too. He/she writes
# two separate length frequency tallies (tables), one for each species. For this reason, we should be able to sum both numbers. 
# however i should check to see if any have more than 2 records (i.e. more than two sexes...)
subset(multi_ytf, nrecords > 2)
subset(multi_cod, nrecords > 2)
# none have more than 2 species, so I presume that this confusion is all due to keen observers breaking out lengths by sex. 

# empty the bins that are above heath and irene's advice. still need to doublecheck the datasheets though
#### heath: ytf > 55 are suspect. Need to check datasheets
#### irene: cod > 100 are suspect in recent years. So for scallop, all ok, but groundfish need to double check.
fishlength_bins_cod <- fishlength_bins_cod[fishlength_bins_cod$fl_fish_length < 101,]
fishlength_bins_ytf <- fishlength_bins_ytf[fishlength_bins_ytf$fl_fish_length < 56,]

# dim(fishlength_bins_cod)
# 1038926      20
# dim(fishlength_bins_ytf)
# 482157     20

# Long format first though
fishlength_bins_cod[is.na(fishlength_bins_cod$fl_num_at_length),]$fl_num_at_length <-0
fishlength_bins_ytf[is.na(fishlength_bins_ytf$fl_num_at_length),]$fl_num_at_length <-0

length_long_cod <- aggregate(data=fishlength_bins_cod, fl_num_at_length ~ fl_fish_length + tripset, function(x) sum(x))
length_long_ytf <- aggregate(data=fishlength_bins_ytf, fl_num_at_length ~ fl_fish_length + tripset, function(x) sum(x))

length_long_cod <- join(length_long_cod, unique(fishlength_bins_cod[,c(1:17,19)]), type="left")
length_long_ytf <- join(length_long_ytf, unique(fishlength_bins_ytf[,c(1:17,19)]), type="left")

length_long_cod <- length_long_cod[,c(names(fishlength_bins_cod)[c(1:17,19)], "fl_fish_length", "fl_num_at_length")]
length_long_cod <- subset(length_long_cod, !is.na(tripcd_id))
length_long_ytf <- length_long_ytf[,c(names(fishlength_bins_ytf)[c(1:17,19)], "fl_fish_length", "fl_num_at_length")]
length_long_ytf <- subset(length_long_ytf, !is.na(tripcd_id))

write.csv(length_long_cod, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_long_cod_FINAL_FULL.csv")
write.csv(length_long_ytf, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_long_ytf_FINAL_FULL.csv")


# relabel bins before reshaping
fishlength_bins_cod$fl_fish_length <- stringr::str_pad(fishlength_bins_cod$fl_fish_length, 3, "left", pad="0")
fishlength_bins_ytf$fl_fish_length <- stringr::str_pad(fishlength_bins_ytf$fl_fish_length, 3, "left", pad="0")
fishlength_bins_cod$fl_fish_length <- paste0("bin_", fishlength_bins_cod$fl_fish_length)
fishlength_bins_ytf$fl_fish_length <- paste0("bin_", fishlength_bins_ytf$fl_fish_length)

# OK - proceeding with reshaping, with sum aggregation within each bin.
fishlength_bins_cod <- fishlength_bins_cod[,c(1,18,20)]

fishlength_bins_ytf <- fishlength_bins_ytf[,c(1,18,20)]

require(reshape2)
cod_length_wide <- dcast(fishlength_bins_cod, tripset ~ fl_fish_length, sum)
ytf_length_wide <- dcast(fishlength_bins_ytf, tripset ~ fl_fish_length, sum)

######## LENGTH FREQUENCIES FOR COD AND YTF ###########
write.csv(cod_length_wide, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_wide_cod_FINAL.csv")
write.csv(ytf_length_wide, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_wide_ytf_FINAL.csv")

### join the other metadata back in
cod_length_wide_full <- join(cod_length_wide, scall.obs.data.cod, type="left") 
ytf_length_wide_full <- join(ytf_length_wide, scall.obs.data.ytf, type="left") 

### rearrange columns
cod_length_wide_full <- cod_length_wide_full[,c('trip', 'set_no', 'tripset', 'vessel_name', 'latitude', 'longitude', 
                                                'setdate','settime', 'source', 'pntcd_id', 'comarea_id', 'num_hook_haul', 
                                                'speccd_id', 'common', 'est_num_caught', 'est_kept_wt', 'est_discard_wt',
                                                colnames(cod_length_wide_full[2:96]))]

# one step only for ytf
ytf_length_wide_full <- ytf_length_wide_full[,c('trip', 'set_no', 'tripset', 'vessel_name', 'latitude', 'longitude', 'setdate',
                                                'settime', 'source', 'pntcd_id', 'comarea_id', 'num_hook_haul', 'speccd_id',
                                                'common', 'est_num_caught', 'est_kept_wt', 'est_discard_wt', colnames(ytf_length_wide_full[2:45]))]
# head(ytf_length_wide_full)

# sort rows by trip and set
cod_length_wide_full <- arrange(cod_length_wide_full, trip, set_no)
ytf_length_wide_full <- arrange(ytf_length_wide_full, trip, set_no)

######## LENGTH FREQUENCIES FOR COD AND YTF WITH METADATA ###########
write.csv(cod_length_wide_full, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_wide_cod_FULL_FINAL.csv")
write.csv(ytf_length_wide_full, "Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_wide_ytf_FULL_FINAL.csv")


######### Doing some data summaries on these data..
#cod_length_wide_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_wide_cod_FULL_FINAL.csv")
#ytf_length_wide_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_wide_ytf_FULL_FINAL.csv")
#ytf_length_long_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_long_ytf_FINAL_FULL.csv")
#cod_length_long_full <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Observer/Scallop/length_long_cod_FINAL_FULL.csv")
#
library(lubridate)
head(cod_length_wide_full)
head(ytf_length_wide_full)
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

sets.per.month <- aggregate(tripset~month,cod_length_wide_full,FUN = function(x) length(x))
sets.per.year <- aggregate(tripset~year,cod_length_wide_full,FUN = function(x) length(x))
