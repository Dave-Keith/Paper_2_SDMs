# DK created Oct 2017 to do some formal QA/QC for meeting with SABS

# Question:  What are main differences b/t nmfs and RV survey?
# Answer:  Timing,vessel and gear make it so we can't really combine the two surveys, but otherwise trends should be comparable  for the two surveys analyized seperately.
# Question:  How are the two surveys integrated for the assessments, do you think we could combine the two sources of information into one and if we could
#            what do we need to be careful about.
# We should not try to integrate the two survey numbers as catchabilty b/t vessels is uncertain.  We also should focus on mature individuals, for the purposes
# of these analyses we should be thikning about mature individuals, so we are pulling the length by set information and looking at cod that are over 42 cm in size 
# (maybe move to 43 cm as I think there is a bin issue for nmfs).  
# Question:  How has the survey changed over time, how would any changes impact these data
# Answer, no changes to DFO survey in the time period (1986 onwards) that we are looking at.  The nmfs survey had an important change in gear type in 2009, there
# is a somewhat contenuous debate about conversion factors for different sized, but long and short of it is that we need to use numbers and not biomass from the catch




rm(list=ls())
source(".Rprofile")
direct.fun <- "F:/local_backup/r/"
direct.fig <- "F:/local_backup/Projects/GB_time_area_closure_SPERA/"
direct.dat <- "F:/local_backup/Projects/GB_time_area_closure_SPERA/Data/Survey/"


library(INLA)
library(boot)
library(fields)
library(PBSmapping)
library(ggplot2)
library(xlsx)
library(reshape2)
library(plyr)
library(mapproj)
inla.upgrade(testing = F)
source(paste(direct.fun,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep=""))
source(paste(direct.fun,"Assessment_fns/maps/ScallopMap.r",sep=""))

###################################### Section 1:  Load in the data from RV and nmfs surveys


species <- c("Cod","Yellowtail")
surveys <- c("RV_survey","NMFS/Spring","NMFS/Fall")
n.species <- length(species)
locs <- paste0(direct.dat,surveys)
n.surveys <- length(locs)

length_by_set <- NULL
# Pull in all the rv survey files
for (i in 1:n.surveys)
{
  for(j in 1:n.species)
  {
    files <- NULL
    files <- list.files(paste0(locs[i],"/",species[j]))
  
    for(k in 1:length(files))
    {
      if(surveys[i] == "RV_survey") year <- 1986 + k
      if(length(grep("NMFS",surveys[i])) == 1) year <- 1969 + k
      # This will give a few NA's as some of the data files have a "total" for the entire survey, I'll remove those at the end...
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]] <- read.xlsx(paste0(paste0(locs[i],"/",species[j]),"/", files[k]), sheetName = "Length by Set")
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$year <- year
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$survey <- surveys[i]
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$species <- species[j]
      # Get rid of the UNITAREA column for those data in which is exists
      if(any(names(length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]) == "UNITAREA")) 
      {
        drop <- which(names(length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]) == "UNITAREA")
        length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]] <- length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]][,-drop]
      } # end if(any(names(length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]) == "UNITAREA"))                                                                           
      # Now do some Freya magic and make this long formate
      
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]] <- reshape2::melt(length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]], 
                                                                                    id.vars=c("STRATA", "SLAT", "SLONG",  "SET", "year","species","survey"))
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$variable <- gsub(length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$variable, 
                                                                                   pattern="X", replacement="")
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$SET <- as.character(length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$SET)
      length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$STRATA <- as.character(length_by_set[[paste0(surveys[i],"/",species[j],"/",year)]]$STRATA)
      
    } # end for(k in 1:length(files))
  } # end for(j in 1:n.species)
} # end for (i in 1:length(rv.files))

# Now stick this all into one gigantic dataframe
dat.length.by.set <- do.call("rbind",length_by_set)
names(dat.length.by.set) <- tolower(names(dat.length.by.set))
# There are some NA's that are in the data, these are from the xlsx data in which there are occasionally column totals (NMFS only) for the entire survey 
# these don't have a lat/lon so we can just toss these ones out...
dat.length.by.set <- dat.length.by.set[which(!is.na(dat.length.by.set$slat)),]
# Get a couple better variable names...

names(dat.length.by.set)[names(dat.length.by.set) == "variable"] <- "size"
names(dat.length.by.set)[names(dat.length.by.set) == "value"] <- "number"

# Some of the lat/lon coordinate are incorrect, the vast majority of cases are a truncation of a 0 when being pulled from the database, these corrections fix the issue.
lat.wrong <- unique(dat.length.by.set$slat[which(dat.length.by.set$slat < 1000)])
lon.wrong <- c(unique(dat.length.by.set$slon[which(dat.length.by.set$slon > 7000)]),unique(dat.length.by.set$slon[which(dat.length.by.set$slon < 1000)]))

for(i in 1:length(lat.wrong))
{

    dat.length.by.set$slat[dat.length.by.set$slat == lat.wrong[i]] <- ifelse(dat.length.by.set$slat[dat.length.by.set$slat== lat.wrong[i]] < 100,
                                                  as.numeric(paste0(substr(lat.wrong[i],1,2),0,0,substr(lat.wrong[i],3,10))),
                                                  as.numeric(paste0(substr(lat.wrong[i],1,2),0,substr(lat.wrong[i],3,10))))
} # end for(i in 1:length(lat.wrong))

# And similarly for the messed up longitudes, 
for(i in 1:length(lon.wrong))
{
  #first there is one that is simply an obvious inversion/typo so change that to 6811, others are same change as the bulk of the latitude issues.
  dat.length.by.set$slong[dat.length.by.set$slong == lon.wrong[i]] <- ifelse(dat.length.by.set$slong[dat.length.by.set$slong== lon.wrong[i]] > 8600,6811,
                                                                             as.numeric(paste0(substr(lon.wrong[i],1,2),0,substr(lon.wrong[i],3,10))))
}

# If the data is from NMFS I need to do this....
dat.length.by.set$lat <- convert.dd.dddd(dat.length.by.set$slat)
dat.length.by.set$lon <- -convert.dd.dddd(dat.length.by.set$slong)
dat.length.by.set$unique_set_ID <- paste0(dat.length.by.set$survey,"-",dat.length.by.set$year,"-",dat.length.by.set$set)

# I need to remove one data point from the nmfs fall and spring survey, the weird set is  for cod AL-197411-0216 and 216 (in 1974) for yellowtail
# can't exactly say what is wrong with these, just obvious the locataion information is wrong.
dat.length.by.set <- dat.length.by.set[which(dat.length.by.set$set != "AL-197411-0216"),]
dat.length.by.set <- dat.length.by.set[-which(dat.length.by.set$year == 1974 & dat.length.by.set$set == "216"),]



nmfs.spring.yt <- dat.length.by.set[dat.length.by.set$survey == "NMFS/Spring" & dat.length.by.set$species == "Yellowtail", ]
nmfs.fall.yt <- dat.length.by.set[dat.length.by.set$survey == "NMFS/Fall" & dat.length.by.set$species == "Yellowtail", ]
nmfs.spring.cod <- dat.length.by.set[dat.length.by.set$survey == "NMFS/Spring" & dat.length.by.set$species == "Cod", ]
nmfs.fall.cod <- dat.length.by.set[dat.length.by.set$survey == "NMFS/Fall" & dat.length.by.set$species == "Cod", ]
rv.cod <- dat.length.by.set[dat.length.by.set$survey == "RV_survey" & dat.length.by.set$species == "Cod", ]
rv.yt <- dat.length.by.set[dat.length.by.set$survey == "RV_survey" & dat.length.by.set$species == "Yellowtail", ]


  
# Now save all of these files
write.csv(nmfs.spring.cod,paste0(direct.dat,"nmfs_spring_cod.csv"),row.names=F)
write.csv(nmfs.fall.yt,paste0(direct.dat,"nmfs_fall_yt.csv"),row.names=F)
write.csv(nmfs.spring.yt,paste0(direct.dat,"nmfs_spring_yt.csv"),row.names=F)
write.csv(nmfs.fall.cod,paste0(direct.dat,"nmfs_fall_cod.csv"),row.names=F)
write.csv(rv.cod,paste0(direct.dat,"rv_cod.csv"),row.names=F)
write.csv(rv.yt,paste0(direct.dat,"rv_yt.csv"),row.names=F)
write.csv(dat.length.by.set,paste0(direct.dat,"all_survey_data.csv"),row.names=F)
################################  END of DATA ENTRY ###############################################  END of DATA ENTRY ###############


############################  Section 2 QA/QC############################  Section 2 QA/QC############################  Section 2 QA/QC
nmfs.spring.cod<- read.csv(paste0(direct.dat,"nmfs_spring_cod.csv"),stringsAsFactors = F)
nmfs.fall.yt <- read.csv(paste0(direct.dat,"nmfs_fall_yt.csv"),stringsAsFactors = F)
nmfs.spring.yt <- read.csv(paste0(direct.dat,"nmfs_spring_yt.csv"),stringsAsFactors = F)
nmfs.fall.cod <- read.csv(paste0(direct.dat,"nmfs_fall_cod.csv"),stringsAsFactors = F)
rv.cod <- read.csv(paste0(direct.dat,"rv_cod.csv"),stringsAsFactors = F)
rv.yt <- read.csv(paste0(direct.dat,"rv_yt.csv"),stringsAsFactors = F)
dat.length.by.set<- read.csv(paste0(direct.dat,"all_survey_data.csv"),stringsAsFactors = F)


# First lets see if the data is where it should be!
# Question:  Based on this map is there anything that seems odd from the RV survey data?
windows(11,11)
ScallopMap(xlim = c(-72,-54),ylim=c(39,45),shore = 'nwatlMR',direct=direct.fun)
points(rv.cod$lon,rv.cod$lat,pch=19,cex=0.2)
points(rv.yt$lon,rv.yt$lat,pch=19,cex=0.2)
# Note the mis-match in area between the NMFS and RV survey locations, not a biggy, can remove RV survey at a later date if so desired.
points(nmfs.fall.cod$lon,nmfs.fall.cod$lat,pch=19,cex=0.2)

points(nmfs.fall.yt$lon,nmfs.fall.yt$lat,pch=19,cex=0.2)

points(nmfs.spring.cod$lon,nmfs.spring.cod$lat,pch=19,cex=0.2)
points(nmfs.spring.yt$lon,nmfs.spring.yt$lat,pch=19,cex=0.2)


# Question:  Based on this map is there anything that seems odd from the nmfs survey data?
# Answer:  No this looks o.k., note that the areas selected differ between the surveys but this is fine, the extent
#          of the nmfs survey data differs from DFO but that should be fine.
# Question:  Why does the RV survey have data in places that the nmfs survey does not, are these incorrect locations
#            or simply did the nmfs survey pull not include certain regions?
# Answer, see above, all good.
# Question:  Based on above answer, what should we be using as our extent and if we need to increase range of nmfs scope is that a problem?
# Answer:  we area ll good, no worries.


#################  First lets do some QA/QC on the Survey data ############################
# Now some quick summaries of these data...
dim(dat.length.by.set)
table(dat.length.by.set$survey)
table(dat.length.by.set$species)

num.sets <- length(unique(dat.length.by.set$set))
num.sets.by.survey  <- aggregate(set ~ set+survey,dat.length.by.set,function(x) length(unique(x)))  # Around 2500 sets over the history of each survey.
num.sets.by.survey.year <- aggregate(set ~ year + set+survey,dat.length.by.set,function(x) length(unique(x))) # these make sense too happily
mean.sets.by.survey <- aggregate(set ~ survey,num.sets.by.survey.year,FUN=mean)
range_sets.by.survey <- aggregate(set ~ survey,num.sets.by.survey.year,FUN=range)

# so we have this many trips for each survey, does this seem correct?


# Now lets check out the total catch information and see if it makes sense
dat.total.by.set <- dat.length.by.set[dat.length.by.set$size == "TOTAL",]

# Love the 2008-2009 yellowtail on RV survey, nuts...
max.sets.by.survey.year.species <- aggregate(number~year+survey + species,dat.total.by.set, FUN = max)
mean.sets.by.survey.year.species <- aggregate(number~year+survey + species,dat.total.by.set, FUN = mean)
median.sets.by.survey.year.species <- aggregate(number~year+survey + species,dat.total.by.set, FUN = median)
range.sets.by.survey.species <- aggregate(number~survey+species,dat.total.by.set, FUN = range)
mean.sets.by.survey.species <- aggregate(number~survey+species,dat.total.by.set, FUN = mean)
median.sets.by.survey.species <- aggregate(number~survey+species,dat.total.by.set, FUN = median)

windows(11,11)
p <- ggplot(dat.total.by.set, aes(year,number)) + facet_wrap(~survey+species,scales = "free") + geom_point()
p

# Given how much these data have been through I can't really think of much else to do with these.
# Let's look just at the mature data... 43 cm for cod (Pers. Comm Don Clark) and 32 cm for yellowtail flounder (DFO website...)
# Let's look at what size we have mature individuals from the RV survey for the yellowtail.

head(rv.yt)
hist(rv.yt$number[grep("M",rv.yt$size)],xlim=20)

M.mat <- data.frame(prop = (rv.yt$number[grep("F",rv.yt$size)]) / 
                                               (rv.yt$number[grep("F",rv.yt$size)] + rv.yt$number[grep("U",rv.yt$size)]),
                    size = rv.yt$size[grep("F",rv.yt$size)])

# Can see that at 23 cm we can pretty much always identify the sex of the Yellowtail, both for Males and Females, but
# that doesn't tell us if they are mature yet or not.
windows(11,11)
p <- ggplot(M.mat, aes(size,prop)) + geom_point() #+ scale_x_continuous(limits=c(0,10))
p

### This bit was done by FK, DK to reproduce with new code....

min(rv.cod$size, na.rm=T)
max(rv.cod$size, na.rm=T)

bins <- sort(as.numeric(unique(rv.cod$size)))
diffs <- bins[53:2] - bins[52:1]
## all bins are 3 cm (?)

# lengthbyset_cod <- expand.grid(SET = rv.cod$set, size = seq(4, 160, by=3), number2=0)
# lengthbyset_cod <- join(lengthbyset_cod, rv.cod, type="left")
# 
# lengthbyset_cod <- join(rv.cod, lengthbyset_cod, type="full")
# lengthbyset_cod$number_complete <- ifelse(is.na(lengthbyset_cod$number)==TRUE, lengthbyset_cod$number2, lengthbyset_cod$number)


lengthbreaks <- seq(10,140, 10)

plot.list <- NULL

for (i in 1:length(lengthbreaks))
{
  subset1 <- rv.cod[rv.cod$size > lengthbreaks[i] & rv.cod$size != "TOTAL",]
  p <- ggplot() + geom_point(data=subset1, aes(lon, lat)) + coord_map() +
  ggtitle(paste0("length > ", lengthbreaks[i])) +
  theme_bw() + theme(panel.grid=element_blank())
  
  plot.list[[i]] <- p
}

pdf("D:/local_backup/Projects/GB_time_area_closure_SPERA/Results/Figures/Data_QA_QC/Spatial_length_QAQC.pdf", width=5, height=5)
print(plot.list)
dev.off()

# # sets by length break
# 
# ggplot() + geom_point(data=subsetted, aes(lengthbreaks, number_of_sets)) + 
#   theme_bw() + theme(panel.grid=element_blank())
# 
# ggplot() + geom_point(data=lengthbyset_cod[lengthbyset_cod$number_complete>0,], aes(size, number_complete, colour=as.factor(year))) +
#   theme_bw() + theme(panel.grid=element_blank())
