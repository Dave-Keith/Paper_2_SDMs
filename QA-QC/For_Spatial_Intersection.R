### Tidying SPERA data for Jessica's Spatial Intersections

all_survey_data <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Survey/all_survey_data.csv", stringsAsFactors = F)
groundfish_yt_cod_catch <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/groundfish_yt_cod_catch.csv")
length_wide_cod_FULL_FINAL_gf <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_cod_FULL_FINAL_gf.csv")
length_wide_cod_FULL_FINAL_scal <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_cod_FULL_FINAL_scal.csv")
length_wide_ytf_FULL_FINAL_gf <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_ytf_FULL_FINAL_gf.csv")
length_wide_ytf_FULL_FINAL_scal <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_ytf_FULL_FINAL_scal.csv")
Otter_trawl_catch_and_effort_clean_yt_and_cod <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/Otter_trawl_catch_and_effort_clean_yt_and_cod.csv")

groundfish_yt_cod_catch$lat.dd <- groundfish_yt_cod_catch$lat
groundfish_yt_cod_catch$lon.dd <- groundfish_yt_cod_catch$lon
write.csv(groundfish_yt_cod_catch, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/groundfish_yt_cod_catch_dd.csv")

length_wide_cod_FULL_FINAL_gf$lat.dd <- length_wide_cod_FULL_FINAL_gf$latitude
length_wide_cod_FULL_FINAL_gf$lon.dd <- -length_wide_cod_FULL_FINAL_gf$longitude
write.csv(length_wide_cod_FULL_FINAL_gf, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_cod_FULL_FINAL_gf_dd.csv")

length_wide_cod_FULL_FINAL_scal$lat.dd <- length_wide_cod_FULL_FINAL_scal$latitude
length_wide_cod_FULL_FINAL_scal$lon.dd <- -length_wide_cod_FULL_FINAL_scal$longitude
write.csv(length_wide_cod_FULL_FINAL_scal, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_cod_FULL_FINAL_scal_dd.csv")

length_wide_ytf_FULL_FINAL_gf$lat.dd <- length_wide_ytf_FULL_FINAL_gf$latitude
length_wide_ytf_FULL_FINAL_gf$lon.dd <- -length_wide_ytf_FULL_FINAL_gf$longitude
write.csv(length_wide_ytf_FULL_FINAL_gf, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_ytf_FULL_FINAL_gf_dd.csv")

length_wide_ytf_FULL_FINAL_scal$lat.dd <- length_wide_ytf_FULL_FINAL_scal$latitude
length_wide_ytf_FULL_FINAL_scal$lon.dd <- -length_wide_ytf_FULL_FINAL_scal$longitude
write.csv(length_wide_ytf_FULL_FINAL_scal, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_ytf_FULL_FINAL_scal_dd.csv")

Otter_trawl_catch_and_effort_clean_yt_and_cod$lat.dd <- Otter_trawl_catch_and_effort_clean_yt_and_cod$lat
Otter_trawl_catch_and_effort_clean_yt_and_cod$lon.dd <- Otter_trawl_catch_and_effort_clean_yt_and_cod$lon
write.csv(Otter_trawl_catch_and_effort_clean_yt_and_cod, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/Otter_trawl_catch_and_effort_clean_yt_and_cod_dd.csv")

# survey data is in long format still so we'll need to flip it into wide
length(unique(all_survey_data$unique_set_ID))

# let's check out our spatial information first though
survey_spatial <- select(all_survey_data, survey, year)
survey_spatial <- unique(survey_spatial)

dim(survey_spatial)
# [1] 10975     6

length(unique(survey_spatial$unique_set_ID))
# [1] 10956
# uhoh, we have mismatching numbers. 

sets <- data.frame(table(survey_spatial$unique_set_ID))
test <- sets[sets$Freq>1,]

survey_spatial[survey_spatial$unique_set_ID=="NMFS/Spring-2013-HB-201302-0298",]
all_survey_data[all_survey_data$unique_set_ID %in% test$Var1[1],]
# looks like the coordinates for the same set differ for each species

# we can't tell which are right (and the raw Stranal pull also has this problem) so we don't want to make any changes to coordinates, however we will separate the species into two
# separate datasets to avoid confusion. We did this for all the other datasets, so this is also more consistent. Then we will flip each species' dataset into wide format
# and intersect the sets with the environmental data

all_survey_data_cod <- all_survey_data[all_survey_data$species == "Cod",]
all_survey_data_ytf <- all_survey_data[all_survey_data$species == "Yellowtail",]

# all_survey_data_cod_wide <- dcast(data=all_survey_data_cod, unique_set_ID + lat + lon + strata + slat + slong + set + year + species + survey ~ as.numeric(size), value.var = "number")
# dim(all_survey_data_cod_wide)
# colnames(all_survey_data_cod_wide)[65] <- "Total"
# all_survey_data_cod_wide$lat.dd <- all_survey_data_cod_wide$lat
# all_survey_data_cod_wide$lon.dd <- all_survey_data_cod_wide$lon
# 
# #write.csv(all_survey_data_cod_wide, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/all_survey_data_cod_wide_dd.csv")

all_survey_data_ytf_wide <- dcast(data=all_survey_data_ytf, unique_set_ID + lat + lon + strata + slat + slong + set + year + species + survey ~ size, value.var = "number")
all_survey_data_ytf_wide$lat.dd <- all_survey_data_ytf_wide$lat
all_survey_data_ytf_wide$lon.dd <- all_survey_data_ytf_wide$lon

all_survey_data_spatial <- select(all_survey_data_ytf_wide, unique_set_ID, lat.dd, lon.dd)

length(all_survey_data_spatial$unique_set_ID)

# why doesn't this match the 10956 from above?

# some of the cod sets are named differently from the ytf sets, so even though they are the same set, they have different unique_set_IDs
# using regular expressions to correct this issue.

NMFS/Spring-1973-AL-197303-0198
teststring <- "NMFS/Spring-1973-AL-197303-0198"
sub('^(.{16}).', '\\1', teststring)
################ HERE! NEED TO SUBSET OUT THE MIDDLE BIT!
substring(teststring, 1, 7)

require(plyr)
test2 <- ddply(.data=all_survey_data, .(unique_set_ID),
      summarize,
      numbersp = length(unique(species)))
dim(subset(test2, numbersp >1))
dim(subset(test2, numbersp ==1))
test3 <- subset(test2, numbersp ==1)

test4 <- all_survey_data[all_survey_data$unique_set_ID %in% c(test3$unique_set_ID),]

unique(test4$year)
unique(nchar(test4$unique_set_ID))

unique(test4$species[test4$unique_set_ID %in% unique(test4$unique_set_ID[nchar(test4$unique_set_ID)==31])])

nmfscod <- all_survey_data[all_survey_data$survey %in% c("NMFS/Spring", "NMFS/Fall") & all_survey_data$species=="Cod" & all_survey_data$year %in% c(1973:2006),]
unique(nchar(nmfscod$set))
nmfscod$set_corr <- substr(nmfscod$set, 12, 14)
nmfscod$unique_set_ID_2 <- paste0(nmfscod$survey, "-", nmfscod$year, "-", nmfscod$set_corr)
nmfscod$unique_set_ID <- nmfscod$unique_set_ID_2
nmfscod$set <- nmfscod$set_corr

nmfscod <- select(nmfscod,  -set_corr, -unique_set_ID_2)

allothers <- all_survey_data[!(all_survey_data$survey %in% c("NMFS/Spring", "NMFS/Fall") & all_survey_data$species=="Cod" & all_survey_data$year %in% c(1973:2006)),]

## MUST BE TRUE
dim(nmfscod)[1] + dim(allothers)[1] == dim(all_survey_data)[1]

all_survey_data_setsfixed <- rbind(allothers, nmfscod)
length(unique(all_survey_data_setsfixed$unique_set_ID))

test2 <- ddply(.data=all_survey_data_setsfixed, .(unique_set_ID),
               summarize,
               numbersp = length(unique(species)))
dim(subset(test2, numbersp >1))
dim(subset(test2, numbersp ==1))
test3 <- subset(test2, numbersp ==1)

#fix these remaining ones manually
unique(all_survey_data_setsfixed$set[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="1985"])
unique(all_survey_data_setsfixed$species[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="1985" & all_survey_data_setsfixed$set==201])
# only yellowtail. OK

unique(all_survey_data_setsfixed$set[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="2003"])
# issue with padding zeros
all_survey_data_setsfixed$set[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="2003" & all_survey_data_setsfixed$set %in% (96:99)] <- 
  str_pad(all_survey_data_setsfixed$set[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="2003" & all_survey_data_setsfixed$set %in% (96:99)], 3, "left", pad = "0")
all_survey_data_setsfixed$unique_set_ID[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="2003"] <- 
  paste0(all_survey_data_setsfixed$survey[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="2003"], 
         "-", all_survey_data_setsfixed$year[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="2003"], 
         "-", all_survey_data_setsfixed$set[all_survey_data_setsfixed$survey=="NMFS/Fall" & all_survey_data_setsfixed$year =="2003"])


unique(all_survey_data_setsfixed$set[all_survey_data_setsfixed$survey=="NMFS/Spring" & all_survey_data_setsfixed$year =="1977"])
unique(all_survey_data_setsfixed$species[all_survey_data_setsfixed$survey=="NMFS/Spring" & all_survey_data_setsfixed$year =="1977" & all_survey_data_setsfixed$set==324])
# only cod. OK

test4 <- all_survey_data_setsfixed[all_survey_data_setsfixed$unique_set_ID %in% c(test3$unique_set_ID),]
View(test4)

length(unique(all_survey_data_setsfixed$unique_set_ID))
# 7489

length(unique(all_survey_data_cod$unique_set_ID))
length(unique(all_survey_data_ytf$unique_set_ID))
# this should be correct because we have one extra set for yellowtail and one for cod

all_survey_data_setsfixed$lat.dd <- all_survey_data_setsfixed$lat
all_survey_data_setsfixed$lon.dd <- all_survey_data_setsfixed$lon

# this should go to Christine:
write.csv(all_survey_data_setsfixed, "Y:/Projects/GB_time_area_closure_SPERA/Data/Survey/all_survey_data_setsfixed_dd.csv")

# truncate this df and we'll have coordinates for jessica to intersect
survey_spatial <- unique(select(all_survey_data_setsfixed, unique_set_ID, lat.dd, lon.dd))
dim(survey_spatial)
# looks like there are still a few funky ones, but much better now. 

# this should go to Jessica:
write.csv(survey_spatial, "Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/survey_spatial_dd.csv")



### Post-intersection clean up

### aggregate based on FIELD1, using average for each covariate (due to coordinates on env polygon boundaries)
### use data.table package for large datasets. these are >50 MB. data.table package will help prevent R crashes
aggfunc <- function(rasterdata="groundfish_yt_cod_catchh_dd_rasterdata.txt", 
                    shpdata="groundfish_yt_cod_catchh_dd_shpdata.txt", 
                    dir="/Volumes/FREYA_2/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/",
                    outdata="groundfish_yt_cod_catchh_dd_env_agg3.csv"){
  require(data.table)
  require(plyr)
  shp_catch <- fread(paste0(dir, shpdata))
  print(head(shp_catch))
  print(dim(shp_catch))
  print(length(unique(shp_catch$Field1)))
  print(summary(shp_catch$Field1))
  print(max(shp_catch$Field1))
  print(min(shp_catch$Field1))

  require(dplyr)
  
  shp_catch_agg <<- shp_catch %>%
    select(Field1, SEDNUM:Fall_hIPR) %>%
    group_by(Field1) %>% 
    summarise_all(funs(mean))

  shp_catch_agg <- as.data.frame(shp_catch_agg)
  print(head(shp_catch_agg))
  print(dim(shp_catch_agg))

  rast_catch <- fread(paste0(dir, rasterdata))
  print(head(rast_catch))
  print(length(unique(rast_catch$Field1)))

  full_catch <- full_join(rast_catch, shp_catch_agg)
  print(dim(full_catch))

  SEDIMENT <- unique(select(shp_catch, SEDIMENT, SEDNUM))

  full_catch <<- full_join(full_catch, SEDIMENT)
  print(dim(full_catch))
  write.csv(full_catch, paste0(dir, outdata))
}

### RUN above, but change the variable names as needed. Must do manually. Not as function (below)
aggfunc(rasterdata="groundfish_yt_cod_catchh_dd_rasterdata.txt",
        shpdata="groundfish_yt_cod_catchh_dd_shpdata.txt",
        dir="/Volumes/FREYA_2/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/",
        outdata="groundfish_yt_cod_catchh_dd_env_agg3.csv")

aggfunc(rasterdata="survey_spatial_dd_rasterintersect.txt",
        shpdata="survey_spatial_dd_shpintersect.txt",
        dir="/Volumes/FREYA_2/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/",
        outdata="survey_spatial_dd_env_agg.csv")

names(full_catch)

### something's not quite right though. There are 239 Field1 values missing from shp_catch_agg, so there are 239 records without env data.
head(full_catch)

fieldsrast <- unique(rast_catch$Field1)
missing <- fieldsrast[!fieldsrast %in% c(unique(shp_catch$Field1))]
length(missing)
7517 - 80 # = 7437
# pick one and check it out. picking 2605

rast_catch_2605 <- rast_catch[rast_catch$Field1 == 2605,]
shp_catch_2605 <- shp_catch[shp_catch$Field1 == 2605,]

base_catch <- fread("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/groundfish_yt_cod_catch_dd.csv")
base_catch_2605 <- base_catch[base_catch$lat.dd==41.91667 & base_catch$lon.dd == -65.68333,]
length(unique(base_catch$set_id)) ## 100390
length(unique(base_catch$V1)) ## 100390
base_catch[2605,]
# is it a rounding issue? 
base_catch_2605 <- base_catch[round(base_catch$lat.dd,5)==41.91667 & round(base_catch$lon.dd, 5) == -65.68333,]
# Seems like yes. There are more records here, and this includes the Field1==2605 missing record.
# Is 72717 in the missing vector? Yes. 
missing[missing==72717]
base_catch[72717,]
# How about 20611? No. 
shp_catch[shp_catch$Field1== 20611,]
# 4280? Yes. 

# something fishy with digits and rounding I think... 

# can i find above Field1 in shp_catch
shp_catch[shp_catch$Field1==20611,]
shp_catch[shp_catch$Field1==2605,]
shp_catch[shp_catch$Field1==72717,]
shp_catch[shp_catch$Field1==4280,]
shp_catch[shp_catch$lat_dd==41.91667 & shp_catch$lon_dd== -65.68333,]

shp_catch[shp_catch$set_id == 94793,]
shp_catch[shp_catch$set_id == 903451,]
### SETS ARE MISSING FROM SHP_CATCH

require(rgdal)
require(rgeos)
require(ggplot2)
shp <- readOGR("/Volumes/FREYA_2/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/shp/survey_spatial_dd.shp")
head(shp)
dim(shp)
shp <- as.data.frame(shp)

shp[shp$set_id == 94793,]
length(unique(shp$Field1))

dim(unique(select(shp_catch, lat_dd, lon_dd)))
dim(unique(select(rast_catch, lat_dd, lon_dd)))

missingshp <- shp[shp$Field1 %in% missing,]

png("/Volumes/FREYA_2/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/points outside environmental boundary.png",
    width=6, height=6, res=100, units="in")
ggplot() + 
  geom_point(data=shp[!shp$Field1 %in% missing,], aes(lon_dd, lat_dd))+
  geom_point(data=missingshp, aes(lon_dd, lat_dd), colour="red") + coord_map() +
  theme_bw()
dev.off()

## anyway let's make sure that the variables are all present. compare to EnvironmentalDataList_SPERA.xlsx
## run the following and make sure none return "character(0)"
names(full_catch)[names(full_catch) %in% "bathy_bank"]
names(full_catch)[names(full_catch) %in% "slope_bank"]
names(full_catch)[names(full_catch) %in% "q1_bank"]
names(full_catch)[names(full_catch) %in% "q1_old_ban"]
names(full_catch)[names(full_catch) %in% "bs_ext_ban"]
names(full_catch)[names(full_catch) %in% "aspect_ban"]
names(full_catch)[names(full_catch) %in% "bpi_5x10"]
names(full_catch)[names(full_catch) %in% "bpi_1x5"]
names(full_catch)[names(full_catch) %in% "bpi_10x50"]
names(full_catch)[names(full_catch) %in% "curv_bank"]
names(full_catch)[names(full_catch) %in% "comldepth"]
names(full_catch)[names(full_catch) %in% "comlslope"]
names(full_catch)[names(full_catch) %in% "comlaspect"]
names(full_catch)[names(full_catch) %in% "botstr_wt"]
names(full_catch)[names(full_catch) %in% "botstr_t"]
names(full_catch)[names(full_catch) %in% "bt_avg96"]
names(full_catch)[names(full_catch) %in% "bt_rg96"]
names(full_catch)[names(full_catch) %in% "chl_avg"]
names(full_catch)[names(full_catch) %in% "chl_rg"] #### not in survey
names(full_catch)[names(full_catch) %in% "complexity"]
names(full_catch)[names(full_catch) %in% "k490_avg"]
names(full_catch)[names(full_catch) %in% "k490_rg"]
names(full_catch)[names(full_catch) %in% "nit_avg96"]
names(full_catch)[names(full_catch) %in% "phos_avg96"]
names(full_catch)[names(full_catch) %in% "sal_avg96"]
names(full_catch)[names(full_catch) %in% "sal_rg96"]
names(full_catch)[names(full_catch) %in% "sil_avg96"]
names(full_catch)[names(full_catch) %in% "sst_avg"]
names(full_catch)[names(full_catch) %in% "sst_rg"]
names(full_catch)[names(full_catch) %in% "strat96"]
names(full_catch)[names(full_catch) %in% "strat_sum9"]
names(full_catch)[names(full_catch) %in% "gravel"]
names(full_catch)[names(full_catch) %in% "Mud"]
names(full_catch)[names(full_catch) %in% "sand"]
names(full_catch)[names(full_catch) %in% "growth"]
names(full_catch)[names(full_catch) %in% "disturb"]
names(full_catch)[names(full_catch) %in% "SEDIMENT"]
names(full_catch)[names(full_catch) %in% "SEDNUM"]
names(full_catch)[names(full_catch) %in% "Year_median"]
names(full_catch)[names(full_catch) %in% "Winter_median"]
names(full_catch)[names(full_catch) %in% "Spring_median"]
names(full_catch)[names(full_catch) %in% "Summer_median"]
names(full_catch)[names(full_catch) %in% "Fall_median"]
names(full_catch)[names(full_catch) %in% "Year_95perc"]
names(full_catch)[names(full_catch) %in% "Winter_95perc"]
names(full_catch)[names(full_catch) %in% "Spring_95perc"]
names(full_catch)[names(full_catch) %in% "Summer_95perc"]
names(full_catch)[names(full_catch) %in% "Fall_95perc"]
names(full_catch)[names(full_catch) %in% "Year_hIPR"] ### not in survey
names(full_catch)[names(full_catch) %in% "Winter_hIPR"] ### not in survey
names(full_catch)[names(full_catch) %in% "Spring_hIPR"] ### not in survey
names(full_catch)[names(full_catch) %in% "Summer_hIPR"] ### not in survey
names(full_catch)[names(full_catch) %in% "Fall_hIPR"] ### not in survey

## all environmental variables in EnvironmentalDataList_SPERA.xlsx seem to be accounted for. 
head(full_catch)
71-16 # 55
# the two "extra" variables are FIDs for intersections with GMAINE data


## checking dim of observer data
length_wide_cod_FULL_FINAL_gf <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_cod_FULL_FINAL_gf_dd.csv")
length_wide_cod_FULL_FINAL_scal <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_cod_FULL_FINAL_scal_dd.csv")
length_wide_ytf_FULL_FINAL_gf <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_ytf_FULL_FINAL_gf_dd.csv")
length_wide_ytf_FULL_FINAL_scal <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/length_wide_ytf_FULL_FINAL_scal_dd.csv")

groundfish_yt_cod_catch <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/groundfish_yt_cod_catch_dd.csv")
Otter_trawl_catch_and_effort_clean_yt_and_cod <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/For_Spatial_Intersection/Otter_trawl_catch_and_effort_clean_yt_and_cod_dd.csv")

dim(groundfish_yt_cod_catch)
dim(Otter_trawl_catch_and_effort_clean_yt_and_cod)

head(groundfish_yt_cod_catch)
head(Otter_trawl_catch_and_effort_clean_yt_and_cod)

Otter_trawl_catch_and_effort_clean_yt_and_cod$cod_kg_e <- Otter_trawl_catch_and_effort_clean_yt_and_cod$cod_kg
Otter_trawl_catch_and_effort_clean_yt_and_cod$yt_kg_e <- Otter_trawl_catch_and_effort_clean_yt_and_cod$yt_kg

catchcheck <- join(select(groundfish_yt_cod_catch, set_id:lon.dd), select(Otter_trawl_catch_and_effort_clean_yt_and_cod, set_id, cod_kg_e:yt_kg_e), type="left")

catchcheck[which(!(catchcheck$cod_kg == catchcheck$cod_kg_e)),] ## problems with cod catch
dim(catchcheck[which(!(catchcheck$cod_kg == catchcheck$cod_kg_e)),]) ## problems with cod catch

catchcheck[which(!(catchcheck$yt_kg == catchcheck$yt_kg_e)),] ## ytf catch all matches

unique(catchcheck$cod_kg_e[which(!(catchcheck$cod_kg == catchcheck$cod_kg_e))]) ## cod catch is 0 in the mismatched records from effort
unique(catchcheck$set_id[which(!(catchcheck$cod_kg == catchcheck$cod_kg_e))])

all.cod.yt.sets <- read.csv("Y:/Projects/GB_time_area_closure_SPERA/Data/Groundfish/groundfish_yt_cod_catch.csv")

all.cod.yt.sets[all.cod.yt.sets$set_id == 7095,]
