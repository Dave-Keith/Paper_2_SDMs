#Here, create  useable summary datasets - with number and PA for Cod and YT, for each unique_set, and with environmental variables 

#For each dataset, for each UNIQUE_SET, for COD and YT, get total NUMBER and PA, for TOTAL, ADULT, and JUVENILE, with all environmental variables for each set
  #-	1) RV survey:  1987-2016; 
  #−	2) NMFS Spring survey:  1970-2016
  #−	3) NMFS Fall survey : 1970-2016
  #−	4) Groundfish commercial data - all 2003-2016
  #−	5) Groundfish commercial data - otter and effort 2003-2016
  #−	6) Groundfish observer data - 2003-2016
  #−	7) Scallop observer data - (presence/absence only)

#############################################
#############################################
### RV SURVEY DATASET 
#############################################
#############################################
#RV survey:  1987-2016; 
#Presence/Absence AND Numbers; FOR each of i) all sizes ii) juvenile, iii) adult for i) Cod and ii) Yellowtail
#Yellowtail has different length at maturity - some extra columns added to accomodate this. 

rm(list=ls())

library(stringr)
library(dplyr)

#direct.fun <- "d:/r/"
path <- "/Users/user/DFO_2017/"
path <- "Y:/Projects/GB_time_area_closure_SPERA/"

surv.dat <- read.csv(paste(path,"Data/Survey/all_survey_data_setsfixed_dd.csv",sep=""),stringsAsFactors = F)
env.dat <-read.csv(paste(path,"Data/For_Spatial_Intersection/survey_spatial_dd_env_agg.csv",sep=""),stringsAsFactors = F)
 
####################################
####################################
##COD COD COD COD ##COD COD COD COD##COD COD COD COD##COD COD COD COD##COD COD COD COD##COD COD COD COD
####Get COD data for ALL, Juvenile and Adults 

#First, grab the correct Survey data, for Cod data (size is not numeric b/c has U, M and F)
rv_c <- surv.dat[surv.dat$survey == "RV_survey" & surv.dat$species == "Cod",]

#Grab the "Total" sizes
rv.t <- rv_c[rv_c$size == "TOTAL",]
#Grab the non- "TOTAL" sizes
rv.n <- rv_c[rv_c$size != "TOTAL",]
rv.n$size <- as.numeric(rv.n$size)
#Now, separate non- "TOTAL" into adult and juvenile
rv.j <- rv.n[rv.n$size < 43 ,]
rv.a <- rv.n[rv.n$size >= 43 ,]

##For each age class, except total (which is already unique), sum the total numbers by unique_set_ID 
rv.t_set <- rv.t %>%
  group_by(unique_set_ID) %>%
  summarize(lat = unique(lat), 
            lon = unique(lon),
            survey = unique(survey),
            year = unique(year),
            set = unique(set),
            slat = unique(slat),
            slong = unique(slong), 
            strata = unique(strata),
            COD_number_Tot = sum(number)
)

rv.j_set <- rv.j %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Juv = sum(number)
  )
rv.a_set <- rv.a %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Adu = sum(number)
  )

#for each - add a presence/absence column
rv.t_set$COD_PA_Tot <- as.numeric(ifelse(rv.t_set$COD_number_Tot == 0, 0, ifelse(rv.t_set$COD_number_Tot > 0, 1, "")))
rv.j_set$COD_PA_Juv <- as.numeric(ifelse(rv.j_set$COD_number_Juv == 0, 0, ifelse(rv.j_set$COD_number_Juv > 0, 1, "")))
rv.a_set$COD_PA_Adu <- as.numeric(ifelse(rv.a_set$COD_number_Adu == 0, 0, ifelse(rv.a_set$COD_number_Adu > 0, 1, "")))
            
#Merge all together
rv_use<-list(rv.t_set, rv.j_set, rv.a_set) %>%
  Reduce(function(rv.t_set, rv.j_set, rv.a_set) full_join(rv.t_set, rv.j_set, rv.a_set, by="unique_set_ID"), .)

##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT 

###Get RV YT data for ALL(rv.t), Juvenile(rv.j) and Adults (rv.o)
#First, grab the RV Cod data (size is not numeric b/c has U, M and F)
rv_y <- surv.dat[surv.dat$survey == "RV_survey" & surv.dat$species == "Yellowtail",]

##Yellowtail is specified as U, M, F, and none. 
rv_y$size2 <- sub('U.','',rv_y$size) #Immature
rv_y$size2 <- sub('F.','',rv_y$size2) #Female
rv_y$size2 <- sub('M.','',rv_y$size2) #Male

#Grab the "Total" sizes
rv.t <- rv_y[rv_y$size == "TOTAL",]
#Grab the non- "TOTAL" sizes
rv.n <- rv_y[rv_y$size != "TOTAL",]
rv.n$size2 <- as.numeric(rv.n$size2)
#Now, separate non- "TOTAL" into adult and juvenile
rv.j <- rv.n[rv.n$size2 < 32 ,]
rv.a <- rv.n[rv.n$size2 >= 32 ,]

##For each age class, except total (which is already unique), sum the total numbers by unique_set_ID 
rv.t_set <- rv.t %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Tot = sum(number)
  )

rv.j_set <- rv.j %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Juv = sum(number)
  )
rv.a_set <- rv.a %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Adu = sum(number)
  )

#for each - add a presence/absence column
rv.t_set$YT_PA_Tot <- as.numeric(ifelse(rv.t_set$YT_number_Tot == 0, 0, ifelse(rv.t_set$YT_number_Tot > 0, 1, "")))
rv.j_set$YT_PA_Juv <- as.numeric(ifelse(rv.j_set$YT_number_Juv == 0, 0, ifelse(rv.j_set$YT_number_Juv > 0, 1, "")))
rv.a_set$YT_PA_Adu <- as.numeric(ifelse(rv.a_set$YT_number_Adu == 0, 0, ifelse(rv.a_set$YT_number_Adu > 0, 1, "")))

#Merge all together
rv_use2<-list(rv.t_set, rv.j_set, rv.a_set) %>%
  Reduce(function(rv.t_set, rv.j_set, rv.a_set) full_join(rv.t_set, rv.j_set, rv.a_set, by="unique_set_ID"), .)

rv_use3<-list(rv_use, rv_use2) %>%
  Reduce(function(rv_use, rv_use2) full_join(rv_use, rv_use2, by="unique_set_ID"), .)


#######################Separate M and F - adults and juveniles - number and P/A for each size. 

rv_y <- surv.dat[surv.dat$survey == "RV_survey" & surv.dat$species == "Yellowtail",]

#Remove the "TOTAL" sizes
rv.n <- rv_y[rv_y$size != "TOTAL",]

##Yellowtail: Make a new column of U, M, F, based on the size column
rv.n$sex <- gsub("\\..*","",rv.n$size)


#Yellowtail: remove the U. F. and M. from the size values
rv.n$size2 <- sub('U.','',rv.n$size)
rv.n$size2 <- sub('F.','',rv.n$size2)
rv.n$size2 <- sub('M.','',rv.n$size2)

rv.n$size2 <- as.numeric(rv.n$size2)

#Use conditions to select Female & adult, Male & Adult, Female and juvenile, Male and Juvenile, All U. 
#females maturity is 26 cm, for males it is 32 cm
rv.n["cat"] <- NA
rv.n$cat[rv.n$sex == "F" & rv.n$size2 < 26] <- "FJ" 
rv.n$cat[rv.n$sex == "F" & rv.n$size2 >= 26] <- "FA"
rv.n$cat[rv.n$sex == "M" & rv.n$size2 < 32] <- "MJ"
rv.n$cat[rv.n$sex == "M" & rv.n$size2 >= 32] <- "MA"
rv.n$cat[rv.n$sex == "U" & rv.n$size2 < 32] <- "UJ" #cut off at the male to be sure adult
rv.n$cat[rv.n$sex == "U" & rv.n$size2 >= 32] <- "UA"


#Now, separate M, F, U, and for each age into new datasets. 
rv.f <- rv.n[rv.n$sex == "F" ,]
rv.m <- rv.n[rv.n$sex == "M" ,]
rv.u <- rv.n[rv.n$sex == "U" ,]
rv.fj <- rv.n[rv.n$cat == "FJ" ,]
rv.fa <- rv.n[rv.n$cat == "FA" ,]
rv.mj <- rv.n[rv.n$cat == "MJ" ,]
rv.ma <- rv.n[rv.n$cat == "MA" ,]
rv.uj <- rv.n[rv.n$cat == "UJ" ,]
rv.ua<- rv.n[rv.n$cat == "UA" ,]

##For each M/F and age class, sum the total numbers by unique_set_ID 
rv.f_set <- rv.f %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_F = sum(number)
  )

rv.fa_set <- rv.fa %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_FA = sum(number)
  )

rv.fj_set <- rv.fj %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_FJ = sum(number)
  )

rv.m_set <- rv.m %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_M = sum(number)
  )

rv.ma_set <- rv.ma %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_MA = sum(number)
  )

rv.mj_set <- rv.mj %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_MJ = sum(number)
  )

rv.u_set <- rv.u %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_U = sum(number)
  )

rv.ua_set <- rv.ua %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_UA = sum(number)
  )

rv.uj_set <- rv.uj %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_UJ = sum(number)
  )

#for each - add a presence/absence column
rv.f_set$YT_PA_F <- as.numeric(ifelse(rv.f_set$YT_number_F == 0, 0, ifelse(rv.f_set$YT_number_F > 0, 1, "")))
rv.fa_set$YT_PA_FA <- as.numeric(ifelse(rv.fa_set$YT_number_FA == 0, 0, ifelse(rv.fa_set$YT_number_FA > 0, 1, "")))
rv.fj_set$YT_PA_FJ <- as.numeric(ifelse(rv.fj_set$YT_number_FJ == 0, 0, ifelse(rv.fj_set$YT_number_FJ > 0, 1, "")))
rv.m_set$YT_PA_M <- as.numeric(ifelse(rv.m_set$YT_number_M == 0, 0, ifelse(rv.m_set$YT_number_M > 0, 1, "")))
rv.ma_set$YT_PA_MA <- as.numeric(ifelse(rv.ma_set$YT_number_MA == 0, 0, ifelse(rv.ma_set$YT_number_MA > 0, 1, "")))
rv.mj_set$YT_PA_MJ <- as.numeric(ifelse(rv.mj_set$YT_number_MJ == 0, 0, ifelse(rv.mj_set$YT_number_MJ > 0, 1, "")))
rv.u_set$YT_PA_U <- as.numeric(ifelse(rv.u_set$YT_number_U == 0, 0, ifelse(rv.u_set$YT_number_U > 0, 1, "")))
rv.ua_set$YT_PA_UA <- as.numeric(ifelse(rv.ua_set$YT_number_UA == 0, 0, ifelse(rv.ua_set$YT_number_UA > 0, 1, "")))
rv.uj_set$YT_PA_UJ <- as.numeric(ifelse(rv.uj_set$YT_number_UJ == 0, 0, ifelse(rv.uj_set$YT_number_UJ > 0, 1, "")))

#Merge all together
rv_use_f<-list(rv.f_set, rv.fa_set, rv.fj_set) %>%
  Reduce(function(rv.f_set, rv.fa_set, rv.fj_set) left_join(rv.f_set, rv.fa_set, rv.fj_set, by="unique_set_ID"), .)

rv_use_m<-list(rv.m_set, rv.ma_set, rv.mj_set) %>%
  Reduce(function(rv.m_set, rv.ma_set, rv.mj_set) left_join(rv.m_set, rv.ma_set, rv.mj_set, by="unique_set_ID"), .)

rv_use_u<-list(rv.u_set, rv.ua_set, rv.uj_set) %>%
  Reduce(function(rv.u_set, rv.ua_set, rv.uj_set) left_join(rv.u_set, rv.ua_set, rv.uj_set, by="unique_set_ID"), .)

rv_use_sex<-list(rv_use_f, rv_use_m, rv_use_u) %>%
  Reduce(function(rv_use_f, rv_use_m, rv_use_u) full_join(rv_use_f, rv_use_m, rv_use_u, by="unique_set_ID"), .)

#Merge with the above summaries (top)
rv_use_all<-list(rv_use3, rv_use_sex) %>%
  Reduce(function(rv_use3, rv_use_sex) full_join(rv_use3, rv_use_sex, by="unique_set_ID"), .)

rv_use_all$cod_at_per <- (rv_use_all$COD_number_Adu/rv_use_all$COD_number_Tot)*100
rv_use_all$yt_at_per <- (rv_use_all$YT_number_Adu/rv_use_all$YT_number_Tot)*100

rv_use_all$cod_at_per[is.na(rv_use_all$cod_at_per)] <- 0
rv_use_all$yt_at_per[is.na(rv_use_all$yt_at_per)] <- 0

#Merge with the environmental data
env.dat$unique_set_ID <- env.dat$unique_set
rv_use_all_env<-list(rv_use_all, env.dat) %>%
  Reduce(function(rv_use_all, env.dat) left_join(rv_use_all, env.dat, by="unique_set_ID"), .)

#remove and reorder few variables
drops <- c("lat", "lon", "survey", "set", "slat", "slong", "unique_set")
rv_use_all_env2<-rv_use_all_env[ , !(names(rv_use_all_env) %in% drops)]
rv_use_all_env3<-rv_use_all_env2[,c(1,38,39,2:37, 40:94)]

#make all -9999 values = NA 
rv_use_all_env3[rv_use_all_env3 == -9999] <- NA
#write all to a csv file. 
write.csv(rv_use_all_env3, paste(path,"Data/Summaries/RVSurvey.csv",sep=""), row.names = F)

#############################################
#############################################
### NMFS Spring survey DATASET 
#############################################
#############################################
#NMFS Spring survey:  1970-2016
# Presence/Absence AND Numbers; FOR each of i) all sizes ii) juvenile, iii) adult 

# rm(list=ls())
# 
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"
# 
# surv.dat <- read.csv(paste(path,"Data/Survey/all_survey_data_setsfixed_dd.csv",sep=""),stringsAsFactors = F)
# env.dat <-read.csv(paste(path,"Data/Survey/survey_spatial_dd_env_agg.csv",sep=""),stringsAsFactors = F)
#   

####################################
####################################
#Grab all the NMFS/Spring data
nm<- surv.dat[surv.dat$survey == "NMFS/Spring",]

#Eight surveys (e.g., NMFS/Spring-2014-HB-201402-0214) had different lat/lon values, very small numbers (rounding?), so I used the min, and max to get a unique value, rather than unique

#write.csv(nm_s, paste(path,"Data/Summaries/test.csv",sep=""), row.names = F)

nm_s <- nm %>%
  group_by(unique_set_ID) %>%
  summarize(lat = min(lat), 
            lon = min(lon),
            survey = unique(survey),
            year = unique(year),
            strata = unique(strata),
            set = unique(set)
  )

##COD COD COD COD ##COD COD COD COD##COD COD COD COD##COD COD COD COD##COD COD COD COD##COD COD COD COD
####Get  COD data for ALL, Juvenile and Adults 
#First, grab the correct Survey Cod data 
rv_c <- surv.dat[surv.dat$survey == "NMFS/Spring" & surv.dat$species == "Cod",]

#Grab the "Total" sizes
rv.t <- rv_c[rv_c$size == "TOTAL",]
#Grab the non- "TOTAL" sizes
rv.n <- rv_c[rv_c$size != "TOTAL",]
rv.n$size <- as.numeric(rv.n$size)
#Now, separate non- "TOTAL" into adult and juvenile
rv.j <- rv.n[rv.n$size < 43 ,]
rv.a <- rv.n[rv.n$size >= 43 ,]

##For each age class, except total (which is already unique), sum the total numbers by unique_set_ID 
rv.t_set <- rv.t %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Tot = sum(number)
  )

rv.j_set <- rv.j %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Juv = sum(number)
  )

rv.a_set <- rv.a %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Adu = sum(number)
  )

#for each - add a presence/absence column
rv.t_set$COD_PA_Tot <- as.numeric(ifelse(rv.t_set$COD_number_Tot == 0, 0, ifelse(rv.t_set$COD_number_Tot > 0, 1, "")))
rv.j_set$COD_PA_Juv <- as.numeric(ifelse(rv.j_set$COD_number_Juv == 0, 0, ifelse(rv.j_set$COD_number_Juv > 0, 1, "")))
rv.a_set$COD_PA_Adu <- as.numeric(ifelse(rv.a_set$COD_number_Adu == 0, 0, ifelse(rv.a_set$COD_number_Adu > 0, 1, "")))

#Merge all together
rv_use<-list( rv.t_set, rv.j_set, rv.a_set) %>%
  Reduce(function( rv.t_set, rv.j_set, rv.a_set) full_join( rv.t_set, rv.j_set, rv.a_set, by="unique_set_ID"), .)

##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT 

###Get RV YT data for ALL(rv.t), Juvenile(rv.j) and Adults (rv.o)
#First, grab the RV Cod data (size is not numeric b/c has U, M and F)
rv_y <- surv.dat[surv.dat$survey == "NMFS/Spring" & surv.dat$species == "Yellowtail",]

#Grab the "Total" sizes
rv.t <- rv_y[rv_y$size == "TOTAL",]
#Grab the non- "TOTAL" sizes
rv.n <- rv_y[rv_y$size != "TOTAL",]
rv.n$size <- as.numeric(rv.n$size)
#Now, separate non- "TOTAL" into adult and juvenile
rv.j <- rv.n[rv.n$size < 32 ,]
rv.a <- rv.n[rv.n$size >= 32 ,]

##For each age class, except total (which is already unique), sum the total numbers by unique_set_ID 
rv.t_set <- rv.t %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Tot = sum(number)
  )
rv.j_set <- rv.j %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Juv = sum(number)
  )
rv.a_set <- rv.a %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Adu = sum(number)
  )

#for each - add a presence/absence column
rv.t_set$YT_PA_Tot <- as.numeric(ifelse(rv.t_set$YT_number_Tot == 0, 0, ifelse(rv.t_set$YT_number_Tot > 0, 1, "")))
rv.j_set$YT_PA_Juv <- as.numeric(ifelse(rv.j_set$YT_number_Juv == 0, 0, ifelse(rv.j_set$YT_number_Juv > 0, 1, "")))
rv.a_set$YT_PA_Adu <- as.numeric(ifelse(rv.a_set$YT_number_Adu == 0, 0, ifelse(rv.a_set$YT_number_Adu > 0, 1, "")))

#Merge all together
rv_use2<-list(rv.t_set, rv.j_set, rv.a_set) %>%
  Reduce(function(rv.t_set, rv.j_set, rv.a_set) full_join(rv.t_set, rv.j_set, rv.a_set, by="unique_set_ID"), .)

rv_use3<-list(rv_use, rv_use2) %>%
  Reduce(function(rv_use, rv_use2) full_join(rv_use, rv_use2, by="unique_set_ID"), .) ##XXX some Unique Set ID's don't match. 

#merge with the uniqe set data
rv_use4<-list(nm_s, rv_use3) %>%
  Reduce(function(nm_s, rv_use3) full_join(nm_s, rv_use3, by="unique_set_ID"), .) ##XXX some Unique Set ID's don't match. 

rv_use4[is.na(rv_use4)] <- 0

#Merge with the environmental data
env.dat$unique_set_ID <- env.dat$unique_set
rv_use_all_env<-list(rv_use4, env.dat) %>%
  Reduce(function(rv_use4, env.dat) left_join(rv_use4, env.dat, by="unique_set_ID"), .)

#remove and reorder few variables
drops <- c("lat", "lon", "survey", "set", "slat", "slong", "unique_set")
rv_use_all_env2<-rv_use_all_env[ , !(names(rv_use_all_env) %in% drops)]
rv_use_all_env3<-rv_use_all_env2[,c(1,18,19,2:17, 20:74)]

#make all -9999 values = NA 
rv_use_all_env3[rv_use_all_env3 == -9999] <- NA
#remove records with repeated "unique_set" (some of the environment data has duplicate unique-sets, but differing values, can't know which is right?)
rv_use_all_env4<-rv_use_all_env3[!duplicated(rv_use_all_env3$unique_set_ID),]

write.csv(rv_use_all_env4, paste(path,"Data/Summaries/NMFSspring.csv",sep=""), row.names = F)

#############################################
#############################################
### NMFS FALL survey DATASET 
#############################################
#############################################
#NMFS FALL survey:  1970-2016
# Presence/Absence AND Numbers; FOR each of i) all sizes ii) juvenile, iii) adult 

# rm(list=ls())
# 
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"
# 
# surv.dat <- read.csv(paste(path,"Data/Survey/all_survey_data_setsfixed_dd.csv",sep=""),stringsAsFactors = F)
# env.dat <-read.csv(paste(path,"Data/Survey/survey_spatial_dd_env_agg.csv",sep=""),stringsAsFactors = F)

#Grab all the NMFS/Spring data
nm<- surv.dat[surv.dat$survey == "NMFS/Fall",]

#write.csv(nm_s, paste(path,"Data/Summaries/test.csv",sep=""), row.names = F)

nm_s <- nm %>%
  group_by(unique_set_ID) %>%
  summarize(lat = min(lat), 
            lon = min(lon),
            survey = unique(survey),
            year = unique(year),
            set = unique(set),
            slat = min(slat),
            slong = min(slong), 
            strata = unique(strata)
  )


####################################
####################################
##COD COD COD COD ##COD COD COD COD##COD COD COD COD##COD COD COD COD##COD COD COD COD##COD COD COD COD
####Get  COD data for ALL, Juvenile and Adults 
#First, grab the correct Survey Cod data 
rv_c <- surv.dat[surv.dat$survey == "NMFS/Fall" & surv.dat$species == "Cod",] 

#Grab the "Total" sizes
rv.t <- rv_c[rv_c$size == "TOTAL",]
#Grab the non- "TOTAL" sizes
rv.n <- rv_c[rv_c$size != "TOTAL",]
rv.n$size <- as.numeric(rv.n$size)
#Now, separate non- "TOTAL" into adult and juvenile
rv.j <- rv.n[rv.n$size < 43 ,]
rv.a <- rv.n[rv.n$size >= 43 ,]

##For each age class, except total (which is already unique), sum the total numbers by unique_set_ID 
rv.t_set <- rv.t %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Tot = sum(number)
  )
rv.j_set <- rv.j %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Juv = sum(number)
  )
rv.a_set <- rv.a %>%
  group_by(unique_set_ID) %>%
  summarize(COD_number_Adu = sum(number)
  )

#for each - add a presence/absence column
rv.t_set$COD_PA_Tot <- as.numeric(ifelse(rv.t_set$COD_number_Tot == 0, 0, ifelse(rv.t_set$COD_number_Tot > 0, 1, "")))
rv.j_set$COD_PA_Juv <- as.numeric(ifelse(rv.j_set$COD_number_Juv == 0, 0, ifelse(rv.j_set$COD_number_Juv > 0, 1, "")))
rv.a_set$COD_PA_Adu <- as.numeric(ifelse(rv.a_set$COD_number_Adu == 0, 0, ifelse(rv.a_set$COD_number_Adu > 0, 1, "")))

#Merge all together
rv_use<-list(rv.t_set, rv.j_set, rv.a_set) %>%
  Reduce(function(rv.t_set, rv.j_set, rv.a_set) full_join(rv.t_set, rv.j_set, rv.a_set, by="unique_set_ID"), .)

##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT ##YT YT YT 

###Get RV YT data for ALL(rv.t), Juvenile(rv.j) and Adults (rv.o)
#First, grab the RV Cod data (size is not numeric b/c has U, M and F)
rv_y <- surv.dat[surv.dat$survey == "NMFS/Fall" & surv.dat$species == "Yellowtail",]

#Grab the "Total" sizes
rv.t <- rv_y[rv_y$size == "TOTAL",]
#Grab the non- "TOTAL" sizes
rv.n <- rv_y[rv_y$size != "TOTAL",]
rv.n$size <- as.numeric(rv.n$size)
#Now, separate non- "TOTAL" into adult and juvenile
rv.j <- rv.n[rv.n$size < 32 ,]
rv.a <- rv.n[rv.n$size >= 32 ,]

##For each age class, except total (which is already unique), sum the total numbers by unique_set_ID 
rv.t_set <- rv.t %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Tot = sum(number)
  )
rv.j_set <- rv.j %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Juv = sum(number)
  )
rv.a_set <- rv.a %>%
  group_by(unique_set_ID) %>%
  summarize(YT_number_Adu = sum(number)
  )

#for each - add a presence/absence column
rv.t_set$YT_PA_Tot <- as.numeric(ifelse(rv.t_set$YT_number_Tot == 0, 0, ifelse(rv.t_set$YT_number_Tot > 0, 1, "")))
rv.j_set$YT_PA_Juv <- as.numeric(ifelse(rv.j_set$YT_number_Juv == 0, 0, ifelse(rv.j_set$YT_number_Juv > 0, 1, "")))
rv.a_set$YT_PA_Adu <- as.numeric(ifelse(rv.a_set$YT_number_Adu == 0, 0, ifelse(rv.a_set$YT_number_Adu > 0, 1, "")))

#Merge all together
rv_use2<-list(rv.t_set, rv.j_set, rv.a_set) %>%
  Reduce(function(rv.t_set, rv.j_set, rv.a_set) full_join(rv.t_set, rv.j_set, rv.a_set, by="unique_set_ID"), .)

rv_use3<-list(rv_use, rv_use2) %>%
  Reduce(function(rv_use, rv_use2) full_join(rv_use, rv_use2, by="unique_set_ID"), .)

#merge with the uniqe set data
rv_use4<-list(nm_s, rv_use3) %>%
  Reduce(function(nm_s, rv_use3) full_join(nm_s, rv_use3, by="unique_set_ID"), .) ##XXX some Unique Set ID's don't match. 

rv_use4[is.na(rv_use4)] <- 0

#Merge with the environmental data
env.dat$unique_set_ID <- env.dat$unique_set
rv_use_all_env<-list(rv_use4, env.dat) %>%
  Reduce(function(rv_use4, env.dat) left_join(rv_use4, env.dat, by="unique_set_ID"), .)

#remove and reorder few variables
drops <- c("lat", "lon", "survey", "set", "slat", "slong", "unique_set")
rv_use_all_env2<-rv_use_all_env[ , !(names(rv_use_all_env) %in% drops)]
rv_use_all_env3<-rv_use_all_env2[,c(1,18,19,2:17, 20:74)]

#make all -9999 values = NA 
rv_use_all_env3[rv_use_all_env3 == -9999] <- NA
#remove records with repeated "unique_set"
rv_use_all_env4<-rv_use_all_env3[!duplicated(rv_use_all_env3$unique_set_ID),]

write.csv(rv_use_all_env4, paste(path,"Data/Summaries/NMFSfall.csv",sep=""), row.names = F)

#########################
#####MERGE NMFS Spring and Fall surveys together. 
#########################
# rm(list=ls())
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

fall <- read.csv(paste(path,"Data/Summaries/NMFSfall.csv",sep=""),stringsAsFactors = F)
fall$season <- "fall"
spring <- read.csv(paste(path,"Data/Summaries/NMFSspring.csv",sep=""),stringsAsFactors = F)
spring$season <- "spring"

#merge fall and spring surveys
NMFS <- rbind(fall, spring)

write.csv(NMFS, paste(path,"Data/Summaries/NMFS.csv",sep=""), row.names = F)


#########################
#########################
#########################
#−	4) Groundfish commercial data - all 2003-2016
#−	5) Groundfish commercial data - otter and effort 2003-2016
#########################
#########################

#12 = otter trawl, 41 = gillnet, 51 = longline, 59 = handline,  
# 
# rm(list=ls())
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

gf <- read.csv(paste(path,"Data/For_Spatial_Intersection/groundfish_yt_cod_catch_dd_env_aggr.csv",sep=""),stringsAsFactors = F)
#isolate otter trawl data (gear = 12)
gf2 <- gf[gf$gear == 12,] 

#Group data by trip - think this tells about the fishery, do they change their fishing if there is high bycatch? 
gf.trips <- gf %>%
  group_by(trip_id) %>%
  summarize(year = min(year),
            month = min(month),
            lat = mean(lat), 
            lon = mean(lon),
            gear_code = unique(gear_code),
            Number_Sets = length(unique(set_id)),
            cod_kg = sum(cod_kg),
            yt_kg = sum(yt_kg)
  )

#for otter trawl only - group by trip
gf_ott.trips <- gf2 %>%
  group_by(trip_id) %>%
  summarize(year = min(year),
            month = min(month),
            lat = mean(lat), 
            lon = mean(lon),
            gear_code = unique(gear_code),
            Number_Sets = length(unique(set_id)),
            #Effort_hours = sum(Effort_hours),
            #Effort_hours_min = min(Effort_hours),
            #Effort_hours_max = max(Effort_hours), 
            #Effort_hours_mean = mean(Effort_hours),
            cod_kg = sum(cod_kg),
            yt_kg = sum(yt_kg)
            #cod_cp = (sum(cod_kg))/(sum(Effort_hours)), 
            #yt_cp = (sum(yt_kg))/(sum(Effort_hours))
  )

#for each - add a presence/absence column
gf$cod_PA <- as.numeric(ifelse(gf$cod_kg == 0, 0, ifelse(gf$cod_kg > 0, 1, "")))
gf$yt_PA <- as.numeric(ifelse(gf$yt_kg == 0, 0, ifelse(gf$yt_kg > 0, 1, "")))
gf.trips$cod_PA <- as.numeric(ifelse(gf.trips$cod_kg == 0, 0, ifelse(gf.trips$cod_kg > 0, 1, "")))
gf.trips$yt_PA <- as.numeric(ifelse(gf.trips$yt_kg == 0, 0, ifelse(gf.trips$yt_kg > 0, 1, "")))
gf_ott.trips$cod_PA <- as.numeric(ifelse(gf_ott.trips$cod_kg == 0, 0, ifelse(gf_ott.trips$cod_kg > 0, 1, "")))
gf_ott.trips$yt_PA <- as.numeric(ifelse(gf_ott.trips$yt_kg == 0, 0, ifelse(gf_ott.trips$yt_kg > 0, 1, "")))

#remove some variables
drops <- c("FID", "Field1", "X", "date_fishe", "lat", "lon")
gf<-gf[ , !(names(gf) %in% drops)]
#reorder some variables
gf<-gf[,c(1:5,8,9,6,7,66,67,10:65)]

#make all -9999 values = NA 
gf[gf == -9999] <- NA
gf.trips[gf.trips == -9999] <- NA
gf_ott.trips[gf_ott.trips == -9999] <- NA

write.csv(gf, paste(path,"Data/Summaries/Groundfish_Sets.csv",sep=""), row.names = F)
write.csv(gf.trips, paste(path, "Data/Summaries/Groundfish_Trips.csv", sep=""), row.names = F)
write.csv(gf_ott.trips, paste(path, "Data/Summaries/Groundfish_OtterT_Trips.csv", sep=""), row.names = F)

##################################
##################################
#−	6) Groundfish observer data - 2003-2016
# 
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

#clean up YT
yt <- read.csv(paste(path,"Data/For_Spatial_Intersection/length_width_ytf_FULL_FINAL_gf_dd_env_agg.csv",sep=""),stringsAsFactors = F)
#summary the  bins, with presence/absence, for all bins, and for juvenile and adult. 
yt$yt_count <- rowSums(yt[,c(21:54)]) #all the "bin_XX" columns
yt$yt_count_j <-rowSums(yt[,c(21:30)]) #bin_022 - bin_031 - juvenile
yt$yt_count_a <-rowSums(yt[,c(31:54)]) #bin_032 - bin_055 - adult
yt$yt_PA <- as.numeric(ifelse(yt$yt_count == 0, 0, ifelse(yt$yt_count > 0, 1, "")))
yt$yt_PA_j <- as.numeric(ifelse(yt$yt_count_j == 0, 0, ifelse(yt$yt_count_j > 0, 1, "")))
yt$yt_PA_a <- as.numeric(ifelse(yt$yt_count_a == 0, 0, ifelse(yt$yt_count_a > 0, 1, "")))
#rename the bycatch columns so both can be merged 
yt$yt_bycatch_wt<-yt$est_bycatc #I assumed this to be "est_bycatch_wt" - the variable originally sent (not the same the second time)
yt$yt_bycatch_wt[is.na(yt$yt_bycatch_wt)] <- 0
yt$yt_PA_bycatch_wt <- as.numeric(ifelse(yt$yt_bycatch_wt == 0, 0, ifelse(yt$yt_bycatch_wt > 0, 1, "")))
#yt - has missing values (e.g., gear when YT not caught), so drop all variables except tripset and yellowtail values
keeps <- c("tripset" ,"yt_count", "yt_count_j", "yt_count_a", "yt_PA", "yt_PA_j", "yt_PA_a", "yt_bycatch_wt", "yt_PA_bycatch_wt")
yt<-yt[ , (names(yt) %in% keeps)]

#clean up COD
cod <- read.csv(paste(path,"Data//For_Spatial_Intersection/length_width_cod_FULL_FINAL_gf_dd_env_agg.csv",sep=""),stringsAsFactors = F)

#summary the bins, with presence/absence, for all bins, and for juvenile and adult. 
cod$cod_count <- rowSums(cod[,c(21:99)]) #all bins
cod$cod_count_j <-rowSums(cod[,c(21:41)]) #bin_022 - bin_042
cod$cod_count_a <-rowSums(cod[,c(42:96)]) #bin_043 - bin_100
cod$cod_PA <- as.numeric(ifelse(cod$cod_count == 0, 0, ifelse(cod$cod_count > 0, 1, "")))
cod$cod_PA_j <- as.numeric(ifelse(cod$cod_count_j == 0, 0, ifelse(cod$cod_count_j > 0, 1, "")))
cod$cod_PA_a <- as.numeric(ifelse(cod$cod_count_a == 0, 0, ifelse(cod$cod_count_a > 0, 1, "")))
#rename the bycatch columns so both can be merged 
cod$cod_bycatch_wt<-cod$est_bycatch_wt
cod$cod_bycatch_wt[is.na(cod$cod_bycatch_wt)] <- 0
cod$cod_PA_bycatch_wt <- as.numeric(ifelse(cod$cod_bycatch_wt == 0, 0, ifelse(cod$cod_bycatch_wt > 0, 1, "")))
#add year & month columns
cod$month <- as.numeric(substr(cod$setdate, 6,7))
cod$year <- as.numeric(substr(cod$setdate, 1,4))
#cod - remove unnecessary columns
cod<-cod[, -grep("bin", colnames(cod))]
drops <- c("X.2", "X.1", "X", "set_no", "latitude", "longitude", "setdate", "settime", "pntcd_id", "num_hook_haul", "comarea_id", "speccd_id", "common","est_bycatch_wt"  )
cod<-cod[ , !(names(cod) %in% drops)]
  
#merge two datasets
obs_g<-list(cod, yt) %>%
  Reduce(function(cod, yt) full_join(cod, yt, by="tripset"), .)

#based on comments from DK - remove
obs_g <- obs_g[ ! obs_g$source %in% c(1), ] #omit rows with source = 1 (unobserved)
drops <- c("source") #omit the source column
obs_g<-obs_g[ , !(names(obs_g) %in% drops)]

obs_g2<-obs_g[,c(1:6,57:58, 49:56, 59:66, 7:48)]

obs_g2[obs_g2 == -9999] <- NA

write.csv(obs_g2, paste(path,"Data/Summaries/Observers_Groundfish_Set.csv",sep=""), row.names = F)

####summarize by trip

#BUT
#multiple gear types on the same trip is causing problems, these are:
# 4 records with "OTTER TWIN TRAWL" instead of "BOTTOM OTTER TRAWL (STERN)"
# 2 in J07-0050 and 2 in J07-0803
# change to "BOTTOM OTTER TRAWL (STERN)"
obs_g$gear[obs_g$gear == "OTTER TWIN TRAWL"] <- "BOTTOM OTTER TRAWL (STERN)"

#many sets (4007) have NA for gear, fill these in
No_NAs<-zoo::na.locf(obs_g[with(obs_g, order(trip, gear)), ])

#reorder the columns
No_NAs<-No_NAs[,c(1:6, 49:66,7:48 )]

#make all -9999 values = NA 
No_NAs[No_NAs == -9999] <- NA

write.csv(No_NAs, paste(path,"Data/Summaries/Observers_Groundfish_TripSet.csv",sep=""), row.names = F)

#summarize by trip - because I think that might be interesting. 
obs_g2 <- read.csv(paste(path,"Data/Summaries/Observers_Groundfish_TripSet.csv",sep=""),stringsAsFactors = F)

#  d<-na.omit(obs_g)
#  test<-transform(obs_g,gear=d$gear[match(trip,d$trip)])

obs_g_trips <- obs_g2 %>%
  group_by(trip) %>%
  summarize(year = min(year),
            month = min(month),
            lat = mean(lat.dd), 
            lon = mean(lon.dd),
            vessel = unique(vessel_name),
            gear = unique(gear),
            Number_Sets = length(unique(tripset)),
            cod_count = sum(cod_count),
            cod_count_j = sum(cod_count_j), 
            cod_count_a = sum(cod_count_a),
            cod_PA_count = sum(cod_PA),
            cod_PA_j_count = sum(cod_PA_j), 
            cod_PA_a_count = sum(cod_PA_a),
            cod_bycatch_wt = sum(cod_bycatch_wt),
            yt_count = sum(yt_count),
            yt_count_j = sum(yt_count_j), 
            yt_count_a = sum(yt_count_a),
            yt_PA_count = sum(yt_PA),
            yt_PA_j_count = sum(yt_PA_j), 
            yt_PA_a_count = sum(yt_PA_a),
            yt_bycatch_wt = sum(yt_bycatch_wt)

  )
obs_g_trips$cod_PA <- as.numeric(ifelse(obs_g_trips$cod_PA_count == 0, 0, ifelse(obs_g_trips$cod_PA_count > 0, 1, "")))
obs_g_trips$cod_PA_j <- as.numeric(ifelse(obs_g_trips$cod_PA_j_count == 0, 0, ifelse(obs_g_trips$cod_PA_j_count > 0, 1, "")))
obs_g_trips$cod_PA_a <- as.numeric(ifelse(obs_g_trips$cod_PA_a_count == 0, 0, ifelse(obs_g_trips$cod_PA_a_count > 0, 1, "")))
obs_g_trips$yt_PA <- as.numeric(ifelse(obs_g_trips$yt_PA_count == 0, 0, ifelse(obs_g_trips$yt_PA_count > 0, 1, "")))
obs_g_trips$yt_PA_j <- as.numeric(ifelse(obs_g_trips$yt_PA_j_count == 0, 0, ifelse(obs_g_trips$yt_PA_j_count > 0, 1, "")))
obs_g_trips$yt_PA_a <- as.numeric(ifelse(obs_g_trips$yt_PA_a_count == 0, 0, ifelse(obs_g_trips$yt_PA_a_count > 0, 1, "")))


#write them to summaries
write.csv(obs_g_trips, paste(path,"Data/Summaries/Observers_Groundfish_Trips.csv",sep=""), row.names = F)

  
##################################
##################################
##################################
#−	7)Scallop observer data - (presence/absence only)
  
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

#clean up YT
yt <- read.csv(paste(path,"Data/For_Spatial_Intersection/length_wide_ytf_FULL_FINAL_scal_dd_env_agg.csv",sep=""),stringsAsFactors = F)

#summary the  bins, with presence/absence, for all bins, and for juvenile and adult. 
yt$yt_count <- rowSums(yt[,c(22:65)]) #bin_012 - bin_055
yt$yt_count_j <-rowSums(yt[,c(22:41)]) #bin_012 - bin_031
yt$yt_count_a <-rowSums(yt[,c(42:65)]) #bin_032 - bin_055
yt$yt_PA <- as.numeric(ifelse(yt$yt_count == 0, 0, ifelse(yt$yt_count > 0, 1, "")))
yt$yt_PA_j <- as.numeric(ifelse(yt$yt_count_j == 0, 0, ifelse(yt$yt_count_j > 0, 1, "")))
yt$yt_PA_a <- as.numeric(ifelse(yt$yt_count_a == 0, 0, ifelse(yt$yt_count_a > 0, 1, "")))
#rename the bycatch columns so both can be merged 
yt$yt_est_num_caught<-yt$est_num_ca
yt$yt_est_num_caught[is.na(yt$yt_est_num_caught)] <- 0
yt$yt_PA_caught <- as.numeric(ifelse(yt$yt_est_num_caught == 0, 0, ifelse(yt$yt_est_num_caught > 0, 1, "")))
#yt - has missing values (e.g., gear when YT not caught), so drop all variables except tripset and yellowtail values
keeps <- c("tripset", "yt_count", "yt_count_j", "yt_count_a", "yt_PA", "yt_PA_j", "yt_PA_a", "yt_est_num_caught", "yt_PA_caught")
yt<-yt[ , (names(yt) %in% keeps)]

#clean up COD
cod <- read.csv(paste(path,"Data/For_Spatial_Intersection/length_wide_cod_FULL_FINAL_scal_dd_env_agg.csv",sep=""),stringsAsFactors = F)
cod$longitude <- -abs(cod$longitude) #lon is positive? 
#summary the the bins, with presence/absence, for all bins, and for juvenile and adult. 
cod$cod_count <- rowSums(cod[,c(21:115)]) #bin_006 - bin_100
cod$cod_count_j <-rowSums(cod[,c(21:57)]) #bin_006 - bin_042
cod$cod_count_a <-rowSums(cod[,c(58:115)]) #bin_043 - bin_100
cod$cod_PA <- as.numeric(ifelse(cod$cod_count == 0, 0, ifelse(cod$cod_count > 0, 1, "")))
cod$cod_PA_j <- as.numeric(ifelse(cod$cod_count_j == 0, 0, ifelse(cod$cod_count_j > 0, 1, "")))
cod$cod_PA_a <- as.numeric(ifelse(cod$cod_count_a == 0, 0, ifelse(cod$cod_count_a > 0, 1, "")))
#rename the bycatch columns so both can be merged 
cod$cod_est_num_caught<-cod$est_num_caught
cod$cod_est_num_caught[is.na(cod$est_num_caught)] <- 0
cod$cod_PA_caught <- as.numeric(ifelse(cod$cod_est_num_caught == 0, 0, ifelse(cod$cod_est_num_caught > 0, 1, "")))
#add year & month columns
cod$year <- as.numeric(substr(cod$setdate, 1,4))
cod$month <- as.numeric(substr(cod$setdate, 6,7))
#cod - remove unnecessary columns
cod<-cod[, -grep("bin", colnames(cod))]
drops <- c( "X.2", "X.1", "X", "latitude", "longitude", "set_no", "setdate", "settime", "source", "pntcd_id", "comarea_id","speccd_id", "common", "est_num_caught", "est_kept_wt", "est_discard_wt")
cod<-cod[ , !(names(cod) %in% drops)]

#merge two datasets
obs_s<-list(cod, yt) %>%
  Reduce(function(cod, yt) full_join(cod, yt, by="tripset"), .)

obs_s[is.na(obs_s)] <- 0


#make all -9999 values = NA 
obs_s[obs_s == -9999] <- NA

#Reorder columns
test<-obs_s[,c(2,57,58,1,3:6,49:56,59:66)] #This isn't working? 
test<-obs_s[,c(1:6,57,58,7:56,59:66)] 
test2<-test[,c(1:8, 51:66, 9:50)]

#Save summarize by tripset
write.csv(test2, paste(path,"Data/Summaries/Observers_Scallop_TripSet.csv",sep=""), row.names = F)

### 
##Summarize by trip - because that might be interesting. 
d<-na.omit(obs_s)
#  test<-transform(obs_g,gear=d$gear[match(trip,d$trip)])

obs_s_trips <- d %>%
  group_by(trip) %>%
  summarize(lat = mean(lat.dd), 
            lon = mean(lon.dd),
            year = min(year),
            month = min(month),
            cod_count = sum(cod_count),
            cod_count_j = sum(cod_count_j), 
            cod_count_a = sum(cod_count_a),
            cod_PA_count = sum(cod_PA),
            cod_PA_j_count = sum(cod_PA_j), 
            cod_PA_a_count = sum(cod_PA_a),
            yt_count = sum(yt_count),
            yt_count_j = sum(yt_count_j), 
            yt_count_a = sum(yt_count_a),
            yt_PA_count = sum(yt_PA),
            yt_PA_j_count = sum(yt_PA_j), 
            yt_PA_a_count = sum(yt_PA_a)
  )
obs_s_trips$cod_PA <- as.numeric(ifelse(obs_s_trips$cod_PA_count == 0, 0, ifelse(obs_s_trips$cod_PA_count > 0, 1, "")))
obs_s_trips$cod_PA_j <- as.numeric(ifelse(obs_s_trips$cod_PA_j_count == 0, 0, ifelse(obs_s_trips$cod_PA_j_count > 0, 1, "")))
obs_s_trips$cod_PA_a <- as.numeric(ifelse(obs_s_trips$cod_PA_a_count == 0, 0, ifelse(obs_s_trips$cod_PA_a_count > 0, 1, "")))
obs_s_trips$yt_PA <- as.numeric(ifelse(obs_s_trips$yt_PA_count == 0, 0, ifelse(obs_s_trips$yt_PA_count > 0, 1, "")))
obs_s_trips$yt_PA_j <- as.numeric(ifelse(obs_s_trips$yt_PA_j_count == 0, 0, ifelse(obs_s_trips$yt_PA_j_count > 0, 1, "")))
obs_s_trips$yt_PA_a <- as.numeric(ifelse(obs_s_trips$yt_PA_a_count == 0, 0, ifelse(obs_s_trips$yt_PA_a_count > 0, 1, "")))

obs_s_trips[is.na(obs_s_trips)] <- 0
#write them to summaries
write.csv(obs_s_trips, paste(path,"Data/Summaries/Observers_Scallop_Trips.csv",sep=""), row.names = F)


#######################
####Temporal Variables
#######################

######################
### RV Survey
######
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"
rv <- read.csv(paste(path,"Data/Summaries/RVSurvey.csv",sep=""),stringsAsFactors = F)

rv_year <- rv %>%
  group_by(year) %>%
  summarize( COD_number_Tot = mean(COD_number_Tot), 
             COD_PA_Tot= mean(COD_PA_Tot), 
             COD_number_Juv= mean(COD_number_Juv), 
             COD_PA_Juv= mean( COD_PA_Juv), 
             COD_number_Adu= mean( COD_number_Adu), 
             COD_PA_Adu= mean( COD_PA_Adu), 
             YT_number_Tot= mean( YT_number_Tot), 
             YT_PA_Tot= mean( YT_PA_Tot), 
             YT_number_Juv= mean( YT_number_Juv), 
             YT_PA_Juv= mean( YT_PA_Juv), 
             YT_number_Adu= mean( YT_number_Adu), 
             YT_PA_Adu= mean( YT_PA_Adu), 
             YT_number_F= mean( YT_number_F), 
             YT_PA_F= mean( YT_PA_F), 
             YT_number_FA= mean( YT_number_FA), 
             YT_PA_FA= mean( YT_PA_FA), 
             YT_number_FJ= mean( YT_number_FJ), 
             YT_PA_FJ= mean( YT_PA_FJ), 
             YT_number_M= mean( YT_number_M), 
             YT_PA_M= mean( YT_PA_M), 
             YT_number_MA= mean( YT_number_MA), 
             YT_PA_MA= mean( YT_PA_MA), 
             YT_number_MJ= mean( YT_number_MJ), 
             YT_PA_MJ= mean( YT_PA_MJ), 
             YT_number_U= mean( YT_number_U), 
             YT_PA_U= mean( YT_PA_U), 
             YT_number_UA= mean( YT_number_UA), 
             YT_PA_UA= mean( YT_PA_UA), 
             YT_number_UJ= mean( YT_number_UJ), 
             YT_PA_UJ= mean( YT_PA_UJ), 
             cod_at_per= mean( cod_at_per), 
             yt_at_per= mean( yt_at_per),
             Field1=mean(Field1),
             comlaspect=mean(comlaspect),
             botstr_t=mean(botstr_t),
             botstr_wt=mean(botstr_wt),
             bt_avg96=mean(bt_avg96),
             bt_rg96=mean(bt_rg96),
             chl_avg=mean(chl_avg),
             complexity=mean(complexity),
             comldepth=mean(comldepth),
             gravel=mean(gravel),
             k490_avg=mean(k490_avg),
             k490_rg=mean(k490_rg),
             Mud=mean(Mud),
             nit_avg96=mean(nit_avg96),
             phos_avg96=mean(phos_avg96),
             sal_avg96=mean(sal_avg96),
             sal_rg96=mean(sal_rg96),
             sand=mean(sand),
             sil_avg96=mean(sil_avg96),
             comlslope=mean(comlslope),
             sst_avg=mean(sst_avg),
             sst_rg=mean(sst_rg),
             strat_sum9=mean(strat_sum9),
             strat96=mean(strat96),
             chl_rg=mean(chl_rg),
             FID_GMAINE=mean(FID_GMAINE),
             Year_median=mean(Year_median),
             Winter_median=mean(Winter_median),
             Spring_median=mean(Spring_median),
             Summer_median=mean(Summer_median),
             Fall_median=mean(Fall_median),
             FID_GMAI_1=mean(FID_GMAI_1),
             Year_95perc=mean(Year_95perc),
             Winter_95perc=mean(Winter_95perc),
             Spring_95perc=mean(Spring_95perc),
             Summer_95perc=mean(Summer_95perc),
             Fall_95perc=mean(Fall_95perc),
             FID_GMAI_hIPR=mean(FID_GMAI_hIPR),
             Year_hIPR=mean(Year_hIPR),
             Winter_hIPR=mean(Winter_hIPR),
             Spring_hIPR=mean(Spring_hIPR),
             Summer_hIPR=mean(Summer_hIPR)#,
             #Fall_hIPR=mean(Fall_hIPR)
  )
write.csv(rv_year, paste(path,"Data/Summaries/Temporal_RV.csv",sep=""), row.names = F)


######################
### NMFSspring
######
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

rv <- read.csv(paste(path,"Data/Summaries/NMFSspring.csv",sep=""),stringsAsFactors = F)

rv_year <- rv %>%
  group_by(year) %>%
  summarize( COD_number_Tot = mean(COD_number_Tot), 
             COD_PA_Tot= mean(COD_PA_Tot), 
             COD_number_Juv= mean(COD_number_Juv), 
             COD_PA_Juv= mean( COD_PA_Juv), 
             COD_number_Adu= mean( COD_number_Adu), 
             COD_PA_Adu= mean( COD_PA_Adu), 
             YT_number_Tot= mean( YT_number_Tot), 
             YT_PA_Tot= mean( YT_PA_Tot), 
             YT_number_Juv= mean( YT_number_Juv), 
             YT_PA_Juv= mean( YT_PA_Juv), 
             YT_number_Adu= mean( YT_number_Adu), 
             YT_PA_Adu= mean( YT_PA_Adu), 
             Field1=mean(Field1),
             comlaspect=mean(comlaspect),
             botstr_t=mean(botstr_t),
             botstr_wt=mean(botstr_wt),
             bt_avg96=mean(bt_avg96),
             bt_rg96=mean(bt_rg96),
             chl_avg=mean(chl_avg),
             complexity=mean(complexity),
             comldepth=mean(comldepth),
             gravel=mean(gravel),
             k490_avg=mean(k490_avg),
             k490_rg=mean(k490_rg),
             Mud=mean(Mud),
             nit_avg96=mean(nit_avg96),
             phos_avg96=mean(phos_avg96),
             sal_avg96=mean(sal_avg96),
             sal_rg96=mean(sal_rg96),
             sand=mean(sand),
             sil_avg96=mean(sil_avg96),
             comlslope=mean(comlslope),
             sst_avg=mean(sst_avg),
             sst_rg=mean(sst_rg),
             strat_sum9=mean(strat_sum9),
             strat96=mean(strat96),
             chl_rg=mean(chl_rg),
             FID_GMAINE=mean(FID_GMAINE),
             Year_median=mean(Year_median),
             Winter_median=mean(Winter_median),
             Spring_median=mean(Spring_median),
             Summer_median=mean(Summer_median),
             Fall_median=mean(Fall_median),
             FID_GMAI_1=mean(FID_GMAI_1),
             Year_95perc=mean(Year_95perc),
             Winter_95perc=mean(Winter_95perc),
             Spring_95perc=mean(Spring_95perc),
             Summer_95perc=mean(Summer_95perc),
             Fall_95perc=mean(Fall_95perc),
             FID_GMAI_hIPR=mean(FID_GMAI_hIPR),
             Year_hIPR=mean(Year_hIPR),
             Winter_hIPR=mean(Winter_hIPR),
             Spring_hIPR=mean(Spring_hIPR),
             Summer_hIPR=mean(Summer_hIPR)#,
             #Fall_hIPR=mean(Fall_hIPR)
  )
write.csv(rv_year, paste(path,"Data/Summaries/Temporal_NMFSspring.csv",sep=""), row.names = F)

######################
### NMFSfall
######
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

rv <- read.csv(paste(path,"Data/Summaries/NMFSfall.csv",sep=""),stringsAsFactors = F)

rv_year <- rv %>%
  group_by(year) %>%
  summarize( COD_number_Tot = mean(COD_number_Tot), 
             COD_PA_Tot= mean(COD_PA_Tot), 
             COD_number_Juv= mean(COD_number_Juv), 
             COD_PA_Juv= mean( COD_PA_Juv), 
             COD_number_Adu= mean( COD_number_Adu), 
             COD_PA_Adu= mean( COD_PA_Adu), 
             YT_number_Tot= mean( YT_number_Tot), 
             YT_PA_Tot= mean( YT_PA_Tot), 
             YT_number_Juv= mean( YT_number_Juv), 
             YT_PA_Juv= mean( YT_PA_Juv), 
             YT_number_Adu= mean( YT_number_Adu), 
             YT_PA_Adu= mean( YT_PA_Adu), 
             Field1=mean(Field1),
             comlaspect=mean(comlaspect),
             botstr_t=mean(botstr_t),
             botstr_wt=mean(botstr_wt),
             bt_avg96=mean(bt_avg96),
             bt_rg96=mean(bt_rg96),
             chl_avg=mean(chl_avg),
             complexity=mean(complexity),
             comldepth=mean(comldepth),
             gravel=mean(gravel),
             k490_avg=mean(k490_avg),
             k490_rg=mean(k490_rg),
             Mud=mean(Mud),
             nit_avg96=mean(nit_avg96),
             phos_avg96=mean(phos_avg96),
             sal_avg96=mean(sal_avg96),
             sal_rg96=mean(sal_rg96),
             sand=mean(sand),
             sil_avg96=mean(sil_avg96),
             comlslope=mean(comlslope),
             sst_avg=mean(sst_avg),
             sst_rg=mean(sst_rg),
             strat_sum9=mean(strat_sum9),
             strat96=mean(strat96),
             chl_rg=mean(chl_rg),
             FID_GMAINE=mean(FID_GMAINE),
             Year_median=mean(Year_median),
             Winter_median=mean(Winter_median),
             Spring_median=mean(Spring_median),
             Summer_median=mean(Summer_median),
             Fall_median=mean(Fall_median),
             FID_GMAI_1=mean(FID_GMAI_1),
             Year_95perc=mean(Year_95perc),
             Winter_95perc=mean(Winter_95perc),
             Spring_95perc=mean(Spring_95perc),
             Summer_95perc=mean(Summer_95perc),
             Fall_95perc=mean(Fall_95perc),
             FID_GMAI_hIPR=mean(FID_GMAI_hIPR),
             Year_hIPR=mean(Year_hIPR),
             Winter_hIPR=mean(Winter_hIPR),
             Spring_hIPR=mean(Spring_hIPR),
             Summer_hIPR=mean(Summer_hIPR)#,
             #Fall_hIPR=mean(Fall_hIPR)
  )
write.csv(rv_year, paste(path,"Data/Summaries/Temporal_NMFSfall.csv",sep=""), row.names = F)

######################
### Groundfish_Sets
# ######
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

rv <- read.csv(paste(path,"Data/Summaries/Groundfish_Sets.csv",sep=""),stringsAsFactors = F)

rv_year <- rv %>%
  group_by(year) %>%
  summarize(cod_kg=mean(cod_kg),
      yt_kg=mean(yt_kg),
      cod_PA=mean(cod_PA),
      yt_PA=mean(yt_PA),
      disturb=mean(rv$disturb, na.rm = T),
      growth=mean(growth, na.rm = T),
      slope_bank=mean(slope_bank, na.rm = T),
      q1_bank=mean(q1_bank, na.rm = T),
      q1_old_ban=mean(q1_old_ban, na.rm = T),
      curv_bank=mean(curv_bank, na.rm = T),
      bs_ext_ban=mean(bs_ext_ban, na.rm = T),
      bpi_5x10=mean(bpi_5x10, na.rm = T),
      bpi_1x5=mean(bpi_1x5, na.rm = T),
      bpi_10x50=mean(bpi_10x50, na.rm = T),
      bathy_bank=mean(bathy_bank, na.rm = T),
      aspect_ban=mean(aspect_ban, na.rm = T),
      comlaspect=mean(comlaspect, na.rm = T),
      botstr_t=mean(botstr_t, na.rm = T),
      botstr_wt=mean(botstr_wt, na.rm = T),
      bt_avg96=mean(bt_avg96, na.rm = T),
      bt_rg96=mean(bt_rg96, na.rm = T),
      chl_avg=mean(chl_avg, na.rm = T),
      chl_rg=mean(chl_rg, na.rm = T),
      complexity=mean(complexity, na.rm = T),
      comldepth=mean(comldepth, na.rm = T),
      gravel=mean(gravel, na.rm = T),
      k490_avg=mean(k490_avg, na.rm = T),
      k490_rg=mean(k490_rg, na.rm = T),
      Mud=mean(Mud, na.rm = T),
      nit_avg96=mean(nit_avg96, na.rm = T),
      phos_avg96=mean(phos_avg96, na.rm = T),
      sal_avg96=mean(sal_avg96, na.rm = T),
      sal_rg96=mean(sal_rg96, na.rm = T),
      sand=mean(sand, na.rm = T),
      sil_avg96=mean(sil_avg96, na.rm = T),
      comlslope=mean(comlslope, na.rm = T),
      sst_avg=mean(sst_avg, na.rm = T),
      sst_rg=mean(sst_rg, na.rm = T),
      strat_sum9=mean(strat_sum9, na.rm = T),
      strat96=mean(strat96, na.rm = T),
      SEDNUM=mean(SEDNUM, na.rm = T),
      FID_GMAINE_median=mean(FID_GMAINE_median, na.rm = T),
      Year_median=mean(Year_median, na.rm = T),
      Winter_median=mean(Winter_median, na.rm = T),
      Spring_median=mean(Spring_median, na.rm = T),
      Summer_median=mean(Summer_median, na.rm = T),
      Fall_median=mean(Fall_median, na.rm = T),
      FID_GMAI_95perc=mean(FID_GMAI_95perc, na.rm = T),
      Year_95perc=mean(Year_95perc, na.rm = T),
      Winter_95perc=mean(Winter_95perc, na.rm = T),
      Spring_95perc=mean(Spring_95perc, na.rm = T),
      Summer_95perc=mean(Summer_95perc, na.rm = T),
      Fall_95perc=mean(Fall_95perc, na.rm = T),
      FID_GMAI_hIPR=mean(FID_GMAI_hIPR, na.rm = T),
      Year_hIPR=mean(Year_hIPR, na.rm = T),
      Winter_hIPR=mean(Winter_hIPR, na.rm = T),
      Spring_hIPR=mean(Spring_hIPR, na.rm = T),
      Summer_hIPR=mean(Summer_hIPR, na.rm = T),
      Fall_hIPR=mean(Fall_hIPR, na.rm = T)
  )
write.csv(rv_year, paste(path,"Data/Summaries/Temporal_Groundfish_Sets.csv",sep=""), row.names = F)


######################
### Observers_Groundfish_Set
######
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

rv <- read.csv(paste(path,"Data/Summaries/Observers_Groundfish_Set.csv",sep=""),stringsAsFactors = F)

rv_year <- rv %>%
  group_by(year) %>%
  summarize(yt_count=mean(yt_count, na.rm = T),
            yt_count_j=mean(yt_count_j, na.rm = T),
            yt_count_a=mean(yt_count_a, na.rm = T),
            yt_PA=mean(yt_PA, na.rm = T),
            yt_PA_j=mean(yt_PA_j, na.rm = T),
            yt_PA_a=mean(yt_PA_a, na.rm = T),
            yt_bycatch_wt=mean(yt_bycatch_wt, na.rm = T),
            yt_PA_bycatch_wt=mean(yt_PA_bycatch_wt, na.rm = T),
            cod_count=mean(cod_count, na.rm = T),
            cod_count_j=mean(cod_count_j, na.rm = T),
            cod_count_a=mean(cod_count_a, na.rm = T),
            cod_PA=mean(cod_PA, na.rm = T),
            cod_PA_j=mean(cod_PA_j, na.rm = T),
            cod_PA_a=mean(cod_PA_a, na.rm = T),
            cod_bycatch_wt=mean(cod_bycatch_wt, na.rm = T),
            cod_PA_bycatch_wt=mean(cod_PA_bycatch_wt, na.rm = T),
            comlaspect=mean(comlaspect, na.rm = T),
            botstr_t=mean(botstr_t, na.rm = T),
            botstr_wt=mean(botstr_wt, na.rm = T),
            bt_avg96=mean(bt_avg96, na.rm = T),
            bt_rg96=mean(bt_rg96, na.rm = T),
            chl_avg=mean(chl_avg, na.rm = T),
            chl_rg=mean(chl_rg, na.rm = T),
            complexity=mean(complexity, na.rm = T),
            comldepth=mean(comldepth, na.rm = T),
            gravel=mean(gravel, na.rm = T),
            k490_avg=mean(k490_avg, na.rm = T),
            k490_rg=mean(k490_rg, na.rm = T),
            Mud=mean(Mud, na.rm = T),
            nit_avg96=mean(nit_avg96, na.rm = T),
            phos_avg96=mean(phos_avg96, na.rm = T),
            sal_avg96=mean(sal_avg96, na.rm = T),
            sal_rg96=mean(sal_rg96, na.rm = T),
            sand=mean(sand, na.rm = T),
            sil_avg96=mean(sil_avg96, na.rm = T),
            comlslope=mean(comlslope, na.rm = T),
            sst_avg=mean(sst_avg, na.rm = T),
            sst_rg=mean(sst_rg, na.rm = T),
            strat96=mean(strat96, na.rm = T),
            FID_GMAINE_median=mean(FID_GMAINE_median, na.rm = T),
            Year_median=mean(Year_median, na.rm = T),
            Winter_median=mean(Winter_median, na.rm = T),
            Spring_median=mean(Spring_median, na.rm = T),
            Summer_median=mean(Summer_median, na.rm = T),
            Fall_median=mean(Fall_median, na.rm = T),
            FID_GMAINE_95th_perc=mean(FID_GMAINE_95th_perc, na.rm = T),
            Year_95perc=mean(Year_95perc, na.rm = T),
            Winter_95perc=mean(Winter_95perc, na.rm = T),
            Spring_95perc=mean(Spring_95perc, na.rm = T),
            Summer_95perc=mean(Summer_95perc, na.rm = T),
            Fall_95perc=mean(Fall_95perc, na.rm = T),
            FID_GMAINE_hIPR=mean(FID_GMAINE_hIPR, na.rm = T),
            Year_hIPR=mean(Year_hIPR, na.rm = T),
            Winter_hIPR=mean(Winter_hIPR, na.rm = T),
            Spring_hIPR=mean(Spring_hIPR, na.rm = T),
            Summer_hIPR=mean(Summer_hIPR, na.rm = T),
            Fall_hIPR=mean(Fall_hIPR, na.rm = T),
            strat_sum9=mean(strat_sum9, na.rm = T)
  )
write.csv(rv_year, paste(path,"Data/Summaries/Temporal_Observers_Groundfish_Set.csv",sep=""), row.names = F)


######################
### Observers_Scallop_Set
######
# rm(list=ls())
# library(stringr)
# library(dplyr)
# 
# #direct.fun <- "d:/r/"
# path <- "/Users/user/DFO_2017/"

rv <- read.csv(paste(path,"Data/Summaries/Observers_Scallop_TripSet.csv",sep=""),stringsAsFactors = F)

rv_year <- rv %>%
  group_by(year) %>%
  summarize(cod_count=mean(cod_count, na.rm = T),
            cod_count_j=mean(cod_count_j, na.rm = T),
            cod_count_a=mean(cod_count_a, na.rm = T),
            cod_PA=mean(cod_PA, na.rm = T),
            cod_PA_j=mean(cod_PA_j, na.rm = T),
            cod_PA_a=mean(cod_PA_a, na.rm = T),
            cod_est_num_caught=mean(cod_est_num_caught, na.rm = T),
            cod_PA_caught=mean(cod_PA_caught, na.rm = T),
            yt_count=mean(yt_count, na.rm = T),
            yt_count_j=mean(yt_count_j, na.rm = T),
            yt_count_a=mean(yt_count_a, na.rm = T),
            yt_PA=mean(yt_PA, na.rm = T),
            yt_PA_j=mean(yt_PA_j, na.rm = T),
            yt_PA_a=mean(yt_PA_a, na.rm = T),
            yt_est_num_caught=mean(yt_est_num_caught, na.rm = T),
            yt_PA_caught=mean(yt_PA_caught, na.rm = T),
            comlaspect=mean(comlaspect, na.rm = T),
            botstr_t=mean(botstr_t, na.rm = T),
            botstr_wt=mean(botstr_wt, na.rm = T),
            bt_avg96=mean(bt_avg96, na.rm = T),
            bt_rg96=mean(bt_rg96, na.rm = T),
            chl_avg=mean(chl_avg, na.rm = T),
            chl_rg=mean(chl_rg, na.rm = T),
            complexity=mean(complexity, na.rm = T),
            comldepth=mean(comldepth, na.rm = T),
            gravel=mean(gravel, na.rm = T),
            k490_avg=mean(k490_avg, na.rm = T),
            k490_rg=mean(k490_rg, na.rm = T),
            Mud=mean(Mud, na.rm = T),
            nit_avg96=mean(nit_avg96, na.rm = T),
            phos_avg96=mean(phos_avg96, na.rm = T),
            sal_avg96=mean(sal_avg96, na.rm = T),
            sal_rg96=mean(sal_rg96, na.rm = T),
            sand=mean(sand, na.rm = T),
            sil_avg96=mean(sil_avg96, na.rm = T),
            comlslope=mean(comlslope, na.rm = T),
            sst_avg=mean(sst_avg, na.rm = T),
            sst_rg=mean(sst_rg, na.rm = T),
            strat96=mean(strat96, na.rm = T),
            FID_GMAINE_median=mean(FID_GMAINE_median, na.rm = T),
            Year_median=mean(Year_median, na.rm = T),
            Winter_median=mean(Winter_median, na.rm = T),
            Spring_median=mean(Spring_median, na.rm = T),
            Summer_median=mean(Summer_median, na.rm = T),
            Fall_median=mean(Fall_median, na.rm = T),
            FID_GMAINE_95th_perc=mean(FID_GMAINE_95th_perc, na.rm = T),
            Year_95perc=mean(Year_95perc, na.rm = T),
            Winter_95perc=mean(Winter_95perc, na.rm = T),
            Spring_95perc=mean(Spring_95perc, na.rm = T),
            Summer_95perc=mean(Summer_95perc, na.rm = T),
            Fall_95perc=mean(Fall_95perc, na.rm = T),
            FID_GMAINE_hIPR=mean(FID_GMAINE_hIPR, na.rm = T),
            Year_hIPR=mean(Year_hIPR, na.rm = T),
            Winter_hIPR=mean(Winter_hIPR, na.rm = T),
            Spring_hIPR=mean(Spring_hIPR, na.rm = T),
            Summer_hIPR=mean(Summer_hIPR, na.rm = T),
            Fall_hIPR=mean(Fall_hIPR, na.rm = T),
            strat_sum9=mean(strat_sum9, na.rm = T)
  )
write.csv(rv_year, paste(path,"Data/Summaries/Temporal_Observers_Scallop_Set.csv",sep=""), row.names = F)
