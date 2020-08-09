#############
###Query the DFO and NMFS RV survey data from spring and summer (DFO), and summer and fall (NMFS). 
###Grab all tows from each of the 4 surveys, and the tows with standandardized weight, for atlantic cod (spec=10) and yellowtail flounder (spec = 42)
###For all of Georges Bank (strata 500+ and 5Z+)
###Join all together, and plot to ensure locations are sensible. 


require(RODBC)
library(dplyr)
chan <- odbcConnect(dsn="ptran", uid=un.ID, pwd = pwd.ID)

##GEORGES is only 500's and 5Z's
# --GEORGES/SPRING
# --  AND TO_CHAR(I.SDATE, 'mm') IN (2,3,4)
# --  AND I.STRAT IN ('551','552','553','554','555','556','557','5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8') -- '470','471','472','473','474','475','476','477','478','479','480','481','482','483','484','485','486','487','488','489','490','491','492','493','494','495',
# 
# --SUMMER
# --  AND TO_CHAR(I.SDATE, 'mm') IN (6,7,8)
# --  AND I.STRAT IN ('434','436','437','438','439',
#                     --                  '440','441','442','443','444','445','446','447','448','449',
#                     --                  '450','451','452','453','454','455','456','457','458','459',
#                     --                 '460','461','462','463','464','465','466','467','468','469',
#                     --                  '470','471','472','473','474','475','476','477','478','479',
#                     --                  '480','481','482','483','484','485','486','487','488','489',
#                     --                  '490','491','492','493','494','495',
#                     --                  '496','497','498',
#                     --                  '5Z1','5Z2')


#####Spring survey (Feb,Mar,April) 
####
#### Cod, limited to GEORGES sets on valid tows (Type=1)
qu.tows <- paste("
SELECT 
I.MISSION,
I.SETNO,
I.GEAR,
I.DIST,
NVL(TO_CHAR(I.STRAT),'NULL') STRAT,
TO_CHAR(I.SDATE, 'yyyy') YEAR,
TO_CHAR(I.SDATE, 'mm') MONTH,
TO_CHAR(I.SDATE, 'yyyy-mm-dd') TOWDATE,
TO_CHAR(I.SDATE, 'hh24:mi:ss') TOWTIME,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5) SLA,
NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL') ELO,
NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL') ELA,
C.SPEC,
--   SUM(C.TOTNO) TOTNORAW,
--   SUM(C.TOTWGT) TOTWGTRAW,
SUM(C.TOTNO)  * (1.75/I.DIST) TOTNO,
SUM(C.TOTWGT) * (1.75/I.DIST) TOTWGT
FROM GROUNDFISH.GSINF I,
GROUNDFISH.GSCAT C
WHERE 
C.MISSION  = I.MISSION
AND C.SETNO    = I.SETNO
AND C.SPEC    IN (10) --10; 42
AND I.TYPE = 1
--GEORGES
AND TO_CHAR(I.SDATE, 'mm') IN (2,3,4)
AND I.STRAT IN ('551','552','553','554','555','556','557','5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8')--'470','471','472','473','474','475','476','477','478','479','480','481','482','483','484','485','486','487','488','489','490','491','492','493','494','495')
GROUP BY  
I.MISSION,
I.SETNO,
I.GEAR,
I.DIST,
NVL(TO_CHAR(I.STRAT),'NULL'),
TO_CHAR(I.SDATE, 'yyyy'),
TO_CHAR(I.SDATE, 'mm'),
TO_CHAR(I.SDATE, 'yyyy-mm-dd'),
TO_CHAR(I.SDATE, 'hh24:mi:ss'),
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5),
ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5),
NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL'),
NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL'),
C.SPEC
", sep="")
dfo.cod <- sqlQuery(chan, qu.tows, stringsAsFactors=FALSE)
## 
#write.csv(dfo.cod, "Y:/Offshore scallop/Assessment/SPERA/Data/RV_survey_CA&US/DFO_GB_spring_cod.csv" , row.names=FALSE)

####
#### yellowtail flounder, limited to GEORGES sets on valid tows (Type=1)
qu.tows <- paste("
                 SELECT 
                 I.MISSION,
                 I.SETNO,
                 I.GEAR,
                 I.DIST,
                 NVL(TO_CHAR(I.STRAT),'NULL') STRAT,
                 TO_CHAR(I.SDATE, 'yyyy') YEAR,
                 TO_CHAR(I.SDATE, 'mm') MONTH,
                 TO_CHAR(I.SDATE, 'yyyy-mm-dd') TOWDATE,
                 TO_CHAR(I.SDATE, 'hh24:mi:ss') TOWTIME,
                 -1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
                 ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5) SLA,
                 NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL') ELO,
                 NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL') ELA,
                 C.SPEC,
                 --   SUM(C.TOTNO) TOTNORAW,
                 --   SUM(C.TOTWGT) TOTWGTRAW,
                 SUM(C.TOTNO)  * (1.75/I.DIST) TOTNO,
                 SUM(C.TOTWGT) * (1.75/I.DIST) TOTWGT
                 FROM GROUNDFISH.GSINF I,
                 GROUNDFISH.GSCAT C
                 WHERE 
                 C.MISSION  = I.MISSION
                 AND C.SETNO    = I.SETNO
                 AND C.SPEC    IN (42) --10; 42
                 AND I.TYPE = 1
                 --GEORGES
                 AND TO_CHAR(I.SDATE, 'mm') IN (2,3,4)
                 AND I.STRAT IN ('551','552','553','554','555','556','557','5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8')--'470','471','472','473','474','475','476','477','478','479','480','481','482','483','484','485','486','487','488','489','490','491','492','493','494','495')
                 GROUP BY 
                 I.MISSION,
                 I.SETNO,
                 I.GEAR,
                 I.DIST,
                 NVL(TO_CHAR(I.STRAT),'NULL'),
                 TO_CHAR(I.SDATE, 'yyyy'),
                 TO_CHAR(I.SDATE, 'mm'),
                 TO_CHAR(I.SDATE, 'yyyy-mm-dd'),
                 TO_CHAR(I.SDATE, 'hh24:mi:ss'),
                 -1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5),
                 ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5),
                 NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL'),
                 NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL'),
                 C.SPEC
                 ", sep="")
dfo.ytf <- sqlQuery(chan, qu.tows, stringsAsFactors=FALSE)
## 
#write.csv(dfo.ytf, "Y:/Offshore scallop/Assessment/SPERA/Data/RV_survey_CA&US/DFO_GB_spring_ytf.csv", row.names=FALSE)

##All valid GEORGES sets tows, regardless of catch  
qu.tows <- paste("
SELECT 
I.MISSION,
I.SETNO,
I.GEAR,
NVL(TO_CHAR(I.STRAT),'NULL') STRAT,
TO_CHAR(I.SDATE, 'yyyy') YEAR,
TO_CHAR(I.SDATE, 'mm') MONTH,
TO_CHAR(I.SDATE, 'yyyy-mm-dd') TOWDATE,
TO_CHAR(I.SDATE, 'hh24:mi:ss') TOWTIME,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5) SLA,
NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL') ELO,
NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL') ELA
FROM GROUNDFISH.GSINF I
WHERE  I.TYPE = 1
--GEORGES
AND TO_CHAR(I.SDATE, 'mm') IN (2,3,4)
AND I.STRAT IN ('551','552','553','554','555','556','557','5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8') --'470','471','472','473','474','475','476','477','478','479','480','481','482','483','484','485','486','487','488','489','490','491','492','493','494','495')
", sep="")
dfo.tows <- sqlQuery(chan, qu.tows, stringsAsFactors=FALSE)
## 
#write.csv(dfo.tows, "Y:/Offshore scallop/Assessment/SPERA/Data/RV_survey_CA&US/DFO_GB_spring_tows.csv", row.names=FALSE)

###
### Stitch 3 files together - merge based on set, add cod/ytf columns

#join tows with cod and ytf - drop SPEC and add "Cod" and "ytf" to TOTNO and TOTWGT
join <- full_join(dfo.tows, dfo.cod)
join$SPEC<-NULL
colnames(join)[which(names(join) == "TOTNO")] <- "Cod_TOTNO"
colnames(join)[which(names(join) == "TOTWGT")] <- "Cod_TOTWGT"
#join[is.na(join)] <- 0

join2 <- full_join(dfo.tows, dfo.ytf)
join2$SPEC<-NULL
colnames(join2)[which(names(join2) == "TOTNO")] <- "Ytf_TOTNO"
colnames(join2)[which(names(join2) == "TOTWGT")] <- "Ytf_TOTWGT"
#join2[is.na(join2)] <- 0

join3 <-right_join(join, join2)
join3[is.na(join3)] <- 0
write.csv(join3, "Y:/Projects/SPERA/Data/RV_survey_CA&US/DFO_GB_spring_tows_cod_ytf.csv", row.names=FALSE)

## Check that locations are sensible. 
loc <- cbind(dfo.tows$SLO, dfo.tows$SLA)
points(loc)


#####Summer survey (June, July, August) 
####
#### Cod, limited to GEORGES sets on valid tows (Type=1)
qu.tows <- paste("
                 SELECT 
                 I.MISSION,
                 I.SETNO,
                 I.GEAR,
                 I.DIST,
                 NVL(TO_CHAR(I.STRAT),'NULL') STRAT,
                 TO_CHAR(I.SDATE, 'yyyy') YEAR,
                 TO_CHAR(I.SDATE, 'mm') MONTH,
                 TO_CHAR(I.SDATE, 'yyyy-mm-dd') TOWDATE,
                 TO_CHAR(I.SDATE, 'hh24:mi:ss') TOWTIME,
                 -1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
                 ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5) SLA,
                 NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL') ELO,
                 NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL') ELA,
                 C.SPEC,
                 --   SUM(C.TOTNO) TOTNORAW,
                 --   SUM(C.TOTWGT) TOTWGTRAW,
                 SUM(C.TOTNO)  * (1.75/I.DIST) TOTNO,
                 SUM(C.TOTWGT) * (1.75/I.DIST) TOTWGT
                 FROM GROUNDFISH.GSINF I,
                 GROUNDFISH.GSCAT C
                 WHERE 
                 C.MISSION  = I.MISSION
                 AND C.SETNO    = I.SETNO
                 AND C.SPEC    IN (10) --10; 42
                 AND I.TYPE = 1
                 --GEORGES
                 AND TO_CHAR(I.SDATE, 'mm') IN (6,7,8)
                 AND I.STRAT IN ('551','552','553','554','555','556','557','5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8')--'470','471','472','473','474','475','476','477','478','479','480','481','482','483','484','485','486','487','488','489','490','491','492','493','494','495')
                 GROUP BY  
                 I.MISSION,
                 I.SETNO,
                 I.GEAR,
                 I.DIST,
                 NVL(TO_CHAR(I.STRAT),'NULL'),
                 TO_CHAR(I.SDATE, 'yyyy'),
                 TO_CHAR(I.SDATE, 'mm'),
                 TO_CHAR(I.SDATE, 'yyyy-mm-dd'),
                 TO_CHAR(I.SDATE, 'hh24:mi:ss'),
                 -1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5),
                 ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5),
                 NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL'),
                 NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL'),
                 C.SPEC
                 ", sep="")
dfo.cod <- sqlQuery(chan, qu.tows, stringsAsFactors=FALSE)
## 
#write.csv(dfo.cod, "Y:/Offshore scallop/Assessment/SPERA/Data/RV_survey_CA&US/DFO_GB_summer_cod.csv" , row.names=FALSE)

####
#### yellowtail flounder, limited to GEORGES sets on valid tows (Type=1)
qu.tows <- paste("
                 SELECT 
                 I.MISSION,
                 I.SETNO,
                 I.GEAR,
                 I.DIST,
                 NVL(TO_CHAR(I.STRAT),'NULL') STRAT,
                 TO_CHAR(I.SDATE, 'yyyy') YEAR,
                 TO_CHAR(I.SDATE, 'mm') MONTH,
                 TO_CHAR(I.SDATE, 'yyyy-mm-dd') TOWDATE,
                 TO_CHAR(I.SDATE, 'hh24:mi:ss') TOWTIME,
                 -1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
                 ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5) SLA,
                 NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL') ELO,
                 NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL') ELA,
                 C.SPEC,
                 --   SUM(C.TOTNO) TOTNORAW,
                 --   SUM(C.TOTWGT) TOTWGTRAW,
                 SUM(C.TOTNO)  * (1.75/I.DIST) TOTNO,
                 SUM(C.TOTWGT) * (1.75/I.DIST) TOTWGT
                 FROM GROUNDFISH.GSINF I,
                 GROUNDFISH.GSCAT C
                 WHERE 
                 C.MISSION  = I.MISSION
                 AND C.SETNO    = I.SETNO
                 AND C.SPEC    IN (42) --10; 42
                 AND I.TYPE = 1
                 --GEORGES
                 AND TO_CHAR(I.SDATE, 'mm') IN (6,7,8)
                 AND I.STRAT IN ('551','552','553','554','555','556','557','5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8')--'470','471','472','473','474','475','476','477','478','479','480','481','482','483','484','485','486','487','488','489','490','491','492','493','494','495')
                 GROUP BY 
                 I.MISSION,
                 I.SETNO,
                 I.GEAR,
                 I.DIST,
                 NVL(TO_CHAR(I.STRAT),'NULL'),
                 TO_CHAR(I.SDATE, 'yyyy'),
                 TO_CHAR(I.SDATE, 'mm'),
                 TO_CHAR(I.SDATE, 'yyyy-mm-dd'),
                 TO_CHAR(I.SDATE, 'hh24:mi:ss'),
                 -1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5),
                 ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5),
                 NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL'),
                 NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL'),
                 C.SPEC
                 ", sep="")
dfo.ytf <- sqlQuery(chan, qu.tows, stringsAsFactors=FALSE)
## 
#write.csv(dfo.ytf, "Y:/Offshore scallop/Assessment/SPERA/Data/RV_survey_CA&US/DFO_GB_summer_ytf.csv", row.names=FALSE)

##All valid GEORGES sets tows, regardless of catch  
qu.tows <- paste("
                 SELECT 
                 I.MISSION,
                 I.SETNO,
                 I.GEAR,
                 NVL(TO_CHAR(I.STRAT),'NULL') STRAT,
                 TO_CHAR(I.SDATE, 'yyyy') YEAR,
                 TO_CHAR(I.SDATE, 'mm') MONTH,
                 TO_CHAR(I.SDATE, 'yyyy-mm-dd') TOWDATE,
                 TO_CHAR(I.SDATE, 'hh24:mi:ss') TOWTIME,
                 -1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
                 ROUND(TRUNC(SLAT            /100)+MOD(SLAT,100)/60,5) SLA,
                 NVL(TO_CHAR(                -1*ROUND(TRUNC(ELONG/100)+MOD(ELONG,100)/60,5)),'NULL') ELO,
                 NVL(TO_CHAR(ROUND(TRUNC(ELAT/100)+MOD(ELAT,100)/60,5)), 'NULL') ELA
                 FROM GROUNDFISH.GSINF I
                 WHERE  I.TYPE = 1
                 --GEORGES
                 AND TO_CHAR(I.SDATE, 'mm') IN (6,7,8)
                 AND I.STRAT IN ('551','552','553','554','555','556','557','5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8') --'470','471','472','473','474','475','476','477','478','479','480','481','482','483','484','485','486','487','488','489','490','491','492','493','494','495')
                 ", sep="")
dfo.tows <- sqlQuery(chan, qu.tows, stringsAsFactors=FALSE)
## 
#write.csv(dfo.tows, "Y:/Offshore scallop/Assessment/SPERA/Data/RV_survey_CA&US/DFO_GB_summer_tows.csv", row.names=FALSE)

###
### Stitch 3 files together - merge based on set, add cod/ytf columns

#join tows with cod and ytf - drop SPEC and add "Cod" and "ytf" to TOTNO and TOTWGT
join <- full_join(dfo.tows, dfo.cod)
join$SPEC<-NULL
colnames(join)[which(names(join) == "TOTNO")] <- "Cod_TOTNO"
colnames(join)[which(names(join) == "TOTWGT")] <- "Cod_TOTWGT"
#join[is.na(join)] <- 0

join2 <- full_join(dfo.tows, dfo.ytf)
join2$SPEC<-NULL
colnames(join2)[which(names(join2) == "TOTNO")] <- "Ytf_TOTNO"
colnames(join2)[which(names(join2) == "TOTWGT")] <- "Ytf_TOTWGT"
#join2[is.na(join2)] <- 0

join3 <-right_join(join, join2)
join3[is.na(join3)] <- 0
write.csv(join3, "Y:/Projects/SPERA/Data/RV_survey_CA&US/DFO_GB_summer_tows_cod_ytf.csv", row.names=FALSE)

## Check that locations are sensible. 
loc <- cbind(dfo.tows$SLO, dfo.tows$SLA)
plot(loc)

####Join DFO spring and summer surveys
spr<-read.csv("Y:/Projects/SPERA/Data/RV_survey_CA&US/DFO_GB_spring_tows_cod_ytf.csv")
sum<-read.csv("Y:/Projects/SPERA/Data/RV_survey_CA&US/DFO_GB_summer_tows_cod_ytf.csv")
## Check that locations are sensible. 
loc <- cbind(spr$SLO, spr$SLA)
plot(loc)
loc2 <- cbind(sum$SLO, sum$SLA)
points(loc2, pch = 2, col= 3)

#Join summer and winter surveys on top of each other? 
join4 <-full_join(spr, sum)
write.csv(join4, "Y:/Projects/SPERA/Data/RV_survey_CA&US/DFO_GB_spring+summer_tows_cod_ytf.csv", row.names=FALSE)


odbcClose(chan)

####################
####################
##US - NMFS Queries
