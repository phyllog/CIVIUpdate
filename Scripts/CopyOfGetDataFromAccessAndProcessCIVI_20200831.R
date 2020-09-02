# ##########################################################################
# ##########################################################################
# Script name: GetDataFromAccessAndProcessCIVI_test_Final.R
# Date: January 25/2019
#
# Script updated Aug 31/2020 to incorporate the updates to HC and TRC
# from Small craft harbours
# Author: Philip Greyson
# This script reads in the CIVI and component sub-index values from the 
# database and splits them into # three scored bins for display on the 
# CIVI website.  Scoring is done on a regional basis (Atlantic and Pacific).
# ##########################################################################
# ##########################################################################

rm(list = ls())

library(RODBC) # needed to connect to MS Access database
library(moments) # needed for the skewness calculation


db <- "E:/GIS/Projects/BIO/SmallCraftHarbour/Infrastructure/SCH_2000_Final.mdb"


# Connect to MS Access db
chan <- odbcConnectAccess(db)

# Import harbour information
SCH <- sqlFetch(chan, "SCH_All")

# Import CanCoast records as the Environmental/Exposure variables ###
# Import the infrastructure and socio-economic variables
EVar <- sqlFetch(chan, "ExposureVariables_all")
IVar <- sqlFetch(chan, "qry_00001AllI_Vars")
SEVar <- sqlFetch(chan, "qry_00001AllSE_Vars")

# break Exposure variables into three zones
tmp_atlantic <- EVar[which(EVar$Zone=="Atlantic"),]
tmp_pacific <- EVar[which(EVar$Zone=="Pacific"),]
tmp_arctic <- EVar[which(EVar$Zone=="Arctic"),]

# -------Exposure variable coding ------------------#

# -- Check the distributions of the variables ====#
# Generate skewness values for untransformed variables (WaveHeight and WindSpeed)
var1 <- c("Atlantic", "Pacific")
var2 <- c("WaveHeight","WindSpeed")
for (i in 1:length(var1)) {
  df=EVar[which(EVar$Zone==var1[i]),]
  for (j in 1:length(var2)) {
    cat(var1[i],var2[j],round(skewness(df[,var2[j]]),3),sep = "\t", "\n")
  }  #print(skewness(atlantic[,var[i]]))
}

# Generate skewness values for arctic Windspeed and WaveHeight
cat("Arctic","Wind",round(skewness(ArcWind[,"WindSpeed"]),3),sep = "\t")
cat("Arctic","Wave",round(skewness(ArcWave[,"WaveHeight"]),3),sep = "\t")

# Generate skewness values for absolute value of SeaIceWeek and SLC100
var1 <- c("Atlantic", "Pacific","Arctic")
var2 <- c("SeaIceWeek","SLC2100")
for (i in 1:length(var1)) {
  df=EVar[which(EVar$Zone==var1[i]),]
  for (j in 1:length(var2)) {
    cat(var1[i],var2[j],round(skewness(abs(df[,var2[j]])),3),sep = "\t", "\n")
  }  #print(skewness(atlantic[,var[i]]))
}

# #good
# skewness(abs(arctic[,"SeaIceWeek"])^0.5)
# #good
# skewness(abs(atlantic[,"SeaIceWeek"])^0.5)
# #good
# skewness(log10(ArcWave[,"WaveHeight"]))
# skewness((ArcWave[,"WaveHeight"])^-.33)
# #good
# skewness((pacific[,"WaveHeight"])^2)
# #good
# skewness((pacific[,"WindSpeed"])^4)

# Code to check distributions of exposure scores

#-------------------------------------------------------------------------------------#
# -- Skewness results and whether transformations are necessary (based upon the assumption
# -- that any distribution with a abs(skewness) > 0.5)
# "If skewness is between -1/2 and 1/2, the distribution is approximately symmetric."
# From http://brownmath.com/stat/shape.htm#SkewnessInterpret
# Atlantic	WaveHeight	-0.001	No		
# Atlantic	WindSpeed	-0.183	No		
# Pacific	WaveHeight	-0.84	Yes	^2	-0.235
# Pacific	WindSpeed	-1.206	Yes	^4 	-0.424
# Arctic	Wind	0.173	No		
# Arctic	Wave	1.202	Yes	^0.33	0.358
# 
# Absolute value of Sea ice and SLC					
# Atlantic	SeaIceWeek	1.182	Yes	^0.5	-0.004
# Atlantic	SLC2100	-0.502	No		
# Pacific	SeaIceWeek	NaN			
# Pacific	SLC2100	0.235	No		
# Arctic	SeaIceWeek	0.977	Yes	^0.5	-0.182
# Arctic	SLC2100	0.213	No		
#-------------------------------------------------------------------------------------#

# Break atlantic variables into 5 equal interval bins based upon the distribution
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"WaveHeight"])),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneWHSCORE") #Name for new field
# Cut the Windspeed distribution into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"WindSpeed"])),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneWSSCORE") #Name for new field
# Cut the Absolute value of SLC distribution into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(abs(tmp_atlantic[,"SLC2100"]))),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneSLCSCORE") #Name for new field
# Cut the Absolute value of transformed Sea ice distribution into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(abs(tmp_atlantic[,"SeaIceWeek"])^0.5)),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneIceSCORE") #Name for new field
#names(tmp_atlantic)
# delete the main variables from tmp_atlantic, leaving only the Zone Scores
tmp_atlantic[2:10] <- list(NULL)
# End Atlantic ====

# Pacific - break into equal divisions ====
# Break pacific variables into 5 equal interval bins based upon the distribution
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"WaveHeight"]^2)),breaks = 5, labels=1:5) #Score the tmp_pacific based on 5 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("ZoneWHSCORE") #Name for new field
# Cut the Windspeed distribution into 5 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"WindSpeed"]^4)),breaks = 5, labels=1:5) #Score the tmp_pacific based on 5 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("ZoneWSSCORE") #Name for new field
# Cut the Absolute value of SLC distribution into 5 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(abs(tmp_pacific[,"SLC2100"]))),breaks = 5, labels=1:5) #Score the tmp_pacific based on 5 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("ZoneSLCSCORE") #Name for new field
#names(tmp_pacific)
# delete the main variables from tmp_pacific, leaving only the Zone Scores
tmp_pacific[2:10] <- list(NULL)
# Add in a blank variable for ZoneIceScore
tmp_pacific$ZoneIceSCORE <- 1 
# names(tmp_pacific)
# End Pacific ====#

#-------------------Arctic scoring---------------------------------------#

# windspeed and waveheight in the Arctic have some -99 values (where the grids didn't cover)
# remove those from the calculation of scores
tmp_ArcWave <- tmp_arctic[which(tmp_arctic$WaveHeight>0),]
tmp_ArcWind <- tmp_arctic[which(tmp_arctic$WindSpeed>0),]

# Code tmp_arctic variables
# Cut the Arctic Waveheight distribution
tmp_ArcWave[length(tmp_ArcWave)+1]<-cut(as.vector(t(tmp_ArcWave[,"WaveHeight"]^0.33)),breaks = 5, labels=1:5) #Score the tmp_arctic based on 5 equal ranges
names(tmp_ArcWave)[length(tmp_ArcWave)]<- c("ZoneWHSCORE") #Name for new field
# Cut the Arctic Windspeed distribution into 5 bins
tmp_ArcWind[length(tmp_ArcWind)+1]<-cut(as.vector(t(tmp_ArcWind[,"WindSpeed"])),breaks = 5, labels=1:5) #Score the tmp_arctic based on 5 equal ranges
names(tmp_ArcWind)[length(tmp_ArcWind)]<- c("ZoneWSSCORE") #Name for new field
# Cut the Absolute value of Sea Ice distribution into 5 bins
tmp_arctic[length(tmp_arctic)+1]<-cut(as.vector(t(abs(tmp_arctic[,"SLC2100"]))),breaks = 5, labels=1:5) #Score the tmp_arctic based on 5 equal ranges
names(tmp_arctic)[length(tmp_arctic)]<- c("ZoneSLCSCORE") #Name for new field
# Cut the Absolute value of transformed SLC distribution into 5 bins
tmp_arctic[length(tmp_arctic)+1]<-cut(as.vector(t(abs(tmp_arctic[,"SeaIceWeek"])^0.5)),breaks = 5, labels=1:5) #Score the tmp_arctic based on 5 equal ranges
names(tmp_arctic)[length(tmp_arctic)]<- c("ZoneIceSCORE") #Name for new field

#names(tmp_arctic)
tmp_arctic[2:10] <- list(NULL)
tmp_ArcWave[2:10] <- list(NULL)
tmp_ArcWind[2:10] <- list(NULL)

tmp_arctic1 <- merge(tmp_ArcWave, tmp_ArcWind, by="OBJECTID", all=FALSE, all.x=TRUE)
tmp_arctic2 <- merge(tmp_arctic1, tmp_arctic, by="OBJECTID", all=FALSE, all.y=TRUE)

# Code all NA values as 1 (one)
tmp_arctic2[is.na(tmp_arctic2)] <- 1

# End Arctic ====#

# Put the three zones back into one file
allEV <-rbind(tmp_atlantic,tmp_pacific,tmp_arctic2)
names(tmp_atlantic)
names(tmp_pacific)
names(tmp_arctic2)
names(allEV)

rm(list = ls(pattern = "^tmp"))

#summary(ExpSI)

# ------------------------End of Exposure Variable section---------------------------#

# Process Infrastructure variables


# Reverse order of degree of protection (DOP) and harbour condition (HC)
# DOP is already in a categorial 1-5 range so the reversed order
# becomes the final score (ZoneDOP)
# break into two zones (atlantic and pacific)
IVar$HCrev <- abs(IVar$HC-6)
IVar$ZoneDOP <- abs(IVar$DOP-6)

tmp_atlantic <- IVar[which(IVar$Zone=="Atlantic"),]
tmp_pacific <- IVar[which(IVar$Zone=="Pacific"),]

# Generate skewness values for untransformed variables
var1 <- c("Atlantic", "Pacific")
var2 <- c("HCrev","TRC")
for (i in 1:length(var1)) {
  df=IVar[which(IVar$Zone==var1[i]),]
  for (j in 1:length(var2)) {
    cat(var1[i],var2[j],round(skewness(df[,var2[j]],na.rm = TRUE),3),sep = "\t", "\n")
  }  #print(skewness(tmp_atlantic[,var[i]]))
}

# With update the HCrev (both Atlantic and Pacific) don't need transformtion

hist(tmp_atlantic$TRC)
hist(tmp_atlantic$HCrev)
hist(tmp_atlantic$HCrev^0.5)

hist(tmp_pacific$HCrev)
hist(tmp_pacific$HCrev^0.5)

# good
skewness(tmp_atlantic$HCrev^0.5, na.rm = TRUE)
hist(tmp_atlantic$HCrev^0.5)
# can't get a good transformation of TRC but Log10 is close
# log10 is the best possible transformation (skewness = hist(log10(tmp_atlantic$TRC)))

skewness(log10(tmp_atlantic$TRC), na.rm = TRUE)
hist(log10(tmp_atlantic$TRC))

#--------------------------------------------------------#
# Using updated data (August 2020)

# HCrev has skewness of 0.693 and -0.118, Atlantic, Pacific respectively
# no need to transform

skewness(log10(tmp_atlantic$TRC), na.rm = TRUE) # not good
skewness(tmp_atlantic$TRC^0.5, na.rm = TRUE) # not good
skewness(tmp_atlantic$TRC^0.33, na.rm = TRUE) # best (skewness = 0.48)

hist(tmp_atlantic$TRC^0.33)

skewness(log10(tmp_pacific$TRC), na.rm = TRUE)
hist(log10(tmp_pacific$TRC))

# Updated HC needs no transformation
# Updated TRC needs ^0.33 for Atlantic and log10 for Pacific

#--------------------------------------------------------#
# END Using updated data (August 2020)

# HCrev has skewness of 0.693 and -0.118, Atlantic, Pacific respectively
# no need to transform
#--------------------------------------------------------#

#good
skewness(tmp_pacific$HCrev^0.5, na.rm = TRUE)
hist(tmp_pacific$HCrev^0.5)

# good skewness = 0.026
skewness(log10(tmp_pacific$TRC), na.rm = TRUE)
hist(log10(tmp_pacific$TRC))


# Break tmp_atlantic variables into 5 equal interval bins based upon the distribution
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"HCrev"]^0.5)),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneHC") #Name for new field
# Cut the TRC distribution into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(log10(tmp_atlantic[,"TRC"]))),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneTRC") #Name for new field
names(tmp_atlantic)

#--------------------------------------------------------#
# - For 2020 update values
#--------------------------------------------------------#
# Atlantic
# Break tmp_atlantic variables into 5 equal interval bins based upon the distribution
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"HCrev"])),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneHC") #Name for new field
# Cut the TRC distribution into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"TRC"]^0.33)),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneTRC") #Name for new field
names(tmp_atlantic)

# Pacific
# Break tmp_pacific variables into 5 equal interval bins based upon the distribution
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"HCrev"])),breaks = 5, labels=1:5) #Score the tmp_pacific based on 5 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("ZoneHC") #Name for new field
# Cut the TRC distribution into 5 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(log10(tmp_pacific[,"TRC"]))),breaks = 5, labels=1:5) #Score the tmp_pacific based on 5 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("ZoneTRC") #Name for new field
names(tmp_pacific)
names(IVar)

#--------------------------------------------------------#
# - END 2020 update values
#--------------------------------------------------------#






# Break tmp_pacific variables into 5 equal interval bins based upon the distribution
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"HCrev"]^0.5)),breaks = 5, labels=1:5) #Score the tmp_pacific based on 5 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("ZoneHC") #Name for new field
# Cut the TRC distribution into 5 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(log10(tmp_pacific[,"TRC"]))),breaks = 5, labels=1:5) #Score the tmp_pacific based on 5 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("ZoneTRC") #Name for new field
names(tmp_pacific)
names(IVar)

names(tmp_atlantic)
names(tmp_pacific)

# am I supposed to remove all fields 2 - 6?  
tmp_atlantic[2:6] <- list(NULL)
tmp_pacific[2:6] <- list(NULL)
# Put the two zones back into one file
allIV <-rbind(tmp_atlantic,tmp_pacific)
names(allIV)

write.csv(allIV, "../Spreadsheets/allIV.csv", row.names = FALSE, col.names = TRUE)

rm(list = ls(pattern = "^tmp"))

#-----------------------------------------------------------------------
# Process SE variables
names(SEVar)

# Reverse population values.  Higher population is considered less vulnerable

# Since I couldn't normalize the reversed population values I normalized the 
# raw values, scored them and then reversed the scoring
# So I don't need to create a field for PopRev
#  SEVar$PopRev <- abs(SEVar$Pop-(max(SEVar$Pop,na.rm = TRUE)+1))

# Break up into atlantic (there's no SE data for the Pacific SCHs)

tmp_atlantic <- SEVar[which(SEVar$Zone=="Atlantic"),]

# Generate skewness values for untransformed variables
var1 <- c("Atlantic")
var2 <- c("PopRev","PropIncome","QPerVessel")
for (i in 1:length(var1)) {
  df=SEVar[which(SEVar$Zone==var1[i]),]
  for (j in 1:length(var2)) {
    cat(var1[i],var2[j],round(skewness(df[,var2[j]],na.rm = TRUE),3),sep = "\t", "\n")
  }  #print(skewness(atlantic[,var[i]]))
}

hist(SEVar$PropIncome)
hist(SEVar$QPerVessel)


# this seems to be the best transformation on the Population values
skewness(log10(SEVar$Pop), na.rm = TRUE)
hist(log10(SEVar$Pop))

# good
summary(SEVar$PropIncome)
hist(SEVar$PropIncome)
skewness(SEVar$PropIncome^0.5, na.rm = TRUE)
hist(SEVar$PropIncome^0.5)
# Better
skewness(SEVar$PropIncome^0.33, na.rm = TRUE)
hist(SEVar$PropIncome^0.33)

# good
skewness(log10(SEVar$QPerVessel), na.rm = TRUE)
skewness(log10(SEVar$QPerVessel), na.rm = TRUE)
hist(log10(SEVar$QPerVessel))

#names(SEVar)
#names(tmp_atlantic)
#summary(tmp_atlantic)

tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"PropIncome"]^0.33)),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZonePropIncome") #Name for new field
# Cut the TRC distribution into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(log10(tmp_atlantic[,"QPerVessel"]))),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneQPerVessel") #Name for new field
# Cut the Population into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(log10(tmp_atlantic[,"Pop"]))),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZonePop") #Name for new field
# Reverse the order of the Population scores so that a higher population has a lower vulnerability score
tmp_atlantic$ZonePop <- abs(as.numeric(tmp_atlantic$ZonePop)-6)

tmp_atlantic[2:5] <- list(NULL)

allSE <- tmp_atlantic
names(allSE)
# Remove all temporary dataframes
rm(list = ls(pattern = "^tmp"))

#--------------------------------------------------------------#
# Export all new Zone scores to table in Access

# Clear the table in Access and append new records
# sqlClear is the equivalent of a SQL TRUNCATE command (deletes all records).  There's also 
# sqlDrop to delete a table

names(allEV)
names(allIV)
names(allSE)


# sqlClear doesn't work, perhaps because it's sending a TRUNCATE command which Jet SQL doesn't use
# sqlClear(chan, "CIVI_ExpVar")
# sqlClear(chan, "CIVI_InVar")
# sqlClear(chan, "CIVI_SEVar")

sqlQuery(chan, "DELETE * FROM CIVI_ExpVar")
sqlQuery(chan, "DELETE * FROM CIVI_InVar")
sqlQuery(chan, "DELETE * FROM CIVI_SEVar")

# in the 2020 update Harbour 3784 is in the original CIVI_InVar TABLE but not the update
sqlQuery(chan, "DELETE * FROM CIVI_InVar WHERE SCHCode <> 3784")


# Save data back to existing table in Access
sqlSave(chan,allEV, tablename = "CIVI_ExpVar", rownames = FALSE, safer = FALSE, append = TRUE)
sqlSave(chan,allIV, tablename = "CIVI_InVar", rownames = FALSE, safer = FALSE, append = TRUE)

# updates to Infrastructure (HC and TRC) loaded August 31/2020

sqlSave(chan,allSE, tablename = "CIVI_SeVar", rownames = FALSE, safer = FALSE, append = TRUE)

#--------------------------------------------------------------------------------
# Alternate ways to get data from Access.  Run SQL directly.  

# Set SQL commands into a variable
qu <- paste("
            SELECT ExposureVariables_all.NTS_SHEET,
            ROUND(([SeaIceScore]*[SLC2100_Score]*[TideScore]*[WHScore]*[WSScore]*[ExposureVariables_all].[CoastalScore])^0.2,1) AS ExpSIAll,
            ExposureSI.ExpSI,
            [ExpSIAll]-[ExpSI] AS Diff
            FROM ExposureVariables_all
            INNER JOIN ExposureSI
            ON ExposureVariables_all.OBJECTID = ExposureSI.OBJECTID;")

# Bring in SE variables
#  working


qu <- paste("SELECT S.SCHCode,
            N.TitleCaseName,
            S.Type,
            S.MarineInland,
            S.SelectionZone,
            Per.PercentIncomeFish,
            Pop.Pop,
            Q.QuantPerVes
            FROM (((SCH_All S
            LEFT JOIN SCH_SE_PercentIncome Per
            ON S.SCHCode = Per.HarbourCode)
            LEFT JOIN SCH_SE_Population Pop
            ON S.SCHCode = Pop.HarbourCode)
            LEFT JOIN SCH_SE_QuantPerVessel Q
            ON S.SCHCode = Q.HarbourCode)
            LEFT JOIN Name_ProperCase N
            ON S.SCHCode         = N.SCHCode
            WHERE (((S.Type)     ='core'
            OR (S.Type)          ='non-core')
            AND ((S.MarineInland)='marine'));")


# query the database using above SQL and store data in dataframe
ExpSI <- sqlQuery(chan,qu, stringsAsFactors=FALSE)

SEVar <- sqlQuery(chan,qu, stringsAsFactors=FALSE)

# Bring in Infrastructure variables
# works
qu <- paste("SELECT S.SCHCode,
            N.TitleCaseName,
            S.Type,
            S.MarineInland,
            S.SelectionZone,
            H.HarbourCondition,
            T.TotalReplacementCost,
            D.DegreeOfProtection
            FROM (((SCH_All S
            LEFT JOIN SCH_I_HarbourCondition H
            ON S.SCHCode = H.HarbourCode)
            LEFT JOIN SCH_I_TotalReplacementCost T
            ON S.SCHCode = T.HarbourCode)
            INNER JOIN Name_ProperCase N
            ON S.SCHCode = N.SCHCode)
            LEFT JOIN SCH_I_DegreeOfProtection AS D
            ON S.SCHCode         = D.HarbourCode
            WHERE (((S.Type)     ='Core'
            OR (S.Type)          ='Non-Core')
            AND ((S.MarineInland)='marine'));")

IVar <- sqlQuery(chan,qu, stringsAsFactors=FALSE)
# ##########################################################################
# End script
# ##########################################################################
