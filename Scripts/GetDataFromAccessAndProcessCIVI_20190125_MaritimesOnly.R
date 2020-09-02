# ##########################################################################
# ##########################################################################
# Script name: GetDataFromAccessAndProcessCIVI_test_Final.R
# Date: January 25/2019
# Author: Philip Greyson
# This script reads in the CIVI and component sub-index values from the 
# database for the Maritimes Region only.
# ##########################################################################
# ##########################################################################

rm(list = ls())

library(RODBC) # needed to connect to MS Access database
library(moments) # needed for the skewness calculation

db <- "E:/GIS/Projects/BIO/SmallCraftHarbour/Infrastructure/SCH_2000_Final_MaritimesOnly.mdb"

# Connect to MS Access db
chan <- odbcConnectAccess(db)

# Import harbour information
SCH <- sqlFetch(chan, "SCH_All")

# Import CanCoast records as the Environmental/Exposure variables ###
# Import the infrastructure and socio-economic variables
EVar <- sqlFetch(chan, "qry_00001AllE_Vars")
IVar <- sqlFetch(chan, "qry_00001AllI_Vars")
SEVar <- sqlFetch(chan, "qry_00001AllSE_Vars")


# -------Exposure variable coding ------------------#

tmp_atlantic <- EVar[which(EVar$Zone=="Atlantic"),]
names(tmp_atlantic)

# -- Check the distributions of the variables ====#
# Generate skewness values for untransformed variables (WaveHeight and WindSpeed)
var1 <- c("Atlantic")
var2 <- c("WaveHeight","WindSpeed")
for (i in 1:length(var1)) {
  df=EVar[which(EVar$Zone==var1[i]),]
  for (j in 1:length(var2)) {
    cat(var1[i],var2[j],round(skewness(df[,var2[j]]),3),sep = "\t", "\n")
  }  #print(skewness(atlantic[,var[i]]))
}

# Atlantic	WaveHeight	-0.085	
# Atlantic	WindSpeed	-0.367



# Generate skewness values for absolute value of SeaIceWeek and SLC100
var1 <- c("Atlantic")
var2 <- c("SeaIceWeek","SLC2100")
for (i in 1:length(var1)) {
  df=EVar[which(EVar$Zone==var1[i]),]
  for (j in 1:length(var2)) {
    cat(var1[i],var2[j],round(skewness(abs(df[,var2[j]])),3),sep = "\t", "\n")
  }  #print(skewness(atlantic[,var[i]]))
}

# Atlantic	SeaIceWeek	2.385	
# Atlantic	SLC2100	-0.236	
# SeaIceWeek can't be transformed well.   It ends up bimodal

# #good
hist(abs(EVar$SeaIceWeek))
skewness(-1/((abs(EVar$SeaIceWeek)+1)^0.5))
skewness(-1/((abs(EVar$SeaIceWeek)+1)^2))
hist(-1/((abs(EVar$SeaIceWeek)+1)^2))

skewness(((abs(EVar$SeaIceWeek)+1)^0.5))
hist(((abs(EVar$SeaIceWeek)+1)^0.5))

skewness(((abs(EVar$SeaIceWeek)+1)^0.33))
skewness(log((abs(EVar$SeaIceWeek)+1)^0.33))

# Code to check distributions of exposure scores

#-------------------------------------------------------------------------------------#
# -- Skewness results and whether transformations are necessary (based upon the assumption
# -- that any distribution with a abs(skewness) > 0.5)
# "If skewness is between -1/2 and 1/2, the distribution is approximately symmetric."
# From http://brownmath.com/stat/shape.htm#SkewnessInterpret
# Atlantic	WaveHeight	-0.085	No		
# Atlantic	WindSpeed	-0.367	No		
# 
# Absolute value of Sea ice and SLC					
# Atlantic	SeaIceWeek	2.385	Yes	^0.5	-0.004
# Atlantic	SLC2100	-0.236	No		

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

names(tmp_atlantic)
# delete the main variables from tmp_atlantic, leaving only the Zone Scores
tmp_atlantic[2:9] <- list(NULL)

# End Atlantic ====



# Put the three zones back into one file
allEV <-tmp_atlantic
names(tmp_atlantic)
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

# Generate skewness values for untransformed variables
var1 <- c("Atlantic")
var2 <- c("HCrev","TRC")
for (i in 1:length(var1)) {
  df=IVar[which(IVar$Zone==var1[i]),]
  for (j in 1:length(var2)) {
    cat(var1[i],var2[j],round(skewness(df[,var2[j]],na.rm = TRUE),3),sep = "\t", "\n")
  }  #print(skewness(tmp_atlantic[,var[i]]))
}

# Atlantic	HCrev	0.912	
# Atlantic	TRC	1.804

hist(tmp_atlantic$TRC)
summary(tmp_atlantic$TRC)
hist(tmp_atlantic$HCrev)


# good
# Atlantic	HCrev	0.912	
# visually HCrev looks good


hist(tmp_atlantic$HCrev^0.5)
# can't get a good transformation of TRC but Log10 is close
# log10 is the best possible transformation (skewness = hist(log10(tmp_atlantic$TRC)))
skewness((tmp_atlantic$TRC)^0.5, na.rm = TRUE)
skewness((tmp_atlantic$TRC)^0.33, na.rm = TRUE)
skewness(log10(tmp_atlantic$TRC), na.rm = TRUE)
hist((tmp_atlantic$TRC)^0.33)
hist(log10(tmp_atlantic$TRC))




# Break tmp_atlantic variables into 5 equal interval bins based upon the distribution
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"HCrev"])),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneHC") #Name for new field
# Cut the TRC distribution into 5 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t((tmp_atlantic[,"TRC"])^0.33)),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("ZoneTRC") #Name for new field
names(tmp_atlantic)


tmp_atlantic[2:6] <- list(NULL)

allIV <-tmp_atlantic
names(allIV)

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

# Atlantic	PopRev	-7.338	
# Atlantic	PropIncome	0.684	
# Atlantic	QPerVessel	4.838	

hist(SEVar$PropIncome)
hist(SEVar$QPerVessel)
hist(SEVar$Pop)


# this seems to be the best transformation on the Population values
skewness(log10(SEVar$Pop), na.rm = TRUE)
skewness((SEVar$Pop)^0.5, na.rm = TRUE)
hist(log10(SEVar$Pop))

# good
summary(SEVar$PropIncome)
hist(SEVar$PropIncome)
skewness(SEVar$PropIncome^0.5, na.rm = TRUE)
hist(SEVar$PropIncome^0.5)


# good
skewness(log10(SEVar$QPerVessel), na.rm = TRUE)
hist(log10(SEVar$QPerVessel))

#names(SEVar)
#names(tmp_atlantic)
#summary(tmp_atlantic)

tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"PropIncome"]^0.5)),breaks = 5, labels=1:5) #Score the tmp_atlantic based on 5 equal ranges
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
names(EVar)

# sqlClear doesn't work, perhaps because it's sending a TRUNCATE command which Jet SQL doesn't use
# sqlClear(chan, "CIVI_ExpVar")
# sqlClear(chan, "CIVI_InVar")
# sqlClear(chan, "CIVI_SEVar")

sqlQuery(chan, "DELETE * FROM CIVI_ExpVar")
sqlQuery(chan, "DELETE * FROM CIVI_InVar")
sqlQuery(chan, "DELETE * FROM CIVI_SEVar")


# Save data back to existing table in Access
sqlSave(chan,allEV, tablename = "CIVI_ExpVar", rownames = FALSE, safer = FALSE, append = TRUE)
sqlSave(chan,allIV, tablename = "CIVI_InVar", rownames = FALSE, safer = FALSE, append = TRUE)
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
