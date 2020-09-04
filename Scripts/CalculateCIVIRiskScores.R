# ##########################################################################
# ##########################################################################
# Script name: CalculateCIVIRiskScores.R
# Date: January 25/2019
# Author: Philip Greyson
# This script reads in the CIVI and component sub-index values from the 
# database and splits them into # three scored bins for display on the 
# CIVI website.  Scoring is done on a regional basis (Atlantic and Pacific).
# ##########################################################################
# ##########################################################################

rm(list = ls())

library(RODBC) # needed to connect to MS Access database
library(moments) # needed for the skewness calculation
library(plyr) # needed for rbind.fill()

# Change directory as needed
db <- "E:/GIS/Projects/BIO/SmallCraftHarbour/Infrastructure/SCH_2000_Final.mdb"

# Connect to MS Access db
chan <- odbcConnectAccess(db)

# Import calculated CIVI and associated Sub-indices
CIVI <- sqlFetch(chan, "0000CalculateCIVI")

# Break dataframe into atlantic and pacific

tmp_atlantic <- CIVI[which(CIVI$Zone=="Atlantic"),]
tmp_pacific <- CIVI[which(CIVI$Zone=="Pacific"),]

# Test distribution and find best normalization method

skewness(tmp_atlantic$ESI)
skewness(tmp_atlantic$CIVI, na.rm = TRUE)
skewness(tmp_atlantic$ISI, na.rm = TRUE)
skewness(tmp_atlantic$SESI, na.rm = TRUE)
skewness(tmp_pacific$ESI^3)
skewness(tmp_pacific$CIVI^3, na.rm = TRUE)
skewness(tmp_pacific$ISI, na.rm = TRUE)

hist(tmp_atlantic$ESI)
hist(tmp_atlantic$CIVI)
hist(tmp_atlantic$ISI)
hist(tmp_atlantic$SESI)
hist(tmp_pacific$ESI^4)
hist(tmp_pacific$CIVI^3)
hist(tmp_pacific$ISI)

# 2020 data#
skewness(tmp_atlantic$ESI)
skewness(tmp_atlantic$CIVI, na.rm = TRUE)
skewness(tmp_atlantic$ISI, na.rm = TRUE)
skewness(tmp_atlantic$SESI, na.rm = TRUE)
skewness(tmp_pacific$ESI^3)
skewness(tmp_pacific$CIVI^2, na.rm = TRUE)
skewness(tmp_pacific$ISI, na.rm = TRUE)

hist(tmp_atlantic$ESI)
hist(tmp_atlantic$CIVI)
hist(tmp_atlantic$ISI)
hist(tmp_atlantic$SESI)
hist(tmp_pacific$ESI^2)
hist(tmp_pacific$CIVI^2)
hist(tmp_pacific$ISI)


# End 

# divide each variable distribution into 3 divisions (Low, Medium, High)
# ----- Atlantic ---
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"ESI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("EScore") #Name for new field
# Cut the ISI distribution into 3 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"ISI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("IScore") #Name for new field
# Cut the SESI into 3 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"SESI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("SScore") #Name for new field
# Cut the CIVI into 3 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"CIVI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("CScore") #Name for new field

# ----- Pacific ---
# ------ there's no SE sub-index for the Pacific region -----
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"ESI"]^3)),breaks = 3, labels=1:3) #Score the tmp_pacific based on 3 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("EScore") #Name for new field
# Cut the ISI distribution into 3 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"ISI"])),breaks = 3, labels=1:3) #Score the tmp_pacific based on 3 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("IScore") #Name for new field
# Cut the CIVI into 3 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"CIVI"]^3)),breaks = 3, labels=1:3) #Score the tmp_pacific based on 3 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("CScore") #Name for new field

# 2020 update
# divide each variable distribution into 3 divisions (Low, Medium, High)
# ----- Atlantic ---
names(tmp_atlantic)
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"ESI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("EScore") #Name for new field
# Cut the ISI distribution into 3 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"ISI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("IScore") #Name for new field
# Cut the SESI into 3 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"SESI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("SScore") #Name for new field
# Cut the CIVI into 3 bins
tmp_atlantic[length(tmp_atlantic)+1]<-cut(as.vector(t(tmp_atlantic[,"CIVI"])),breaks = 3, labels=1:3) #Score the tmp_atlantic based on 3 equal ranges
names(tmp_atlantic)[length(tmp_atlantic)]<- c("CScore") #Name for new field

# ----- Pacific ---
# ------ there's no SE sub-index for the Pacific region -----
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"ESI"]^2)),breaks = 3, labels=1:3) #Score the tmp_pacific based on 3 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("EScore") #Name for new field
# Cut the ISI distribution into 3 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"ISI"])),breaks = 3, labels=1:3) #Score the tmp_pacific based on 3 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("IScore") #Name for new field
# Cut the CIVI into 3 bins
tmp_pacific[length(tmp_pacific)+1]<-cut(as.vector(t(tmp_pacific[,"CIVI"]^2)),breaks = 3, labels=1:3) #Score the tmp_pacific based on 3 equal ranges
names(tmp_pacific)[length(tmp_pacific)]<- c("CScore") #Name for new field



# remove extra fields from dataframes
tmp_atlantic[2:7] <- list(NULL)
tmp_pacific[2:7] <- list(NULL)

names(tmp_atlantic)
names(tmp_pacific)

# -------------------Put the two zones back into one file ----------------------
# using the rbind.fill() append pacific to atlantic, adding in missing values in
# pacific for SScore
# rbind.fill() does the equivalent of an SQL outer join

allScore <- rbind.fill(tmp_atlantic,tmp_pacific)

# alternatively, I could add a 'SScore' field to tmp_Pacific
# and then reorder the fields to match tmp_pacfic
# tmp_pacific$SScore <- NA
# tmp_pacific=tmp_pacific[,c("SCHCode", "EScore",  "IScore",  "SScore",  "CScore")]

# remove temporary dataframes
rm(list = ls(pattern = "^tmp"))

# Delete all old records from the CIVI_Scores table in Access
sqlQuery(chan, "DELETE * FROM CIVI_Scores")


# Save data back to existing table in Access
sqlSave(chan,allScore, tablename = "CIVI_Scores", rownames = FALSE, safer = FALSE, append = TRUE)
# ##########################################################################
# End script
# ##########################################################################