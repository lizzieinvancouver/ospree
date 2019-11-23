rm(list=ls()) 
options(stringsAsFactors = FALSE)

#faith playing with teh TRY data cleaning

setwd("C:\\Users\\Faith Jones\\Documents\\ubc\\OspreeTraits")

library(tidyr)
library(data.table)

#read in teh try data 
tryData <- fread("TRYtraitdataNov2019.txt")
names(tryData)
View(tryData)
#select the first 20 observations
tryData20 <- tryData[tryData$ObservationID %in%  unique(tryData$ObservationID)[1:2000],]
tryData20$ObservationID_Trait <- paste(tryData20$Observation, tryData20$TraitID, sep = "_")

#long to wide data  
#------------

colums <- c("LastName","FirstName","DatasetID","Dataset",
	"SpeciesName","ObservationID","ObsDataID","OrigUnitStr", "ObservationID_Trait", 
	"DataName", "StdValue","OrigValueStr")

colums2 <- c("LastName","FirstName","DatasetID","Dataset",
	"SpeciesName","ObservationID","ObsDataID","OrigUnitStr", "ObservationID_Trait", 
	"DataName2", "StdValue","OrigValueStr")



#try in dplyr for all - doesnt work, too big. onlyt 2000 observation ids 
dataA1 <- tryData20 %>%
	select(colums)%>% 
	group_by(ObservationID_Trait) %>%
	spread(key = DataName, value = OrigValueStr)

tryData20$DataName2 <- tryData20$DataName
tryData20$DataName2 <-  paste("std",tryData20$DataName, sep="_")

#try in dplyr for all - doesnt work, too big. onlyt 2000 observation ids 
dataA2 <- tryData20 %>%
	group_by(ObservationID_Trait) %>%
	select(colums2)%>% 
	spread(key = DataName2, value = StdValue)


#combined stadard and original data 
outAll3 <- merge(dataA1, dataA2, by = c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","ObsDataID", "ObservationID_Trait"))
outAll3$ObsDataID <- NULL #we dont want thsi column because it is the id of each row, and we are trying to collapse rows to plant observations

head(outAll3)

variableColumns <- colnames(outAll3)[!colnames(outAll3) %in%  c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID", "ObservationID_Trait")]

#Try to collapse for individual observations
#----------------------------------------------------------

Observation1 <- outAll3[outAll3$ObservationID == "1135904",]
Observation2 <- outAll3[outAll3$ObservationID == "93437",]

collapsed1 <- Observation1 %>% 
    group_by(ObservationID) %>%
	select_if(~sum(!is.na(.)) > 0) %>%
    summarise_each(funs(first(.[!is.na(.)]))) %>% 
    ungroup()

collapsed2 <- Observation2 %>% 
    group_by(ObservationID) %>%
	select_if(~sum(!is.na(.)) > 0) %>%
    summarise_each(funs(first(.[!is.na(.)]))) %>% 
    ungroup()

bind_rows(collapsed1 , collapsed2 )
#try for multiple  ObservationIDs 

#try to loop through multiple two observations
#------------------------------------------------

trialData <- outAll3[outAll3$ObservationID %in% c("1135904","93437"),]

observationList <- list()
counter <- 1

for(i in unique(trialData$ObservationID)){

	observationDatai <- trialData[trialData$ObservationID == i,]

	collapsedi <- data.frame(observationDatai %>%
		group_by(ObservationID) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
    		summarise_each(funs(first(.[!is.na(.)]))) %>% 
    		ungroup())

	observationList[[counter]] <- collapsedi

	counter <- counter + 1
}


TwoObservations <- bind_rows(observationList)

#loop through all observations 
#----------------------------------

observationListAll <- list()
counter <- 1

for(i in unique(outAll3$ObservationID_Trait)){

	observationDatai2 <- outAll3[outAll3$ObservationID_Trait == i,]

	collapsedi2 <- data.frame(observationDatai2 %>%
		group_by(ObservationID_Trait) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
    		summarise_each(funs(first(.[!is.na(.)]))) %>% 
    		ungroup())

	observationListAll[[counter]] <- collapsedi2

	counter <- counter + 1
}


allObservations <- bind_rows(observationListAll)
head(allObservations)

write.csv( allObservations, "subsetTry2.csv")













