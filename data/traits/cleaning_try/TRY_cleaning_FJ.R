rm(list=ls()) 
options(stringsAsFactors = FALSE)

#faith playing with teh TRY data cleaning

setwd("C:\\Users\\Faith Jones\\Documents\\ubc\\OspreeTraits")

library(tidyr)
library(dplyr)
library(data.table)

#read in teh try data 
tryData <- fread("TRYtraitdataNov2019.txt")

#drop some columns to keep things simpler
names(tryData)

tryData$V28 <- NULL
tryData$Comment <- NULL
tryData$ObsDataID <- NULL

#select the first 2000 observations because my comuputer cant manage the file file at once
tryData20 <- tryData[tryData$ObservationID %in%  unique(tryData$ObservationID)[1:2000],]

head(tryData20)
unique(tryData20$TraitID)

#trying to make unique observation and Trait IDs
#tryData20$ObservationID_Trait <- paste(tryData20$Observation, tryData20$TraitID, sep = "_")
#validObsIDs <- tryData20$ObservationID_Trait[!is.na(tryData20$TraitID)]

#how often are there multiple traits?
tryData20 %>% 
	group_by(ObservationID) %>%
	summarise(nMultiTraits = n_distinct(TraitID, na.rm = TRUE)) %>%
	filter (nMultiTraits > 1)
	
#make a unique id column
#-----------------------------------

#make a column with the info on how many traits there are in each observation
tryData202 <- data.frame(tryData20 %>%
	group_by (ObservationID) %>%
	mutate(n_Trait = n_distinct(TraitID, na.rm = TRUE)))

#Make a new column for Observation_Trait_ID
tryData202$Observation_TraitID <- tryData202$ObservationID

#subset of data to play with 

	tryDataObs3 <- tryData202[tryData202$ObservationID ==  unique(tryData202$ObservationID)[200],]

	#for the 1st trait in teh selected observation

	#make a list of the different traits in the observation 
	traits <- unique(tryDataObs3$TraitID[!is.na(tryDataObs3$TraitID)])

	#remove the trait data that isn't the trait we are currently focused on
	Trait1 <- traits[1]
	notTrait <- traits[!traits %in% Trait1]
	traitData_1 <- tryDataObs3[!tryDataObs3$TraitID %in% notTrait,]

	#make the observation id for this set of trait data
	traitData_1$Observation_TraitID <- paste(traitData_1$ObservationID, Trait1, sep = "_")

	#add the subset of data int the main list 

#try with three traits

tryDataObs3_ids <- list()

#number of traits for that observation
ntrait <- tryDataObs3$n_Trait[1] 

#make a list of the different traits in the observation 
traits <- unique(tryDataObs3$TraitID[!is.na(tryDataObs3$TraitID)])

#make a counter for the number of traits each observation
i <- 1

for (i in 1:ntrait){

	#remove the trait data that isn't the trait we are currently focused on
	Traiti <- traits[i]
	notTraiti <- traits[!traits %in% Traiti]
	traitData_i <- tryDataObs3[!tryDataObs3$TraitID %in% notTraiti,]

	#make the observation id for this set of trait data
	traitData_i$Observation_TraitID <- paste(traitData_i$ObservationID, Traiti, sep = "_")

	#add the subset of data int the main list 
	tryDataObs3_ids[[ traitData_i$Observation_TraitID[1] ]] <- traitData_i

	i <- 1+ 1
}
#bring all the data tabels in teh list together into a single data table 
data.table::rbindlist(tryDataObs3_ids)

#Run the observation_Trait loop for all observations in teh trydata20 data
#----------------------------------------------------------------------------
tryData20IDList <- list()

i <- 1

for(obs in unique(tryData202$ObservationID)[1:3]){

	obsData <- tryData202[tryData20$ObservationID == obs,]

	#number of traits for that observation
	ntrait <- obsData$n_Trait[1] 

	#make a list of the different traits in the observation 
	traits <- unique(obsData$TraitID[!is.na(obsData$TraitID)])

	#make a counter for the number of traits each observation
	i <- 1

	for (i in 1:ntrait){

		#remove the trait data that isn't the trait we are currently focused on
		Traiti <- traits[i]
		notTraiti <- traits[!traits %in% Traiti]
		traitData_i <- obsData[!obsData$TraitID %in% notTraiti,]

		#make the observation id for this set of trait data
		traitData_i$Observation_TraitID <- paste(traitData_i$ObservationID, Traiti, sep = "_")

		#add the subset of data int the main list 
		tryData20IDList [[ traitData_i$Observation_TraitID[1] ]] <- traitData_i

		i <- i + 1
	}

}

#bring all the data tabels in teh list together into a single data table 
tryData20ID <- data.table::rbindlist(tryData20IDList )





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













