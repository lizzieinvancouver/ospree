rm(list=ls()) 
options(stringsAsFactors = FALSE)

#writen by Faith Jones 
#last modefied 25/11/2019 
#purpose is to clean the TRY data
#data is inputed as the original TRY data in the txt format, and makes 
#a single csv file at the end of teh script

#i couldnt run the whole TRY dataset in one script, so the script is 
#only set up to run through the first 2000 Observations. A computer 
#with more ram than mine will probably be able to do it in one go. 

#this code has three different steps. 
#	1. makes a unique id column for each trait in each observation
#	2. converts from long to wide
#	3. condenses the data into one row per Observation_ID	

setwd("C:\\Users\\Faith Jones\\Documents\\ubc\\OspreeTraits")

library(tidyr)
library(dplyr)
library(data.table)

#read in teh try data 
tryData <- fread("TRYtraitdataNov2019.txt")

#drop some columns to keep things simpler


tryData$V28 <- NULL
tryData$Comment <- NULL
tryData$ObsDataID <- NULL



#select the first 2000 observations because my comuputer cant manage the file file at once
tryData20 <- tryData[tryData$ObservationID %in%  unique(tryData$ObservationID)[1:2000],]

#make a unique id column
#-----------------------------------

#make a column with the info on how many traits there are in each observation
tryData202 <- data.frame(tryData20 %>%
	group_by (ObservationID) %>%
	mutate(n_Trait = n_distinct(TraitID, na.rm = TRUE)))


#Make a new column for Observation_Trait_ID
tryData202$Observation_TraitID <- tryData202$ObservationID

#run the main loop that replicates the additional information for 
#each observation-trait so we can make a useful id column 

tryData20IDList <- list()

i <- 1

for(obs in unique(tryData202$ObservationID)){

	obsData <- tryData202[tryData202$ObservationID == obs,]

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
	"SpeciesName","ObservationID","OrigUnitStr", "Observation_TraitID", 
	"DataName","OrigValueStr", "TraitID", "TraitName")

colums2 <- c("LastName","FirstName","DatasetID","Dataset",
	"SpeciesName","ObservationID","OrigUnitStr", "Observation_TraitID", 
	"DataName2", "StdValue", "UnitName", "TraitID", "TraitName")


#try in dplyr for all - doesnt work, too big. onlyt 2000 observation ids 
dataA1 <- tryData20ID %>%
	select(colums)%>% 
	group_by(Observation_TraitID) %>%
	spread(key = DataName, value = OrigValueStr)

tryData20ID$DataName2 <- tryData20ID$DataName
tryData20ID$DataName2 <-  paste("std",tryData20ID$DataName, sep="_")

#try in dplyr for all - doesnt work, too big. onlyt 2000 observation ids 
dataA2 <- tryData20ID %>%
	group_by(Observation_TraitID) %>%
	select(colums2)%>% 
	spread(key = DataName2, value = StdValue)


#combined stadard and original data 
outAll3 <- merge(dataA1, dataA2, by = c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID", "Observation_TraitID", "TraitID", "TraitName"))

#loop through all observations to make a wide format dataset 
#----------------------------------

#make a list to hold all the data
observationListAll <- list()
counter <- 1

for(i in unique(outAll3$Observation_TraitID)){

	#select relevent observation/trait combination 
	observationDatai2 <- outAll3[outAll3$Observation_TraitID == i,]

	#collaps all the different values into a single row for that observation/trait
	collapsedi2 <- data.frame(observationDatai2 %>%
		group_by(Observation_TraitID) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
    		summarise_each(funs(first(.[!is.na(.)]))) %>% 
    		ungroup())

	observationListAll[[counter]] <- collapsedi2

	counter <- counter + 1
}

#bind_rows can cope with teh fact that there are different columns in the 
#different sections of teh list. where there is no value for a column it just puts NA

allObservations2000 <- bind_rows(observationListAll)
head(allObservations2000 )

write.csv(allObservations20000, "subsetTry20000.csv")





