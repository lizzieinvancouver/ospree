rm(list = ls())
## No one likes factors
options(stringsAsFactors = FALSE)


##setwd("C:\\Users\\Faith Jones\\Documents\\ubc\\OspreeTraits")
setwd("/home/faith/Documents/UBC/ospree")

## Load libraries
library(tidyr)
library(dplyr)
library(data.table)

## Read the data (modify path as needed) 
#tryData <- fread("~/Downloads/TRYtraitdataNov2019.txt")
tryData2 <- fread("TryData.txt")
tryData2$counterID <- 1:nrow(tryData2)
tryData <- tryData2 #make sure we have a copy of the unaltered dataset as we;ll 

#---------------------------------------------------
#Geoff's cleaning code
#---------------------------------------------------


## Drop some columns to keep things simpler
tryData$V28 <- NULL
tryData$Comment <- NULL
tryData$ObsDataID <- NULL
tryData$Reference <- NULL 

## What traits do we have data on?
traitN <- unique(tryData$TraitName)
traitN
## Let's clean this up a bit
### Drop rows where TraitName is blank (these are location or reference data; these can be merged later)
tryData <- subset(tryData, TraitName != "")
### Skip plant biomass data (only 104 measurements)
tryData <- subset(tryData, TraitName != c("Plant biomass and allometry: Leaves per plant (emergent, mature, senescent)"))
### Lump together all SLA measurements
tryData$TraitName <- gsub(pattern = "Leaf area per leaf dry mass.*uded", replacement = "Specific_leaf_area", x = tryData$TraitName)
### Lump together all Leaf area measurements
tryData$TraitName <- gsub(pattern = "Leaf area .*)", replacement = "Leaf_area", x = tryData$TraitName)
### Rename Stem specific density
tryData$TraitName <- gsub(pattern = "Stem specific density.*)", replacement = "Stem_specific_density", x = tryData$TraitName)
### Rename Leaf dry mass per leaf
tryData$TraitName <- gsub(pattern = "Leaf dry mass per leaf.*)", replacement = "Leaf_dry_matter_content", x = tryData$TraitName)
### Rename Crown height
tryData$TraitName <- gsub(pattern = "Crown.*)", replacement = "Crown_height", x = tryData$TraitName)

#remove any other symbols that might caouse problems later 
tryData$TraitName  <- gsub( " ", "_", tryData$TraitName )
tryData$TraitName  <- gsub( "\\(", ".", tryData$TraitName )
tryData$TraitName  <- gsub( "\\)", ".", tryData$TraitName )
tryData$TraitName <- gsub( "\\:", ".", tryData$TraitName )
tryData$TraitName <- gsub( ",", ".", tryData$TraitName )



### Now how we do look?
traitN <- unique(tryData$TraitName)
traitN # much nicer

## What type of trait values do we have?
traitT <- unique(tryData$ValueKindName)
traitT
### How many measurements for each one?
aggregate(StdValue ~ ValueKindName, data = tryData, FUN = length)
### Keep only "Single"
tryData <- subset(tryData, ValueKindName == c("Single"))
## Keep only data with standardized values (StdValue)
tryData <- tryData[!is.na(tryData$StdValue), ]

## What kind of units are we dealing with?
aggregate(UnitName ~ TraitName, data = tryData, unique) # should only be 1 per TraitName
### Remove "g/m2/d" measurements for Leaf photosynthesis (only 81 measurements)
tryData <- subset(tryData, UnitName != c("g/m2/d"))
aggregate(UnitName ~ TraitName, data = tryData, unique) # much better

## Generate sample plots of key traits (hopefully not much variation)
testspecies <- subset(tryData, SpeciesName == c("Abies alba"))
trait.list <- c("Stem specific density", "Leaf dry matter content", "Crown height", "Specific leaf area")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 1))
for(i in 1:length(trait.list)){
    temp <- subset(testspecies, TraitName == trait.list[i])
    plot(temp$StdValue, main = trait.list[i], xlab = "Observation", ylab = "Trait value")
}

#-----------------------------------------------------------------
#faith's Cleaning code
#--------------------------------------------------------------



#merge back the reference data
tryDataReferenceData <- subset(tryData2, TraitName == "")
tryDataRef <- rbind(tryData, tryDataReferenceData, fill = TRUE)
tryDataRef <- tryDataRef[order(tryDataRef$counterID),] 

#fill TraitName column with the the extra reference data, so now we know what rows refer to 
#lat and long data for example. This makes reshaping data later much easier  
tryDataRef$TraitName[tryDataRef$TraitName == ""] <- tryDataRef$DataName[tryDataRef$TraitName == ""]

#make a second Trait column for the unstandardised values of non-traits 
tryDataRef$TraitName2 <- tryDataRef$TraitName

#select the first 2000 observations because my comuputer cant manage the file file at once (i dodnt need to do that now fj)
#tryData20 <- tryDataRef[tryDataRef$ObservationID %in%  unique(tryDataRef$ObservationID)[1:2000],]

tryData20 <- tryDataRef

#make a unique id column
#-----------------------------------

#make a column with the info on how many traits there are in each observation
#so we can see how much of a problem this is and for the loop below 
tryData202 <- data.frame(tryData20 %>%
	group_by (ObservationID) %>%
	mutate(n_Trait = n_distinct(TraitID, na.rm = TRUE)))


#Make a new column for Observation_Trait_ID
tryData202$Observation_TraitID <- tryData202$ObservationID

#run the main loop that replicates the additional information for 
#each observation-trait so we can make a useful id column (Observation_TraitID)

tryData20IDList <- list()

i <- 1
#obs <- 16000

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

head(tryData20ID)
tryData20ID$Observation_TraitID

#long to wide data  
#------------
tryData20ID$TraitNameStd <- tryData20ID$TraitName
tryData20ID$TraitNameStd <-  paste("std",tryData20ID$TraitName, sep="_")

colums <- c("LastName","FirstName","DatasetID","Dataset",
	"SpeciesName","ObservationID","OrigUnitStr", "Observation_TraitID", 
	"TraitName","OrigValueStr", "TraitID")

colums2 <- c("LastName","FirstName","DatasetID","Dataset",
	"SpeciesName","ObservationID","OrigUnitStr", "Observation_TraitID", 
	"TraitNameStd","StdValue", "UnitName", "TraitID")

tryData20ID[c(217123, 217124),]


#spread data 
dataA1 <- tryData20ID %>%
	select(colums)%>% 
	group_by(Observation_TraitID) %>%
	group_by(TraitName) %>% 
	mutate(group_ID = row_number()) %>% # thsi creates an index group to avoid an error 
	spread(key = TraitName, value = OrigValueStr) %>%
    select(-group_ID)  # drop the index


#spread standard values 
dataA2 <- tryData20ID %>%
	select(colums2)%>% 
	filter(!is.na(StdValue)) %>%
	group_by(Observation_TraitID) %>%
	group_by(TraitNameStd) %>%
	mutate(group_ID = row_number()) %>%
	spread(key = TraitNameStd, value = StdValue)%>%
    select(-group_ID)  # drop the index

#combined stadard and original data 
#outAll3 <- merge(dataA1, dataA2, by = c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID", "Observation_TraitID", "TraitID"))

#loop through all observations to make a wide format dataset - seperatly for original and standardized data 
#----------------------------------

#make a list to hold all the data for unstandard data
observationListAll <- list()
counter <- 1

for(i in unique(dataA1$Observation_TraitID)){

	#select relevent observation/trait combination 
	observationDatai1 <- dataA1[dataA1$Observation_TraitID == i,]

	#collaps all the different values into a single row for that observation/trait
	collapsedi1 <- data.frame(observationDatai1 %>%
		group_by(Observation_TraitID) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
    		summarise_each(funs(first(.[!is.na(.)]))) %>% 
    		ungroup())

	observationListAll[[counter]] <- collapsedi1

	counter <- counter + 1
}

#bind_rows can cope with the fact that there are different columns in the 
#different sections of teh list. where there is no value for a column it just puts NA

allObservations2000 <- bind_rows(observationListAll)

#select trait columns 
colnames(allObservations2000)

traitColumns <- names(allObservations2000)[names(allObservations2000) %in% traitN]

longTraits <- allObservations2000 %>%
	pivot_longer(cols = traitColumns , names_to = "Traits", values_drop_na = TRUE)

#repeat, but for standard data 
#----------------------------------

#make a list to hold all the data for standard data
observationListAll2 <- list()
counter2 <- 1

for(i in unique(dataA2$Observation_TraitID)){

	#select relevent observation/trait combination 
	observationDatai2 <- dataA2[dataA2$Observation_TraitID == i,]

	#collaps all the different values into a single row for that observation/trait
	collapsedi2 <- data.frame(observationDatai2 %>%
		group_by(Observation_TraitID) %>%
		select_if(~sum(!is.na(.)) > 0) %>%
    		summarise_each(funs(first(.[!is.na(.)]))) %>% 
    		ungroup())

	observationListAll2[[counter2]] <- collapsedi2

	counter2 <- counter2 + 1
}

#bind_rows can cope with the fact that there are different columns in the 
#different sections of teh list. where there is no value for a column it just puts NA

allObservations20002 <- bind_rows(observationListAll2)

#select only standard trait columns 
columnSelect <- c("Observation_TraitID", "LastName",   "FirstName",  "DatasetID",   
	"Dataset" , "SpeciesName",   "ObservationID" ,"OrigUnitStr",    "UnitName",  "TraitID",
	"std_Latitude", "std_Longitude", "std_Altitude"  )  

columnsStd <- names(allObservations20002)[!names(allObservations20002) %in% columnSelect]

#move to long format so we have a column for standard trait 
longTraitsStd <- allObservations20002 %>%
	pivot_longer(cols = columnsStd, names_to = "Std_Traits", values_drop_na = TRUE)

#move to long format but keep all NAs because otherwise we lose the times when there is 
	#a standard lat/long but no standard trait 
longTraitsStdNAs <- allObservations20002 %>%
	pivot_longer(cols = columnsStd, names_to = "Std_Traits", values_drop_na = FALSE) %>%
	filter(is.na(value), !is.na(std_Latitude) | !is.na(std_Altitude)) %>%
	mutate(Std_Traits = replace(Std_Traits , !is.na(Std_Traits), NA))	

#combine the two data tables so we have standard trait data and standard lat/long/altitude data 
LonTraitsStdAll <- rbind(longTraitsStd, longTraitsStdNAs)


#merge long trait and long standardisd traits/values together 

allTraitsLong <- merge(longTraits, LonTraitsStdAll, by = c("Observation_TraitID", "LastName",
	"DatasetID", "SpeciesName", "ObservationID", "TraitID", "Dataset", "FirstName"))

#make sure there are no duplicated lines
allTraitsLong <- allTraitsLong[!duplicated(allTraitsLong),]
head(allTraitsLong)
#write.csv(allTraitsLong, "subsetTry20000.csv")
write.csv(allTraitsLong, "TryDataCleaned.csv")
