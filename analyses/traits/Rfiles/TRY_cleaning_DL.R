rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse) 

# Select georeferenced Datasets -------------------------------------------
setwd("~/Desktop/trait_analysis") # Working directory 
data <- read.csv("input/Try_data_Dec2018.csv")

str(data)
head(data)
names(data)
#To help simlify what we are looking at,  I am removing the Reference and Comment col
data<-data[,1:25]

#First identify how many unique datasets there are:

length(unique(data$Dataset))


#I separated every the full table in one csv for each DatasetID in TRY as I later examined every data set separately and I did not 
#want to loads everytime this one large data file
#Get the list of unique MP names
for (name in unique(data$Dataset)){
  #Subset the data by dataset name
  tmp <- subset(data,Dataset==name)
  #Create a new filename for each data set - the folder 'Dataset_georef_longlat' should already exist
  name2=gsub('/','', name)
  fn=paste('sep_data/', gsub(' ','',name2),'.csv',sep='')
  #Save the CSV file containing only one data set 
  print(fn)
  write.csv(tmp,fn, row.names=FALSE)
}

# Finding ITV in georeference datasets (each dataset separately) ------------------------------------
##Extacting Datasets with ITV data
setwd("/media/j_kuppler/WD-Store/WORK_documents/2017/Worldmap_CV/Data/Raw/TRY/Data/2720_03012017101853/Dataset_georef_longlat/")
list.files()
temp <- list.files(pattern="*.csv") # select all csv-files
myfiles <- lapply(temp, function(x) read_csv(x)) # import all files listed in temp: data.frames saved in one list, each ASC-files is one list-object

# Dataset 1 - 
length(myfiles)
temp[1]
data <- tbl_df(myfiles[[1]])
data
unique(data$Reference) # check if data set has multiple references and data need to be examined separately
unique(data$DataName) # what information for each observationID are available
unique(data$ObservationID) # Number of unique observations

#Select data from one reference
data2 <- data
Ref2 <- unique(data2$Reference)
#Ref 1
data <- data2 %>% filter(Reference %in% Ref2[3]) 

unique(data$Reference) # check if data set has multiple references and data need to be examined separately
unique(data$DataName) # what information for each observationID are available
unique(data$ObservationID) # Number of unique observations
unique(data$ValueKindName) # Single measurement, mean or median given

#Examine DataName that are important or relevant what levels are available
du_1_5 <- data %>% filter(ObservationID == "801026") # Check information given for one observation ID
View(du_1_5)

du_1_1 <- data %>% filter(DataName == "Location Site ID")
unique(du_1_1$OrigValueStr)

du_1_2 <- data %>% filter(DataName == "Month of sampling")
unique(du_1_2$OrigValueStr)

du_1_3 <- data %>% filter(DataName == "Year of sampling")
unique(du_1_3$OrigValueStr)

du_1_4 <- data %>% filter(DataName == "Latitude")
unique(du_1_4$OrigValueStr)

du_1_4 <- data %>% filter(DataName == "Longitude")
unique(du_1_4$OrigValueStr)

# check original and TRY traitname
data %>% select(UnitName, DatasetID, OriglName, DataName, TraitName) %>% distinct() %>% na.omit() 


#additionally recheck information in the reference publication! Also check sampling design, etc.
#Or you can do it later

##Now I only reformat the the data set for my needs, this usually also needs to be adjusted to different data sets.
#Create a new column with Lat_Long for each Observation ID
#combining LongLat in own column
d_ll <- data %>% 
  select(ObservationID, DataName, StdValue) %>%
  filter(DataName == "Longitude" | DataName == "Latitude") %>% 
  spread(., DataName, StdValue) %>%
  unite_(., "LongLat", c("Latitude", "Longitude"))
d_ll2 <- rbind(d_ll, d_ll)
data_ll <- data %>% left_join(.,d_ll, by = "ObservationID")

#Create dataframe with unit for each Trait
du_UN_DID <- data %>% select(UnitName, DatasetID, TraitName) %>% distinct() %>% na.omit()

#Create a dataframe with PlotID (if available) for each ObservationID
d_ll2 <- data %>% 
  select(ObservationID, DataName, OrigValueStr) %>%
  filter(DataName == "Location Site ID") 
colnames(d_ll2)[3] <- "PlotID"
d_ll2 <- d_ll2[,-2]

data_ll2 <- data_ll %>% left_join(.,d_ll2, by = "ObservationID") #add LongLat and PlotID to original dataframe

data_final <- data_ll2 %>%  
  group_by(LongLat, PlotID, AccSpeciesName, TraitName) %>% filter(!(is.na(TraitName))) %>%
  summarise(avg = mean(StdValue), SD = sd(StdValue), n = length(StdValue)) %>% 
  left_join(., du_UN_DID, by = "TraitName") 

data_final2 <- data_final %>% separate(.,LongLat, c("Longitude", "Latitude"), sep="_")
data_final2[,"LongLat"] <- data_final$LongLat

name <- paste(temp[2], '_CV.csv', sep="")
write_csv(data_final2, name)


