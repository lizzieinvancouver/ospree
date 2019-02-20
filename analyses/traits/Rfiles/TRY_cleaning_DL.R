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


#Separating the datasets into indiviudal files to clean
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

# Cleaning 
xylem36<-read.csv("sep_data/XylemFunctionalTraits(XFT)Database.csv")
head(xylem36)
str(xylem36)

#create a subset of the columns that actually contain data and the unique identifier (in this case ObservationID)
temp<-xylem36[,c(4,5,8,13,15)]
str(temp)

#Now, using reshape2, convert it from long to wide formatted data
require(reshape2)

wide<-dcast(temp, Dataset+ObservationID+SpeciesName~DataName, value.var = "OrigValueStr", na.rm=TRUE)
head(wide)


