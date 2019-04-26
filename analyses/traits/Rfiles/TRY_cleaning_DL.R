rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse) 
require(reshape2)
# Select georeferenced Datasets -------------------------------------------
setwd("~/Desktop/trait_analysis") # Working directory 
#data <- read.csv("input/Try_data_Dec2018.csv")
#data <- read.csv("input/TRY_data_April2019.csv")
data<-data[,1:25]
str(data)
head(data)
names(data)
#To help simlify what we are looking at,  I also removed the Reference and Comment col


#First identify how many unique datasets there are:

length(unique(data$Dataset))
# March data has 76 unique datasets
# 
# #Separating the datasets into indiviudal files to clean
# for (name in unique(data$Dataset)){
#   #Subset the data by dataset name
#   tmp <- subset(data,Dataset==name)
#   #Create a new filename for each data set - the folder 'Dataset_georef_longlat' should already exist
#   name2=gsub('/','', name)
#   fn=paste('sep_data/', gsub(' ','',name2),'.csv',sep='')
#   #Save the CSV file containing only one data set
#   print(fn)
#   write.csv(tmp,fn, row.names=FALSE)
# }

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

##For loop that reads in the files, widens the column with the data and renames the indiviudal datasets to be examined individually
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse) 
require(reshape2)

folder<-"~/Desktop/trait_analysis/sep_data_test/"
file_list <- list.files(path=folder, pattern="*.csv") 

for (i in 1:length(file_list)){
  out<-assign(file_list[i],
              read.csv(paste(folder, file_list[i], sep='')))
  (out<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE))
  assign(paste("dat",i,sep="_"), out)
}

# for (i in 1:length(file_list)){
#   out<-assign(file_list[i],
#          read.csv(paste(folder, file_list[i], sep='')))
#   (out<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+OrigUnitStr~DataName, value.var = "OrigValueStr", na.rm=TRUE))
#          assign(paste("dat",i,sep="_"), out)
# }

#twp files did not work
dat_41<-read.csv("~/Desktop/trait_analysis/sep_data/PlantTraits.csv")
head(dat_41)
dat_41<-dcast(dat_41, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
head(out)

#Column for Species name is off by one
dat_42<-read.csv("~/Desktop/trait_analysis/sep_data/LeafStructure.csv")
head(dat_42)
dat_42<-dcast(dat_42, LastName+FirstName+DatasetID+Dataset+AccSpeciesID+ObsDataID~OriglName, value.var = "OrigUnitStr", na.rm=TRUE)
head(dat_42)
# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[1]
#[1] "LeafStructureandChemistry.csv"
#LDMC values are suspicious (all the same value of 258 and 2645) and in the wrong column
#I am deciding to exclude that trait from the dataset
names(dat_1)
head(dat_1)
temp1<-dat_1[,c(1:6,14,16,17,22,24,25,29)]
head(temp1)

unique(temp1$`Plant developmental status / plant age / maturity / plant life stage`)
unique(temp1$Exposition) 
#all trees are young trees at 0.5-3m hts & natural environments
# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[2]
#[2] Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv
#despite having SLA in the file name, no SLA data and non-values for Stem diameter at breast height 
#NOT A USEFUL DATASET
names(dat_2)
temp2<-dat_2[,c(1:6,8:9,12,14,18)]
head(temp2)

unique(temp2$`Plant developmental status / plant age / maturity / plant life stage`)
unique(temp2$Exposition) 
unique(temp2$`Stem diameter at breast height (1.3 m`)
# has juvenile and mature trees
# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[3]
# LeafTraitsinCentralApenninesBeechForests.csv

names(dat_3)
temp3<-dat_3[,c(1:6,9:11,13,15)]
head(temp3)

unique(temp3$`Plant developmental status / plant age / maturity / plant life stage`)
unique(temp3$`Treatment exposition`) 

#All mature, all in a natural environment

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[4]
# LeafveindensityofFagussylvaticaL.andQuercusfagineaLam..csv

names(dat_4)
temp4<-dat_4[,c(1:6,8,10,14,15:18,21)]
head(temp4)

unique(temp4$`Exposition`) 

#All mature, all in a natural environment

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[5]
# MARGINS-leaftraitsdatabase.csv
head(dat_5)
names(dat_5)
temp5<-dat_5[,c(1:6,11,13,15,19,35:38)]
head(temp5)

unique(temp5$`Exposition`) # Why is this all number?
temp5$`Stomata density on lower surface top` 

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[6]
# MycorrhizaDatabase.csv
head(dat_6)
names(dat_6)
#No trait data = woodiness

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[7]
# OvertonWrightNewZealandDatabase.csv
head(dat_7)
names(dat_7)
temp7<-dat_7[,c(1:6,8,10,14)]
head(temp7)

#only one height value/line in the entire dataset

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[8]
# OzarkTreeleaftraits.csv
names(dat_8)
temp8<-dat_8[,c(1:7)]
head(temp8)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[9]
# PhotosynthesisTraitsWorldwide.csv
names(dat_9)
temp9<-dat_9[,c(1:6,20,25,39,40,70)]
head(temp9)

unique(temp9$`Plant developmental status / plant age / maturity / plant life stage`)
# JUST saplings 

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[10]
#PLANTATT-AttributesofBritishandIrishPlants.csv

#heights are in cm so use StdValue
# names(dat_10)
# temp10<-dat_10[,c(1:6,13)]
# head(temp10)
# 
# #Height is in cm, so converting to m
dat_10<-read.csv("~/Desktop/trait_analysis/sep_data/PLANTATT-AttributesofBritishandIrishPlants.csv")
head(dat_10)
dat_10<-dcast(dat_10, LastName+FirstName+DatasetID+Dataset+AccSpeciesID+ObsDataID~OriglName, value.var = "StdValue", na.rm=TRUE)
names(dat_10)

temp10<-dat_10[,c(1:6,11)]
head(temp10)
temp10<-temp10[complete.cases(temp10[, 7]), ]

#Now all we are left with is the rows with actual height values. 
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[11]
# PhotosynthesisTraitsWorldwide.csv
names(dat_11)
temp11<-dat_11[,c(1:6,20,25,39,40,70)]
head(temp11)

unique(temp11$`Plant developmental status / plant age / maturity / plant life stage`)
# JUST saplings 
require(plyr)
final<-rbind.fill(temp1,temp3)
head(final)
# rbind.all.columns <- function(x, y) {
#   
#   x.diff <- setdiff(colnames(x), colnames(y))
#   y.diff <- setdiff(colnames(y), colnames(x))
#   
#   x[, c(as.character(y.diff))] <- NA
#   
#   y[, c(as.character(x.diff))] <- NA
#   
#   return(rbind(x, y))
# }
# 
# test<-rbind.all.columns(tmp24_sub,tmp25_sub)
# test2<-rbind.all.columns(tmp26_sub,tmp27_sub)
# test3<-rbind.all.columns(tmp28_sub,tmp29_sub)
# test4<-rbind.all.columns(tmp30_sub,tmp31_sub)
# test5<-rbind.all.columns(tmp32_sub,tmp33_sub)
# test6<-rbind.all.columns(tmp34_sub,tundra_sub)
# test7<-rbind.all.columns(test6,xylem_sub)
# 
# test8<-rbind.all.columns(test7,test5)
# test9<-rbind.all.columns(test8,test4)
# test10<-rbind.all.columns(test9,test3)
# test11<-rbind.all.columns(test10,test2)
# final<-rbind.all.columns(test11,test)
# 
# str(final) #73 variables
# 
# names(final)
# 
# length(unique(final$DatasetID)) #13 unique datasets combined into one
# 
# 

