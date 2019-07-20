rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse) 
require(reshape2)
# Select georeferenced Datasets -------------------------------------------
setwd("~/Desktop/trait_analysis") # Working directory 
#data <- read.csv("input/Try_data_Dec2018.csv")
# data <- read.csv("input/TRY_data_April2019.csv")
# data<-data[,1:25]
# str(data)
# head(data)
# names(data)
#To help simlify what we are looking at,  I also removed the Reference and Comment col


#First identify how many unique datasets there are:

length(unique(data$Dataset))
# March data has 78 unique datasets

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


##For loop that reads in the files, widens the column with the data and renames the indiviudal datasets to be examined individually
#Note: I have removed all the files (12) that have differences in their structure and for which this code does not work. These are dealt with below under the "inconsistent" heading. I have a notes file that outlines how each is different. 

folder<-"~/Desktop/trait_analysis/sep_data_test/"
file_list <- list.files(path=folder, pattern="*.csv") 
file_list[1]
for (i in 1:length(file_list)){
  out<-assign(file_list[i], 
              read.csv(paste(folder, file_list[i], sep='')))
  (out<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE))
  assign(paste("dat",i,sep="_"), out)
}

#Gives 16 datasets renamed dat_1 to dat_16


# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[1]
# LeafTraitsinCentralApenninesBeechForests.csv
#Looking at the columns and which traits are in the dataset 
names(dat_1)

#Subsetting to a temporary dataset that just incldues the columns we want and the traits
temp1<-dat_1[,c(1:6,9:11,13,15)]
head(temp1)

names(temp1)
beech<-melt(temp1, 
            id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude","Treatment exposition"),
            measure.vars=c("Leaf dry matter content per leaf water-saturated mass (LDMC)"),
            variable.name = "Trait",
            value.name = "value")
colnames(beech)[colnames(beech)=="Treatment exposition"] <- "Exposition"
head(beech)
#writing a new csv file with just the data we will use
write.csv(beech, '~/Desktop/trait_analysis/clean/wide/LeafTraitsinCentralApenninesBeechForests.csv', row.names=FALSE)


#All mature, all in a natural environment
names(temp1)
unique(temp1[,11])

unique(temp1$`Plant developmental status / plant age / maturity / plant life stage`)
unique(temp1$`Treatment exposition`)

#all mature and natural.

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[2]
# LeafveindensityofFagussylvaticaL.andQuercusfagineaLam.csv

names(dat_2)

temp2<-dat_2[,c(1:6,8,10,14,15:18,21)]
head(temp2)
names(temp2)


vein<-melt(temp2, 
           id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude","Exposition"),
           measure.vars=c("Leaf vein density: primary veins","Leaf vein density: major veins","Leaf vein density: secondary veins","Leaf vein density: tertiary veins"),
           variable.name = "Trait",
           value.name = "value")

write.csv(vein, '~/Desktop/trait_analysis/clean/LeafveindensityofFagussylvaticaL.andQuercusfagineaLam.csv', row.names=FALSE)

unique(temp2$`Exposition`) 

#All mature, all in a natural environment

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[3]
# OvertonWrightNewZealandDatabase.csv

head(dat_3)
names(dat_3)
temp3<-dat_3[,c(1:10,14)]
head(temp3)
names(temp3)
#no exposition
nz<-melt(temp3, 
           id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
           measure.vars=c("Plant height vegetative"),
           variable.name = "Trait",
           value.name = "value")

write.csv(nz, '~/Desktop/trait_analysis/clean/OvertonWrightNewZealandDatabase.csv', row.names=FALSE)

#only one height value/line in the entire dataset

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[4]
# OzarkTreeleaftraits.csv
dat4_2<-read.csv("~/Desktop/trait_analysis/sep_data_test/OzarkTreeleaftraits.csv")
out<-dcast(dat4_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OriglName", na.rm=TRUE); names(out)
temp4<-out[,c(1:7,11:13)]

temp18<-merge(temp2, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp18)
write.csv(temp18, '~/Desktop/trait_analysis/clean/Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv', row.names=FALSE)
names(dat_4)
temp4<-dat_4[,c(1:15)]
head(temp4)

#no exposition
ozark<-melt(temp4, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","lat","lon"),
         measure.vars=c(" DBH"), #There is a space in front of DBH
         variable.name = "Trait",
         value.name = "value")
colnames(ozark)[colnames(ozark)=="lat"] <- "Latitude"
colnames(ozark)[colnames(ozark)=="lon"] <- "Longitude"

write.csv(ozark, '~/Desktop/trait_analysis/clean/OzarkTreeleaftraits.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[5]
#PhotosynthesisTraitsWorldwide.csv

names(dat_5)
temp5<-dat_5[,c(1:6,20,25,39,40,70)]
head(temp5)
names(temp5)
#no exposition
photo<-melt(temp5, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Photosynthesis per leaf area at leaf temperature (A_area)","Stomata conductance to water vapour per leaf area"),
         variable.name = "Trait",
         value.name = "value")
colnames(photo)[colnames(photo)=="Treatment exposition"] <- "Exposition"
write.csv(temp5, '~/Desktop/trait_analysis/clean/PhotosynthesisTraitsWorldwide.csv', row.names=FALSE)

unique(temp5$`Plant developmental status / plant age / maturity / plant life stage`)
# JUST saplings 

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[6]
#PLANTATT-AttributesofBritishandIrishPlants.csv

names(dat_6)
temp6<-dat_6[,c(1:6,13)]
head(temp6)
names(temp6)
#no exposition
plantatt<-melt(temp6, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
         measure.vars=c("Plant height (unspecified if vegetative or reproductive)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(plantatt, '~/Desktop/trait_analysis/clean/PLANTATT-AttributesofBritishandIrishPlants.csv', row.names=FALSE)

#heights are in cm

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[7]
#PlantTraits,Virginia,USA.csv

names(dat_7)
temp7<-dat_7[,c(1:7,9,11:13,15:16)]
head(temp7)
names(temp7)
#no exposition
virg<-melt(temp7, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude","Treatment exposition"),
         measure.vars=c("Plant height vegetative","Number of Leaves per plant","Stem diameter at base (basal diameter)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(virg, '~/Desktop/trait_analysis/clean/PlantTraits,Virginia,USA.csv', row.names=FALSE)

unique(temp7$`Plant developmental status / plant age / maturity / plant life stage`)
unique(temp7$`Treatment exposition`)

#All juvenile and all natural envirts

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[8]
#"QuercusLeafC&NDatabase.csv"
names(dat_8)
temp8<-dat_8[,c(1:6,8:11,14)]
head(temp8)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp8, '~/Desktop/trait_analysis/clean/QuercusLeafC&NDatabase.csv', row.names=FALSE)

unique(temp8$`Plant developmental status / plant age / maturity / plant life stage`)

# young and old trees

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[9]
#SpecificleafareaforwoodyspeciesatSmithsonian-ForestGEOplot-Virginia,USA.csv

names(dat_9)
head(dat_9)
temp9<-dat_9[,c(1:6,8,10,11,13,17,18)]
head(temp9)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp9, '~/Desktop/trait_analysis/clean/SpecificleafareaforwoodyspeciesatSmithsonian-ForestGEOplot-Virginia,USA.csv', row.names=FALSE)

unique(temp9$`Plant developmental status / plant age / maturity / plant life stage`)
unique(temp9$`Treatment exposition`)
#BOTH juvenile and mature individuals; all in natural environments 

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[10]
#TheFunctionalEcologyofTrees(FET)Database-Jena.csv

names(dat_10)
head(dat_10)
temp10<-dat_10[,c(1:6,9,10,12,14,27,28,30,35:45,66:68,75,94,112)]
head(temp10)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp10, '~/Desktop/trait_analysis/clean/TheFunctionalEcologyofTrees(FET)Database-Jena.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[11]
#TheNetherlandsPlantTraitsDatabase.csv

names(dat_11)
head(dat_11)
temp11<-dat_11[,c(1:9,12,23)]
head(temp11)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp11, '~/Desktop/trait_analysis/clean/TheNetherlandsPlantTraitsDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[12]
#TheXylemPhloemDatabase.csv

names(dat_12)
head(dat_12)
temp12<-dat_12[,c(1:6,13,17,24)]
head(temp12)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp12, '~/Desktop/trait_analysis/clean/TheXylemPhloemDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[13]
#TundraPlantTraitsDatabase.csv

names(dat_13)
head(dat_13)
temp13<-dat_13[,c(1:8,10)]
head(temp13)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp13, '~/Desktop/trait_analysis/clean/TundraPlantTraitsDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[14]
#Woodcarboncontentdatabase.csv

names(dat_14)
head(dat_14)
temp14<-dat_14[,c(1:8)]
head(temp14)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp14, '~/Desktop/trait_analysis/clean/Woodcarboncontentdatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[15]
#Woodcarbondatabase.csv

names(dat_15)
head(dat_15)
temp15<-dat_15[,c(1:8,10,13)]
head(temp15)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")


write.csv(temp15, '~/Desktop/trait_analysis/clean/Woodcarbondatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[16]
#XylemFunctionalTraits(XFT)Database.csv

names(dat_16)
head(dat_16)
temp16<-dat_16[,c(1:6,27,33,35,65:67,81,97)]
head(temp16)
names(temp3)
#no exposition
nz<-melt(temp3, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(temp16, '~/Desktop/trait_analysis/clean/XylemFunctionalTraits(XFT)Database.csv', row.names=FALSE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#Cleaning the inconsistent datasets:
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list=ls()) 
options(stringsAsFactors = FALSE)

folder<-"~/Desktop/trait_analysis/inconsistent_ones/"
file_list <- list.files(path=folder, pattern="*.csv") 
file_list[1]
for (i in 1:length(file_list)){
  out<-assign(file_list[i], 
              read.csv(paste(folder, file_list[i], sep='')))
  (out<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE))
  assign(paste("dat",i,sep="_"), out)
}

#######################################################
file_list[1]
#[1] "LeafStructureandChemistry.csv"

names(dat_1)
head(dat_1)
temp1<-dat_1[,c(1:6,14,16,17,22,24,25,29)]
head(temp1)
unique(temp1$`Leaf carbon content per dry mass`)
#get the LMDC and SLA values
dat1_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/LeafStructureandChemistry.csv")
out<-dcast(dat1_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE); names(out)
out<-out[,c(1:7,15)]


temp17<-merge(temp1, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
colnames(temp17)[colnames(temp17)=="258"] <- "LDMC"
head(temp17)
write.csv(temp17, '~/Desktop/trait_analysis/clean/LeafStructureandChemistry.csv', row.names=FALSE)
#######################################################
file_list[2]
#[2] Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv

names(dat_2)
temp2<-dat_2[,c(1:6,8:9,12,14)]
head(temp2)

dat2_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv")
out<-dcast(dat2_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE); names(out)
out<-out[,c(1:6,18)]


temp18<-merge(temp2, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp18)
write.csv(temp18, '~/Desktop/trait_analysis/clean/Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv', row.names=FALSE)
#######################################################
file_list[3]
#MARGINS-leaftraitsdatabase.csv

head(dat_3)
names(dat_3)
temp3<-dat_3[,c(1:6,11,13,15,19,35:38)]
head(temp3)

#get the LMDC and SLA values
dat3_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/MARGINS-leaftraitsdatabase.csv")
out<-dcast(dat3_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
out<-out[,c(1:7)]
head(out)

temp19<-merge(temp3, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
colnames(temp19)[colnames(temp19)=="258"] <- "LDMC"
head(temp19)
write.csv(temp19, '~/Desktop/trait_analysis/clean/MARGINS-leaftraitsdatabase.csv', row.names=FALSE)
#######################################################
file_list[4]
#PlantPhysiologyDatabase.csv

names(dat_4)
head(dat_4) 
temp4<-dat_4[,c(1:6,9,11,13:14)]
head(temp4)

#get the LMDC and SLA values
dat4_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/PlantPhysiologyDatabase.csv")
out<-dcast(dat4_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
out<-out[,c(1:7,15)]
head(out)

temp20<-merge(temp4, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
colnames(temp20)[colnames(temp20)=="258"] <- "LDMC"
head(temp20)
write.csv(temp20, '~/Desktop/trait_analysis/clean/PlantPhysiologyDatabase.csv', row.names=FALSE)

unique(temp20$`Plant developmental status / plant age / maturity / plant life stage`)
# JUST seedlings

#######################################################
file_list[5]
# "PLANTSdataUSDA.csv"
names(dat_5)
temp5<-dat_5[,c(1:8)]
head(temp5)

#mature tree height values are under the unit colmn and default is in feet not meters
dat5_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/PLANTSdataUSDA.csv")
out<-dcast(dat5_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "StdValue", na.rm=TRUE)
out<-out[,c(1:7)]

dat5_3<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/PLANTSdataUSDA.csv")
out2<-dcast(dat5_3, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "UnitName", na.rm=TRUE)
out2<-out2[,c(1:6,8)]
head(out2)

out_c<-merge(out2,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
temp21<-merge(temp5, out_c, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))

write.csv(temp21, '~/Desktop/trait_analysis/clean/PLANTSdataUSDA.csv', row.names=FALSE)

#######################################################
file_list[6]
# PlantTraitsforPinusandJuniperusForestsinArizona.csv
names(dat_6)
#both LDMC and DBH are in the OrigUnitStr
temp6<-dat_6[,c(1:6,9:11,13:14,18)]
head(temp6)

dat6_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/PlantTraitsforPinusandJuniperusForestsinArizona.csv")
out<-dcast(dat6_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
head(out)
out<-out[,c(1:7,17)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp22<-merge(temp6,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp22)
write.csv(temp22, '~/Desktop/trait_analysis/clean/PlantTraitsforPinusandJuniperusForestsinArizona.csv', row.names=FALSE)

#######################################################
file_list[7]
#PlantTraitsfromCirceoNationalPark,Italy.csv
names(dat_7)
temp7<-dat_7[,c(1:6,10,12)]
head(temp7)

dat7_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/PlantTraitsfromCirceoNationalPark,Italy.csv")
out<-dcast(dat7_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "StdValue", na.rm=TRUE)
out<-out[,c(1:7,9,11)]
head(out)

#now combining into one dataset
temp23<-merge(temp7,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))

head(temp23)
write.csv(temp23, '~/Desktop/trait_analysis/clean/PlantTraitsfromCirceoNationalPark,Italy.csv', row.names=FALSE)

#######################################################
file_list[8]
#PlantTraitsfromCirceoNationalPark.csv
names(dat_8)

#Trait values are in the UnitNam col
dat8_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/PlantTraitsfromCirceoNationalPark.csv")
out<-dcast(dat8_2, LastName+FirstName+ Dataset+SpeciesName+ObservationID~OrigValueStr, value.var = "UnitName", na.rm=TRUE)
names(out)
temp24<-out[,c(1:8,10,11)]
head(temp24)
#All mature in natural envrt
write.csv(temp24, '~/Desktop/trait_analysis/clean/PlantTraitsfromCirceoNationalPark.csv', row.names=FALSE)

#######################################################
file_list[9]
#PlantTraitsFromSpanishMediteraneanshrublands.csv
names(dat_9)

#LDMC data 
temp9<-dat_9[,c(1:6,8,11,12,13,15)]
head(temp9)
dat9_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/PlantTraitsFromSpanishMediteraneanshrublands.csv")
out<-dcast(dat9_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp25<-merge(temp9,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp25)
write.csv(temp25, '~/Desktop/trait_analysis/clean/PlantTraitsFromSpanishMediteraneanshrublands.csv', row.names=FALSE)

#######################################################

# PlantTraitsofCanadianForests.csv
# 
# # #Height is in cm, so converting to m
dat_10<-read.csv("~/Desktop/trait_analysis/sep_data/PlantTraitsofCanadianForests.csv")
dat_10<-dcast(dat_10, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "StdValue", na.rm=TRUE)

#Height is in cm, so converting to m
#heights are in cm so use StdValue
names(dat_10)
temp26<-dat_10[,c(1:6,8,9,11,13,20:22)]
head(temp10)
write.csv(temp26, '~/Desktop/trait_analysis/clean/PlantTraitsofCanadianForests.csv', row.names=FALSE)

# head(dat_10)
# dat_10<-dcast(dat_10, LastName+FirstName+DatasetID+Dataset+AccSpeciesID+ObsDataID~OriglName, value.var = "StdValue", na.rm=TRUE)
# names(dat_10)
# 
# temp10<-dat_10[,c(1:6,11)]
# head(temp10)


#SHOULD BE Canadian forests dataset

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[11]
#RollinsonDBH.csv
names(dat_11)

temp11<-dat_11[,c(1:7,9,10)]
head(temp11)

#Stem diameter is in the OrigUnitStr col
dat11_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/RollinsonDBH.csv")
out<-dcast(dat11_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
out<-out[,c(1:6,13)]
head(out)

temp27<-merge(temp11,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp27)
write.csv(temp27, '~/Desktop/trait_analysis/clean/RollinsonDBH.csv', row.names=FALSE)

unique(temp27$`Exposition`)
#All natural

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[12]
#Sheffield&SpainWoodyDatabase.csv
names(dat_12)
head(dat_12)
temp12<-dat_12[,c(1:6,8,9,10,12,13,19)]
head(temp12)
write.csv(temp12, '~/Desktop/trait_analysis/clean/Sheffield&SpainWoodyDatabase.csv', row.names=FALSE)

#not sure why I put this one in the inconsistent folder

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[13]
#SheffieldDatabase.csv
names(dat_13)
head(dat_13)
temp13<-dat_13[,c(1:6,14, 16,19,22,24,27,32:35)]
head(temp13)

#LDMC is under OrigUnitStr
dat13_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/SheffieldDatabase.csv")
out<-dcast(dat13_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out)
head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

#Leaf water content is under ValueKindName
out2<-dcast(dat13_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "ValueKindName", na.rm=TRUE)
names(out2); head(out2)
out2<-out2[,c(1:6,8)]
colnames(out2)[colnames(out2)=="54"] <- "LWC"

out_c<-merge(out2,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
temp28<-merge(temp13, out_c, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
write.csv(temp28, '~/Desktop/trait_analysis/clean/SheffieldDatabase.csv', row.names=FALSE)

head(temp28)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[14]
#StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv

names(dat_14)
head(dat_14)
temp14<-dat_14[,c(1:6,8,9,10:14,16)]
head(temp14)

#LDMC is under OrigUnitStr
dat14_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv")
out<-dcast(dat14_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp29<-merge(temp14, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
write.csv(temp29, '~/Desktop/trait_analysis/clean/StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv', row.names=FALSE)

unique(temp29$`Plant developmental status / plant age / maturity / plant life stage`)
unique(temp29$`Exposition`)
#BOTH juvenile and mature individuals; all in natural environments 

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[15]
#TheChinaPlantTraitDatabase.csv

names(dat_15)
head(dat_15)
temp15<-dat_15[,c(1:6,14,16,17,20)]
head(temp15)

#LDMC is under OrigUnitStr
dat15_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/TheChinaPlantTraitDatabase.csv")
out<-dcast(dat15_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp30<-merge(temp15, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp30)
write.csv(temp30, '~/Desktop/trait_analysis/clean/TheChinaPlantTraitDatabase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[16]
#TheGlobalLeafTraits.csv

names(dat_16)
head(dat_16)
temp16<-dat_16[,c(1:6,29:33,37,74)]
head(temp16)

#LDMC is under OrigUnitStr
dat16_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/TheGlobalLeafTraits.csv")
out<-dcast(dat16_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

#Leaf water content is under ValueKindName
out2<-dcast(dat16_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "ValueKindName", na.rm=TRUE)
names(out2); head(out2)
out2<-out2[,c(1:6,8)]
colnames(out2)[colnames(out2)=="54"] <- "LWC"

out_c<-merge(out2,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
temp31<-merge(temp16, out_c, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))

head(temp31)
write.csv(temp31, '~/Desktop/trait_analysis/clean/TheGlobalLeafTraits.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[17]
#TheLEDATraitbase.csv
names(dat_17)
head(dat_17)
temp17<-dat_17[,c(1:6,10,13,16:18,23:25,27)]
head(temp17)

dat17_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/TheLEDATraitbase.csv")
out<-dcast(dat17_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp32<-merge(temp17, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp32)
write.csv(temp32, '~/Desktop/trait_analysis/clean/TheLEDATraitbase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[18]
#TraitsfromSubarcticPlantSpeciesDatabase.csv
names(dat_18)
head(dat_18)
temp18<-dat_18[,c(1:6,9,10:13,15)]
head(temp18)

dat18_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/TraitsfromSubarcticPlantSpeciesDatabase.csv")
out<-dcast(dat18_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp33<-merge(temp18, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp33)
write.csv(temp33, '~/Desktop/trait_analysis/clean/TraitsfromSubarcticPlantSpeciesDatabase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[19]
#WholePlantHydraulicConductance.csv
names(dat_19)
head(dat_19)
temp19<-dat_19[,c(1:7)]
head(temp19)

dat19_2<-read.csv("~/Desktop/trait_analysis/inconsistent_ones/WholePlantHydraulicConductance.csv")
out<-dcast(dat19_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:6,9)]

temp34<-merge(temp19, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp34)

write.csv(temp34, '~/Desktop/trait_analysis/clean/WholePlantHydraulicConductance.csv', row.names=FALSE)
