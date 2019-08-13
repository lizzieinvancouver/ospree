rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse) 
require(reshape2)
# Select georeferenced Datasets -------------------------------------------
setwd("~/Documents/github/ospree/analyses/traits/rfiles")
#data <- read.csv("input/Try_data_Dec2018.csv")
# data <- read.csv("input/TRY_data_April2019.csv")
# data<-data[,1:25]
# str(data)
# head(data)
# names(data)
#To help simlify what we are looking at,  I also removed the Reference and Comment col


#First identify how many unique datasets there are:

#length(unique(data$Dataset))
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

folder<-"~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/standardformat/"
file_list <- list.files(path=folder, pattern="*.csv") 
file_list[1]
for (i in 1:length(file_list)){
  out<-assign(file_list[i], 
              read.csv(paste(folder, file_list[i], sep='')))
  (out<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+UnitName~DataName, value.var = "StdValue", na.rm=TRUE))
  assign(paste("dat",i,sep="_"), out)
}

#Gives 16 datasets renamed dat_1 to dat_16


# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[1]
# LeafTraitsinCentralApenninesBeechForests.csv
#Looking at the columns and which traits are in the dataset 
names(dat_1)

#Subsetting to a temporary dataset that just incldues the columns we want and the traits
#temp1<-dat_1[,c(1:6,9:11,13,15)]
temp1<-dat_1
head(temp1)

names(temp1)
beech<-melt(temp1, 
            #id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude","Treatment exposition"
            #          ),
            measure.vars=c("Leaf dry matter content per leaf water-saturated mass (LDMC)"),
            variable.name = "Trait",
            value.name = "value")
#colnames(beech)[colnames(beech)=="Treatment exposition"] <- "Exposition"
head(beech)
#writing a new csv file with just the data we will use
write.csv(beech, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/LeafTraitsinCentralApenninesBeechForests.csv', row.names=FALSE)


#All mature, all in a natural environment
names(temp1)
unique(temp1[,11])

unique(temp1$`Plant developmental status / plant age / maturity / plant life stage`)
#unique(temp1$`Treatment exposition`)

#all mature and natural.

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[2]
# LeafveindensityofFagussylvaticaL.andQuercusfagineaLam.csv

names(dat_2)

temp2<-dat_2[,c(1:6,8,10,14,15:18,21)]
head(temp2)
names(temp2)


vein<-melt(temp2, 
           # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
           #           ),
           measure.vars=c("Leaf vein density: primary veins","Leaf vein density: major veins","Leaf vein density: secondary veins","Leaf vein density: tertiary veins"),
           variable.name = "Trait",
           value.name = "value")

write.csv(vein, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/LeafveindensityofFagussylvaticaL.andQuercusfagineaLam.csv', row.names=FALSE)

#unique(temp2$`Exposition`) 

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
#           id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
           measure.vars=c("Plant height vegetative"),
           variable.name = "Trait",
           value.name = "value")

write.csv(nz, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/OvertonWrightNewZealandDatabase.csv', row.names=FALSE)

#only one height value/line in the entire dataset

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[4]
# OzarkTreeleaftraits.csv
dat4_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/standardformat/OzarkTreeleaftraits.csv")
out<-dcast(dat4_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OriglName", na.rm=TRUE); names(out)
temp4<-out[,c(1:7,11:13)]


names(dat_4)
temp4<-dat_4[,c(1:15)]
head(temp4)

#no exposition
ozark<-melt(temp4, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","lat","lon"),
         measure.vars=c(" DBH"), #There is a space in front of DBH
         variable.name = "Trait",
         value.name = "value")
colnames(ozark)[colnames(ozark)=="lat"] <- "Latitude"
colnames(ozark)[colnames(ozark)=="lon"] <- "Longitude"

write.csv(ozark, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/OzarkTreeleaftraits.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[5]
#PhotosynthesisTraitsWorldwide.csv

names(dat_5)
temp5<-dat_5[,c(1:6,20,25,39,40,70)]
head(temp5)
names(temp5)
#no exposition
photo<-melt(temp5, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Photosynthesis per leaf area at leaf temperature (A_area)","Stomata conductance to water vapour per leaf area"),
         variable.name = "Trait",
         value.name = "value")
#colnames(photo)[colnames(photo)=="Treatment exposition"] <- "Exposition"
write.csv(photo, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PhotosynthesisTraitsWorldwide.csv', row.names=FALSE)

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
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
         measure.vars=c("Plant height (unspecified if vegetative or reproductive)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(plantatt, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PLANTATT-AttributesofBritishandIrishPlants.csv', row.names=FALSE)

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
         # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Treatment exposition"
         #           ),
         measure.vars=c("Plant height vegetative","Number of Leaves per plant","Stem diameter at base (basal diameter)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(virg, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PlantTraits,Virginia,USA.csv', row.names=FALSE)

unique(temp7$`Plant developmental status / plant age / maturity / plant life stage`)
#unique(temp7$`Treatment exposition`)

#All juvenile and all natural envirts

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[8]
#"QuercusLeafC&NDatabase.csv"
names(dat_8)
temp8<-dat_8[,c(1:6,8:11,14)]
head(temp8)
names(temp8)
#no exposition
qcn<-melt(temp8, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Leaf carbon content per dry mass","Leaf carbon/nitrogen (C/N) ratio"),
         variable.name = "Trait",
         value.name = "value")

write.csv(qcn, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/QuercusLeafC&NDatabase.csv', row.names=FALSE)

unique(temp8$`Plant developmental status / plant age / maturity / plant life stage`)

# young and old trees

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[9]
#SpecificleafareaforwoodyspeciesatSmithsonian-ForestGEOplot-Virginia,USA.csv

names(dat_9)
head(dat_9)
temp9<-dat_9[,c(1:6,8,10,11,13,17,18)]
head(temp9)
names(temp9)

smith<-melt(temp9, 
         # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Treatment exposition"
         #           ),
         measure.vars=c("Stem diameter at breast height (1.3 m, DBH)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(smith, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/SpecificleafareaforwoodyspeciesatSmithsonian-ForestGEOplot-Virginia,USA.csv', row.names=FALSE)

unique(temp9$`Plant developmental status / plant age / maturity / plant life stage`)
#unique(temp9$`Treatment exposition`)
#BOTH juvenile and mature individuals; all in natural environments 

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[10]
#TheFunctionalEcologyofTrees(FET)Database-Jena.csv

names(dat_10)
head(dat_10)
temp10<-dat_10[,c(1:6,9,10,12,14,27,28,30,35:45,66:68,75,94,112)]
head(temp10)
names(temp10)
#no exposition
fet<-melt(temp10, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Basal area","Canopy height of stand","Diameter at breast height (DBH) of the forest stand","Leaf age at measurement","Leaf area index of the site (LAI)","Maximum diameter at breast height max","Maximum diameter at breast height mean","Maximum height","Plant height vegetative","SLA leaf area type"),
         variable.name = "Trait",
         value.name = "value")

write.csv(fet, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TheFunctionalEcologyofTrees(FET)Database-Jena.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[11]
#TheNetherlandsPlantTraitsDatabase.csv

names(dat_11)
head(dat_11)
temp11<-dat_11[,c(1:9,12,23)]
head(temp11)
names(temp11)
#no exposition
nether<-melt(temp11, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Leaf carbon/nitrogen (C/N) ratio","Plant height vegetative","Wood density; stem specific density; wood specific gravity"),
         variable.name = "Trait",
         value.name = "value")

write.csv(nether, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TheNetherlandsPlantTraitsDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[12]
#TheXylemPhloemDatabase.csv

names(dat_12)
head(dat_12)
temp12<-dat_12[,c(1:6,13,17,24)]
head(temp12)
names(temp12)
#no exposition
xylem<-melt(temp12, 
 #        id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Plant height (unspecified if vegetative or reproductive)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(xylem, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TheXylemPhloemDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[13]
#TundraPlantTraitsDatabase.csv

names(dat_13)
head(dat_13)
temp13<-dat_13[,c(1:8,10)]
head(temp13)
names(temp13)
#no exposition
tundra<-melt(temp13, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
         measure.vars=c("Plant height vegetative","Woodiness"),
         variable.name = "Trait",
         value.name = "value")

write.csv(tundra, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TundraPlantTraitsDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[14]
#Woodcarboncontentdatabase.csv

names(dat_14)
head(dat_14)
temp14<-dat_14[,c(1:8)]
head(temp14)
names(temp14)
#no exposition
wccd<-melt(temp14, 
         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName"),
         measure.vars=c("Leaf carbon content per dry mass"),
         variable.name = "Trait",
         value.name = "value")

write.csv(wccd, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/Woodcarboncontentdatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[15]
#Woodcarbondatabase.csv

names(dat_15)
head(dat_15)
temp15<-dat_15[,c(1:8,10,13)]
head(temp15)
names(temp15)
#no exposition
wcd<-melt(temp15, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Leaf carbon content per dry mass"),
         variable.name = "Trait",
         value.name = "value")


write.csv(wcd, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/Woodcarbondatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[16]
#XylemFunctionalTraits(XFT)Database.csv

names(dat_16)
head(dat_16)
temp16<-dat_16[,c(1:6,27,33,35,65:67,81,97)]
head(temp16)
names(temp16)
#no exposition
xft<-melt(temp16, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Maximum plant height","Photosynthesis per leaf area at leaf temperature (A_area)","Plant height observed","Stomata conductance per leaf area at Asat measurement","Wood density; stem specific density; wood specific gravity"),
         variable.name = "Trait",
         value.name = "value")

write.csv(xft, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/XylemFunctionalTraits(XFT)Database.csv', row.names=FALSE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#Cleaning the inconsistent datasets:
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list=ls()) 
options(stringsAsFactors = FALSE)

folder<-"~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/"
file_list <- list.files(path=folder, pattern="*.csv") 
file_list[1]
for (i in 1:length(file_list)){
  out<-assign(file_list[i], 
              read.csv(paste(folder, file_list[i], sep='')))
  (out<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "StdValue", na.rm=TRUE))
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
dat1_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/LeafStructureandChemistry.csv")
out<-dcast(dat1_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE); names(out)
out<-out[,c(1:7,15)]


temp17<-merge(temp1, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
colnames(temp17)[colnames(temp17)=="258"] <- "LDMC"
names(temp17)

struc<-melt(temp17, 
          # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
          #           ),
          measure.vars=c("Leaf carbon content per dry mass","Plant height observed","Stem diameter at base (basal diameter)","LDMC"),
          variable.name = "Trait",
          value.name = "value")


write.csv(struc, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/LeafStructureandChemistry.csv', row.names=FALSE)
#######################################################
file_list[2]
#[2] Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv

names(dat_2)
temp2<-dat_2[,c(1:6,8:9,12,14)]
head(temp2)

dat2_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv")
out<-dcast(dat2_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE); names(out)
out<-out[,c(1:6,18)]


temp18<-merge(temp2, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp18)
names(temp18)

struc<-melt(temp18, 
            # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
            #           ),
            measure.vars=c("Stem diameter at breast height (1.3 m"),
            variable.name = "Trait",
            value.name = "value")

write.csv(struc, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv', row.names=FALSE)
#######################################################
file_list[3]
#MARGINS-leaftraitsdatabase.csv

head(dat_3)
names(dat_3)
temp3<-dat_3[,c(1:6,11,13,15,19,35:38)]
head(temp3)

#get the LMDC and SLA values
dat3_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/MARGINS-leaftraitsdatabase.csv")
out<-dcast(dat3_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
out<-out[,c(1:7)]
head(out)

temp19<-merge(temp3, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
colnames(temp19)[colnames(temp19)=="258"] <- "LDMC"
head(temp19)
names(temp19)

margins<-melt(temp19, 
            # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
            #           ),
            measure.vars=c("Height of measurement from the ground / height from which sample was collected","Stomata density on lower surface","Stomata density on lower surface inside","Stomata density on lower surface outside","Stomata density on lower surface top","LDMC"),
            variable.name = "Trait",
            value.name = "value")

write.csv(margins, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/MARGINS-leaftraitsdatabase.csv', row.names=FALSE)
#######################################################
file_list[4]
#PlantPhysiologyDatabase.csv

names(dat_4)
head(dat_4) 
temp4<-dat_4[,c(1:6,9,11,13:14)]
head(temp4)

#get the LMDC and SLA values
dat4_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantPhysiologyDatabase.csv")
out<-dcast(dat4_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
out<-out[,c(1:7,15)]
head(out)

temp20<-merge(temp4, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
colnames(temp20)[colnames(temp20)=="258"] <- "LDMC"
head(temp20)
names(temp20)

pphysio<-melt(temp20, 
              # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"#,"Exposition"
              #           ),
              measure.vars=c("Photosynthesis per leaf area at leaf temperature (A_area)","LDMC","Specific leaf area (SLA) per fresh weight"),
              variable.name = "Trait",
              value.name = "value")

write.csv(pphysio, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PlantPhysiologyDatabase.csv', row.names=FALSE)

unique(temp20$`Plant developmental status / plant age / maturity / plant life stage`)
# JUST seedlings

#######################################################
file_list[5]
# "PLANTSdataUSDA.csv"
names(dat_5)
temp5<-dat_5[,c(1:6)]
head(temp5)

#mature tree height values are under the unit colmn and default is in feet not meters
dat5_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PLANTSdataUSDA.csv")
out<-dcast(dat5_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "StdValue", na.rm=TRUE)
out<-out[,c(1:7)]
out

dat5_3<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PLANTSdataUSDA.csv")
out2<-dcast(dat5_3, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "UnitName", na.rm=TRUE)
out2<-out2[,c(1:6,8)]
head(out_c)

out_c<-merge(out2,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
temp21<-merge(temp5, out_c, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp21)

usda<-melt(temp21, 
 #             id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
              measure.vars=c("Height at 20 Years","Plant height vegetative"),
              variable.name = "Trait",
              value.name = "value")
head(usda)
write.csv(usda, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PLANTSdataUSDA.csv', row.names=FALSE)

#######################################################
file_list[6]
# PlantTraitsforPinusandJuniperusForestsinArizona.csv
names(dat_6)
#both LDMC and DBH are in the OrigUnitStr
temp6<-dat_6[,c(1:6,9:11,13:14,18)]
head(temp6)

dat6_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsforPinusandJuniperusForestsinArizona.csv")
out<-dcast(dat6_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
head(out)
out<-out[,c(1:7,17)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp22<-merge(temp6,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp22)
names(temp22)
arzn<-melt(temp22, 
#           id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
           measure.vars=c("Leaf carbon content per dry mass",
                          "Leaf carbon/nitrogen (C/N) ratio",                          
                          "Longitude",                                                 
                          "Maximum plant height",                                      
                          "Wood density; stem specific density; wood specific gravity",
                          "LDMC",                                                      
                          "Stem diameter at breast height (1.3 m"),
           variable.name = "Trait",
           value.name = "value")

write.csv(arzn, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PlantTraitsforPinusandJuniperusForestsinArizona.csv', row.names=FALSE)

#######################################################
file_list[7]
#PlantTraitsfromCirceoNationalPark,Italy.csv
names(dat_7)
temp7<-dat_7[,c(1:6,10,12)]
head(temp7)

dat7_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsfromCirceoNationalPark,Italy.csv")
out<-dcast(dat7_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "StdValue", na.rm=TRUE)
out<-out[,c(1:7,9,11)]
head(out)

#now combining into one dataset
temp23<-merge(temp7,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))

head(temp23)
names(temp23)
italy<-melt(temp23, 
           # id.vars=c("LastName",
           #           "FirstName",
           #           "DatasetID",
           #           "Dataset",
           #           "SpeciesName",
           #           "ObservationID",
           #           "Latitude",
           #           "Longitude"#,
           #           #"Treatment exposition"
           #           ),
           measure.vars=c("Plant height vegetative"),
           variable.name = "Trait",
           value.name = "value")
#colnames(beech)[colnames(italy)=="Treatment exposition"] <- "Exposition"

write.csv(italy, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PlantTraitsfromCirceoNationalPark,Italy.csv', row.names=FALSE)

#######################################################
file_list[8]
#PlantTraitsfromCirceoNationalPark.csv
names(dat_8)

#Trait values are in the UnitNam col
dat8_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsfromCirceoNationalPark.csv")
out<-dcast(dat8_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~StdValue, value.var = "UnitName", na.rm=TRUE)
names(out)
temp24<-out[,c(1:9,10,11)]
head(temp24)
names(temp24)

circeo<-melt(temp24, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID",
            #           "latitude",
            #           "longitude" #,
            #           #"exposition"
            #           ),
            measure.vars=c("Height (cm)"),
            variable.name = "Trait",
            value.name = "value")
#colnames(circeo)[colnames(circeo)=="exposition"] <- "Exposition"
colnames(circeo)[colnames(circeo)=="longitude"] <- "Longitude"
colnames(circeo)[colnames(circeo)=="latitude"] <- "Latitude"
#All mature in natural envrt
write.csv(circeo, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PlantTraitsfromCirceoNationalPark.csv', row.names=FALSE)

#######################################################
file_list[9]
#PlantTraitsFromSpanishMediteraneanshrublands.csv
names(dat_9)

#LDMC data 
temp9<-dat_9[,c(1:6,8,11,12,13,15)]
head(temp9)
dat9_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsFromSpanishMediteraneanshrublands.csv")
out<-dcast(dat9_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp25<-merge(temp9,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp25)
names(temp25)
medit<-melt(temp25, 
             # id.vars=c("LastName",
             #           "FirstName",
             #           "DatasetID",
             #           "Dataset",
             #           "SpeciesName",
             #           "ObservationID",
             #           "Latitude",
             #           "Longitude"),
             measure.vars=c("Plant height reproductive / generative","Plant height vegetative",
                            "Wood density; stem specific density; wood specific gravity",
                            "LDMC"),
             variable.name = "Trait",
             value.name = "value")
write.csv(medit, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PlantTraitsFromSpanishMediteraneanshrublands.csv', row.names=FALSE)

#######################################################

# PlantTraitsofCanadianForests.csv
# 
# # #Height is in cm, so converting to m
dat_10<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsofCanadianForests.csv")
dat_10<-dcast(dat_10, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "StdValue", na.rm=TRUE)

#Height is in cm, so converting to m
#heights are in cm so use StdValue
names(dat_10)
temp26<-dat_10[,c(1:6,8,9,11,13,20:22)]
names(temp26)

canad<-melt(temp26, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID",
            #           "Latitude",
            #           "Longitude"),
            measure.vars=c("Height of measurement: stem diameter",
                           "Plant height vegetative",
                           "Stem diameter 10 cm above soil surface",
                           "Stem diameter at base (basal diameter)",
                           "Stem diameter at breast height (1.3 m" 
                           ),
            variable.name = "Trait",
            value.name = "value")
write.csv(canad, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/PlantTraitsofCanadianForests.csv', row.names=FALSE)

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
dat11_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/RollinsonDBH.csv")
out<-dcast(dat11_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
out<-out[,c(1:6,13)]
head(out)

temp27<-merge(temp11,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp27)
names(temp27)

roll<-melt(temp27, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID",
            #           "Latitude",
            #           "Longitude"#,
            #           #"Exposition"
            #           ),
            measure.vars=c("Stem diameter at breast height (1.3 m"
            ),
            variable.name = "Trait",
            value.name = "value")

write.csv(roll, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/RollinsonDBH.csv', row.names=FALSE)

#unique(temp27$`Exposition`)
#All natural

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[12]
#Sheffield&SpainWoodyDatabase.csv
names(dat_12)
head(dat_12)
temp12<-dat_12[,c(1:6,8,9,10,12,13,19)]
head(temp12)
names(temp12)

sheffwood<-melt(temp12, 
           # id.vars=c("LastName",
           #           "FirstName",
           #           "DatasetID",
           #           "Dataset",
           #           "SpeciesName",
           #           "ObservationID"
           #           #,"Exposition"
           #           ),
           measure.vars=c("Leaf mass per area based on fresh weight",
                          "Specific leaf area (SLA) per fresh weight",
                          "Stomata density",
                          "Wood density; stem specific density; wood specific gravity"
           ),
           variable.name = "Trait",
           value.name = "value")

write.csv(sheffwood, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/Sheffield&SpainWoodyDatabase.csv', row.names=FALSE)

#not sure why I put this one in the inconsistent folder

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[13]
#SheffieldDatabase.csv
names(dat_13)
head(dat_13)
temp13<-dat_13[,c(1:6,14, 16,19,22,24,27,32:35)]
head(temp13)

#LDMC is under OrigUnitStr
dat13_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/SheffieldDatabase.csv")
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

names(temp28)

sheff<-melt(temp28, 
                # id.vars=c("LastName",
                #           "FirstName",
                #           "DatasetID",
                #           "Dataset",
                #           "SpeciesName",
                #           "ObservationID",
                #           #"Exposition temperature",
                #           "Latitude",
                #           "Longitude"),
                measure.vars=c("Leaf mass per area based on fresh weight",
                               "Specific leaf area (SLA) per fresh weight",
                               "Stomata density",
                               "stomata density: lower surface only",                
                               "stomata density: upper+lower / one-sided leaf area",
                               "LWC",                                                
                               "LDMC"
                ),
                variable.name = "Trait",
                value.name = "value")

write.csv(sheff, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/SheffieldDatabase.csv', row.names=FALSE)

head(temp28)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[14]
#StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv

names(dat_14)
head(dat_14)
temp14<-dat_14[,c(1:6,8,9,10:14,16)]
head(temp14)

#LDMC is under OrigUnitStr
dat14_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv")
out<-dcast(dat14_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp29<-merge(temp14, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
names(temp29)

fin<-melt(temp29, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID",
            #           #"Exposition",
            #           "Latitude",
            #           "Longitude"),
            measure.vars=c("Leaf carbon content per dry mass",
                           "Leaf carbon/nitrogen (C/N) ratio",                     
                           "LDMC"
            ),
            variable.name = "Trait",
            value.name = "value")
write.csv(fin, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv', row.names=FALSE)

unique(temp29$`Plant developmental status / plant age / maturity / plant life stage`)
#unique(temp29$`Exposition`)
#BOTH juvenile and mature individuals; all in natural environments 

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[15]
#TheChinaPlantTraitDatabase.csv

names(dat_15)
head(dat_15)
temp15<-dat_15[,c(1:6,14,16,17,20)]
head(temp15)

#LDMC is under OrigUnitStr
dat15_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TheChinaPlantTraitDatabase.csv")
out<-dcast(dat15_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp30<-merge(temp15, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp30)
names(temp30)

china<-melt(temp30, 
          # id.vars=c("LastName",
          #           "FirstName",
          #           "DatasetID",
          #           "Dataset",
          #           "SpeciesName",
          #           "ObservationID",
          #           "Latitude",
          #           "Longitude"),
          measure.vars=c("Leaf carbon content per dry mass",
                         "Leaf photosynthesis at saturating light and ambient CO2 per leaf area (Asat)",                     
                         "LDMC"
          ),
          variable.name = "Trait",
          value.name = "value")
write.csv(china, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TheChinaPlantTraitDatabase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[16]
#TheGlobalLeafTraits.csv

names(dat_16)
head(dat_16)
temp16<-dat_16[,c(1:6,29:33,37,74)]
head(temp16)

#LDMC is under OrigUnitStr
dat16_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TheGlobalLeafTraits.csv")
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
names(temp31)

glbl<-melt(temp31, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID",
            #           "Latitude",
            #           "Longitude"),
            measure.vars=c("Leaf area index of the site (LAI)", 
                           "Leaf carbon content per dry mass", 
                           "Leaf carbon/nitrogen (C/N) ratio",
                           "Leaf lifespan (longevity",                 
                           "Stomata density",
                           "LWC",
                           "LDMC"
            ),
            variable.name = "Trait",
            value.name = "value")

write.csv(glbl, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TheGlobalLeafTraits.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[17]
#TheLEDATraitbase.csv
names(dat_17)
head(dat_17)
temp17<-dat_17[,c(1:6,10,13,16:18,23:25,27)]
head(temp17)

dat17_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TheLEDATraitbase.csv")
out<-dcast(dat17_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp32<-merge(temp17, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp32)
names(temp32)

leda<-melt(temp32, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID",
            #           "Latitude",
            #           "Longitude"),
            measure.vars=c("Plant height reproductive / generative",
                           "Plant height vegetative",
                           "Seed number per reproducton unit (seed / ovule ratio)",
                           "Seed number: reproduction subunit measured",
                           "Seed number: reproduction unit measured",
                           "Wood density; stem specific density; wood specific gravity",          
                           "LDMC"    
            ),
            variable.name = "Trait",
            value.name = "value")

write.csv(leda, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TheLEDATraitbase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[18]
#TraitsfromSubarcticPlantSpeciesDatabase.csv
names(dat_18)
head(dat_18)
temp18<-dat_18[,c(1:6,9,10:13,15)]
head(temp18)

dat18_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TraitsfromSubarcticPlantSpeciesDatabase.csv")
out<-dcast(dat18_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7)]
colnames(out)[colnames(out)=="258"] <- "LDMC"

temp33<-merge(temp18, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp33)
names(temp33)

subartic<-melt(temp33, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID",
            #           "Latitude",
            #           "Longitude"#,
            #           #"Exposition"
            #           ),
            measure.vars=c("Leaf carbon content per dry mass",
                           "Leaf carbon/nitrogen (C/N) ratio", 
                           "LDMC"
            ),
            variable.name = "Trait",
            value.name = "value")

write.csv(subartic, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/TraitsfromSubarcticPlantSpeciesDatabase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[19]
#WholePlantHydraulicConductance.csv
names(dat_19)
head(dat_19)
temp19<-dat_19[,c(1:7)]
head(temp19)

dat19_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/WholePlantHydraulicConductance.csv")
out<-dcast(dat19_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigUnitStr", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:6,9)]

temp34<-merge(temp19, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp34)
names(temp34)

whydr<-melt(temp34, 
            # id.vars=c("LastName",
            #           "FirstName",
            #           "DatasetID",
            #           "Dataset",
            #           "SpeciesName",
            #           "ObservationID"
            #           ),
            measure.vars=c("Plant height observed",
                           "Stem diameter at breast height (1.3 m"
            ),
            variable.name = "Trait",
            value.name = "value")


write.csv(whydr, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/WholePlantHydraulicConductance.csv', row.names=FALSE)

#Now merge all files into one:
myfiles = list.files(path="~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/cleanlong/", pattern="*.csv", full.names=TRUE)
myfiles
dat_clean = ldply(myfiles, read_csv)
dat_clean

head(dat_clean)
length(unique(dat_clean$Dataset))

write.csv(dat_clean, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/try_cleanlong_dl.csv', row.names=FALSE)
