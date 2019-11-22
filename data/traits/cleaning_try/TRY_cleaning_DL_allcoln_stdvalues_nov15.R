rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse) 
require(tidyr)

require(reshape2)
# Select georeferenced Datasets -------------------------------------------
setwd("~/Desktop/ospree_trait_analysis")
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

not_all_na <- function(x) any(!is.na(x))


folder<-"~/Desktop/ospree_trait_analysis/input/TRY_Nov12/Deirdre/"
file_list <- list.files(path=folder, pattern="*.csv") 

##########################################################################
file_list[1]

out<-assign(file_list[1], 
            read.csv(paste(folder, file_list[1], sep=''))); names(out)
out1<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+OrigUnitStr~DataName, 
            value.var = c("OrigValueStr"), na.rm=TRUE)
out$DataName<-paste("std",out$DataName, sep="_")
out2<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+UnitName~DataName, 
            value.var = "StdValue", na.rm=TRUE)
out3<-merge(out1, out2, by = c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","ObsDataID"))

names(out3)
out3<-out3[,c(1:17,20,23:28,32:35,37:43,53,54,56,57,64,65,71)]
beech<-melt(out3, 
            #id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude","Treatment exposition"
            #          ),
            measure.vars=c("std_Leaf area: in case of compound leaves leaflet; petiole excluded","std_Leaf carbon content per dry mass","std_Leaf nitrogen content per area (Narea)","std_Leaf nitrogen content per dry mass (Nmass)","std_Photosynthesis per leaf area at leaf temperature (A_area)","std_Plant height vegetative","std_SLA: petiole  excluded"),
            variable.name = "Trait",
            value.name = "value")
#colnames(beech)[colnames(beech)=="Treatment exposition"] <- "Exposition"
names(beech)
#writing a new csv file with just the data we will use
write.csv(beech, '~/Desktop/ospree_trait_analysis/Abisko_SheffieldDatabase.csv', row.names=FALSE)
unique(out3$'std_Leaf area: in case of compound leaves leaflet; petiole excluded')

################################################################

for (i in 1:length(file_list)){
  out<-assign(file_list[i], 
              read.csv(paste(folder, 
                             file_list[i], 
                             sep='')))
  out1<-dcast(out, 
              LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+OrigUnitStr~DataName, 
              value.var = c("OrigValueStr"), 
              na.rm=TRUE);head(out1)
  out$DataName<-paste("std",out$DataName, sep="_")
  out2<-dcast(out, 
              LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+UnitName~DataName, 
               value.var = "StdValue", 
              na.rm=TRUE)
  out2%>% select_if(not_all_na)
  out3<-merge(out1,
              out2, 
              by =c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","ObsDataID"))
  assign(paste("dat",i,sep="_"), out3)
}

#Gives 16 datasets renamed dat_1 to dat_16


# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[1]
# "Abisko_SheffieldDatabase.csv"
#Looking at the columns and which traits are in the dataset 
file_list[1]

out<-assign(file_list[1], 
            read.csv(paste(folder, file_list[1], sep=''))); names(out)
out1<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+OrigUnitStr~DataName, 
            value.var = c("OrigValueStr"), na.rm=TRUE)
out$DataName<-paste("std",out$DataName, sep="_")
out2<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+ObsDataID+UnitName~DataName, 
            value.var = "StdValue", na.rm=TRUE)
out3<-merge(out1, out2, by = c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","ObsDataID"))

names(out3)
out3<-out3[,c(1:17,20,23:28,32:35,37:43,53,54,56,57,64,65,71)]
abisko<-melt(out3, 
             # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"
             #),
             measure.vars=c('std_Leaf area: in case of compound leaves leaflet; petiole excluded',"std_Leaf carbon content per dry mass","std_Leaf nitrogen content per area (Narea)","std_Leaf nitrogen content per dry mass (Nmass)","std_Photosynthesis per leaf area at leaf temperature (A_area)","std_Plant height vegetative","std_SLA: petiole  excluded"),
             variable.name = "Trait",
             value.name = "value")

length(abisko$Trait)
length(unique(abisko$value))

abisko$label<-paste(abisko$ObservationID,abisko$Trait, sep=".")
head(abisko$label)
ids <- unique(abisko$label)
#ids<-unique(abisko$ObservationID)
stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(abisko, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}
stor
unique(stor$value)

#colnames(beech)[colnames(beech)=="Treatment exposition"] <- "Exposition"
names(abisko)
#writing a new csv file with just the data we will use
write.csv(stor, '~/Desktop/ospree_trait_analysis/Abisko_SheffieldDatabase.csv', row.names=FALSE)
unique(out3$'std_Leaf area: in case of compound leaves leaflet; petiole excluded')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[2]
# AGlobalDataSetofLeafPhotosyntheticRates,LeafNandP,andSpecificLeafArea

names(dat_2)

dat_2<-dat_2[,c(1:12,16:21,27:28,30:35,40:42,56)]

ag<-melt(dat_2,
           measure.vars=c("std_Leaf age at measurement","std_Leaf nitrogen content per area (Narea)","std_Leaf nitrogen content per dry mass (Nmass)","std_SLA: undefined if petiole in- or excluded"),
           variable.name = "Trait",
           value.name = "value")

ag$label<-paste(ag$ObservationID,ag$Trait, sep=".")

ids <- unique(ag$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(ag, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}
head(stor)
unique(stor$value)


write.csv(stor, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/AGlobalDataSetofLeafPhotosyntheticRates,LeafNandP,andSpecificLeafArea.csv', row.names=FALSE)
names(ag)
unique(ag$`Experimental treatment`)
unique(ag$`Comments, notes, methods`)
unique(ag$`Treatment CO2`) # ele, amb, no applic
unique(ag$`Treatment conditions`) # not applic, months post treatment initiation, tree ago
unique(ag$`Treatment light`) # no app, shade, sun, upper, lower, high, low
#experimental treatments are numbers? The only comment is 'no'

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[3]
# AllometricCoefficientsofAbovegroundTreeBiomass
head(dat_3)
names(dat_3)
dat_3<-dat_3[,c(1:7,11:13,16:20,24:26,27)]


allo<-melt(dat_3, 
#           id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
           measure.vars=c("std_Maximum diameter at breast height","std_Minimum diameter at breast height" ),
           variable.name = "Trait",
           value.name = "value")

allo$label<-paste(allo$ObservationID,allo$Trait, sep=".")

ids <- unique(allo$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(allo, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}
head(stor)
unique(stor$value)

write.csv(allo, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/AllometricCoefficientsofAbovegroundTreeBiomass.csv', row.names=FALSE)

#only one height value/line in the entire dataset

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[4]
# AltitudinalVicariantsSpain
names(dat_4)
temp4<-dat_4[,c(1:13,17:19,23:26,31:35,38,40)]
head(temp4)


av<-melt(temp4, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","lat","lon"),
         measure.vars=c( "std_Leaf area: in case of compound leaves leaf; petiole excluded","std_Leaf nitrogen content per area (Narea)"                              
                         ,"std_Leaf nitrogen content per dry mass (Nmass)",
                         "std_Plant height vegetative","std_SLA: petiole  excluded"), #There is a space in front of DBH
         variable.name = "Trait",
         value.name = "value")

av$label<-paste(av$ObservationID,av$Trait, sep=".")

ids <- unique(av$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(av, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}

head(stor)

write.csv(av, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/AltitudinalVicariantsSpain.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[5]
#BAAD_abiomassandallometrydatabaseforwoodyplants
# VERY SLOW TO RUN
names(dat_5)
unique(dat_5$`Leaf area index of the site (LAI)`)
temp5<-dat_5[,c(1:8,51,55,58:64,68:72,80,135,141,137:139,145,147:149,151)]
head(temp5)
names(temp5)

baad<-melt(temp5, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Leaf area index of the site (LAI)",
                        "std_Leaf area: in case of compound leaves leaflet; undefined if petiole in- or excluded"
                        ,"std_Leaf nitrogen content per area (Narea)"                                             
                       ,"std_Leaf nitrogen content per dry mass (Nmass)"                                         
                       ,"std_Plant height vegetative"                                                            
                       ,"std_SLA: undefined if petiole in- or excluded"                                          
                        ,"std_Stem diameter at base (basal diameter)"                                             
                        ,"std_Stem diameter at breast height (1.3 m, DBH)"                                        
                        ,"std_Wood density; stem specific density; wood specific gravity"  ),
         variable.name = "Trait",
         value.name = "value")
#colnames(photo)[colnames(photo)=="Treatment exposition"] <- "Exposition"

baad$label<-paste(baad$ObservationID,baad$Trait, sep=".")

ids <- unique(baad$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(baad, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}

head(stor)
write.csv(baad, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/BAAD_abiomassandallometrydatabaseforwoodyplants.csv', row.names=FALSE)

unique(temp5$`Plant developmental status / plant age / maturity / plant life stage`)
# JUST saplings 

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[6]
#Baccara-PlantTraitsofEuropeanForests.csv
names(dat_6)
temp6<-dat_6[,c(1:8,10,16,17,19,20,22,23)]


bacc<-melt(temp6, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
         measure.vars=c("std_Leaf nitrogen content per dry mass (Nmass)"
                       ,"std_Maximum plant height"                                       
                       ,"std_Plant height vegetative"                                   
                        ,"std_Stem diameter at breast height (1.3 m, DBH)"               
                       ,"std_Wood density; stem specific density; wood specific gravity"),
         variable.name = "Trait",
         value.name = "value")

bacc$label<-paste(bacc$ObservationID,bacc$Trait, sep=".")

ids <- unique(bacc$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(bacc, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}

head(stor)

write.csv(bacc, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/Baccara-PlantTraitsofEuropeanForests.csv', row.names=FALSE)

#heights are in cm

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[7]
#BASECO_afloristicandecologicaldatabaseofMediterraneanFrenchflora

names(dat_7)

temp7<-dat_7[,c(1:8,10:11)]
head(temp7)
names(temp7)

baseco<-melt(temp7, 
         # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Treatment exposition"
         #           ),
         measure.vars=c("std_Plant height (unspecified if vegetative or reproductive)"),
         variable.name = "Trait",
         value.name = "value")

baseco$label<-paste(baseco$ObservationID,baseco$Trait, sep=".")

ids <- unique(baseco$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(baseco, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}

head(stor)
write.csv(baseco, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/BASECO_afloristicandecologicaldatabaseofMediterraneanFrenchflora.csv', row.names=FALSE)

#unique(temp7$`Treatment exposition`)

#All juvenile and all natural envirts

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[8]
#Biomassallocationinbeechandspruceseedlings.csv
names(dat_8)
temp8<-dat_8[,c(1:10,11,15,16:17,20,23)]

babs<-melt(temp8, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("std_Plant height observed","std_Stem diameter" ),
         variable.name = "Trait",
         value.name = "value")

babs$label<-paste(babs$ObservationID,babs$Trait, sep=".")

ids <- unique(babs$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(babs, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}

head(stor)
write.csv(babs, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/Biomassallocationinbeechandspruceseedlings.csv', row.names=FALSE)

unique(temp8$`Plant developmental status / plant age / maturity / plant life stage`)

# Na and 4

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[9]
#BIOTREETraitShadeExperiment.csv

names(dat_9)
temp9<-dat_9[,c(1:15,17,19:33,35:39,46,53,47,49,65)]
head(temp9)
names(temp9)

biotree<-melt(temp9, 
         # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Treatment exposition"
         #           ),
         measure.vars=c("std_Leaf carbon content per dry mass","std_Leaf nitrogen content per dry mass (Nmass)","std_SLA: petiole  included"),
         variable.name = "Trait",
         value.name = "value")

baseco$label<-paste(baseco$ObservationID,baseco$Trait, sep=".")

ids <- unique(baseco$label)

stor <- data.frame()
for(i in 1:length(ids)){
  temp <- subset(baseco, label == ids[i])
  stor.temp <- temp[1, ]
  for (j in 1:nrow(temp)){
    temp2<-which(!is.na(as.vector(temp[j,])))
    stor.temp[temp2]<-temp[j, temp2] #for all elements not NA replace
  }
  stor <- rbind(stor, stor.temp)
}

head(stor)

write.csv(smith, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/BIOTREETraitShadeExperiment.csv', row.names=FALSE)

unique(temp9$`Plant developmental status / plant age / maturity / plant life stage`)
#unique(temp9$`Treatment exposition`)
#BOTH juvenile and mature individuals; all in natural environments 

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[10]
#"BROTPlantTraitDatabase.csv"

names(dat_10)

temp10<-dat_10[,c(1:9,10,12,15,18,20)]


fet<-melt(temp10, 
         measure.vars=c("std_Leaf area: in case of compound leaves leaf; undefined if petiole in- or excluded", "std_Plant height vegetative"),
         variable.name = "Trait",
         value.name = "value")

write.csv(fet, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/BROTPlantTraitDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[11]
#CanopyTraitsforTemperateTreeSpeciesUnderHighN-Deposition.csv

names(dat_11)
unique(dat_11$`Height of measurement from the ground / height from which sample was collected`)
temp11<-dat_11[,c(1:9,10,11,12,15:19,23,28,29,35:37)]
head(temp11)
names(temp11)

canopy<-melt(temp11, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("Height of measurement from the ground / height from which sample was collected",
                        "std_Leaf carbon content per dry mass"                                 ,"std_Leaf nitrogen content per dry mass (Nmass)","std_Stomata conductance to water vapour per leaf area"                             , "std_Stomata density"                                               , "std_Stomata length"),
         variable.name = "Trait",
         value.name = "value")
head(canopy)
write.csv(canopy, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/CanopyTraitsforTemperateTreeSpeciesUnderHighN-Deposition.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[12]
#ChineseTraits.csv
unique(dat_12$`C amount at 13C measurement`)
names(dat_12)

temp12<-dat_12[,c(1:9,11,17,18,22:24,26,28:32,35:37)]

ct<-melt(temp12, 
 #        id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("std_C amount at 13C measurement","std_Leaf area: in case of compound leaves undefined if leaf or leaflet; undefined if petiole and rhachis in- or excluded","std_Leaf carbon content per dry mass","std_Leaf dry matter content per leaf water-saturated mass (LDMC)","std_Leaf nitrogen content per area (Narea)","std_Leaf nitrogen content per dry mass (Nmass)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(ct, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/ChineseTraits.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[13]
#ColdTolerance,SeedSizeandHeightofNorthAmericanForestTreeSpecies.csv

names(dat_13)

temp13<-dat_13[,c(1:8,10,11)]


tundra<-melt(temp13, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
         measure.vars=c("std_Maximum plant height"),
         variable.name = "Trait",
         value.name = "value")

write.csv(tundra, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/ColdTolerance,SeedSizeandHeightofNorthAmericanForestTreeSpecies.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[14]
#DayandNightGasExchangeofDeciduousTreeSeedlingsinResponsetoExperimentalWarmingandPreci.csv

names(dat_14)

temp14<-dat_14[,c(1:38,40:54,85)]
head(temp14)
names(temp14)

day<-melt(temp14, 
         #id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName"),
         measure.vars=c( "std_Photosynthesis per leaf area at leaf temperature (A_area)"),
         variable.name = "Trait",
         value.name = "value")

write.csv(day, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/DayandNightGasExchangeofDeciduousTreeSeedlingsinResponsetoExperimentalWarmingandPreci.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[15]
#ECOCRAFT.csv

names(dat_15)
temp15<-dat_15[,c(1:9,11:12,14:18,20,26:36,38,40:42,48:56,58,61,67,69:73,85,87,91:92,94,95)]
head(temp15)
names(temp15)

wcd<-melt(temp15, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("std_Average tree height","std_Diameter at base","std_Green crown length"                             ,"std_Leaf carbon content per dry mass"                                                            ,"std_Leaf nitrogen content per area (Narea)"                                                     ,"std_Leaf nitrogen content per dry mass (Nmass)"                                                 ,"std_Leaf photosynthesis at saturating light and saturating CO2 per leaf area (Amax)" ,"std_Light-saturated rate of photosynthesis, at the growth CO2 concentration, on a projected area basis"),
         variable.name = "Trait",
         value.name = "value")


write.csv(wcd, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/ECOCRAFT.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[16]
#EcologicalFloraoftheBritishIsles.csv

names(dat_16)

temp16<-dat_16[,c(1:8,12,15,16:18,20,21)]
head(temp16)
names(temp16)

xft<-melt(temp16, 
#         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
         measure.vars=c("std_Leaf area: in case of compound leaves undefined if leaf or leaflet; undefined if petiole and rhachis in- or excluded", "std_Leaf nitrogen content per dry mass (Nmass)", "std_Plant height (unspecified if vegetative or reproductive)","std_Stomata density on lower surface","std_Stomata density on upper surface"),
         variable.name = "Trait",
         value.name = "value")

write.csv(xft, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/EcologicalFloraoftheBritishIsles.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[17]
#EuropeanNorthRussia.csv

names(dat_17)

temp17<-dat_17[,c(1:9,20,24,26,28:37,39:41,43)]


euronr<-melt(temp17, 
          #         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
          measure.vars=c("std_Leaf area, whole leaf, petiole excluded (max)"               ,"std_Leaf area, whole leaf, petiole excluded (min)"               , "std_Leaf area: in case of compound leaves leaf; petiole excluded","std_Leaf dry matter content (max)"                               ,"std_Leaf dry matter content (min)"                               ,"std_Leaf dry matter content per leaf water-saturated mass (LDMC)","std_Leaf specific area (SLA): petiole excluded  (max)"           ,"std_Leaf specific area (SLA): petiole excluded (min)", "std_Plant height generative (maximum)","std_Plant height generative (minimum)", "std_Plant height reproductive / generative","std_SLA: petiole  excluded"),
          variable.name = "Trait",
          value.name = "value")

write.csv(euronr, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/EuropeanNorthRussia.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[18]
#"FloridianLeafTraitsDatabase.csv"

names(dat_18)

temp18<-dat_18[,c(1:11,13:16,18,19,21,25,30,33)]


flor<-melt(temp18, 
             #         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
             measure.vars=c("std_Leaf area: in case of compound leaves leaf; petiole excluded","std_Plant height vegetative","std_SLA: petiole  excluded"),
             variable.name = "Trait",
             value.name = "value")

write.csv(flor, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/FloridianLeafTraitsDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[19]
#FRED-FineRootEcologyDatabase.csv

unique(dat_19$`std_Leaf area index of the site (LAI)`)
unique(dat_19$`std_Basal area by species`)

names(dat_19)

#None of the traits have std values
temp19<-dat_19[,c(1:128)]


fred<-melt(temp19, 
             #         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
             measure.vars=c("Aboveground live biomass dry weight per ground area", "Basal area","Basal area by area","Basal area by species","Basal area by species by area","Canopy height observed", "Canopy height observed: maximum","Canopy height observed: minimum","Diameter at breast height (DBH) maximum","Diameter at breast height (DBH) minimum","Leaf area index of the site (LAI)","Photosynthesis per leaf area at leaf temperature (A_area)","Root diameter class maximum","Root diameter class minimum","Stem diameter at breast height (1.3 m, DBH)"),
             variable.name = "Trait",
             value.name = "value")

write.csv(fred, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/FRED-FineRootEcologyDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[20]
#FunctionalResilienceofTemperateForestsDataset.csv

names(dat_20)

temp20<-dat_20[,c(1:8,12,14,15,16,17,19)]


funres<-melt(temp20, 
             #         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
             measure.vars=c("std_Leaf area: in case of compound leaves leaf; petiole and rhachis included","std_Plant height vegetative","std_SLA: undefined if petiole in- or excluded","std_Wood density; stem specific density; wood specific gravity"),
             variable.name = "Trait",
             value.name = "value")

write.csv(funres, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/FunctionalResilienceofTemperateForestsDataset.csv', row.names=FALSE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[21]
#Functionaltraitsexplainingvariationinplantlifehistorystrategies.csv

names(dat_21)

dat_21<-dat_21[,c(1:8,13:17)]

fev<-melt(dat_21,
         measure.vars=c("std_Leaf nitrogen content per area (Narea)"                    
                        ,"std_Leaf nitrogen content per dry mass (Nmass)"                
                        ,"std_SLA: undefined if petiole in- or excluded (1)"             
                        ,"std_Wood density; stem specific density; wood specific gravity"),
         variable.name = "Trait",
         value.name = "value")

write.csv(fev, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/Functionaltraitsexplainingvariationinplantlifehistorystrategies.csv', row.names=FALSE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[22]
#FunctionalTraitsofTrees.csv

names(dat_22)

dat_22<-dat_22[,c(1:15,18,26,27)]

ftt<-melt(dat_22,
          measure.vars=c("std_SLA: undefined if petiole in- or excluded","std_Wood density; stem specific density; wood specific gravity"),
          variable.name = "Trait",
          value.name = "value")

write.csv(ftt, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/FunctionalTraitsofTrees.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[23]
# Global15NDatabase.csv

names(dat_23)
dat_23<-dat_23[,c(1:9,11:22,24)]


glb<-melt(dat_23, 
           #           id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
           measure.vars=c("std_Leaf nitrogen content per dry mass (Nmass)"),
           variable.name = "Trait",
           value.name = "value")

write.csv(glb, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/Global15NDatabase.csv', row.names=FALSE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[24]
# GlobalLeafGasExchangeDatabase(I).csv
names(dat_24)
temp24<-dat_24[,c(1:29,32:38,40:47,69,78)]

gge<-melt(temp24, 
            #         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","lat","lon"),
            measure.vars=c( "std_Photosynthesis per leaf area at leaf temperature (A_area)","std_Stomata conductance to water vapour per leaf area"), #There is a space in front of DBH
            variable.name = "Trait",
            value.name = "value")
write.csv(gge, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/GlobalLeafGasExchangeDatabase(I).csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[25]
#GlobalLeafGasExchangeDatabase(II).csv
unique(dat_25$`std_Stomata conductance to water vapour per leaf area`)
names(dat_25)

temp25<-dat_25[,c(1:24,26:29,31,32,34:41,58,63,66)]

ggeII<-melt(temp25, 
            measure.vars=c("std_Photosynthesis per leaf area at leaf temperature (A_area)","std_SLA: petiole  excluded","std_Stomata conductance to water vapour per leaf area"),
            variable.name = "Trait",
            value.name = "value")

write.csv(ggeII, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/GlobalLeafGasExchangeDatabase(II).csv', row.names=FALSE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[26]
#Globalleafsizedataset.csv

names(dat_26)
temp26<-dat_26[,c(1:12,15:19,24,25)]


gls<-melt(temp26, 
               #         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
               measure.vars=c( "std_Leaf area: in case of compound leaves leaf; undefined if petiole in- or excluded", "std_Leaf area: in case of compound leaves leaflet; undefined if petiole in- or excluded"),
               variable.name = "Trait",
               value.name = "value")

write.csv(gls, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/Globalleafsizedataset.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[27]
#GlobalRespirationDatabase.csv

names(dat_27)

temp27<-dat_27[,c(1:14,19:27,29:30,33:36,43:46,56,59,60)]


grd<-melt(temp27, 
           # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Treatment exposition"
           #           ),
           measure.vars=c("std_Leaf nitrogen content per area (Narea)"                        ,"std_Leaf nitrogen content per dry mass (Nmass)"                    ,"std_Leaf respiration: fixed Q10 for temperature standardization","std_Leaf respiration: variable Q10 for temperature standardization","std_Photosynthesis per leaf area at leaf temperature (A_area)","std_SLA: petiole  included","std_Stomata conductance to water vapour per leaf area"),
           variable.name = "Trait",
           value.name = "value")

write.csv(grd, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/GlobalRespirationDatabase.csv', row.names=FALSE)

#unique(temp7$`Treatment exposition`)

#All juvenile and all natural envirts

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[28]
#GlobalSeedMass,PlantHeightDatabase.csv
names(dat_28)
temp28<-dat_28[,c(1:13,15:27,28:31,37)]

gsm<-melt(temp28, 
          #         id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"),
          measure.vars=c("std_Maximum plant height"),
          variable.name = "Trait",
          value.name = "value")

write.csv(gsm, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/GlobalSeedMass,PlantHeightDatabase.csv', row.names=FALSE)


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[29]
#GlobalWoodDensityDatabase.csv

names(dat_29)
temp29<-dat_29[,c(1:9,11,13)]

gwdd<-melt(temp29,
            measure.vars=c("std_Wood density; stem specific density; wood specific gravity"),
            variable.name = "Trait",
            value.name = "value")

write.csv(gwdd, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/GlobalWoodDensityDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[30]
#GLOPNET-GlobalPlantTraitNetworkDatabase.csv

names(dat_30)

temp30<-dat_30[,c(1:14,19:69,72:75,78,80:110,117:120,172,173,178,179,181)]

glop<-melt(temp30, 
          measure.vars=c("std_Leaf area: in case of compound leaves leaf; undefined if petiole in- or excluded","std_Leaf area: in case of compound leaves leaflet; undefined if petiole in- or excluded","std_Leaf nitrogen content per area (Narea)","std_Leaf nitrogen content per dry mass (Nmass)","std_Photosynthesis per leaf area at leaf temperature (A_area)","std_Plant height vegetative","std_SLA: undefined if petiole in- or excluded","std_SLA: undefined if petiole in- or excluded (1)","std_Stomata conductance to water vapour per leaf area" ),
          variable.name = "Trait",
          value.name = "value")

write.csv(glop, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/GLOPNET-GlobalPlantTraitNetworkDatabase.csv', row.names=FALSE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[31]
#GrasslandPlantTraitDatabase.csv

names(dat_31)
unique(dat_31$`LDMC max`)
temp31<-dat_31[,c(1:14,21:26,30:34,41:46,53:55)]

gpt<-melt(temp31,
             measure.vars=c("std_LDMC max","std_LDMC min","std_Leaf area: in case of compound leaves leaf; petiole and rhachis included","std_Leaf area: in case of compund leaves leaf; petiole included (1)","std_Leaf area: in case of compund leaves leaf; petiole included (2)","std_Leaf dry matter content per leaf water-saturated mass (LDMC)","std_SLA: petiole  included","std_SLA: petiole included (1)","std_SLA: petiole included (2)"),
             variable.name = "Trait",
             value.name = "value")

write.csv(gpt, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/GrasslandPlantTraitDatabase.csv', row.names=FALSE)

#only one vaue for each trait!









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
  (out<-dcast(out, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+UnitName~DataName, value.var = "StdValue", na.rm=TRUE))
  assign(paste("dat",i,sep="_"), out)
}

#######################################################
file_list[1]
#[1] "LeafStructureandChemistry.csv"

names(dat_1)
dim(dat_1)
temp1<-dat_1[,c(1:7,9:30)]
head(temp1)

struc<-melt(temp1, 
            # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
            #           ),
            measure.vars=c("Leaf carbon content per dry mass","Plant height observed","Stem diameter at base (basal diameter)"),
            variable.name = "Trait",
            value.name = "value")

#get the LMDC and SLA values
dat1_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/LeafStructureandChemistry.csv")
out<-dcast(dat1_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE); names(out); head(out)
out<-out[,c(1:9)]
head(out)
#Change the name of RelUnvertaintyPercent to what it is, unitname
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"
colnames(out)[colnames(out)=="258"] <- "LDMC_includingpetiole"
colnames(out)[colnames(out)=="2645"] <- "LDMC_excludingpetiole"

struc1<-melt(out, 
            # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
            #           ),
            measure.vars=c("LDMC_includingpetiole","LDMC_excludingpetiole"),
            variable.name = "Trait",
            value.name = "value")

head(struc)
struc<-melt(temp1, 
          # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
          #           ),
          measure.vars=c("Leaf carbon content per dry mass","Plant height observed","Stem diameter at base (basal diameter)","LDMC_includingpetiole","LDMC_excludingpetiole"),
          variable.name = "Trait",
          value.name = "value")


write.csv(struc, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/LeafStructureandChemistry_1.csv', row.names=FALSE)

write.csv(struc1, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/LeafStructureandChemistry_2.csv', row.names=FALSE)
#######################################################
file_list[2]
#[2] Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv

names(dat_2)
temp2<-dat_2[,c(1:7,8:18)]
head(temp2)

dat2_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv")
out<-dcast(dat2_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE); head(out)
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"
out<-out[,c(1:7,19)]
head(out)

temp18<-merge(temp2, out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp18)
names(temp18)

struc<-melt(temp18, 
            # id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID","Latitude","Longitude"#,"Exposition"
            #           ),
            measure.vars=c("Stem diameter at breast height (1.3 m"),
            variable.name = "Trait",
            value.name = "value")
head(struc)
unique(struc$mat)
write.csv(struc, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/Leaftraitsdata(SLA)for56woodyspeciesattheSmithsonianConservationBiologyInstitute-Forest.csv', row.names=FALSE)
#######################################################
file_list[3]
#MARGINS-leaftraitsdatabase.csv

head(dat_3)
names(dat_3)
temp3<-dat_3[,c(1:7,9:39)]
head(temp3)

#get the LMDC and SLA values
dat3_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/MARGINS-leaftraitsdatabase.csv")
out<-dcast(dat3_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
out<-out[,c(1:8)]
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"
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

write.csv(margins, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/MARGINS-leaftraitsdatabase.csv', row.names=FALSE)
#######################################################
file_list[4]
#PlantPhysiologyDatabase.csv

names(dat_4)
head(dat_4) 
temp4<-dat_4[,c(1:7,9:15,17:26)]
head(temp4)

#get the LMDC and SLA values
dat4_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantPhysiologyDatabase.csv")
out<-dcast(dat4_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
out<-out[,c(1:8,16)]
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"
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

write.csv(pphysio, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/PlantPhysiologyDatabase.csv', row.names=FALSE)

unique(temp20$`Plant developmental status / plant age / maturity / plant life stage`)
# JUST seedlings

#######################################################
file_list[5]
# "PLANTSdataUSDA.csv"
head(dat_5)
temp5<-dat_5[,c(1:7,8)]
head(temp5)

#mature tree height values are under the unit colmn and default is in feet not meters
dat5_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PLANTSdataUSDA.csv")
out<-dcast(dat5_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+UnitName~DataName, value.var = "StdValue", na.rm=TRUE)
out<-out[,8]
colnames(out)[colnames(out)=="UnitName"]<-"Plant height vegetative"
head(out)
dim(out2)

dat5_3<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PLANTSdataUSDA.csv")
out2<-dcast(dat5_3, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
out2<-out2[,c(1:7,9)]
colnames(out2)[colnames(out2)=="RelUncertaintyPercent"] <-"UnitName"
head(out2)


out_c<-cbind(out2,out);head(out_c)
colnames(out_c)[colnames(out_c)=="out"] <-"Height at 20 Years"

usda<-melt(out_c, 
 #             id.vars=c("LastName","FirstName","DatasetID","Dataset","SpeciesName","ObservationID"),
              measure.vars=c("Height at 20 Years","Plant height vegetative"),
              variable.name = "Trait",
              value.name = "value")
head(usda)
unique(usda$Trait)
write.csv(usda, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/PLANTSdataUSDA.csv', row.names=FALSE)

#######################################################
file_list[6]
# PlantTraitsforPinusandJuniperusForestsinArizona.csv
names(dat_6)
#both LDMC and DBH are in the OrigUnitStr
temp6<-dat_6[,c(1:7,9:17,19)]
head(temp6)

dat6_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsforPinusandJuniperusForestsinArizona.csv")
out<-dcast(dat6_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
head(out)
out<-out[,c(1:8,18)]
colnames(out)[colnames(out)=="258"] <- "LDMC"
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"

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

write.csv(arzn, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/PlantTraitsforPinusandJuniperusForestsinArizona.csv', row.names=FALSE)

#######################################################
file_list[7]
#PlantTraitsfromCirceoNationalPark,Italy.csv
names(dat_7)
temp23<-dat_7
head(temp7)
#this one actually looks fine, not sure why I thought it should be in this folder
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
head(italy)
write.csv(italy, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/PlantTraitsfromCirceoNationalPark,Italy.csv', row.names=FALSE)

#######################################################
file_list[8]
#PlantTraitsfromCirceoNationalPark.csv
names(dat_8)

#Trait values are in the UnitNam col; none in stdvalues
dat8_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsfromCirceoNationalPark.csv")
temp24<-dcast(dat8_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~OriglName, value.var = "UnitName", na.rm=TRUE)
names(temp24)
#temp24<-out[,c(1:9,10,11)]
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
            measure.vars=c("Plant height vegetative"),
            variable.name = "Trait",
            value.name = "value")
#colnames(circeo)[colnames(circeo)=="exposition"] <- "Exposition"

#All mature in natural envrt
write.csv(circeo, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/PlantTraitsfromCirceoNationalPark.csv', row.names=FALSE)

#######################################################
file_list[9]
#PlantTraitsFromSpanishMediteraneanshrublands.csv
names(dat_9)

#LDMC data 
temp9<-dat_9[,c(1:7,9:16)]
head(temp9)

dat9_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsFromSpanishMediteraneanshrublands.csv")
out<-dcast(dat9_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:8)]
colnames(out)[colnames(out)=="258"] <- "LDMC"
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"

dim(temp9)
dim(out)
temp25<-cbind(temp9,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
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
write.csv(medit, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/PlantTraitsFromSpanishMediteraneanshrublands.csv', row.names=FALSE)

#######################################################

# PlantTraitsofCanadianForests.csv
# 
# # #Height is in cm, so converting to m
dat_10<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/PlantTraitsofCanadianForests.csv")
dat_10<-dcast(dat_10, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+UnitName~DataName, value.var = "StdValue", na.rm=TRUE)

#Height is in cm, so converting to m
#heights are in cm so use OrigValueStr
names(dat_10)
temp26<-dat_10#[,c(1:7,8,9,11,13,20:22)]
names(temp26)
head(temp26)
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
write.csv(canad, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/PlantTraitsofCanadianForests.csv', row.names=FALSE)

# head(dat_10)
# dat_10<-dcast(dat_10, LastName+FirstName+DatasetID+Dataset+AccSpeciesID+ObsDataID~OriglName, value.var = "StdValue", na.rm=TRUE)
# names(dat_10)
# 
# temp10<-dat_10[,c(1:7,11)]
# head(temp10)


#SHOULD BE Canadian forests dataset

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[11]
#RollinsonDBH.csv
names(dat_11)

temp11<-dat_11[,c(1:7,8:13)]
head(temp11)

#Stem diameter is in the OrigUnitStr col
dat11_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/RollinsonDBH.csv")
out<-dcast(dat11_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
out<-out[,c(1:7,14)]
head(out)
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"

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

write.csv(roll, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/RollinsonDBH.csv', row.names=FALSE)

#unique(temp27$`Exposition`)
#All natural

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[12]
#Sheffield&SpainWoodyDatabase.csv
names(dat_12)
head(dat_12)
temp12<-dat_12[,c(1:7,9:20)]
head(temp12)
names(temp12)

dat12_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/Sheffield&SpainWoodyDatabase.csv")
out<-dcast(dat12_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
head(out)
out<-out[,c(1:8)]
colnames(out)[colnames(out)=="258"] <- "LDMC"
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"

temp28<-merge(temp12,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset"))
head(temp28)

sheffwood<-melt(temp28, 
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

write.csv(sheffwood, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/Sheffield&SpainWoodyDatabase.csv', row.names=FALSE)

#not sure why I put this one in the inconsistent folder

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[13]
#SheffieldDatabase.csv
names(dat_13)
head(dat_13)
temp13<-dat_13[,c(1:7,10:42)]
head(temp13)

#LDMC is under OrigUnitStr
dat13_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/SheffieldDatabase.csv")
out<-dcast(dat13_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
names(out)
head(out)
out<-out[,c(1:8)]
head(out)
colnames(out)[colnames(out)=="258"] <- "LDMC"


#Leaf water content is under ValueKindName
out2<-dcast(dat13_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "RelUncertaintyPercent", na.rm=TRUE)
names(out2); head(out2)
out2<-out2[,c(1:7,8)]
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

write.csv(sheff, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/SheffieldDatabase.csv', row.names=FALSE)

head(temp28)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[14]
#StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv

names(dat_14)
head(dat_14)
temp14<-dat_14[,c(1:7,9:18)]
head(temp14)

#LDMC is under OrigUnitStr
dat14_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv")
out<-dcast(dat14_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:8)]
colnames(out)[colnames(out)=="258"] <- "LDMC"
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"

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
write.csv(fin, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv', row.names=FALSE)

unique(temp29$`Plant developmental status / plant age / maturity / plant life stage`)
#unique(temp29$`Exposition`)
#BOTH juvenile and mature individuals; all in natural environments 

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[15]
#TheChinaPlantTraitDatabase.csv

names(dat_15)
head(dat_15)
temp15<-dat_15[,c(1:7,9:74)]
head(temp15)

#LDMC is under OrigUnitStr
dat15_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TheChinaPlantTraitDatabase.csv")
out<-dcast(dat15_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:8)]
colnames(out)[colnames(out)=="258"] <- "LDMC"
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"

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
write.csv(china, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/TheChinaPlantTraitDatabase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[16]
#TheGlobalLeafTraits.csv

names(dat_16)
temp16<-dat_16[,c(1:7,10:92)]
head(temp16)

#LDMC is under OrigUnitStr
dat16_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TheGlobalLeafTraits.csv")
out<-dcast(dat16_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
names(out)
out<-out[,c(1:8)]
colnames(out)[colnames(out)=="258"] <- "LDMC"
colnames(out)[colnames(out)=="RelUncertaintyPercent"] <-"UnitName"

#Leaf water content is under ValueKindName
out2<-dcast(dat16_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "RelUncertaintyPercent", na.rm=TRUE)
names(out2); head(out2)
out2<-out2[,c(1:7,8)]
colnames(out2)[colnames(out2)=="54"] <- "LWC"
unique(out2$LWC)

out_c<-merge(out2,out, by=c("LastName","FirstName","SpeciesName","ObservationID","DatasetID","Dataset")); head(out2)
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

write.csv(glbl, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/TheGlobalLeafTraits.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[17]
#TheLEDATraitbase.csv
names(dat_17)
head(dat_17)
temp17<-dat_17[,c(1:7,10,13,16:18,23:25,27)]
head(temp17)

dat17_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TheLEDATraitbase.csv")
out<-dcast(dat17_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
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

write.csv(leda, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/TheLEDATraitbase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[18]
#TraitsfromSubarcticPlantSpeciesDatabase.csv
names(dat_18)
head(dat_18)
temp18<-dat_18[,c(1:7,9,10:13,15)]
head(temp18)

dat18_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/TraitsfromSubarcticPlantSpeciesDatabase.csv")
out<-dcast(dat18_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
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

write.csv(subartic, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/TraitsfromSubarcticPlantSpeciesDatabase.csv', row.names=FALSE)

# #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
file_list[19]
#WholePlantHydraulicConductance.csv
names(dat_19)
head(dat_19)
temp19<-dat_19[,c(1:7)]
head(temp19)

dat19_2<-read.csv("~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/inconsistentformat/WholePlantHydraulicConductance.csv")
out<-dcast(dat19_2, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID+RelUncertaintyPercent~DataName, value.var = "UnitName", na.rm=TRUE)
names(out); head(out)
out<-out[,c(1:7,9)]

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


write.csv(whydr, '~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/WholePlantHydraulicConductance.csv', row.names=FALSE)

#Now merge all files into one:
myfiles = list.files(path="~/Desktop/ospree_trait_analysis/cleaned_compressed_nov/", pattern="*.csv", full.names=TRUE)
myfiles
dat_clean = ldply(myfiles, read_csv)
dat_clean

head(dat_clean)
length(unique(dat_clean$Dataset))

write.csv(dat_clean, '~/Documents/github/ospree/analyses/traits/input/try_cleaning_dl/try_cleanlong_dl_origvalue.csv', row.names=FALSE)
