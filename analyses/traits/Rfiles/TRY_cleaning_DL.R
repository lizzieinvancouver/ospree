rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse) 
require(reshape2)
# Select georeferenced Datasets -------------------------------------------
#setwd("~/Desktop/trait_analysis") # Working directory 
#data <- read.csv("input/Try_data_Dec2018.csv")
#data <- read.csv("input/TRY_data_Mar2019.csv")
# str(data)
# head(data)
# names(data)
# #To help simlify what we are looking at,  I am removing the Reference and Comment col
# data<-data[,1:25]
# 
# #First identify how many unique datasets there are:
# 
# length(unique(data$Dataset))
# March data has 66 unique datasets
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
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
#Data now sep into indiv csv files 
setwd("~/Documents/github/ospree/analyses/traits/Cleaning_TRY")
# Cleaning Xylem fucntional trait study to test whether this works
xylem36<-read.csv("To_clean/XylemFunctionalTraits(XFT)Database.csv")

str(xylem36)
names(xylem36)
xylem<-xylem36[,c(1:5,8:9,11,13,15,16)]

names(xylem)

#Now, using reshape2, convert it from long to wide formatted data

xylem_wide<-dcast(xylem, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
head(xylem_wide)
names(xylem_wide)

#There are 97 variables, many of which are weather eg rel humidity for ea month, sum of precip for ea month
#Subsetting just the trait/site data
tmp1<-xylem_wide[,c(1:6, 26,32,34:35, 63:64,66,80,93,97)]
str(tmp1)
names(tmp1)

#write.csv(tmp1, file="xylemFunctionalTraits.csv", row.names = TRUE)
#This seems to have worked

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
#Data now sep. into indiv csv files and added to OPREE
setwd("~/Documents/github/ospree/analyses/traits/Rfiles")
# Cleaning Tundra fucntional trait study to test whether this workstu
tundra35<-read.csv("Cleaning/TundraPlantTraitsDatabase.csv")

str(tundra35)
tundra<-tundra35[,c(1:5,8:9,11,13,15,16)]

head(tundra)

#Now, using reshape2, convert it from long to wide formatted data

tundra_wide<-dcast(tundra, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
names(tundra_wide)
##there are 10 variables, 
tmp2<-tundra_wide[,c(1:5,7:8)]
head(tmp2)

#write.csv(tmp2, file="tundraplanttrait.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#
# Cleaning 
d34<-read.csv("sep_data/TraitsfromSubarcticPlantSpeciesDatabase.csv")

str(d34)
tmp34<-d34[,c(1:5,8:9,11,13,15,16)]

head(tmp34)

#Now, using reshape2, convert it from long to wide formatted data

tmp34_wide<-dcast(tmp34, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp34_wide)
names(tmp34_wide)
#There are 18 variaples, some about soil and phylogeny

tmp34_sub<-tmp34_wide[,c(1:11, 13:14)]
head(tmp34_sub)
write.csv(tmp34_wide, file="TraitsSubarticPlant.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d33<-read.csv("sep_data/TheNetherlandsPlantTraitsDatabase.csv")
#str(d33)
tmp33<-d33[,c(1:5,8:9,11,13,15,16)]

head(tmp33)

#Now, using reshape2, convert it from long to wide formatted data

tmp33_wide<-dcast(tmp33, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp33_wide)
names(tmp33_wide)
#There are 22 variaples, some about soil conditions

tmp33_sub<-tmp33_wide[,c(1:10,22)]
head(tmp33_sub)

write.csv(tmp33_wide, file="NetherlandsPlantTraits.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d32<-read.csv("sep_data/TheLEDATraitbase.csv")
str(d32)
tmp32<-d32[,c(1:5,8:9,11,13,15,16)]

head(tmp32)

#Now, using reshape2, convert it from long to wide formatted data

tmp32_wide<-dcast(tmp32, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp32_wide)
names(tmp32_wide)
#There are 23 variaples, I am going to remove the comments and notes

tmp32_sub<-tmp32_wide[,c(1:13)]
head(tmp32_sub)
names(tmp32_sub)

write.csv(tmp32_wide, file="LEDATraits.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d31<-read.csv("sep_data/TheFunctionalEcologyofTrees(FET)Database-Jena.csv")
str(d31)
tmp31<-d31[,c(1:5,8:9,11,13,15,16)]

head(tmp31)

#Now, using reshape2, convert it from long to wide formatted data

tmp31_wide<-dcast(tmp31, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp31_wide)
names(tmp31_wide)
#There are 8 variaples, I am not going to remove any

tmp31_sub<-tmp31_wide

write.csv(tmp31_wide, file="Functionalecologyoftrees.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d30<-read.csv("sep_data/StructuralandbiochemicalleaftraitsofborealtreespeciesinFinland.csv")
str(d30)
tmp30<-d30[,c(1:5,8:9,11,13,15,16)]

str(tmp30)

#Now, using reshape2, convert it from long to wide formatted data

tmp30_wide<-dcast(tmp30, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp30_wide)
names(tmp30_wide)
#There are 16 variaples, I am not going to remove the canopy position, we might want to select for just sun leaves

tmp30_sub<-tmp30_wide

write.csv(tmp30_wide, file="Structural&biochemicalleaftraits.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d29<-read.csv("sep_data/SheffieldDatabase.csv")
str(d29)
tmp29<-d29[,c(1:5,8:9,11,13,15,16)]

head(tmp29)

#Now, using reshape2, convert it from long to wide formatted data

tmp29_wide<-dcast(tmp29, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp29_wide)
names(tmp29_wide)
#There are 35 variaples, I removing the litter and treatment specific col

tmp29_sub<-tmp29_wide[,c(1:7,13:21, 29,34:35)]
head(tmp29_sub)

write.csv(tmp29_wide, file="sheffield.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d28<-read.csv("sep_data/Sheffield&SpainWoodyDatabase.csv")
str(d28)
tmp28<-d28[,c(1:5,8:9,11,13,15,16)]

head(tmp28)

#Now, using reshape2, convert it from long to wide formatted data

tmp28_wide<-dcast(tmp28, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp28_wide)
names(tmp28_wide)
#There are 18 variaples, I am going to leave the treatment columns in case we want to subset for one ie control/ambient

tmp28_sub<-tmp28_wide
head(tmp28_sub)
names(tmp28_sub)

write.csv(tmp28_wide, file="sheffield&spain.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d27<-read.csv("sep_data/MARGINS-leaftraitsdatabase.csv")
str(d27)
tmp27<-d27[,c(1:5,8:9,11,13,15,16)]

head(tmp27)

#Now, using reshape2, convert it from long to wide formatted data

tmp27_wide<-dcast(tmp27, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp27_wide)
names(tmp27_wide)
#There are 34 variaples, I am going to remove site and soil columns

tmp27_sub<-tmp27_wide[,c(1:7,10:12,14:19)]
head(tmp27_sub)
names(tmp27_sub)

write.csv(tmp27_wide, file="margins_leaftrait.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d26<-read.csv("sep_data/LeafTraitsinItalianCentralApenninesBeechForests.csv")
str(d26)
tmp26<-d26[,c(1:5,8:9,11,13,15,16)]

head(tmp26)

#Now, using reshape2, convert it from long to wide formatted data

tmp26_wide<-dcast(tmp26, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp26_wide)
names(tmp26_wide)
#There are 15 variaples, I am going to remove the bedrock column

tmp26_sub<-tmp26_wide[,c(1:7,9:15)]
head(tmp26_sub)
names(tmp26_sub)

write.csv(tmp26_wide, file="ItalianCentralApennienesbeechforest.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d25<-read.csv("sep_data/LeafTraitsinCentralApenninesBeechForests.csv")
str(d25)
tmp25<-d25[,c(1:5,8:9,11,13,15,16)]

head(tmp25)

#Now, using reshape2, convert it from long to wide formatted data

tmp25_wide<-dcast(tmp25, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp25_wide)
names(tmp25_wide)
#There are 15 variaples, I am going to remove site and soil columns

tmp25_sub<-tmp25_wide[,c(1:7,9:15)]
head(tmp25_sub)
names(tmp25_sub)

write.csv(tmp25_wide, file="CentralApennienesbeechforest.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

# Cleaning 
d24<-read.csv("sep_data/LeafStructureandChemistry.csv")
str(d24)
tmp24<-d24[,c(1:5,8:9,11,13,15,16)]

head(tmp24)

#Now, using reshape2, convert it from long to wide formatted data

tmp24_wide<-dcast(tmp24, LastName+FirstName+DatasetID+Dataset+SpeciesName+ObservationID~DataName, value.var = "OrigValueStr", na.rm=TRUE)
str(tmp24_wide)
names(tmp24_wide)
#There are 24 variaples, but all seem pretty relevant 

tmp24_sub<-tmp24_wide
head(tmp24_sub)
names(tmp24_sub)

write.csv(tmp24_wide, file="LeafStructureandChemistry.csv", row.names = TRUE)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>#

rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

test<-rbind.all.columns(tmp24_sub,tmp25_sub)
test2<-rbind.all.columns(tmp26_sub,tmp27_sub)
test3<-rbind.all.columns(tmp28_sub,tmp29_sub)
test4<-rbind.all.columns(tmp30_sub,tmp31_sub)
test5<-rbind.all.columns(tmp32_sub,tmp33_sub)
test6<-rbind.all.columns(tmp34_sub,tundra_sub)
test7<-rbind.all.columns(test6,xylem_sub)

test8<-rbind.all.columns(test7,test5)
test9<-rbind.all.columns(test8,test4)
test10<-rbind.all.columns(test9,test3)
test11<-rbind.all.columns(test10,test2)
final<-rbind.all.columns(test11,test)

str(final) #73 variables

names(final)

length(unique(final$DatasetID)) #13 unique datasets combined into one


