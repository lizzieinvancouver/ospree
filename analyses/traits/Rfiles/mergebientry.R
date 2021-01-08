# The aim of this code is to clean up the try and bien data and combine it into one useable file! There are many steps that need to be completed for this to happen:

#1. Fix typos try species names
#2. Remove columns in try that we don't need right now, there is tons of weather data and factors regarding site conditions
#3. Remove data from experiments where they manipulated factors that might change trails: fert additions, temp manip, chamber studies
#4. Check that the BIEN data is standardized in its trait units, lat/long
#5. Remove any dup data
#6. Subset to only deciduous species
#7. Subset to spp. with sufficient data, focus on leaf economic traits and SSD (one of the only wood economic traits)

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(stringr)
library(plyr)
library(dplyr)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis/")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

#Since the try data is still being cleaned, we are moving forward with the cleaning and plotting/preliminary testing of our hypotheses with the BIEN data
#biendat<-read.csv("input/newspp_BIEN_traitdata_Nov11.csv", header=TRUE)

# head(biendat)
# length(unique(biendat$scrubbed_species_binomial))
#94 species represented in some form

#source("Rfiles/cleaning_try/TRY_cleaning_master.R")
trydat<-read.csv("input/try_subsptraits.csv",head=TRUE)
#trydat<-read.csv("input/TryDataCleaned22012020.csv", header=TRUE)

#source("Rfiles/cleaning_bien/BIEN_cleaning.R")
biendat<-read.csv("input/bien_cleaned_Nov2020.csv")

length(unique(biendat$new.SpeciesName)) #74
length(unique(trydat$new.SpeciesName)) #94


###########################################################
#Changing names in BIEN to better match those in try
colnames(biendat)[colnames(biendat)=="trait_name"] <-"TraitName"
colnames(biendat)[colnames(biendat)=="trait_value"] <-"TraitValue" # Are the bien trait values standardized, something to check!! 
colnames(biendat)[colnames(biendat)=="unit"] <- "UnitName"
colnames(biendat)[colnames(biendat)=="longitude"] <- "Longitude"
colnames(biendat)[colnames(biendat)=="latitude"] <- "Latitude"

colnames(trydat)[colnames(trydat)=="StdValue"] <-"TraitValue"

# adding a new column witht the database the data is from
biendat$database<-"bien"
trydat$database<-"try"

#issue with the biend longtitude
trydat$Longitdue<-as.numeric(as.character(trydat$Longitude))

###########################################################
#bien has sla as and ldmc as
unique(biendat$TraitName)
unique(trydat$TraitName)

biendat$TraitName[which(biendat$TraitName == "leaf area per leaf dry mass")] <- "Specific_leaf_area"
biendat$TraitName[which(biendat$TraitName == "leaf dry mass per leaf fresh mass")] <- "Leaf_dry_matter_content"
biendat$TraitName[which(biendat$TraitName == "maximum whole plant height")] <- "Plant_height_vegetative"
biendat$TraitName[which(biendat$TraitName == "whole plant height")] <- "Plant_height_vegetative"
biendat$TraitName[which(biendat$TraitName == "stem wood density")] <- "Stem_specific_density"

##########################################################################################

# Bien has a number of experiments, data from satellites, databases that we have decided to remvoe
#the one excpetion we are making is for seed data, since Kew is the usual souce, we will keep in that data for now

# Remvoing the following studies:
sort(unique(biendat$project_pi))
notfield <- c("Aakala T", #common garden
              #"Ameztegui A", #datamined
              #"Charles Price", #GLOPNET database
              "Dalponte M", #not direct measure, calc from drones
              #"Lopez-Gonzalez G", # repo of wood economic traits
              "Maire V", #altered soil
              #"Michael Kleyer", #LEDA database
              # "Price CA", # GLOPNET database
              "Zanne AE" #decaying plant matter
) 

biendatfield <- biendat[!biendat$project_pi %in% notfield,]

##########################################################################################
###########################################################
# Removing extaneous columns that are not needed
biendatfield.sub<-biendatfield[,c("SpeciesName","TraitName","TraitValue","UnitName","Latitude","Longitude","project_pi","genus","species","new.SpeciesName", "database")]

trydat.sub<-trydat[,c("SpeciesName","TraitName","TraitValue","UnitName","Latitude","Longitude","project_pi","genus","species","new.SpeciesName", "database", "DatasetID","Reference","Reference...source")]


# Merge the beind and try trait data

trybien<-rbind.fill(trydat.sub, biendatfield.sub)
names(trybien)

write.csv(trybien,"input/try_bien.csv", row.names=FALSE)

require(corrplot)
require(tidyr)
require(reshape2)

trait_wide<-spread(trybien, TraitName, TraitValue)


# ##########################################################################################
# # Calculating the average trait value for the try dat
# unique(trybien$UnitName)
# unique(trybien$TraitName)
# unique(trybien$new.SpeciesName)
# 
# 
# # trtmean<-trybien %>% 
# #   group_by(new.SpeciesName,TraitName) %>% 
# #   summarize(trait.mean=mean(TraitValue,na.rm=TRUE),)
# # unique(trtmean$new.SpeciesName)
# # unique(ospree.deci$new.SpeciesName)
# 
# # to deal with outliers, looking at the median values
# trtmedian <- aggregate(TraitValue ~ new.SpeciesName * TraitName,
#                        data = trybien,
#                        FUN = function(X) { median(X, na.rm = TRUE)})
# ##################################################################################
# 
# # fin<-merge(trtmean,ospree.deci, by="new.SpeciesName")
# 
# fin<-merge(trtmedian,ospree.deci, by="new.SpeciesName")
# head(fin)
# fin<-fin[,c("new.SpeciesName", "TraitName","TraitValue","Coefficient","mean")]
# # now that we have removed the databases, we lost a lot of leaf lifespan data, cn, photosynthesis data
# 
# cn<-subset(fin, TraitName=="leaf carbon content per leaf nitrogen content"); unique(cn$new.SpeciesName) #only one! 
# nit<-subset(fin, TraitName=="Leaf_nitrogen_.N._content_per_leaf_dry_mass"); unique(nit$new.SpeciesName) #53 species
# carb<-subset(fin, TraitName=="Leaf_carbon_.C._content_per_leaf_dry_mass"); unique(carb$new.SpeciesName) #36 species
# life<-subset(fin, TraitName=="leaf life span"); unique(life$new.SpeciesName) #only one! 
# stem<-subset(fin, TraitName=="Leaf_photosynthesis_rate_per_leaf_area"); unique(stem$new.SpeciesName)
# 
# # This is code written by Geoff to look into complete cases across traits
# dat<-fin
# species <- unique(fin$new.SpeciesName)
# traits <- c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter","Leaf_carbon_.C._content_per_leaf_dry_mass","seed mass")
# 
# coefficients <- c("b_force", "b_chill", "b_photo")
# 
# mat <- matrix(NA, ncol = length(traits)+1 + length(coefficients), nrow = length(species))
# 
# for(i in 1:length(species)){
#   temp <- subset(fin, new.SpeciesName == species[i])
#   for(j in 1:length(traits)){
#     mat[i, j] <- subset(temp, TraitName == traits[j])$trait.median[1]
#   }
#   for(k in 1:length(coefficients)){
#     mat[i, (length(traits) + k)] <- subset(temp, Coefficient == coefficients[k])$mean[1]
#   }
# }
# 
# colnames(mat) <- c("Height", "SLA",
#                    "N", "SSD", "LDMC", "Stem","C","seedmass", "force", "chill", "photo")
# mat
# 
# mat8trt <- mat[complete.cases(mat), ]# with 8 traits, we have 16 species
# 
# mat7 <- mat[, c(1:5,7:10)] #if we get rid of stem diameter
# mat7trt <- mat7[complete.cases(mat7), ]# with 7 traits, we have 23 species
# 
# mat6 <- mat[, c(1:5,7,9:10)] #if we get rid of stem diameter & seed mass
# mat6trt <- mat6[complete.cases(mat6), ]# with 6 traits, we have 26 species
# 
# ##################################################################################
# 
# unique(fin$new.SpeciesName) #70
# unique(fin$TraitName) #11 traits
# 
# head(fin)
# unique(fin$TraitName)
# 
# ##################################################################################
# 
# #create the new files
# write.csv(fin, "input/try_bien_ospree.csv", row.names=FALSE)
# write.csv(trybien,"input/try_bien.csv", row.names=FALSE)
# write.csv(mat7trt,"input/matrix.data.cc.csv", row.names=FALSE)
# 
# ##################################################################################
