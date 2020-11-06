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

require(stringr)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

#Since the try data is still being cleaned, we are moving forward with the cleaning and plotting/preliminary testing of our hypotheses with the BIEN data
#biendat<-read.csv("input/newspp_BIEN_traitdata_Nov11.csv", header=TRUE)

# head(biendat)
# length(unique(biendat$scrubbed_species_binomial))
#94 species represented in some form

trydat<-read.csv("input/try_subsptraits.csv",head=TRUE)
#trydat<-read.csv("input/TryDataCleaned22012020.csv", header=TRUE)

ospree<-read.csv("input/traitors_bb_results_nocomplex.csv", header=TRUE)
ospree<-ospree[,c("Coefficient","Species","mean")]


length(unique(trydat$new.SpeciesName)) #62
length(unique(ospree$Species)) #234

##################################################################################
# Removing the gymnosperm from the ospree data 

breakname <- strsplit(as.character(ospree$Species), "_", fixed=TRUE)
ospree$genus <- unlist(lapply(breakname, function(x) x[1]))
ospree$species <- unlist(lapply(breakname, function(x) x[2]))

gymno<-c("Abies","Pinus","Picea","Pseudotsuga")

ospree.deci <- ospree[!ospree$genus %in% gymno,] # only want the deciduous species
ospree.deci$new.SpeciesName<-paste(ospree.deci$genus,ospree.deci$species, sep="_")
sort(unique(ospree.deci$new.SpeciesName))
length(unique(ospree.deci$new.SpeciesName)) # left with 222 species

##########################################################################################
# Calculating the average trait value for the try dat
unique(trydat$UnitName)
unique(trydat$Traits)
unique(trydat$new.SpeciesName)

trtmean<-trydat %>% 
  group_by(new.SpeciesName,Traits) %>% 
  summarize(trait.mean=mean(TraitValue_std,na.rm=TRUE),)

##################################################################################
fin<-merge(trtmean,ospree.deci, by="new.SpeciesName")
unique(fin$new.SpeciesName)

head(fin)

write.csv(fin, "try_ospreecoeff.csv")
#Changing names in BIEN to better match those in try
colnames(biendat)[colnames(biendat)=="scrubbed_species_binomial"] <- "SpeciesName"
colnames(biendat)[colnames(biendat)=="trait_name"] <-"Traits"
colnames(biendat)[colnames(biendat)=="trait_value"] <-"TraitValue" # Are the bien trait values standardized, something to check!! 
colnames(biendat)[colnames(biendat)=="unit"] <- "UnitName"
colnames(biendat)[colnames(biendat)=="longitude"] <- "Longitdue"
colnames(biendat)[colnames(biendat)=="latitude"] <- "Latitude"

##################################################################################
# But is the BIEN data standardized, ie lat long and trait units?
unique(biendat$unit)



# What about lat/long
unique(biendat$latitude)
unique(biendat$longitude)

unique(trydat$std_Latitude) 
unique(trydat$std_Longitude) 

# Are there any duplicates in the dataset

# To get the ball rolling on the analysis, we can start working with a still curated subset of the try data that does not include experiments and subsets species to those that have several functional traits

# Start by subsetting out studies that are growth chamber studies or experiments
trysub<-subset(trydat, Exposition == "Botanical garden"| Exposition == "Natural Vegetation"| Exposition == "natural vegetation, but not top canopy"| Exposition == "natural environment"| Exposition == "forest stand"| Exposition == "natural"| is.na(Exposition)) 



###########################################################################
#6. Remove conifer species - focus on just deciduous that have leaf trait data

# want to remove the Abies alba, Picea abies, all Pinus, Pseudotsuga menziesii

###########################################################################

#7. Subsetting the species to those that have the most trait data
# how many species remain in this subset dataset?
length(unique(trysub$SpeciesName)) 

# What traits?
unique(trysub$Traits)

library(dplyr)

table<- trysub1 %>%
  group_by(SpeciesName,Traits) %>%
  summarise(no_rows = length(Traits))
table

# At minimum, I think we want species with SLA, LDMC, LNC, LCC, height, stem.diameter/DBH
trysubtrait<-subset(trydat, Traits == "Leaf_nitrogen_.N._content_per_leaf_dry_mass"| Traits == "Leaf_nitrogen_.N._content_per_leaf_dry_mass" | Traits == "Specific_leaf_area"| Traits == "Plant_height_vegetative"| Traits == "Leaf_dry_matter_content" | Traits == "Stem_diameter" | Traits == "Stem_specific_density" | Traits == "Leaf_photosynthesis_rate_per_leaf_area") 

table<- trysubtrait %>%
  group_by(SpeciesName,Traits) %>%
  summarise(no_rows = length(Traits))
table

table<- trysubtrait %>%
  group_by(Traits, SpeciesName) %>%
  summarise(no_rows = length(Traits))
table

length(unique(trysubtrait$SpeciesName))
unique(trysubtrait$TraitValue_std)


