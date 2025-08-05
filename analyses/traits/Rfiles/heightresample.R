## Started 30 January 2021 ##
## Modified by Deirdre 14 February 2021## 
## The bien datasets includes a large amount of height data, with a particularly large dataset submitted by Greg Reams.
## To make the height data more managable, we are subsampling the data to limit the number of height measurements per species to 5000

#housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

require(dplyr)
library(stringr)
library(plyr)

# Set working directory:
# Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if
(length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits")
}

# Get the data
dat1 <- read.csv("input/try_bien_nodups_1.csv")
dat2 <- read.csv("input/try_bien_nodups_2.csv")

 dat <- rbind(dat1, dat2)
# names(dat)
# dat <- dat[,c("SpeciesName", "TraitName", "TraitValue", "UnitName", "Latitude",
#     "Longitude", "project_pi", "database", "DatasetID")]
# names(dat) <- c("latbi", "traitname", "traitvalue", "unit", "lat", "long", "project_pi",
#     "database", "DatasetID")

# Percentage of data that is height?
#nrow(heighter)/nrow(dat)
 traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus","Acer_saccharum","Aesculus_hippocastanum","Alnus_glutinosa","Alnus_incana","Betula_papyrifera","Betula_pendula","Betula_populifolia","Betula_pubescens","Corylus_avellana","Fagus_grandifolia","Fagus_sylvatica","Fraxinus_excelsior","Fraxinus_nigra","Hamamelis_virginiana","Juglans_cinerea","Juglans_regia","Populus_grandidentata","Populus_tremula","Prunus_avium","Prunus_padus","Prunus_pensylvanica","Prunus_persica","Prunus_serotina","Quercus_alba","Quercus_coccifera","Quercus_ellipsoidalis","Quercus_ilex","Quercus_petraea","Quercus_robur","Quercus_rubra","Quercus_shumardii","Quercus_velutina","Rhamnus_cathartica","Sorbus_aucuparia","Ulmus_pumila")
 #
# # Subset data to traitors species list
 tdat <- subset(dat, dat$speciesname %in% traitors.sp)

 height <- subset(tdat, traitname == "Plant_height_vegetative")

# For this trait we are really only interested in adults - selecting only values greater than 1.42 m, the height at which you can measure DBH
small <- subset(height, traitvalue < 2) # this is 2.1% of the data 

adult <- subset(height, traitvalue > 2)

#other.trt <- subset(dat, traitname != "Plant_height_vegetative")

# Remove duplicated height values:
adult$dup <- duplicated(adult[, c("traitname", "traitvalue","unitname","latitude","longitude","piname","speciesname","database" )])

temp <- subset(adult, dup == "TRUE") # 3456
adultNoDup<- subset(adult, dup != "TRUE") #629094

## sampling height: start by separating out the species with few obs
a.lot.ht <- c("Quercus_ellipsoidalis", "Alnus_rubra", "Fraxinus_nigra", "Populus_grandidentata", "Betula_lenta", "Betula_alleghaniensis", "Betula_papyrifera", "Fagus_grandifolia", "Quercus_velutina", "Prunus_serotina", "Quercus_rubra", "Acer_saccharum", "Quercus_alba")

few <- adultNoDup[!adultNoDup$speciesname %in% a.lot.ht, ] 

alot <- adultNoDup[adultNoDup$speciesname %in% a.lot.ht, ] #769505

# now sampling for species with a lot of data
ht <- data.frame(speciesname = character(), height = numeric())
species <- unique(alot$speciesname)

for (sp in 1: length(species)){
  testsp <- subset(alot, speciesname == species[sp])
  mysample <- sample_n(testsp, 3000)
  #dfadd <- data.frame(speciesname = species[sp], traitvalue = mysample)
  # mysample$speciesname <- species[sp]
  ht <- rbind(ht, mysample)
}

heightData <- rbind(few, ht)

# head(ht.sample)
# hist(ht.sample$traitvalue)
# length(unique(ht.sample$speciesname))
