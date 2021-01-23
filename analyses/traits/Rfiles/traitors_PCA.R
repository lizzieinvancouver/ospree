# This code is adapted by Deirdre from Geoff's traitors_try_bien code on January 22, 2021

# Aim: to do two PCA on the trait data to look for the relationships between traits found in the LES and published literature. The first PCA will be of all the data, and the second will just be using the geometric mean.

## Load libraries

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(vegan)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/ospree_trait_analysis/")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

dat <- read.csv("input/try_bien_nodups.csv") 
names(dat)

dat<-dat[,c("new.SpeciesName","TraitName","TraitValue","UnitName","Latitude","Longitude","project_pi","database","DatasetID")]

## I started by trying to just alter Geoff's code (below) and while it worked well for the mean values, I am not sure how to get it to work with the raw data
species <- unique(dat$new.SpeciesName)
traits <- c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_photosynthesis_rate_per_leaf_area", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter")

mat <- matrix(NA, ncol = length(traits), nrow = length(species))

for(i in 1:length(species)){
    temp <- subset(dat, new.SpeciesName == species[i])
    for(j in 1:length(traits)){
        mat[i, j] <- subset(temp, TraitName == traits[j])$TraitValue[1]
    }
}

mat

colnames(mat) <- c("Height", "SLA", "Photosyn",
                   "N", "SSD", "LDMC", "StemD")
rownames(mat) <- species
mat

## Remove photosynthesis 
 mat2 <- mat[, c("Height", "SLA",
                 "N", "SSD", "LDMC", "StemD")]
 mat2 <- mat[complete.cases(mat2), ]

## Standardize (normalize) trait values
#mat2[, 1:6] <- apply(mat2[, 1:6], MARGIN = 2, FUN = function(X){ decostand(X, method = "standardize")})

head(dat)
# Attempt 2
require(reshape2)
temp<-reshape2::dcast(dat, new.SpeciesName + DatasetID ~TraitName , value.var= "TraitValue")

head(temp)

# Attempt 3 using tidyr
test<-spread(dat,TraitName,TraitValue)


## PCA plots 
ranges <- list(c(-.4, .4),
               c(-.2, .2),
               c(-.1, .1))
par(mfrow = c(1, 3), mar = c(5, 5, 2, 2), oma = c(0, 0, 0, 0))
for(i in 1:length(ranges)){
    biplot(prcomp(mat2), cex = c(.7, 1.25), col = "black")
}


# 
##############################################################################################
# Doing the PCA for the geometric mean:
# Start by calculating the average by speceis x trait
mtrt <- aggregate(dat["TraitValue"], d[c("new.SpeciesName","TraitName")], FUN=mean) 

# create a new matrix for feeding into the PCA
species <- unique(dat$new.SpeciesName)
traits <- c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_photosynthesis_rate_per_leaf_area", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter")

mat <- matrix(NA, ncol = length(traits), nrow = length(species))

for(i in 1:length(species)){
    temp <- subset(mtrt, new.SpeciesName == species[i])
    for(j in 1:length(traits)){
        mat[i, j] <- subset(temp, TraitName == traits[j])$TraitValue[1]
    }
}

mat
colnames(mat) <- c("Height", "SLA", "Photosyn",
                   "N", "SSD", "LDMC", "StemD")
    
#rownames(mat) <- species
mat

## Remove photosynthesis 
mat2 <- mat[, c("Height", "SLA","N", "SSD", "LDMC", "StemD")]
mat2 <- mat2[complete.cases(mat2), ]

## Standardize (normalize) trait values
#mat2[, 1:6] <- apply(mat2[, 1:6], MARGIN = 2, FUN = function(X){ decostand(X, method = "standardize")})

## PCA plots 
ranges <- list(c(-.4, .4),
               c(-.2, .2),
               c(-.1, .1))
for(i in 1:length(ranges)){
    biplot(prcomp(mat2), col = "black")
}
