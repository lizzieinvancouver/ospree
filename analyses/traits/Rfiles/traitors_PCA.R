# This code is adapted by Deirdre from Geoff's traitors_try_bien code on January 22, 2021

# Aim: to do two PCA on the trait data to look for the relationships between traits found in the LES and published literature. The first PCA will be of all the data, and the second will just be using the geometric mean.

## Load libraries

rm(list = ls()) 
options(stringsAsFactors = FALSE)

library(tidyr)
library(plyr)
library(dplyr)
library(vegan)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd()) > 0)) {  setwd("~/Documents/ospree_trait_analysis/")
} else if
(length(grep("Lizzie", getwd()) > 0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
} 

dat <- read.csv("input/try_bien_nodups.csv") 
names(dat)

dat <- dat[, c("new.SpeciesName", "TraitName", "TraitValue", "UnitName", "Latitude", "Longitude",
    "project_pi", "database", "DatasetID")]

## I started by trying to just alter Geoff's code (below) and while it worked well for the mean values, I am not sure how to get it to work with the raw data
# Does the code below actually take the geometric mean per species? It looks to me like it takes the first value?!
species <- unique(dat$new.SpeciesName)
traits <- c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_photosynthesis_rate_per_leaf_area", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter")

# looking at the number of observations per trait, we could imagine we would need 801485 rows for all the height data
nopertrait <- dat %>%
    group_by(TraitName) %>%
    summarise(no_rows = length(TraitName))

mat <- matrix(NA, ncol = length(traits), nrow = 801485) # the max number of rows of the matrix should be the nrows for the trait we have the most data for, height, which is 801485

for(i in 1:length(species)){
    temp <- subset(dat, new.SpeciesName == species[i])
    for(j in 1:length(traits)){
       mat[i,j] <- subset(temp, TraitName == traits[j])$TraitValue[i]
    }
}
mat

temp <- subset(dat, new.SpeciesName == species[1])
temp2 <- subset(temp, TraitName == traits[1])

colnames(mat) <- c("Height", "SLA", "Photosyn",
                   "N", "SSD", "LDMC", "StemD")
rownames(mat) <- species
mat

## Which traits to drop to keep more species?
for (whichcol in 1:ncol(mat)){
    print(colnames(mat)[whichcol])
    print(sum(is.na(mat[, whichcol])))
    }

## Remove photosynthesis ...and StemD?
 mat2 <- mat[, c("Height", "SLA",
                 "N", "SSD", "LDMC", "StemD")]
 mat2 <- mat[complete.cases(mat2), ]

## Standardize (normalize) trait values
#mat2[, 1:6] <- apply(mat2[, 1:6], MARGIN = 2, FUN = function(X){ decostand(X, method = "standardize")})

if(FALSE){
head(dat)
# Attempts by Lizzie ...
datsm <- subset(dat, select=c("new.SpeciesName", "TraitName", "TraitValue"))
tryreshape <- reshape(datsm, timevar="TraitName", idvar="new.SpeciesName", direction="wide")
require(reshape2)
trydcast <- dcast(dat, new.SpeciesName + Latitude + Longitude ~ TraitName, value.var = "TraitValue", drop = FALSE)
}
# Attempt 2
temp <- reshape2::dcast(dat, new.SpeciesName + DatasetID ~ TraitName , value.var= "TraitValue")

# Attempt 3 using tidyr
test <- spread(dat, key = TraitName, value = TraitValue)

## PCA plots 
ranges <- list(c(-.4, .4),
               c(-.2, .2),
               c(-.1, .1))
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), oma = c(0, 0, 0, 0))
for(i in 1:length(ranges)){
    biplot(prcomp(mat2), cex = c(.7, 1.25), col = "black")
}
##############################################################################################

# Doing the PCA for the geometric mean:
# Start by calculating the average by speceis x trait
mtrt.ddply <- ddply(dat, c("new.SpeciesName", "TraitName"),
                    summarise, mean = mean(TraitValue),
                    sd = sd(TraitValue),
                    sem = sd(TraitValue)/sqrt(length(TraitValue)),
                    geomean = exp(sum(log(TraitValue)/length(TraitValue))) )
head(mtrt.ddply)

#This is one of the mehtods mentioned on stack over.flow, https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in 
mtrt.stk <- exp(tapply(log(dat$TraitValue),
                       dat[c("new.SpeciesName","TraitName")], 
                       mean, na.rm=TRUE))
head(mtrt.stk)

#This is another of the methods mentioned on stack over.flow, this function is suppose to deal with missing values, but it results in NA for traits that only have one observation, which isn't really what we want.

# gm_mean = function(x, na.rm=TRUE){
#     exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
# }

species <- sort(unique(dat$new.SpeciesName))
traits <- c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_photosynthesis_rate_per_leaf_area",
            "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter")

gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x) / length(x)))
}

spsub <- subset(dat, new.SpeciesName == species[5]) 
sptrtsub <- subset(spsub, TraitName == traits[4]) 
gmmean <- gm_mean(sptrtsub$TraitValue); gmmean  

# this value is matches the above for several of the species and traits, confirming that the funciton gives the same values as those in mtrt.ddply and mtrt.stk

# DL did try to make a loop to calculate the mean with the funciton, but it doesn't quite work yet and is slower than the above methods
# tm <- aggregate(dat["TraitValue"], dat[c("new.SpeciesName","TraitName")], FUN=length) 
# 
# trtmeans <- vector()
# for(i in 1:length(species)){
#     spsub <- subset(dat, new.SpeciesName == species[i])
#     for(j in 1:length(traits)){
#         sptrtsub <- subset(spsub, TraitName == traits[j])
#         gmmean <- gm_mean(sptrtsub$TraitValue)
#         trtmeans <- rbind(trtmeans, gmmean)
#     }
# }
# tm$mean <- trtmeans[, 1]
# head(tm)

#########################################################################
# create a new matrix for feeding into the PCA
species <- sort(unique(dat$new.SpeciesName))
traits <- c("Plant_height_vegetative", "Specific_leaf_area","Leaf_nitrogen_.N._content_per_leaf_dry_mass", 
            "Stem_specific_density", "Leaf_dry_matter_content",
            "leaf life span","seed mass", "Leaf_carbon_.C._content_per_leaf_dry_mass")

mat <- matrix(NA, ncol = length(traits), nrow = length(species))

for(i in 1:length(species)){
    temp <- subset(mtrt.ddply, new.SpeciesName == species[i])
    for(j in 1:length(traits)){
        mat[i, j] <- subset(temp, TraitName == traits[j])$geomean[1]
    }
}
colnames(mat) <- c("Height", "SLA",  "N", "SSD", "LDMC", "llife", "seed", "C")
#rownames(mat) <- species
mat

## With the above 8 traits, we would only have 13 species represented, without leaf lifespan and C, we have 28 species
mat2 <- mat[, c("Height", "SLA",  "N", "SSD", "LDMC", "seed")]
mat2 <- mat2[complete.cases(mat2), ]
mat2
## Standardize (normalize) trait values
#mat2[, 1:6] <- apply(mat2[, 1:6], MARGIN = 2, FUN = function(X){ decostand(X, method = "standardize")})

## PCA plots 
ranges <- list(c(-.7, .7),
               c(-.4, .4),
               c(-.1, .1))
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), oma = c(0, 0, 0, 0))
for(i in 1:length(ranges)){
    biplot(prcomp(mat2), col = "black")
}
