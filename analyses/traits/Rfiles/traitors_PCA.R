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
names(dat)
dat <- dat[, c("speciesname", "traitname", "traitvalue", "unitname", "latitude", "longitude",
    "piname", "database", "datasetid")]

## starting with a PCA of the raw data, with sampling for species with a lot of height data

#starting by separating out the height data:
height <- subset(dat, traitname == "Plant_height_vegetative")

other.trt <- subset(dat, traitname != "Plant_height_vegetative")

## I started by trying to just alter Geoff's code (below) and while it worked well for the mean values, I am not sure how to get it to work with the raw data
# Does the code below actually take the geometric mean per species? It looks to me like it takes the first value?!
species <- unique(other.trt$speciesname)
traits <- c("Specific_leaf_area", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Leaf_carbon_.C._content_per_leaf_dry_mass", "seed mass")

# looking at the number of observations per trait, we could imagine we would need 801485 rows for all the height data

tm <- aggregate(other.trt["traitvalue"], other.trt[c("traitname")], FUN=length) 
# 9682 is the max rows needed for a given trait
mat <- matrix(NA, ncol = 7, nrow = 9682) # the max number of rows of the matrix should be the nrows for the trait we have the most data for, height, which is 801485

species <-species[1:3]
for(i in 1:length(species)){
    temp <- subset(other.trt, speciesname == species[i])
    for(j in 1:length(traits)){
        temp2 <- subset(temp, traitname == traits[j])
        for (k in 1:nrow(temp2)){
            mat[k,j] <- temp2$traitvalue[k]
        }
    }
}
# this is arranging the traits by columns, but it is compressing the matricies of different species together I can't figure out how to add species as the row names.

mat <- matrix(NA, ncol = 7, nrow = 9682) # the max number of rows of the matrix should be the nrows for the trait we have the most data for, height, which is 801485
species <-species[1:6]
for(i in 1:length(species)){
    temp <- subset(other.trt, speciesname == species[i])
    for(j in 1:length(traits)){
        temp2 <- subset(temp, traitname == traits[j])
        for (k in 1:nrow(temp2)){
            mat2[k,j] <- temp2$traitvalue[k]
            mat2[k,7] <- temp2$speciesname[k]
        }
    }
}

head(mat)
colnames(mat) <- c("SLA", "N", "SSD", "ldmc", "lcc", "seed", "speciesname")
rownames(mat) <- species
mat[, 2]

# test <- data.frame(speciesname = character, sla = numeric(),  lnc = numeric(), 
#                    sdd = numeric(), ldmc = numeric(), lcc = numeric(), seed = numeric())
# 
# for(i in 1:length(species)){
#     temp <- subset(other.trt, speciesname == species[1])
#     for(j in 1:length(traits)){
#         temp2 <- subset(temp, traitname == traits[1])
#         temp2 <- temp2[,c("speciesname", "traitvalue")]
#         names(temp2) <- c("speciesname", traits[1])
#     }
# }
# 
# dfadd <- data.frame(resample.n = i, mean = mean(mysample), sd = sd(mysample))
# 
# for( i in 1: length(species)){
#     temp <- subset(other.trt, speciesname == species[1])
#     for(j in 1:length(traits)){
#         temp2 <- subset(temp, traitname == traits[1])
#         temp3 <- merge(test, temp2, by == speciesname)
#     }
# }
# temp <- subset(dat, new.SpeciesName == species[1])
# temp2 <- subset(temp, TraitName == traits[1])
# 
# colnames(mat) <- c("Height", "SLA", "Photosyn",
#                    "N", "SSD", "LDMC", "StemD")
# rownames(mat) <- species
# mat

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
temp <- reshape2::dcast(other.trt, speciesname + no ~ traitname , value.var= "traitvalue")

# Attempt 3 using tidyr
other.trt$no <- 1:nrow(other.trt)
test <- spread(other.trt, key = traitname, value = traitvalue)

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
rownames(mat) <- species
mat

## With the above 8 traits, we would only have 13 species represented, without leaf lifespan and C, we have 28 species
mat2 <- mat[, c("Height", "SLA",  "N", "SSD", "LDMC", "seed")]
mat2 <- mat2[complete.cases(mat2), ]
mat2
## Standardize (normalize) trait values
#mat2[, 1:6] <- apply(mat2[, 1:6], MARGIN = 2, FUN = function(X){ decostand(X, method = "standardize")})

## PCA plots 

trtGeoPca <- prcomp(mat2, center = T, scale. = T)
summary(trtGeoPca)

library(ggbiplot)

ggbiplot(trtGeoPca, labels = rownames(mat2))

## This method also works, but with fewer steps and without needing to use a matrix, which gives an error message
mtrt.stk<-as.data.frame(mtrt.stk)

mtrt.stk$species <- rownames(mtrt.stk)

mtrt.sub <- mtrt.stk[, c( "Leaf_dry_matter_content", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Plant_height_vegetative", "seed mass", "Specific_leaf_area", "Stem_specific_density")]
names(mtrt.sub) <- c("LDMC","N", "Height", "seed","SLA","SSD")
mtrt.sub <- mtrt.sub[complete.cases(mtrt.sub), ]

trtGeoPca <- prcomp(mtrt.sub, center = T, scale. = T)

#pdf(file.path( "figures/trait_pca.pdf"), width = 7, height = 8)
ggbiplot(trtGeoPca, labels = rownames(mtrt.sub))
#dev.off()


c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_photosynthesis_rate_per_leaf_area",
  "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter")