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
head(dat)
## starting with a PCA of the raw data, with sampling for species with a lot of height data

#starting by separating out the height data:
height <- subset(dat, traitname == "Plant_height_vegetative")

other.trt <- subset(dat, traitname != "Plant_height_vegetative")

## sampling height: start by separating out the species with few obs
a.lot.ht <- c("Quercus_ellipsoidalis", "Alnus_rubra", "Fraxinus_nigra", "Populus_grandidentata", "Betula_lenta", "Betula_alleghaniensis", "Betula_papyrifera", "Fagus_grandifolia", "Quercus_velutina", "Prunus_serotina", "Quercus_rubra", "Acer_saccharum", "Quercus_alba")

sm <- height[!height$speciesname %in% a.lot.ht, ]
sm.sub <- sm[, c("traitname", "traitvalue", "speciesname")]

lg <- height[height$speciesname %in% a.lot.ht, ]

# now sampling for species with a lot of data
ht <- data.frame(speciesname = character(), height = numeric())
species <- unique(lg$speciesname)

for (sp in 1: length(species)){
    testsp <- subset(lg, speciesname == species[sp])
    mysample <- sample(testsp$traitvalue, 5000)
    dfadd <- data.frame(speciesname = species[sp], traitvalue = mysample)
    # mysample$speciesname <- species[sp]
    ht <- rbind(ht, dfadd)
}

ht$traitname <- "Plant_height_vegetative"

ht.sample <- rbind(sm.sub, ht)

####################################################################################################################
## I started by trying to just alter Geoff's code (below) and while it worked well for the mean values, I am not sure how to get it to work with the raw data

species <- unique(other.trt$speciesname)
traits <- c("Specific_leaf_area", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Leaf_carbon_.C._content_per_leaf_dry_mass", "seed mass")

# looking at the number of observations per trait, we could imagine we would need 9685 rows for all the trait data
tm <- aggregate(other.trt["traitvalue"], other.trt[c("traitname", "speciesname", "datasetid")], FUN=length) 

# 9682 is the max rows needed for a given trait
species <- c("Betula_occidentalis", "Rhododendron_prinophyllum")
sp.sub2 <- subset(other.trt, speciesname == "Rhododendron_prinophyllum")
sp.sub <- subset(other.trt, speciesname == "Betula_occidentalis")

mat <- matrix(NA, ncol = length(traits)+1, nrow = 9682) # the max number of rows of the matrix should be the nrows for the trait we have the most data for, height, which is 801485

for(i in 1:length(species)){
    temp <- subset(other.trt, speciesname == species[i])
    for(j in 1:length(traits)){
        temp2 <- subset(temp, traitname == traits[j])
        for (k in 1:nrow(temp2)){
            mat[k,j] <- temp2$traitvalue[k]
            mat[k,7] <- temp2$speciesname[k]
        }
    }
}

head(mat)

colnames(mat) <- c("SLA", "N", "SSD", "ldmc", "lcc", "seed", "speciesname")

print(i)
print(j)
print(k)

# I think the issue is that the matrix gets overwritten with each loop beacuse k is always the first row

mat <- matrix(NA, ncol = length(traits)+1, nrow = 7)  # 7 is the rows of data for these two species


# This is my idea, which adds a new loop for the row of the matrix (f), but it doesn't work and gives the below error
for (f in 1:nrow(mat)){ 
    for(i in 1:length(species)){
    temp <- subset(other.trt, speciesname == species[i])
    
        for(j in 1:length(traits)){
        temp2 <- subset(temp, traitname == traits[j])
        
            for (k in 1:nrow(temp2)){
              mat[f,j] <- temp2$traitvalue[k]
              mat[f,7] <- temp2$speciesname[k]
          }
        }
    }
}
# Error in mat[f, j] <- temp2$traitvalue[k] : replacement has length zero
mat

print(i)
print(j)
print(k)
print(f)

mat <- matrix(NA, ncol = length(traits)+1, nrow = 7) 

f <- 1
i <- 2
temp <- subset(other.trt, speciesname == species[i])
        
j <- 1
temp2 <- subset(temp, traitname == traits[j])
            
k <- 1
mat[f,j] <- temp2$traitvalue[k]
mat[f,7] <- temp2$speciesname[k]

head(mat)

# next, I will start to put it back together and see where it goes wrong
mat <- matrix(NA, ncol = length(traits)+1, nrow = 7) 

f <- 1
i <- 2
temp <- subset(other.trt, speciesname == species[i])

for(j in 1:length(traits)){
    temp2 <- subset(temp, traitname == traits[j])
    
    for (k in 1:nrow(temp2)){  
        mat[f,j] <- temp2$traitvalue[k]
        mat[f,7] <- temp2$speciesname[k]
    }
}
head(mat)



f <- 1
i <- 2
temp <- subset(other.trt, speciesname == species[i])


for(i in 1:length(species)){
    temp <- subset(other.trt, speciesname == species[i])
    mat <- matrix(NA, ncol = length(traits)+1, nrow = 10) 
    for(j in 1:length(traits)){
        temp2 <- subset(temp, traitname == traits[j])
        for (k in 1:nrow(temp2)){
            mat[k,j] <- temp2$traitvalue[k]
            mat[k,7] <- temp2$speciesname[k]
        }
        mat <- rbind(mat[i], mat[i])
    }
}
head(mat)
 ## <><><><><><><><><><><><<><><><><><><><><><
# DL exploring the data more
trt.rm <- c("leaf life span", "Stem_diameter", "leaf carbon content per leaf nitrogen content", "Leaf_photosynthesis_rate_per_leaf_area")

trt.dataset <- dat %>%
    group_by(datasetid, speciesname) %>%
    summarise(no_rows = length(unique(traitname)))

dat <- dat[!dat$traitname %in% trt.rm,]
mtrt.ddply <- ddply(dat, c("speciesname", "traitname", "datasetid"),
                    summarise, mean = mean(traitvalue),
                    sd = sd(traitvalue),
                    sem = sd(traitvalue)/sqrt(length(traitvalue)),
                    geomean = exp(sum(log(traitvalue)/length(traitvalue))) )
head(mtrt.ddply)

#This is one of the mehtods mentioned on stack over.flow, https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in 
dat$lab <- paste(dat$speciesname, dat$datasetid, sep = "_")
mtrt.stk <- tapply(dat$traitvalue,
                      dat[c("lab","traitname")], 
                       mean)
head(mtrt.stk)
mtrt.stk <- mtrt.stk %>% separate(lab, c("speciesname", "studyid","database"))

species <- sort(unique(dat$speciesname))
dsid <- sort(unique(dat$datasetid))
traits <- c("Plant_height_vegetative", "Specific_leaf_area","Leaf_nitrogen_.N._content_per_leaf_dry_mass", 
            "Stem_specific_density", "Leaf_dry_matter_content",
            "leaf life span","seed mass", "Leaf_carbon_.C._content_per_leaf_dry_mass")

sp.study <- unique(paste(dat$speciesname, dat$datasetid, sep ="_")) # there are 840 uniuqe combinations of species by studyid
mat <- matrix(NA, ncol = length(traits), nrow = length(sp.study))

for (id in 1:length(dsid)){
    temp <- subset(mtrt.ddply, datasetid == dsid[id])
for(i in 1:length(sp.study)){
    temp2 <- subset(temp, speciesname == species[i])
    for(j in 1:length(traits)){
        mat[i, j] <- subset(temp2, traitname == traits[j])$geomean[1]
    }
    }
}
colnames(mat) <- c("Height", "SLA",  "N", "SSD", "LDMC", "llife", "seed", "C")
rownames(mat) <- sort(sp.study)
head(mat)

## With the above 8 traits, we would only have 13 species represented, without leaf lifespan and C, we have 28 species
mat2 <- mat[, c("Height", "SLA",  "N", "SSD", "LDMC", "seed")]
mat2 <- mat2[complete.cases(mat2), ]
## <><><><><><><><><><><><<><><><><><><><><><
# Below are my other attempts -- to be deleted
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
# temp <- subset(dat, speciesname == species[1])
# temp2 <- subset(temp, traitname == traits[1])
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
datsm <- subset(dat, select=c("speciesname", "traitname", "traitvalue"))
tryreshape <- reshape(datsm, timevar="traitname", idvar="speciesname", direction="wide")
require(reshape2)
trydcast <- dcast(dat, speciesname + latitude + longitude + datasetid ~ traitname, value.var = "traitvalue", drop = FALSE)
}
# Attempt 2
temp <- reshape2::dcast(other.trt, speciesname + datasetid ~ traitname , value.var= "traitvalue")

# Attempt 3 using tidyr
other.trt$no <- 1:nrow(other.trt)
test <- spread(other.trt, key = c(traitname, datasetid), value = traitvalue)

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
mtrt.ddply <- ddply(dat, c("speciesname", "traitname"),
                    summarise, mean = mean(traitvalue),
                    sd = sd(traitvalue),
                    sem = sd(traitvalue)/sqrt(length(traitvalue)),
                    geomean = exp(sum(log(traitvalue)/length(traitvalue))) )
head(mtrt.ddply)

#This is one of the mehtods mentioned on stack over.flow, https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in 
mtrt.stk <- exp(tapply(log(dat$traitvalue),
                       dat[c("speciesname","traitname")], 
                       mean, na.rm=TRUE))
head(mtrt.stk)

#This is another of the methods mentioned on stack over.flow, this function is suppose to deal with missing values, but it results in NA for traits that only have one observation, which isn't really what we want.

# gm_mean = function(x, na.rm=TRUE){
#     exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
# }

species <- sort(unique(dat$speciesname))
traits <- c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_photosynthesis_rate_per_leaf_area",
            "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter")

gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x) / length(x)))
}

spsub <- subset(dat, speciesname == species[5]) 
sptrtsub <- subset(spsub, traitname == traits[4]) 
gmmean <- gm_mean(sptrtsub$traitvalue); gmmean  

# this value is matches the above for several of the species and traits, confirming that the funciton gives the same values as those in mtrt.ddply and mtrt.stk

# DL did try to make a loop to calculate the mean with the funciton, but it doesn't quite work yet and is slower than the above methods
# tm <- aggregate(dat["traitvalue"], dat[c("speciesname","traitname")], FUN=length) 
# 
# trtmeans <- vector()
# for(i in 1:length(species)){
#     spsub <- subset(dat, speciesname == species[i])
#     for(j in 1:length(traits)){
#         sptrtsub <- subset(spsub, traitname == traits[j])
#         gmmean <- gm_mean(sptrtsub$traitvalue)
#         trtmeans <- rbind(trtmeans, gmmean)
#     }
# }
# tm$mean <- trtmeans[, 1]
# head(tm)

#########################################################################
# create a new matrix for feeding into the PCA
species <- sort(unique(dat$speciesname))
traits <- c("Plant_height_vegetative", "Specific_leaf_area","Leaf_nitrogen_.N._content_per_leaf_dry_mass", 
            "Stem_specific_density", "Leaf_dry_matter_content",
            "leaf life span","seed mass", "Leaf_carbon_.C._content_per_leaf_dry_mass")

mat <- matrix(NA, ncol = length(traits), nrow = length(species))

for(i in 1:length(species)){
    temp <- subset(mtrt.ddply, speciesname == species[i])
    for(j in 1:length(traits)){
        mat[i, j] <- subset(temp, traitname == traits[j])$geomean[1]
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