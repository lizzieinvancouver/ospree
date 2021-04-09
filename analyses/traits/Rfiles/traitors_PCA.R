# This code is adapted by Deirdre from Geoff's traitors_try_bien code on January 22, 2021

# Aim: to do two PCA on the trait data to look for the relationships between traits found in the LES and published literature. The first PCA will be of all the data, and the second will just be using the geometric mean.

## Load libraries

rm(list = ls()) 
options(stringsAsFactors = FALSE)

library(tidyr)
library(plyr)
library(dplyr)
library(vegan)
library(ggbiplot)
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
dat$traitname[which(dat$traitname == "seed mass")] <- "Seed_mass"
## starting with a PCA of the raw data, with sampling for species with a lot of height data

#starting by separating out the height data:
height <- subset(dat, traitname == "Plant_height_vegetative")

# I think for this trait we are really only interested in adults - selecting only values greater than 1.42 m, the height at which you can measure DBH
small <- subset(height, traitvalue < 1.42) # this is 1.7% of the data 

height <- subset(height, traitvalue > 1.42)

other.trt <- subset(dat, traitname != "Plant_height_vegetative")

## sampling height: start by separating out the species with few obs
a.lot.ht <- c("Quercus_ellipsoidalis", "Alnus_rubra", "Fraxinus_nigra", "Populus_grandidentata", "Betula_lenta", "Betula_alleghaniensis", "Betula_papyrifera", "Fagus_grandifolia", "Quercus_velutina", "Prunus_serotina", "Quercus_rubra", "Acer_saccharum", "Quercus_alba")

sm <- height[!height$speciesname %in% a.lot.ht, ]

lg <- height[height$speciesname %in% a.lot.ht, ]

# now sampling for species with a lot of data
ht <- data.frame(speciesname = character(), height = numeric())
species <- unique(lg$speciesname)

for (sp in 1: length(species)){
    testsp <- subset(lg, speciesname == species[sp])
    mysample <- sample_n(testsp, 5000)
    #dfadd <- data.frame(speciesname = species[sp], traitvalue = mysample)
    # mysample$speciesname <- species[sp]
    ht <- rbind(ht, mysample)
}

ht.sample <- rbind(sm, ht)

trt.dat <- rbind(other.trt, ht.sample)
#write.csv(trt.dat, "input/trt.dat.htsampled.csv")

ht <-subset(trt.dat, traitname == "Plant_height_vegetative") 
length(unique(ht$speciesname))
lnc <-subset(trt.dat, traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")
length(unique(lnc$speciesname))
sla <-subset(trt.dat, traitname == "Specific_leaf_area")
length(unique(sla$speciesname))
sm <-subset(trt.dat, traitname == "Seed_mass")
length(unique(sm$speciesname))

####################################################################################################################
# These are traits that do not have enough data to be worth including but I wish we did
trt.rm <- c("leaf life span", "Stem_diameter", "leaf carbon content per leaf nitrogen content", "Leaf_photosynthesis_rate_per_leaf_area")
trt.sub <- trt.dat[!trt.dat %in% trt.rm,]

trt.dataset <- trt.sub %>%
    group_by(datasetid, speciesname) %>%
    summarise(no_rows = length(unique(traitname)), .groups = 'drop')

trt.dat$lab <- paste(trt.dat$speciesname, trt.dat$datasetid, sep = "_")

mtrt.lab <- tapply(trt.dat$traitvalue,
                   trt.dat[c("lab","traitname")], 
                   mean)

# with these five traits, we are only left with two species!
# mtrt.lab <- mtrt.lab[, c("Specific_leaf_area","Leaf_nitrogen_.N._content_per_leaf_dry_mass", 
#                        "Leaf_dry_matter_content", "Leaf_carbon_.C._content_per_leaf_dry_mass","Plant_height_vegetative")]

# with these four traits traits, we are only left with 33 species!
mtrt.lab <- mtrt.lab[, c("Specific_leaf_area","Leaf_nitrogen_.N._content_per_leaf_dry_mass",
                       "Leaf_dry_matter_content", "Leaf_carbon_.C._content_per_leaf_dry_mass")]
colnames(mtrt.lab) <- c("SLA","LNC","LDMC", "LCC")

mtrt.cc <- mtrt.lab[complete.cases(mtrt.lab), ]

# making a pca for the 40 studies that have complete data for these four traits
lab.Pca <- prcomp(mtrt.cc, center = TRUE, scale. = TRUE)
summary(lab.Pca)

ggbiplot(lab.Pca, labels = rownames(mtrt.cc))

ggbiplot(lab.Pca)

##################################################################
# Ideally we would use data from unique individuals to make the PCA, which is denoted by observationid
obid <- subset(trt.dat, !is.na(observationid))
length(unique(obid$observationid)) # we have 29292 unique obid's

trt.obid <- obid %>%
    group_by(observationid, speciesname) %>%
    summarise(no_rows = length(unique(traitname)), .groups = "drop")

temp <- subset(trt.obid, no_rows > 3)
obid4 <- unique(temp$observationid)

obid.sub <- obid[obid$observationid %in% obid4,]

trts <- c("Specific_leaf_area","Leaf_nitrogen_.N._content_per_leaf_dry_mass",
  "Leaf_dry_matter_content", "Leaf_carbon_.C._content_per_leaf_dry_mass")

obid.sub <- obid[obid$traitname %in% trts,]

mtrt.obid <- tapply(obid.sub$traitvalue,
                   obid.sub[c("observationid","traitname")], 
                   mean)

colnames(mtrt.obid) <- c("C", "LDMC", "N", "SLA")

mtrt.obid.cc <- mtrt.obid[complete.cases(mtrt.obid), ]

# making a pca for the 40 studies that have complete data for these four traits
obid.Pca <- prcomp(mtrt.obid.cc, center = TRUE, scale. = TRUE)
summary(obid.Pca)

ggbiplot(obid.Pca, labels = rownames(mtrt.obid.cc))

ggbiplot(obid.Pca)

# How many species is represented in the matrix?
obs.rep <- rownames(mtrt.obid.cc)
spp <- obid[obid$observationid %in% obs.rep,]
spp.obsid <- unique(spp$speciesname)
#"Prunus_padus"        "Betula_albosinensis" "Ulmus_parvifolia"    "Rhododendron_simsii"
##############################################################################################
## PCA of the geometric means
##############################################################################################
species <- sort(unique(dat$speciesname))
traits <- c("Plant_height_vegetative", "Specific_leaf_area","Leaf_nitrogen_.N._content_per_leaf_dry_mass", 
            "Stem_specific_density", "Leaf_dry_matter_content",
           "Seed_mass", "Leaf_carbon_.C._content_per_leaf_dry_mass")

mtrt.ddply <- ddply(dat, c("speciesname", "traitname"),
                    summarise, mean = mean(traitvalue),
                    sd = sd(traitvalue),
                    sem = sd(traitvalue)/sqrt(length(traitvalue)),
                    geomean = exp(sum(log(traitvalue)/length(traitvalue))) )

mat <- matrix(NA, ncol = length(traits), nrow = length(species))

for(i in 1:length(species)){
    temp <- subset(mtrt.ddply, speciesname == species[i])
    for(j in 1:length(traits)){
        mat[i, j] <- subset(temp, traitname == traits[j])$geomean[1]
    }
}
colnames(mat) <- c("Height", "SLA",  "N", "SSD", "LDMC", "seed", "C")
rownames(mat) <- species

## With the above 8 traits, we would only have 13 species represented, without leaf lifespan and C, we have 26 species
mat2 <- mat[, c("Height", "SLA",  "N", "SSD", "LDMC", "seed")]
mat2 <- mat2[complete.cases(mat2), ]

write.csv(mat2, "geoPCA_mat.csv")
## Standardize (normalize) trait values
mat.stan <- mat2[, c("Height", "SLA",  "N", "SSD", "LDMC", "seed")] <- apply(mat2[, 1:6], MARGIN = 2, FUN = function(X){ decostand(X, method = "standardize")})

## PCA plots 
trtGeoPca <- prcomp(mat2, center = TRUE, scale. = TRUE)
summary(trtGeoPca)

ggbiplot(trtGeoPca, labels = rownames(mat2))

pcageom <- ggbiplot(trtGeoPca) + xlim(-3,3)


# Finally, making a plot comparable to the one with data of multiple traits per individual
mat.comp <- mat[, c("SLA","N","LDMC", "C")]
mat.comp <- mat.comp[complete.cases(mat.comp), ]

mat.comp <- mat.comp[c("Prunus_padus","Betula_albosinensis", "Ulmus_parvifolia","Rhododendron_simsii"),]
mat.comp

geo.obsid <- prcomp(mat.comp, center = TRUE, scale. = TRUE)
summary(geo.obsid)

ggbiplot(geo.obsid, labels = rownames(mat.comp))

ggbiplot(geo.obsid)

traitors_sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa",
"Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")