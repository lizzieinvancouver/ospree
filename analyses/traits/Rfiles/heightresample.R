## Started 30 January 2021 ##
## By Lizzie so far ... ## 


#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

require(dplyr)

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
# dat <- dat[,c("SpeciesName", "TraitName", "TraitValue", "UnitName", "Latitude",
#     "Longitude", "project_pi", "database", "DatasetID")]
# names(dat) <- c("latbi", "traitname", "traitvalue", "unit", "lat", "long", "project_pi",
#     "database", "DatasetID")

heighter <- subset(dat, traitname == "Plant_height_vegetative")

# Percentage of data that is height?
nrow(heighter)/nrow(dat)

# Eeek! Definitely some name cleaning still needed
sort(unique(heighter$speciesname)) # 78 species

# Just do one example species for now... 
testsp <- subset(heighter, speciesname == "Sorbus_aria")
nrow(testsp)

howmanydf <- data.frame(resample.n = numeric(), mean = numeric(), sd = numeric())
for(i in 1:15000){
    mysample <- sample(testsp$traitvalue, i)
    dfadd <- data.frame(resample.n = i, mean = mean(mysample), sd = sd(mysample))
    howmanydf <- rbind(howmanydf, dfadd)
}

# the central limit theorem!
plot(mean~resample.n, data=howmanydf)
plot(sd~resample.n, data=howmanydf)

# need to repeat with other species and across studyID? ... and,
# come up with methods to write in paper that we like

# DL edits: 
# Based on Lizzie's code above on line 40-49, I think we could have as few as 5000? 

# But how many species have more than 5000 rows of data? 
ht.sp <- heighter %>%
  group_by(speciesname) %>%
  summarise(no_obs = length(speciesname))

# howmanydf <- data.frame(resample.n = numeric(), mean = numeric(), sd = numeric())
# species <- ht.sp$speciesname
# obs <- ht.sp$no_obs
# 
# for (sp in 1 : length(species)){
#   htdat <- subset(heighter, speciesname == species[4])
#   obs <- nrow(htdat)
#  
# if(obs > 5000){
#       for (i in 1:5000){
#     mysample <- sample(htdat$traitvalue, i)
#     dfadd <- data.frame(resample.n = i, mean = mean(mysample), sd = sd(mysample))
#     } else {
#       dfadd <- subst(heighter, speciesname == i)
#     }
#     howmanydf <- rbind(howmanydf, htdat)
#   }
#}

# I can't figure out how to get it all working in one loop, so here is my more round about approach:
a.lot.ht <- c("Quercus_ellipsoidalis", "Alnus_rubra", "Fraxinus_nigra", "Populus_grandidentata", "Betula_lenta", "Betula_alleghaniensis", "Betula_papyrifera", "Fagus_grandifolia", "Quercus_velutina", "Prunus_serotina", "Quercus_rubra", "Acer_saccharum", "Quercus_alba")

sm <- heighter[!heighter$speciesname %in% a.lot.ht, ]
sm.sub <- sm[, c("traitname", "traitvalue", "speciesname")]
lg <- heighter[heighter$speciesname %in% a.lot.ht, ]

#resampling for sp with a large amount of height data
howmanydf <- data.frame(resample.n = numeric(), mean = numeric(), sd = numeric())
species <- unique(lg$speciesname)

sp.sub <- species[1:3]
for (sp in 1: length(species)){
  testsp <- subset(lg, speciesname == species[sp])
for(i in 1:5000){
  mysample <- sample(testsp$traitvalue, i)
  dfadd <- data.frame(resample.n = i, mean = mean(mysample), sd = sd(mysample))
  dfadd$speciesname <- species[sp]
  howmanydf <- rbind(howmanydf, dfadd)
}
}

# the central limit theorem!
plot(mean~resample.n, data=howmanydf)
plot(sd~resample.n, data=howmanydf)


# There are 12 species that have over 5000 observations, also of note, there are 31 of th 72 species that have less than 10 obs. 


#resampling for sp with a large amount of height data
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

unique(ht.sample$speciesname)
