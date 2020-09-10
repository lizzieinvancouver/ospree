## Started 10 Sept 2020 ##
## By Cat from models_stanforranges.R ##

## Goal is to look at cues with few interactions or less data

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(reshape2)
library(rstan)
library(rstanarm)
library(dplyr)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Our flags for ranges, for now ... (see issue #379)
use.chillports = FALSE
use.zscore = TRUE
use.allspp = TRUE
use.multcuespp = FALSE
use.cropspp = TRUE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE


setwd("..//bb_analysis")
source("source/bbstanleadin.R")
setwd("..//ranges")

bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")

### find only studies with 2 or more lattitudes
multilats<-bb.stan %>% group_by(datasetID) %>% dplyr::summarise(Unique_lats = n_distinct(provenance.lat))
multilats<-filter(multilats, Unique_lats>=2)
bb.stan.lat<-filter(bb.stan,datasetID %in% c(multilats$datasetID)) ###### this is the datasheet for the intra/inter model

## Do some population stuff, by latitude
getpopz1 <- subset(bb.stan.lat, select=c("latbi", "provenance.lat")) # "datasetID", "study",
getpopz2 <- getpopz1[!duplicated(getpopz1), ]
getpopz <- aggregate(getpopz2["provenance.lat"], getpopz2["latbi"], FUN=length)
getpopz5 <- subset(getpopz, provenance.lat>4) # 4
getpopz3 <- subset(getpopz, provenance.lat>2) # 13
getpopz2 <- subset(getpopz, provenance.lat>1) # 39

# Species list ...
naspp <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra",
           "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_nigra", #"Alnus_rubra",
           "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis", "Acer_saccharum",
           "Alnus_incana", "Acer_rubrum", "Corylus_cornuta", "Picea_glauca","Robinia_pseudoacacia","Populus_tremuloides") # Will be Corylus_cornuta once data updated

eurspp <- c("Abies_alba", "Acer_pseudoplatanus", "Aesculus_hippocastanum", "Alnus_glutinosa",
            "Alnus_incana", "Betula_pendula", "Betula_pubescens", "Carpinus_betulus",
            "Cornus_mas", "Corylus_avellana", "Fagus_sylvatica", "Fraxinus_excelsior", "Larix_decidua", "Picea_abies", "Populus_tremula", "Prunus_avium", "Prunus_padus", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Sorbus_aucuparia", "Tilia_cordata")    

allspphere <- c(naspp, eurspp)
allspphere[which(!allspphere %in% unique(bb.stan$latbi))]

################################################
## Start sidebar on how we picked these studies

# species in more than two papers
getspp2papers1 <- subset(bb.stan, select=c("latbi", "datasetID")) 
getspp2papers2 <- getspp2papers1[!duplicated(getspp2papers1), ]
getspp2papers3 <- aggregate(getspp2papers2["datasetID"], getspp2papers2["latbi"], FUN=length)
spp2papers <- subset(getspp2papers3, datasetID>1)
spp2papers[order(spp2papers$latbi),]

spp3cues1 <- subset(bb.stan, chill_type!="fldest")
spp3cues2 <- subset(spp3cues1, select=c("latbi", "datasetID", "study", "force", "photo", "chill"))
spp3cuescounts <-
  ddply(spp3cues2, c("latbi", "datasetID", "study"), summarise,
        nforce = length(unique(force)),
        nphoto = length(unique(photo)),
        nchill = length(unique(chill)))

spp3cues <- subset(spp3cuescounts, nforce>1 & nphoto>1 & nchill>1) # this is 172 spp if you exclude field chilling you get worrall67 and flynn18 added

justcues1 <- subset(bb.stan, chill_type!="fldest")
justcues2 <- subset(justcues1, select=c("latbi", "force", "photo", "chill"))
justcuescounts <-
  ddply(justcues2, c("latbi"), summarise,
        nforce = length(unique(force)),
        nphoto = length(unique(photo)),
        nchill = length(unique(chill)))

sppcuecounts <- subset(justcuescounts, nforce>2 & nphoto>2 & nchill>2) # 5 species

unique(spp3cues$latbi)
sort(union(unique(spp3cues$latbi), spp2papers$latbi))
setdiff(allspphere, union(unique(spp3cues$latbi), spp2papers$latbi))

setdiff(union(unique(spp3cues$latbi), spp2papers$latbi), allspphere)

# Okay, will update what we did in issue #379, as best I can guess it now.

## End sidebar on how we picked these studies
################################################


bb.stan.orig <- bb.stan
bb.stan<- bb.stan[which(bb.stan$latbi %in% allspphere),] # uses about 50% of the bb.stan.orig data

# Check on ambient-only studies ... delete some rows
bb.stanamb <- subset(bb.stan, photo_type=="amb" | force_type=="amb")
unique(bb.stanamb$latbi) # I am not going to check Fagus_sylvatica, but I checked the rest and they all have exp treatments also
# bb.stan <- subset(bb.stan, photo_type!="amb" | force_type!="amb") # deletes about 100 rows 

bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))
bb.stan.lat$latbinum <- as.numeric(as.factor(bb.stan.lat$latbi))

bb.stan.pop5 <- bb.stan.lat[which(bb.stan.lat$latbi %in% getpopz5$latbi),] # 4 species!
bb.stan.pop3 <- bb.stan.lat[which(bb.stan.lat$latbi %in% getpopz3$latbi),] # 13 species
bb.stan.pop2 <- bb.stan.lat[which(bb.stan.lat$latbi %in% getpopz2$latbi),] # 39 species


### Now let's count up the data:
# First we need to subset the data a bit to make this more informative
bb.stan.pop5.sub <- subset(bb.stan.pop5, select=c("datasetID", "latbi", "provenance.lat", "photo", "force", "chill"))
bb.stan.pop5.sub <- bb.stan.pop5.sub[!duplicated(bb.stan.pop5.sub),]

bb.stan.pop3.sub <- subset(bb.stan.pop3, select=c("datasetID", "latbi", "provenance.lat", "photo", "force", "chill"))
bb.stan.pop3.sub <- bb.stan.pop3.sub[!duplicated(bb.stan.pop3.sub),]

bb.stan.pop2.sub <- subset(bb.stan.pop2, select=c("datasetID", "latbi", "provenance.lat", "photo", "force", "chill"))
bb.stan.pop2.sub <- bb.stan.pop2.sub[!duplicated(bb.stan.pop2.sub),]




countintrxns <- function(xx){
  
  #### Now count number of different values of each cue for each datasetID & exp
  xx <- within(xx, { numphotos <- as.numeric(ave(xx$photo, xx$datasetID, xx$latbi, xx$provenance.lat, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numforces <- as.numeric(ave(xx$force, xx$datasetID, xx$latbi, xx$provenance.lat,  FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numchills <- as.numeric(ave(xx$chill, xx$datasetID, xx$latbi, xx$provenance.lat,  FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numspps <- as.numeric(ave(xx$latbi, xx$datasetID, xx$provenance.lat,  FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numsites <- as.numeric(ave(xx$provenance.lat, xx$datasetID, xx$latbi,  FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  
  #### Now count number of different cues for each species
  xx$numforcebyspp <- ifelse(!is.na(xx$force) & !is.na(xx$force), as.numeric(ave(xx$force, xx$latbi, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numphotobyspp <- ifelse(!is.na(xx$photo) & !is.na(xx$photo), as.numeric(ave(xx$photo, xx$latbi, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numchillbyspp <- ifelse(!is.na(xx$chill) & !is.na(xx$chill), as.numeric(ave(xx$chill, xx$latbi, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  
  #### Now count number of different cues for each site
  xx$numforcebysite <- ifelse(!is.na(xx$force) & !is.na(xx$force), as.numeric(ave(xx$force, xx$provenance.lat, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numphotobysite <- ifelse(!is.na(xx$photo) & !is.na(xx$photo), as.numeric(ave(xx$photo, xx$provenance.lat, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numchillbysite <- ifelse(!is.na(xx$chill) & !is.na(xx$chill), as.numeric(ave(xx$chill, xx$provenance.lat, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
                              
  #### Now count number of different cues for each site AND species
  xx$numforcebysppsite <- ifelse(!is.na(xx$force) & !is.na(xx$force), as.numeric(ave(xx$force, xx$latbi, xx$provenance.lat, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numphotobysppsite <- ifelse(!is.na(xx$photo) & !is.na(xx$photo), as.numeric(ave(xx$photo, xx$latbi, xx$provenance.lat, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numchillbysppsite <- ifelse(!is.na(xx$chill) & !is.na(xx$chill), as.numeric(ave(xx$chill, xx$latbi, xx$provenance.lat, xx$datasetID, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  
  return(xx)
}

commoncues5 <- countintrxns(bb.stan.pop5.sub)
unique(commoncues5$latbi) #[1] "Betula_pendula"  "Fagus_sylvatica" "Picea_abies" "Ribes_nigrum" 
### Not many species!!!!! Plus we want to remove Ribes
length(unique(commoncues5$provenance.lat)) # 36
unique(commoncues5$numforcebysppsite) # [1]  1  3 15 12 11  2  4  5
unique(commoncues5$numphotobysppsite) # [1] 1 2 3 4 6 5
unique(commoncues5$numchillbysppsite) # [1]  1  2  9 12  8  3  5 20 25  4  7 23 15  
### Chill cue seems the best, then forcing but overall not a lot of species



commoncues3 <- countintrxns(bb.stan.pop3.sub)
unique(commoncues3$latbi) ### 14 species minues Ribes and Vitis
length(unique(commoncues3$provenance.lat)) # 52 lats
unique(commoncues3$numforcebysppsite) # [1]  1  2  4  3 15 12 11  5 14
unique(commoncues3$numphotobysppsite) # [1] 1 2 3 4 6 5
unique(commoncues3$numchillbysppsite) # [1]  1 19  7  3  8 12  2  9  5 20 14 25  4 23 15 22 16 
### Again, chill cue seems the best, then forcing


commoncues2 <- countintrxns(bb.stan.pop2.sub)
unique(commoncues2$latbi) ### 39 species minues Ribes, Vitis and Malus
length(unique(commoncues2$provenance.lat)) # 56 lats
unique(commoncues2$numforcebysppsite) # [1]  1  2  4  3 15 12 11  5 14
unique(commoncues2$numphotobysppsite) # [1] 1 2 3 4 6 5
unique(commoncues2$numchillbysppsite) # [1]  1 19  3  7  8 12  2  9  5 20 14 42 38 25  4 18 23 15 22 16
### Again, chill cue seems the best, then forcing
  

cropspp <- c("Actinidia_deliciosa", "Malus_domestica", "Vitis_vinifera", "Ribes_nigrum", 
             "Vaccinium_ashei", "Vaccinium_corymbosum", "Prunus_persica")


### Thoughts... 
# 1) We should remove crops
# 2) Maybe we should go back to 2 populations for each species
  ## a. Should we try using sites rather than strictly unique latitudes?
# 3) Should we remove studies that only have one cue for each spp x site interaction?



