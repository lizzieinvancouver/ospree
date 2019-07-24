## Started 23 July 2019 ##
## By Cat ##
## Try to sort through performance data to see if useful ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(brms)
library(dplyr)
library(tidyr)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

### Check out the Wiki page for Hypotheses and details: https://github.com/lizzieinvancouver/ospree/wiki/Performance-Data
## Hypothesis 1: Earlier budbursting individuals will grow more
bbosp <- read.csv("output/ospree_clean_withchill_BB_taxon.csv", header=TRUE)

## Try to subset the dataframe to studies that have daystobudburst and growth information...
resptypes <- c("daystobudburst", "growth")
bbosp.growth <- bbosp[(bbosp$respvar.simple%in%resptypes),]

# Let's find the studies that do both
tt <- as.data.frame(table(bbosp.growth$datasetID, bbosp.growth$respvar.simple))
tt <- tt[(tt$Freq>0),]
tt.studies <- as.data.frame(table(tt$Var1))
tt.studies <- tt.studies[(tt.studies$Freq>1),]
studies <- unique(tt.studies$Var1)

bbosp.growth <- bbosp.growth[(bbosp.growth$datasetID%in%studies),]

### Alright, before building a model, let's check out the dimensions of this...
## 300 rows of data across 4 datasetIDs
unique(bbosp.growth$complex) # 7 species
#[1] "Tilia_complex"       "Acer_pseudoplatanus" "Fagus_sylvatica"     "Quercus_petraea"     "Picea_abies"        
#[6] "Pinus_complex"       "Ribes_nigrum"

bbosp.growth$resp <- as.numeric(bbosp.growth$response)
bbosp.growth$budburst <- as.numeric(bbosp.growth$response.time)
bbosp.growth$chill <- as.numeric(bbosp.growth$Total_Utah_Model)
bbosp.growth$photo <- as.numeric(bbosp.growth$photoperiod_day)
bbosp.growth$force <- as.numeric(bbosp.growth$forcetemp)

# Hypothesis 1: Earlier budbursting individuals will grow more
## This means we need a budburst time for each individual and then an amount of growth
# It will be tough determining which individual budburst date corresponds to which growth value
columnstokeepbb <- c("datasetID", "force", "photo", "respvar.simple", "provenance.lat",
                    "resp", "budburst", "chill", "complex")

bbosp.growth.stan <- subset(bbosp.growth, select=columnstokeepbb)
bbosp.growth.stan <- bbosp.growth.stan[!duplicated(bbosp.growth.stan),]

bbosp.bb <- bbosp.growth.stan[(bbosp.growth.stan$respvar.simple=="daystobudburst"),]
bbosp.bb$resp <- bbosp.bb$respvar.simple <- NULL

bbosp.ht <- bbosp.growth.stan[(bbosp.growth.stan$respvar.simple=="growth"),]
bbosp.ht$budburst <- bbosp.ht$respvar.simple <- NULL

bbgrowth.stan <- left_join(bbosp.bb, bbosp.ht)

bbgrowth.stan <- bbgrowth.stan[!is.na(bbgrowth.stan$resp),]
bbgrowth.stan <- bbgrowth.stan[!is.na(bbgrowth.stan$budburst),]

## Quick check on model:
mod.bb <- brm(resp ~ budburst + (budburst|complex), data=bbgrowth.stan,
              iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#Population-Level Effects: 
 # Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#Intercept     3.34      3.68    -3.97    10.50       2617 1.00
#budburst      0.00      0.24    -0.37     0.60        799 1.00

#Family Specific Parameters: 
 # Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#sigma     5.46      0.29     4.94     6.06       5840 1.00

#Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
#is a crude measure of effective sample size, and Rhat is the potential 
#scale reduction factor on split chains (at convergence, Rhat = 1).
#Warning message:
 # There were 38 divergent transitions after warmup. Increasing adapt_delta above 0.99 may help.
#See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 


######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

## Let's look into Hypothesis 2 now... increased chilling will correspond to increased growth
# For sake of consistency and comfort of knowing data has been cleaned, I will use the bb_analysis dataframe
osp.growth <- bbosp[(bbosp$respvar.simple=="growth"),]

## It's important to make sure we only have one value for each individual.. or we can determine the relative growth rate if response.time is appropriately recorded
#First, let's get rid of the NAs and blanks
osp.growth <- osp.growth[!is.na(osp.growth$response),]
osp.growth <- osp.growth[(osp.growth$response!=""),]

columnstokeep <- c("datasetID", "study", "genus", "species", "provenance.lat", "provenance.long",
                   "year", "fieldsample.date", "chilltemp", "chilldays", "forcetemp", "photoperiod_day", "respvar",
                   "response", "Total_Utah_Model", "complex")
growth.stan <- subset(osp.growth, select=columnstokeep)
growth.stan <- growth.stan[!duplicated(growth.stan),] ## 1429 rows of data...?

# Again, before building a model, let's check out the dimensions...
## 1429 rows of data across 14 datasets
unique(growth.stan$complex) # 20 species
#[1] "Tilia_complex"        "Acer_pseudoplatanus"  "Fagus_sylvatica"      "Quercus_petraea"     
#[5] "Picea_abies"          "Pinus_complex"        "Picea_complex"        "Pyrus_complex"       
#[9] "Malus_complex"        "Prunus_complex"       "Prunus_avium"         "Sorbus_aucuparia"    
#[13] "Sorbus_complex"       "Sorbus_commixta"      "Ribes_nigrum"         "Populus_complex"     
#[17] "Betula_pendula"       "Betula_pubescens"     "Rhododendron_complex" "Prunus_persica"

## Okay, and now, what's the range of respvars and values
range(growth.stan$response) ## -0.3 to 98

growth.stan$resp <- as.numeric(growth.stan$response)
growth.stan$chill <- as.numeric(growth.stan$Total_Utah_Model)
growth.stan$photo <- as.numeric(growth.stan$photoperiod_day)
growth.stan$force <- as.numeric(growth.stan$forcetemp)

growth.stan <- growth.stan[!is.na(growth.stan$resp),]
growth.stan <- growth.stan[!is.na(growth.stan$chill),]
growth.stan <- growth.stan[!is.na(growth.stan$photo),]
growth.stan <- growth.stan[!is.na(growth.stan$force),]

## Let's check out a really quick brms model: # once we remove NAs there are only 371 rows of data

mod.growth <- brm(resp ~ chill + force + photo + (1 | complex), data=growth.stan,
                  iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#Population-Level Effects: 
 # Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#Intercept    19.20     16.24   -11.18    52.46       1505 1.00
#chill        -0.00      0.00    -0.01     0.01       4907 1.00
#force        -0.37      0.43    -1.23     0.47       5191 1.00
#photo         0.63      0.29     0.06     1.19       4780 1.00

#Family Specific Parameters: 
 # Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#sigma    30.85      1.16    28.75    33.28       4396 1.00


######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
### Hypothesis 3: Growth will decrease with increasing provenance latitude.
columnstokeeplat <- c("datasetID", "provenance.lat", "forcetemp", "photoperiod_day", "respvar",
                   "response", "Total_Utah_Model", "complex")
lat.growth <- subset(osp.growth, select=columnstokeeplat)
lat.growth <- lat.growth[!duplicated(lat.growth),] ## 1420 rows of data

tlat <- as.data.frame(table(lat.growth$complex, lat.growth$provenance.lat))
tlat <- tlat[(tlat$Freq>0),]
lat.spp <- as.data.frame(table(tlat$Var1))
lat.spp <- lat.spp[(lat.spp$Freq>1),]
spplat <- unique(lat.spp$Var1)

lat.growth <- lat.growth[(lat.growth$complex%in%spplat),]

lat.growth$resp <- as.numeric(lat.growth$response)
lat.growth$lat <- as.numeric(lat.growth$provenance.lat)

lat.growth <- lat.growth[!is.na(lat.growth$resp),]
lat.growth <- lat.growth[!is.na(lat.growth$lat),]

### Okay, final model looking at the effect of provenance on growth... 

mod.lat <- brm(resp ~ lat + (lat | complex), data=lat.growth,
               iter=4000, warmup=2500, control=list(max_treedepth = 15,adapt_delta = 0.99))

#Population-Level Effects: 
 # Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#Intercept    13.51     15.25   -20.94    41.16       3049 1.00
#lat           0.33      0.39    -0.38     1.13       2709 1.00

#Family Specific Parameters: 
 # Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
#sigma    16.12      0.42    15.32    16.98       8095 1.00

#Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
#is a crude measure of effective sample size, and Rhat is the potential 
#scale reduction factor on split chains (at convergence, Rhat = 1).
#Warning message:
 # There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.99 may help.
#See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

