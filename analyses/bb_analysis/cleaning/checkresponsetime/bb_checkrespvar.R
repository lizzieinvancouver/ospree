## Started 11 September 2017 ##
## Lizzie took some of this from bbmodels_stan.R and fileswithin ##

## Need to check our response variables ...
## Have found some are daystobudburst from START OF YEAR
## Check all greater than 30 or 60 days?
## Check all seedlings or saplings

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

##
## Okay, STEP 1: Get the data close to the data we will use.... 
##

# below file is cleaned, had chilling added and some BB cleaning done also
bb.all <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

# file to adjust species into species or species complexes ... 
taxon <- read.csv("output/bb_analysis/taxon/complex_levels.csv", header=TRUE)

## read taxon data to get the 'type' category, then delete what we don't need
bb.all.wtaxa <- merge(bb.all, taxon, by=c("genus","species"), all.x=TRUE)
bb.all.wtaxa$complex <- NULL
bb.all.wtaxa$use <- NULL

## just the bb data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- bb.all.wtaxa[which(bb.all.wtaxa$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)
# bb.resp$force <- as.numeric(bb.resp$forcetemp) # we're updating this juts now
bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$chill <- as.numeric(bb.resp$Total_Chilling_Hours)
bb.resp$resp <- as.numeric(bb.resp$response.time)

## remove the NAs 
bb.noNA <- subset(bb.resp, is.na(photo)==FALSE &
    is.na(chill)==FALSE & is.na(resp)==FALSE) # is.na(force)==FALSE & 

##
## STEP 2: Flag the non-cutting experiments so we can check ALL of them
##

unique(bb.noNA$material)
noncuttingnames <- c("seedlings", "plants", "sapling", "clones", "coppiced seedlings", "seedling",
   "potted sapling", "seedlings (1 yr old)", "9-month old potted seedling",
   "potted cuttings from 14-year old tree", "2-year old potted seedling",
   "7-month old potted seedling")
noncutt <- bb.noNA[which(bb.noNA$material %in% noncuttingnames),]
cutt <- bb.noNA[which(!bb.noNA$material %in% noncuttingnames),]
nrow(bb.noNA)
nrow(noncutt) + nrow(cutt)

##
## STEP 3: Flag the cutting experiments over 60 days to we can check ALL of them
##
checkme60 <- subset(cutt, response.time>60)

##
## STEP 4: Bind together what needs to be checked and write it out in a useful way
##
checkme <- rbind(checkme60, noncutt)
checkme.sm <- subset(checkme, select=c("datasetID", "study", "material", "response.time",
    "figure.table..if.applicable."))

checkme.agg <- aggregate(checkme.sm["response.time"], checkme.sm[c("datasetID", "study",
    "material","figure.table..if.applicable.")], FUN=mean)

checkme.agg <- checkme.agg[with(checkme.agg, order(datasetID)),]

nrow(checkme.agg)/4

##
## STEP 5: Write it out: 
##

write.csv(checkme.agg, "bb_analysis/cleaning/checkresponsetime/checkmeout.csv", row.names=FALSE)
