## Started 2 February 2016 ##
## By Lizzie (for now) ##

###################################
## Try to run models Ospree data ##
## Trying rstanarm (because, why not?) ##
###################################

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstanarm)
library(ggplot2)
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
  } else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")


# get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
labgroups <- read.csv("output/labgroups.csv", header=TRUE)

# merge in labgroup (we could do this elsewhere someday
bb.wlab <- merge(bb, labgroups, by="datasetID", all.x=TRUE)

columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
    "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", "cat")

bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)

# make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$chill <- as.numeric(bb.wlab.sm$Total_Chilling_Hours)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)

bb.wlab.sm$photo.cen <- scale(bb.wlab.sm$photo, center=TRUE, scale=TRUE)
bb.wlab.sm$force.cen <- scale(bb.wlab.sm$force, center=TRUE, scale=TRUE)
bb.wlab.sm$chill.cen <- scale(bb.wlab.sm$chill, center=TRUE, scale=TRUE)


# trying a model!

# Syntax from Ailene!
# m.try <- stan_lmer(resp ~ photo*chill+ (photo*chill||genus) + (1|cat), data = bb.wlab.sm)

m1.force <- stan_lmer(resp ~ force + (force|genus) + (1|cat), data = bb.wlab.sm)
m1.photo <- stan_lmer(resp ~ photo + (photo|genus) + (1|cat), data = bb.wlab.sm)
m1.chill <- stan_lmer(resp ~ chill + (chill|genus) + (1|cat), data = bb.wlab.sm)


m1 <- stan_lmer(resp ~ force + photo + chill + force*photo + force*chill + photo*chill +
    (force + photo + chill + force*photo + force*chill + photo*chill | genus) +  (1|cat),
    data = bb.wlab.sm)

m1.nolabgroup  <- stan_lmer(resp ~ force.cen + photo.cen + chill.cen + force.cen*photo.cen + force.cen*chill.cen + 
    photo.cen*chill.cen + (force.cen + photo.cen + chill.cen + force.cen*photo.cen + force.cen*chill.cen +
    photo.cen*chill.cen | genus), data = bb.wlab.sm, adapt_delta=0.99,cores=4, QR = TRUE)


m1.fp.nolabgroup <- stan_lmer(resp ~ force.cen*photo.cen + (1 | genus), data = bb.wlab.sm, cores=4, QR = TRUE)

m1.fp.nolabgroup.full <- stan_lmer(resp ~ force.cen*photo.cen + (force.cen*photo.cen | genus),
    data = bb.wlab.sm, cores=4, QR = TRUE)

# save the models

save(m1.force, file="output/m1.force.Rdata")
save(m1.photo, file="output/m1.photo.Rdata")
save(m1.chill, file="output/m1.chill.Rdata")
save(m1.fp.nolabgroup.full, file="output/m1.fp.nolabgroup.full.Rdata")

save(m1.nolabgroup, file="output/m1.nolabgroup.Rdata")
str(m1.nolabgroup)

launch_shinystan(m1.chill)
launch_shinystan(m1.force)
launch_shinystan(m1.nolabgroup)

summary(m1.chill)
summary(m1.photo)
summary(m1.force)

