# Started 29 October 2019 by Cat
## Using code from Emily Simmond's sliding window approach: https://github.com/emilygsimmonds/Cue_Identification

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(climwin)
library(lubridate)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis/simmonds_slidingwin")
} else if(length(grep("lizzie", getwd()))>0) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/simmonds_slidingwin")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis/simmonds_slidingwin") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis/simmonds_slidingwin")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/simmonds_slidingwin")


set.seed(123)

## We'll input variables here based off of pep_sims:
## Adjust `postcctemp` depending on amount of warming as below
# precctemp is the avg spring temp, postcctemp is +1deg of warming, etc, preccbb is the expected day of budburst (DOY), postccbb is after warming
precctemp <- 10
postcctemp <- 11
sigmatemp <- 5
fstar <- 400

source("fakedata_swfx.R") ## can take some time...
sim1 <- simgenerate(precctemp, postcctemp, sigmatemp, fstar)
postcctemp <- 12
sim2 <- simgenerate(precctemp, postcctemp, sigmatemp, fstar)
postcctemp <- 13
sim3 <- simgenerate(precctemp, postcctemp, sigmatemp, fstar)
postcctemp <- 14
sim4 <- simgenerate(precctemp, postcctemp, sigmatemp, fstar)

bbdata1 <- sim1[[1]]
climate.data1 <- sim1[[2]]
bbdata2 <- sim2[[1]]
climate.data2 <- sim2[[2]]
bbdata3 <- sim3[[1]]
climate.data3 <- sim3[[2]]
bbdata4 <- sim4[[1]]
climate.data4 <- sim4[[2]]
  
  
source("Run_SW.R")
# refday = c(day, mon)
# climate is a datafile that must include col = temp
# datafile = biological data
# default = absolute but can also run relative
#run_SW <- function(absolute = TRUE, datafile, climate, refday)

### Now checking Simmond's sliding window approach:

if(FALSE){ ## using Simmond's paper data to test
### Test run:
datafile <- read.csv("bio_data.csv")
climate <- read.csv("climate_data.csv")

names(datafile) <- c("Year", "bb_date", "bb_mean", "doy95")
}

#### Now to run using differing simulations:
refday <- c(01, 03)
datafile <- bbdata4 #### CHANGE BASED ON SIMULATION!!! i.e., bbdataX (X = 1-4)
climate <- climate.data4 #### CHANGE BASED ON SIMULATION!!! i.e., bbdataX (X = 1-4)
climate$X <- NA ### needed in order to run... 

Results_SWA4 <- run_SW(absolute=TRUE, datafile, climate, refday) ## takes a while to run
write.csv(Results_SWA4[[2]], file="output/results_swa4.csv")


swa1 <- Results_SWA1[[2]]
swa2 <- Results_SWA2[[2]]
swa3 <- Results_SWA3[[2]]
swa4 <- Results_SWA4[[2]]



