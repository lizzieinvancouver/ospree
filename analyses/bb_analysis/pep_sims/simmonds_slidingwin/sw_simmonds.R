# Started 29 October 2019 by Cat
## Using code from Emily Simmond's sliding window approach: https://github.com/emilygsimmonds/Cue_Identification

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(climwin)
library(lubridate)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis/pep_sims/simmonds_slidingwin")
} else if(length(grep("lizzie", getwd()))>0) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims/simmonds_slidingwin")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis/pep_sims/simmonds_slidingwin") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis/pep_sims/simmonds_slidingwin")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims/simmonds_slidingwin")

################# If looking at results jump to line 84 ##################
set.seed(123)

## We'll input variables here based off of pep_sims:
## Adjust `postcctemp` depending on amount of warming as below
# precctemp is the avg spring temp, postcctemp is +1deg of warming, etc, preccbb is the expected day of budburst (DOY), postccbb is after warming
precctemp <- 6
postcctemp <- 7
sigmatemp <- 3
fstar <- 150

source("fakedata_swfx.R") ## can take some time...
sim1 <- simgenerate(precctemp, postcctemp, sigmatemp, fstar)
postcctemp <- 8
sim2 <- simgenerate(precctemp, postcctemp, sigmatemp, fstar)
postcctemp <- 9
sim3 <- simgenerate(precctemp, postcctemp, sigmatemp, fstar)
postcctemp <- 10
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
datafile <- bbdata1 #### CHANGE BASED ON SIMULATION!!! i.e., bbdataX (X = 1-4)
climate <- climate.data1 #### CHANGE BASED ON SIMULATION!!! i.e., bbdataX (X = 1-4)
climate$X <- NA ### needed in order to run... 

Results_SWA1 <- run_SW(absolute=TRUE, datafile, climate, refday) ## takes a while to run
write.csv(Results_SWA1[[2]], file="output/results_swa1.csv")
write.csv(Results_SWA1[[1]], file="output/sumstats_swa1.csv")

if(FALSE){
swa1 <- Results_SWA1[[2]]
swa2 <- Results_SWA2[[2]]
swa3 <- Results_SWA3[[2]]
swa4 <- Results_SWA4[[2]]
}


##### Now let's check out the results and see how these compare to our original simulation:
swa1 <- read.csv("output/results_swa1.csv")
swa1$degwarm <- 1
swa2 <- read.csv("output/results_swa2.csv")
swa2$degwarm <- 2
swa3 <- read.csv("output/results_swa3.csv")
swa3$degwarm <- 3
swa4 <- read.csv("output/results_swa4.csv")
swa4$degwarm <- 4

#Step 6: Whoop! Now we can caculate temperature sensitivities.
varstatsfx <- function(df){

  df$cc <- ifelse(df$X<=34, "precc", "postcc")
  estprecc <- lm(yvar~climate, data=subset(df, cc=="precc"))
  estpostcc <- lm(yvar~climate, data=subset(df, cc=="postcc"))

  diffbefore.after <- coef(estprecc)[2]-coef(estpostcc)[2]
# negative means a decline in sensitivity AFTER climate change
  draws <- rbind(draws, diffbefore.after)

# get a few more things
  preccdf <- subset(df, cc=="precc")
  postccdf <- subset(df, cc=="postcc")
  precc.sens <-  rbind(precc.sens, coef(estprecc)[2])
  postcc.sens <-  rbind(postcc.sens, coef(estpostcc)[2])
  var.lo.precc <- rbind(var.lo.precc, var(preccdf$yvar, na.rm=TRUE))
  var.lo.postcc <- rbind(var.lo.postcc, var(postccdf$yvar, na.rm=TRUE))


  # Okay, now build a df with a few things we want...
  df.return <- data.frame(cbind(draws, precc.sens, postcc.sens, var.lo.precc, var.lo.postcc), row.names = NULL) 
  names(df.return) <- c("diffbefore.after", "precc.sens", "postcc.sens", "var.lo.precc", "var.lo.postcc")
}





