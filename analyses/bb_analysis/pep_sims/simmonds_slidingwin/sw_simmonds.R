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
bbdatapre1 <- bbdata1[which(bbdata1$Year<=1979 & bbdata1$Year>=1970),]
bbdatapost1 <-  bbdata1[which(bbdata1$Year>=1981 & bbdata1$Year<=1990),]
climate.data1 <- sim1[[2]]
climate.datapre1 <- subset(climate.data1, climate.data1$year<1980)
climate.datapost1 <- subset(climate.data1, climate.data1$year>=1980)

bbdata2 <- sim2[[1]]
bbdatapre2 <- bbdata2[which(bbdata2$Year<=1979 & bbdata2$Year>=1970),]
bbdatapost2 <-  bbdata2[which(bbdata2$Year>=1981 & bbdata2$Year<=1990),]
climate.data2 <- sim2[[2]]
climate.datapre2 <- subset(climate.data2, climate.data2$year<1980)
climate.datapost2 <- subset(climate.data2, climate.data2$year>=1980)

bbdata3 <- sim3[[1]]
bbdatapre3 <- bbdata3[which(bbdata3$Year<=1979 & bbdata3$Year>=1970),]
bbdatapost3 <-  bbdata3[which(bbdata3$Year>=1981 & bbdata3$Year<=1990),]
climate.data3 <- sim3[[2]]
climate.datapre3 <- subset(climate.data3, climate.data3$year<1980)
climate.datapost3 <- subset(climate.data3, climate.data3$year>=1980)

bbdata4 <- sim4[[1]]
bbdatapre4 <- bbdata4[which(bbdata4$Year<=1979 & bbdata4$Year>=1970),]
bbdatapost4 <-  bbdata4[which(bbdata4$Year>=1981 & bbdata4$Year<=1990),]
climate.data4 <- sim4[[2]]
climate.datapre4 <- subset(climate.data4, climate.data4$year<1980)
climate.datapost4 <- subset(climate.data4, climate.data4$year>=1980)
  
  
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
refday <- c(01, 02)
datafile <- bbdatapost3 #### CHANGE BASED ON SIMULATION!!! i.e., bbdataX (X = 1-4)
climate <- climate.datapost3 #### CHANGE BASED ON SIMULATION!!! i.e., bbdataX (X = 1-4)
climate$X <- NA ### needed in order to run... 

Results_SWRpost3 <- run_SW(absolute=TRUE, datafile, climate, refday) ## takes a while to run
write.csv(Results_SWRpost3[[2]], file="output/results_swrpost3.csv")
write.csv(Results_SWRpost3[[1]], file="output/sumstats_swrpost3.csv")

if(FALSE){
## Get data and parameters for prediction
source('Params_SW.R')

# extract parameters for complete dataset
Parameters_SWR1 <- get_params_SW(Results_SWR1, bbdata1$bb_mean, "complete", type = "Params")
# SAVE
write.csv(Parameters_SWR1, "output/parameters_swr1.csv", row.names=T)

# extract climate data for complete dataset 'best' window
Results_data_SWR1 <- get_params_SW(Results_SWR1, bbdata1$bb_mean, "complete", type = "Data")
# SAVE
write.csv(Results_data_SWR1, "output/results_data_swr1.csv")
}


if(FALSE){
swapre1 <- Results_SWRpre1[[2]]
swapost1 <- Results_SWRpost1[[2]]
swapre2 <- Results_SWRpre2[[2]]
swapost2 <- Results_SWRpost2[[2]]
swapre3 <- Results_SWRpre3[[2]]
swapost3 <- Results_SWRpost3[[2]]
swapre4 <- Results_SWRpre4[[2]]
swapost4 <- Results_SWRpost4[[2]]

swapre1_stat <- Results_SWRpre1[[1]]
swapost1_stat <- Results_SWRpost1[[1]]
swapre2_stat <- Results_SWRpre2[[1]]
swapost2_stat <- Results_SWRpost2[[1]]
swapre3_stat <- Results_SWRpre3[[1]]
swapost3_stat <- Results_SWRpost3[[1]]
swapre4_stat <- Results_SWRpre4[[1]]
swapost4_stat <- Results_SWRpost4[[1]]
}


##### Now let's check out the results and see how these compare to our original simulation:
swapre1 <- read.csv("output/results_swrpre1.csv")
swapost1 <- read.csv("output/results_swrpost1.csv")
swapre1$cc <- "precc"
swapost1$cc <- "postcc"
swa1 <- rbind(swapre1, swapost1)
swa1$degwarm <- 1

swapre2 <- read.csv("output/results_swrpre2.csv")
swapost2 <- read.csv("output/results_swrpost2.csv")
swapre2$cc <- "precc"
swapost2$cc <- "postcc"
swa2 <- rbind(swapre2, swapost2)
swa2$degwarm <- 2

swapre3 <- read.csv("output/results_swrpre3.csv")
swapost3 <- read.csv("output/results_swrpost3.csv")
swapre3$cc <- "precc"
swapost3$cc <- "postcc"
swa3 <- rbind(swapre3, swapost3)
swa3$degwarm <- 3

swapre4 <- read.csv("output/results_swrpre4.csv")
swapost4 <- read.csv("output/results_swrpost4.csv")
swapre4$cc <- "precc"
swapost4$cc <- "postcc"
swa4 <- rbind(swapre4, swapost4)
swa4$degwarm <- 4


draws <- c()
precc.sens <- c()
postcc.sens <- c()
var.lo.precc <- c()
var.lo.postcc <- c()
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

  return(df.return)
}

swa1_results <- varstatsfx(swa1)


degwarmalt.runs <- rbind(sim.1deg.alt, sim.2deg.alt, sim.3deg.alt, sim.4deg.alt)
write.csv(degwarmalt.runs, "output/degwarmpepsims6CFstar150.csv", row.names=FALSE)

meanhere.alt <- aggregate(degwarmalt.runs[c("diffbefore.after", "precc.sens", "postcc.sens",
                                            "var.lo.precc", "var.lo.postcc")], degwarmalt.runs["degwarm"], FUN=mean)

sdhere.alt <- aggregate(degwarmalt.runs[c("diffbefore.after", "precc.sens", "postcc.sens",
                                          "var.lo.precc", "var.lo.postcc")], degwarmalt.runs["degwarm"], FUN=sd)



