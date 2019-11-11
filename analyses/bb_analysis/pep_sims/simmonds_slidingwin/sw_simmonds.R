# Started 29 October 2019 by Cat
## Using code from Emily Simmond's sliding window approach: https://github.com/emilygsimmonds/Cue_Identification

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(climwin)
library(lubridate)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd()))>0) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


# Get budburst data across 45 sites for BETPEN
# Betula puendula data from PEP (both have has GDD from 1 Jan to leafout)
# bp has mat from March 1st to June 1st and mat.lo is 30 days before leafout (uses tg -- aka mean -- data from E-OBS)
# bpalt is similar, but calculated uses txtm -- aka min and max (and we caculate the mean ourselves from those values) -- data from E-OBS) ... we don't use this currently 
bp <- read.csv("PEP_climate/output/betpen_allchillsandgdds_45sites_mat_forsims.csv", header=TRUE)
  
bbsw <- subset(bp, select=c("year", "lo", "siteslist"))
bbsw$bb_date <- as.Date(bbsw$lo, origin=paste0(bbsw$year, "-01-01"))
bbsw$bb_date <- as.character(bbsw$bb_date)
bbsw$doy95 <- bbsw$lo - 4

bbsw <- subset(bbsw, select=c("year", "bb_date", "lo", "doy95", "siteslist"))
colnames(bbsw) <- c("Year", "bb_date", "bb_mean", "doy95", "spatial")
bbsw$bb_date <- as.character(bbsw$bb_date)

bbswpre <- bbsw[(bbsw$Year<=1960),]
bbswpost <- bbsw[(bbsw$Year>1960),]

bbswtest <- bbswpre[(bbswpre$spatial==1),]

### Now get the climate data for 45 sites for BETPEN (from betpen_climate_slidingwin.R)
climatedatapre <- read.csv("pep_sims/simmonds_slidingwin/bp_climatedatapre.csv")
climatedatapost <- read.csv("pep_sims/simmonds_slidingwin/bp_climatedatapost.csv")
  
source("pep_sims/simmonds_slidingwin/Run_SW.R")
# refday = c(day, mon)
# climate is a datafile that must include col = temp
# datafile = biological data
# default = absolute but can also run relative
#run_SW <- function(absolute = TRUE, datafile, climate, refday)

### Now checking Simmond's sliding window approach:
refday <- c(31, 05) ### results in folders are from a ref day of 01-03, I think this new ref day is more appropriate for PEP leafout data - to rerun
datafile <- bbswpre
climate <- climatedatapre
climate$X <- NA ### needed in order to run... 

Results_SWRpre <- run_SW(absolute=TRUE, datafile, climate, refday) ## takes a long time to run
write.csv(Results_SWRpre[[2]], file="pep_sims/simmonds_slidingwin/output/results_swapre_bp_mayref.csv")
write.csv(Results_SWRpre[[1]], file="pep_sims/simmonds_slidingwin/output/sumstats_swapre_bp_mayref.csv")


## Get data and parameters for prediction
source('pep_sims/simmonds_slidingwin/Params_SW.R')

# extract parameters for complete dataset
Parameters_SWRpost <- get_params_SW(Results_SWRpost, bbswpost$bb_mean, "complete", type = "Params")
# SAVE
write.csv(Parameters_SWRpost, "pep_sims/simmonds_slidingwin/output/parameters_swapost.csv", row.names=T)


### Now let's check out the data
swapre <- Results_SWRpre[[2]]
swapost <- Results_SWRpost[[2]]

swapre_stat <- Results_SWRpre[[1]]
swapost_stat <- Results_SWRpost[[1]]


## Now using code from `bb_analysis/PEP_climate/comparetopepsims.R`
swapre$site <- rep(1:45)
swapost$site <- rep(1:45)

swa <- rbind(swapre, swapost)
swa$cc <- rep(c("pre", "post"), each=450)

bpest <- data.frame(siteslist=numeric(), cc=character(), meanbb=numeric(), varbb=numeric(),  
                    sdbb=numeric(), meanclim=numeric(), varclim=numeric(), sdclim=numeric(),
                    climslope=numeric(), climslopese=numeric())


sitez <- 1:45

for(i in c(1:length(sitez))){ # i <- 1
  subby <- subset(swa, site==sitez[i])
  for(ccstate in c(1:2)){
    subbycc <- subset(subby, cc==unique(swa$cc)[ccstate])
    meanbb <- mean(subbycc$yvar, na.rm=TRUE)
    varbb <- var(subbycc$yvar, na.rm=TRUE)
    sdbb <- sd(subbycc$yvar, na.rm=TRUE)
    meanclim <- mean(subbycc$climate, na.rm=TRUE)
    varclim <- var(subbycc$climate, na.rm=TRUE)
    sdclim <- sd(subbycc$climate, na.rm=TRUE)
    lmclim <- lm(yvar~climate, data=subbycc)
    lmclimse <- summary(lmclim)$coef[2,2]
    bpestadd <- data.frame(siteslist=sitez[i], cc=unique(swa$cc)[ccstate], meanbb=meanbb, 
                           varbb=varbb, sdbb=sdbb, meanclim=meanclim, varclim=varclim, sdclim=sdclim, 
                           climslope=coef(lmclim)["climate"], climslopese=lmclimse)
    bpest <- rbind(bpest, bpestadd)
  }
}    

meanhere <- aggregate(bpest[c("meanbb", "varbb", "sdbb", "meanclim", "varclim", "sdclim", 
                              "climslope", "climslopese")], bpest["cc"], FUN=mean)
sdhere <- aggregate(bpest[c("meanbb", "varbb", "meanclim", "varclim","climslope")],
                    bpest["cc"], FUN=sd)

#cc            meanbb     varbb      sdbb   meanclim   varclim   sdclim climslope   climslopese
#1 post       106.3356  45.47136  6.510241 14.49351  1.391118 1.110196 -2.048819  2.111988
#2  pre       113.8089 132.02667 11.087655 10.81471 10.969837 2.826528 -2.683118  1.468094


#   cc   meanbb    varbb  meanclim   varclim  climslope
#1 post 4.759305 27.06196 1.436775  1.062947 2.605993
#2  pre 3.926242 67.44772 1.637718 12.125168 1.923587



#### Compared to Lizzie's approach to BETPEN PEP data:
#          cc  meanmat   varmat    sdmat meanmatlo varmatlo  sdmatlo   meanlo     varlo     sdlo meanutah  meangdd  matslope matslopese
# 1950-1960 5.365163 3.005094 1.731358  6.814883 1.363054 1.086849 113.8089 110.51111 10.25803 2246.987 68.70881 -4.534630   1.258845
# 2000-2010 6.450939 1.251629 1.111780  6.615273 1.431603 1.152353 106.3356  46.95728  6.57374 2235.493 61.50754 -3.611025   1.579758



#### But when we compare to sliding window approach:
Parameters_SWRpre
#     WindowOpen WindowClose Variable      Int       EST        SE        R2
#1       -239        -238        1     136.3565 -2.084899 0.1192322 0.4043209

Parameters_SWRpost
#      WindowOpen WindowClose Variable      Int       EST        SE        R2
#1       -184        -176        2     143.0215 -2.531195 0.1695477 0.3307278



