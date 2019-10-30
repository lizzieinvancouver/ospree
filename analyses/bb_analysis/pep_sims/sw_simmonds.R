## Code from Lizzie's pepvarsim.R
# Started 29 October 2019 by Cat
## Using code from Emily Simmond's sliding window approach: https://github.com/emilygsimmonds/Cue_Identification

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(lubridate)
library(climwin)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis/pep_sims")
} else if(length(grep("lizzie", getwd()))>0) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis/pep_sims") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis/pep_sims")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims")


set.seed(113)

## Okay we'll use similar code based on above to create df.sample
daysperyr <- 80
nsites <- 15
endyr <- 2019
yrs <- rep(1950:endyr)
precc <- 10
postcc <- 12
sigma <- 5
preccbb <- 50
postccbb <- 45
sigmabb <- 8
yrstotal <- 70
dayz <- rep(1:daysperyr, yrstotal)

cc <- c(rep("precc", yrstotal/2), rep("postcc", yrstotal/2))
 
bb <- c() 
for (i in 1:yrstotal){
  if (i<(yrstotal/2+1)) {
    bbhere <- rpois(n = nsites, preccbb)
  } else {
    bbhere <- rpois(n = nsites, postccbb)
  }
  bb <- c(bb, bbhere)
}


# Step 2: here we go, set up some empty vectors, then fill with random draws based on temps above
dailytemp <- c()

for (i in 1:yrstotal){
  if (i<(yrstotal/2+1)) {
    dailytemphere <- rnorm((daysperyr*nsites), precc, sigma)
  } else {
    dailytemphere <- rnorm((daysperyr*nsites), postcc, sigma)
  }
  dailytemp <- c(dailytemp, dailytemphere)
}
# Step 3: Make a data frame and get the mean temp per year (needed later to calculate sensitivities)
clim <- data.frame(cbind(year=rep(yrs, each=daysperyr), dayz, dailytemp)) #, gdd
bbdata <- data.frame(cbind(bb, site=rep(1:nsites), year=rep(yrs, each=nsites)))

#Columns are; Year (year of study), lay_date (annual population mean lay date in YYYY-MM-DD format), lay_mean (annual population mean lay date as day of year), doy95 (day of the year by which 95% of the population have laid)
biodata <- bbdata[(bbdata$year>=1951 & bbdata$year<=2018),]
biodata$bb_date <- as.Date(biodata$bb, origin=as.Date(paste0(as.character(biodata$year), "-01-01")))
biodata$bb_mean <- ave(biodata$bb, biodata$year)
biodata$doy95 <- biodata$bb - 4

biodata <- subset(biodata, select=c("year", "bb_date", "bb_mean", "doy95"))
names(biodata) <- c("Year", "bb_date", "bb_mean", "doy95")

# Columns are; date, year, yday (day of year), day (day of month), month, temp (daily mean temperature in degrees celsius)
climdata <- clim
climdata$date <- as.Date(climdata$dayz, origin=as.Date(paste0(as.character(climdata$year), "-01-01")))
climdata$yday <- climdata$dayz
climdata$day <- substr(climdata$date, 6, 7)
climdata$month <- substr(climdata$date, 9, 10)
climdata$temp <- climdata$dailytemp
climdata$spatial <- rep(1:nsites, each=daysperyr*yrstotal)

climdata <- subset(climdata, select=c("date", "year", "yday", "day", "month", "temp", "spatial"))

refday <- c(15, 02)

# refday = c(day, mon)
# climate is a datafile that must include col = temp
# datafile = biological data
# default = absolute but can also run relative
#run_SW <- function(absolute = TRUE, datafile, climate, refday)

### Now checking Simmond's sliding window approach:
source("simmondsswfx.R")

run_SW(absolute=TRUE, biodata, climdata, refday)

## Getting the following error which doesn't make sense...
# Initialising, please wait...
#Error in convertdate(bdate = bdate, cdate = cdate, xvar = xvar, cinterval = cinterval,  : 
 #                      Climate data does not cover all years of biological data at site 1. 
  #                   Latest climate data is 2019-03-22 Latest biological data is 2018-02-27. 
   #                  Please increase range of climate data


