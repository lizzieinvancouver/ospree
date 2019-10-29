## Code from Lizzie's pepvarsim.R
# Started 29 October 2019 by Cat
## Using code from Emily Simmond's sliding window approach: https://github.com/emilygsimmonds/Cue_Identification

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

set.seed(113)

# Need to simulate data as GDD system; only really need forcing #

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

figpath <- "figures"

source("pepvarsimfxs.R")

## Okay we'll use similar code based on above to create df.sample
daysperyr <- 80
endyr <- 2018
yrs <- rep(1950:endyr, each=daysperyr)
dayz <- rep(1:daysperyr, 69)
precc <- 10
postcc <- 12
sigma <- 5
fstar <- 400
yrstotal <- 69

cc <- c(rep("precc", yrstotal/2), rep("postcc", yrstotal/2))

# Step 2: here we go, set up some empty vectors, then fill with random draws based on temps above
dailytemp <- c()
gdd <- c()

for (i in 1:yrstotal){
  if (i<(yrstotal/2+1)) {
    dailytemphere <- rnorm(daysperyr, precc, sigma)
  } else {
    dailytemphere <- rnorm(daysperyr, postcc, sigma)
  }
  dailytemp <- c(dailytemp, dailytemphere)
  gdd <- c(gdd, cumsum(dailytemphere))
}
# Step 3: Make a data frame and get the mean temp per year (needed later to calculate sensitivities)
df <- data.frame(cbind(yrs, dayz, dailytemp, gdd))
df.meantemp <- aggregate(df["dailytemp"], df[c("yrs")], FUN=mean)
#plot(dailytemp ~ yrs, data=df.meantemp)

# Step 4: Now, in a very slow, painful way I get the BB date
df$bb.YN <- NA

for (i in c(1:nrow(df))){ # This loop just makes a Yes/No vector for budburst
  if (df$gdd[i]<fstar) {
    df$bb.YN[i] <- "N"
  } else {
    df$bb.YN[i] <- "Y"
  }
}

# Step 5: Now we remove rows based on sampling frequency and then calculate the observed BB date
df.sw <- df

#Columns are; Year (year of study), lay_date (annual population mean lay date in YYYY-MM-DD format), lay_mean (annual population mean lay date as day of year), doy95 (day of the year by which 95% of the population have laid)
biodata <- subset(df.sw, select=c("yrs", "dayz", "bb.YN", "gdd"))
biodata <- biodata[(biodata$yrs>=1951),]
biodata <- biodata[(biodata$bb.YN=="Y"),]
biodata$bb_day <- ave(biodata$dayz, biodata$yrs, FUN=min)
biodata$bb_date <- as.character(biodata$bb_day, origin=paste0("01/01",as.character(biodata$yrs)))
biodata$bb_mean <- mean(biodata$bb_day)

biodata <- subset(biodata, select=c("yrs", "bb_date", "bb_mean"))
names(biodata) <- c("Year", "bb_date", "bb_mean")

# Columns are; date, year, yday (day of year), day (day of month), month, temp (daily mean temperature in degrees celsius)
climdata <- subset(df.sw, select=c("yrs", "dayz", "dailytemp"))
climdata$date <- as.Date(climdata$dayz, origin=paste0("01/01",as.character(climdata$yrs))
climdata$year <- climdata$yrs
climdata$yday <- climdata$dayz
climdata$day <- substr(climdata$date, 6, 7)
climdata$month <- substr(climdata$date, 9, 10)
climdata$temp <- climdata$dailytemp

climdata <- subset(climdata, select=c("date", "year", "yday", "day", "month", "temp"))

refday <- c(01, 03)

# refday = c(day, mon)
# climate is a datafile that must include col = temp
# datafile = biological data
# default = absolute but can also run relative
#run_SW <- function(absolute = TRUE, datafile, climate, refday)

### Now checking Simmond's sliding window approach:
library(climwin)
source("simmondsswfx.R")

run_SW(absolute=TRUE, biodata, climdata, refday)




