# Started 29 October 2019 by Cat
## Using code from Emily Simmond's sliding window approach: https://github.com/emilygsimmonds/Cue_Identification

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(climwin)

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


set.seed(113)

## Okay we'll use similar code based on above to create df.sample
precc <- 10
postcc <- 12
sigma <- 5
preccbb <- 50
postccbb <- 45
sigmabb <- 8

x<-seq(as.Date("1951-01-01"),as.Date("2018-12-31"),by="day")

# Step 1: create budburst dataframe 
#Columns are: Year, bb_date (in character format (%Y-%d-%m)), bb_mean (essentially day of year of budburst), doy95 (day of year where 95% bursted bud)
bb <- data.frame(cbind(Year=as.numeric(substr(x,1,4))))
bb$cc <- ifelse(bb$Year>1985, "postcc", "precc")
bb <- bb[!duplicated(bb),]
 
bbpre <- c() 
bbpost <- c()
for (i in c(1:nrow(bb))){
  if (bb$cc[i]=="precc") {
    bbpre <- rpois(n = nrow(bb[(bb$cc=="precc"),]), preccbb)
  } else {
    bbpost <- rpois(n = nrow(bb[(bb$cc=="postcc"),]), postccbb)
  }
  bb_mean <- c(bbpre, bbpost)
}

bb <- data.frame(cbind(bb, bb_mean))
bbsw <- bb[(bb$Year>=1952 & bb$Year<=2018),]
bbsw$bb_date <- as.character(as.Date(bbsw$bb_mean, origin=as.Date(paste0(bbsw$Year, "-01-01"))))
bbsw$doy95 <- bbsw$bb_mean - 4

bbtest <- subset(bbsw, select=c("Year", "bb_date", "bb_mean", "doy95"))

# Step 2: create climate data
df <- data.frame(cbind(date=as.character(x), yday=yday(x), year=substr(x, 1, 4), cc=rep(c("precc"))))
df$cc <- ifelse(df$year>1985, "postcc", df$cc)
dailytemp <- c()
dailytemppre <- c() 
dailytemppost <- c()

for (i in c(1:nrow(df))){
  if (df$cc[i]=="precc") {
    dailytemppre <- rnorm(nrow(df[(df$cc=="precc"),]), precc, sigma)
  } else {
    dailytemppost <- rnorm(nrow(df[(df$cc=="postcc"),]), postcc, sigma)
  }
  dailytemp <- c(dailytemppre, dailytemppost)
}

clim <- data.frame(cbind(df, dailytemp))

# Columns are; date, year, yday (day of year), day (day of month), month, temp (daily mean temperature in degrees celsius)
climdata <- clim
climdata$day <- as.numeric(substr(climdata$date, 9, 10))
climdata$month <- as.numeric(substr(climdata$date, 6, 7))
climdata$temp <- climdata$dailytemp

climtest <- subset(climdata, select=c("date", "year", "yday", "day", "month", "temp")) 

# refday = c(day, mon)
refday <- c(15, 02)
# climate is a datafile that must include col = temp
# datafile = biological data
# default = absolute but can also run relative
#run_SW <- function(absolute = TRUE, datafile, climate, refday)

### Now checking Simmond's sliding window approach:
source("Run_SW.R")

datafile <- bbtest
climate <- climtest

run_SW(absolute=TRUE, datafile, climate, refday) ## takes a while to run
