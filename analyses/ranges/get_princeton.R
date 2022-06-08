## Started 7 June 2022 ##
## By Lizzie ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# library
library(HelpersMG)

setwd("~/Desktop/princetonclimate")

# make lists of what we want...
yrz <- c(1979:2016) # start at 1990

tminlist <- c()

for (i in yrz){
    tminlistadd <- paste("tmin_daily_", i, "-", i, ".nc", sep="")
    tminlist <- c(tminlist, tminlistadd)
}

tmaxlist <- c()

for (i in yrz){
    tmaxlistadd <- paste("tmax_daily_", i, "-", i, ".nc", sep="")
    tmaxlist <- c(tmaxlist, tmaxlistadd)
    }

for (i in tmaxlist){
    wget(c(paste("http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/", i, sep="")), options(timeout = max(600, getOption("timeout"))))
         }

for (i in tminlist){
    wget(c(paste("http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/", i, sep="")), options(timeout = max(1000, getOption("timeout"))))
         }
