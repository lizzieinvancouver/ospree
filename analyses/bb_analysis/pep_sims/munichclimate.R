## Started 15 June 2019 ##
## By Lizzie ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis/pep_sims")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis/pep_sims") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims")

figpath <- "figures"

d <- read.csv("input/munichclim_noheader.csv", header=FALSE)
names(d) <- c("year", "month", "day", "meanC")
d$date <- as.Date(paste(d$year, d$month, d$day, sep="-"), format="%Y-%m-%d")
d$doy <- as.numeric(format(d$date, "%j"))

dsm <- subset(d, year>1949)
dsm$when <- NA
dsm$when[dsm$year<1981] <- "before"
dsm$when[dsm$year>1980] <- "after"

dsm.agg <- aggregate(dsm[c("meanC")], dsm[c("doy", "when")], FUN=mean)
dsm.agg.spr <- subset(dsm.agg, doy<150)

plot(meanC~doy, data=dsm.agg.spr, type="n")
lines(meanC~doy, data=subset(dsm.agg.spr, when=="before"), col="blue")
lines(meanC~doy, data=subset(dsm.agg.spr, when=="after"), col="darkred")
abline(v=115, col="blue")
abline(v=105, col="darkred")

afterLO <- subset(dsm.agg.spr, when=="after" & doy<105 & doy>(105-30))
beforeLO <- subset(dsm.agg.spr, when=="before" & doy<115 & doy>(115-30))

mean(afterLO$meanC)
mean(beforeLO$meanC)
