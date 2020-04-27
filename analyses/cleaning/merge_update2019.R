## Started in August 2019 (ish) ##
## Merge the 2019 OSPREE data update ##
## By Lizzie so far ... ###

## Also, some significant clean up of malyshev18 hapens here ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses")
}else if 
(length(grep("faith", getwd()))>0) {setwd("~/Documents/github/ospree/analyses")
} else 
setwd("~/Documents/git/ospree/analyses")


# Get the data
cjc <- read.csv("input/update2019/ospree_2019update_cjc.csv")
dl <- read.csv("input/update2019/ospree_2019update_dl.csv")
dmb <- read.csv("input/update2019/ospree_2019update_dmb.csv")
dss <- read.csv("input/update2019/ospree_2019update_DSS.csv")
ks <- read.csv("input/update2019/ospree_2019update_ks.csv")
my <- read.csv("input/update2019/ospree_2019update_my.csv")
maly18add <- read.csv("input/update2019/ospree_2019update_DSSaddmaly18.csv")

# See how the data looks and do a little clean-up
dim(cjc)
head(cjc)
tail(cjc)

dim(dl)
dim(dmb)
dim(dss) # dim is wrong
dim(ks)
dim(my)
head(dss[,54:75])
dss[,54:75] <- NULL
dim(dss)

# Malyshev18 was mis-entered -- field sample date was originally incorrect; DSS scraped missing x axis from Fig 2 in Apr 2020 to help
# Below is a painful fix using scraped field sample dates, but it should be working (as best we can tell) 
nrow(dss[which(dss$datasetID=="malyshev18"),])==nrow(maly18add) # good - there are the same no. rows in the original data
#and the new data with correct dates, so suggests there are the same number of data points (181)
identical(paste(dss$genus[which(dss$datasetID=="malyshev18")], # correct species names - they all match 
    dss$species[which(dss$datasetID=="malyshev18")]), (paste(maly18add$genus, maly18add$species))) # good
# Now, get from the 'days since Dec 1' that the figure gives to a field sample date
maly18add$dayssince_dec1stadj <- round(maly18add$dayssince_dec1st) # rounding up dates because day 13.11 after Dec 1st is silly 
getdec1 <- format(as.Date("1-12-2013", format="%d-%m-%Y"), "%j")
maly18add$dayssince_dec1stadjmore <- maly18add$dayssince_dec1stadj+as.numeric(getdec1) # add day of year for 1st Dec to each day to get real day of year 
maly18add$fieldsampledate <- as.Date(NA) # tell R this new column will hold a date 
maly18add$fieldsampledate[which(maly18add$dayssince_dec1stadjmore<=365)] <- # for days of year that are 365 or less
    as.Date(paste(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore<=365)], # add the year (2013) to the column, and convert to proper dates
    rep("2013", length(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore<=365)]))), "%j %Y")
maly18add$fieldsampledate[which(maly18add$dayssince_dec1stadjmore>365)] <- # for days of year more than 365, this means they are actually in 2014.
    as.Date(paste(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore>365)]-365,
    rep("2014", length(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore>365)]))), "%j %Y")

dss$fieldsample.date[which(dss$datasetID=="malyshev18")] <- format(maly18add$fieldsampledate, format="%Y-%b-%d")# replace dates in malyshev18.
# this code assumes the data ae in the exact same row order (that's dangerous bit, see commented out code below where we check it)

if(FALSE){
# Check by plotting and compare to paper
  malycheck <- dss[which(dss$datasetID=="malyshev18"),]
  anothercheck <- data.frame(response.time=dss$response.time[which(dss$datasetID=="malyshev18")],
      dayssince=maly18add$dayssince_dec1st, photoperiod_day=dss$photoperiod_day[which(dss$datasetID=="malyshev18")],
      genus=dss$genus[which(dss$datasetID=="malyshev18")])
  library(ggplot2)
  ggplot(malycheck, aes(fieldsample.date, response.time, group=photoperiod_day, col=photoperiod_day))+
      geom_point() + facet_wrap(genus~.) # looks wrong, but is because date format is not the same as paper
  ggplot(anothercheck, aes(dayssince, response.time, group=photoperiod_day, col=photoperiod_day))+
      geom_point() + facet_wrap(genus~.) # look good
  plot(x = malycheck$fieldsample.date, y = malycheck$dayssince_dec1st) # the days since Dec 1st seems to match with new dates
  # Remaining issue .. 
  max(malycheck$response.time[malycheck$genus == "Acer"])
}
 
## end of fix malyshev18

# fix weird name
dmb$datasetID[dmb$datasetID=="fu_2018"] <- "fu18"

# extract year for ramos17
checknayear <- ks[which(is.na(ks$year)==TRUE),]
unique(checknayear$datasetID)
ks$year[which(is.na(ks$year)==TRUE)] <- format(as.Date(ks$fieldsample.date[which(is.na(ks$year)==TRUE)],
    format="%d/%b/%Y"), "%Y")
ks[which(is.na(ks$year)==TRUE),]

# Put the data together (not pretty but not really worth it to apply now)
d <- rbind(cjc, dl)
d <- rbind(d, dmb)
d <- rbind(d, dss)
d <- rbind(d, ks)
d <- rbind(d, my)

sum(nrow(cjc), nrow(dl), nrow(dmb), nrow(dss), nrow(ks), nrow(my)) # check! 

write.csv(d, "output/ospree2019update.csv", row.names=FALSE)

