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
maly18add <- read.csv("input/update2019/ospree_2019update_DSSaddmaly18.csv") #fj couldnt fine thsi file. I found it ina different place though 
maly18add <-read.csv("/home/faith/Documents/github/ospree/data/update2019/ospree_2019update_DSSaddmaly18.csv")
maly18add <-read.csv("~/Documents/git/ospree/data/update2019/ospree_2019update_DSSaddmaly18.csv")

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

#Faith looking at the data
head(dss[dss$datasetID=="malyshev18",])
tail(dss[dss$datasetID=="malyshev18",])
malData <- dss[dss$datasetID=="malyshev18",]
malData$fieldsample.date # the problem at the moment is that all the dates say 31st October, when actually there shoudl eb different feild sample dates
#this causes problems for calculating chill
head(maly18add)

# fix malyshev18 issues: field sampel date was originally incorrect; DSS scraped missing x axis from Fig 2 in Apr 2020 to help
# We're going to do something semi-dangerous here (note to Lizzie: you did this); we're going to paste in the new data with lots of checks for safety.. 
nrow(dss[which(dss$datasetID=="malyshev18"),])==nrow(maly18add) # good - there are the same no. rows in the original data
#and the new data with correct dates, so suggests there are the same number of data points (181)
#is 181 teh correct number of samples though? It can be tricky to see teh exact number of data points from the grafs 
#because black point can overlap hollow points. FJ had a quick count and also came up with 181 data points, but there was some 
#ambiguity  
identical(paste(dss$genus[which(dss$datasetID=="malyshev18")], #correct species names - they all match 
    dss$species[which(dss$datasetID=="malyshev18")]), (paste(maly18add$genus, maly18add$species))) # good
# det to field sample date
maly18add$dayssince_dec1stadj <- round(maly18add$dayssince_dec1st) #rounding up dates because day 13.11 after Dec 1st is silly 
getdec1 <- format(as.Date("1-12-2013", format="%d-%m-%Y"), "%j")
maly18add$dayssince_dec1stadjmore <- maly18add$dayssince_dec1stadj+as.numeric(getdec1) #add day of year for 1st Dec to each day to get real day of year 
maly18add$fieldsampledate <- as.Date(NA)#tell R this new column will hold a date 
maly18add$fieldsampledate[which(maly18add$dayssince_dec1stadjmore<=365)] <- # for days of year that are 365 or less
    as.Date(paste(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore<=365)], # add the year (2013) to the column, and convert to proper dates
    rep("2013", length(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore<=365)]))), "%j %Y")
maly18add$fieldsampledate[which(maly18add$dayssince_dec1stadjmore>365)] <- # for days of year more than 365, this means they are actually in 2014.
    as.Date(paste(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore>365)]-365,
    rep("2014", length(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore>365)]))), "%j %Y")

dss$fieldsample.date[which(dss$datasetID=="malyshev18")] <- format(maly18add$fieldsampledate, format="%Y-%b-%d")#replace dates in malyshev18.
#this code assumes the data ae in the exact same row order. 

if(FALSE){
# Check by plotting and compare to paper
  malycheck <- dss[which(dss$datasetID=="malyshev18"),]
  str(malycheck) # at the moment malucheck$feildsampledate is a character rather than a Date 
  #when you convert it to a date, then the plots look fine. (exept for one data point)
  anothercheck <- data.frame(response.time=dss$response.time[which(dss$datasetID=="malyshev18")],
      dayssince=maly18add$dayssince_dec1st, photoperiod_day=dss$photoperiod_day[which(dss$datasetID=="malyshev18")],
      genus=dss$genus[which(dss$datasetID=="malyshev18")])
  library(ggplot2)
  ggplot(malycheck, aes(fieldsample.date, response.time, group=photoperiod_day, col=photoperiod_day))+
      geom_point() + facet_wrap(genus~.)
  ggplot(anothercheck, aes(dayssince, response.time, group=photoperiod_day, col=photoperiod_day))+
      geom_point() + facet_wrap(genus~.)
}

#More plotting to compare - Fj playing with data 
head(dss)
head(malycheck)#from main dss, selcted response time, photo period, days sincedec1st, genus. Old data.
malycheck$dayssince_dec1st  <- maly18add$dayssince_dec1st # add in teh days since dec 1 data to our "final" dataset
malycheck$fieldsample.date <- as.Date(malycheck$fieldsample.date, "%Y-%b-%d")
malycheck$photoperiod_day <- as.factor(malycheck$photoperiod_day)
plot(x = malycheck$fieldsample.date, y = malycheck$dayssince_dec1st)# the days since ce1st seems to match with new dates

#this plot looks ok, exept for the one odd point in Acer. But Lizzies above looks wrong. Why? Answer: I think the date format?
properDates <- ggplot(data = malycheck, aes(x = fieldsample.date, y = response.time, colour = photoperiod_day))
properDates + geom_point() + facet_wrap(genus~.)

sinceDec1dates <- ggplot(data = malycheck, aes(x = dayssince_dec1st, y = response.time, colour = photoperiod_day))
sinceDec1dates + geom_point() + facet_wrap(genus~.)

max(malycheck$response.time[malycheck$genus == "Acer"])
# next steps  ...
# (1) Figure out why the plot from malycheck does not look like Figure 2 (or another check) ... maybe date is not plotting properly?
# fj - I dont understand what teh problem is with this figure. Too me it looks the same, exept that species are in a different order
#and there is only the top pannel of Figure 2. This is how it shoud lbe though because we didnt want to scrape the lower pannel data (chilling hours)
#because we estimate that from feild sample date?

# (2) If there is no clear fix, I think the only good option is to re-scrape the data ... unless someone sees another way
# (3) For all of dss, chang ex1 or ex2 etc. to exp1, tidy up anything else you seein dss
 
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

