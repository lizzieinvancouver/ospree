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

# fix malyshev18 issues: field sampel date was originally incorrect; DSS scraped missing x axis from Fig 2 in Apr 2020 to help
# We're going to do something semi-dangerous here (note to Lizzie: you did this); we're going to paste in the new data with lots of checks for safety.. 
nrow(dss[which(dss$datasetID=="malyshev18"),])==nrow(maly18add) # good
identical(paste(dss$genus[which(dss$datasetID=="malyshev18")],
    dss$species[which(dss$datasetID=="malyshev18")]), (paste(maly18add$genus, maly18add$species))) # good
# det to field sample date
maly18add$dayssince_dec1stadj <- round(maly18add$dayssince_dec1st)
getdec1 <- format(as.Date("1-12-2013", format="%d-%m-%Y"), "%j")
maly18add$dayssince_dec1stadjmore <- maly18add$dayssince_dec1stadj+as.numeric(getdec1)
maly18add$fieldsampledate <- as.Date(NA)
maly18add$fieldsampledate[which(maly18add$dayssince_dec1stadjmore<=365)] <-
    as.Date(paste(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore<=365)],
    rep("2013", length(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore<=365)]))), "%j %Y")
maly18add$fieldsampledate[which(maly18add$dayssince_dec1stadjmore>365)] <-
    as.Date(paste(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore>365)]-365,
    rep("2014", length(maly18add$dayssince_dec1stadjmore[which(maly18add$dayssince_dec1stadjmore>365)]))), "%j %Y")
dss$fieldsample.date[which(dss$datasetID=="malyshev18")] <- format(maly18add$fieldsampledate, format="%Y-%b-%d")

if(FALSE){
# Check by plotting and compare to paper
  malycheck <- dss[which(dss$datasetID=="malyshev18"),]
  anothercheck <- data.frame(response.time=dss$response.time[which(dss$datasetID=="malyshev18")],
      dayssince=maly18add$dayssince_dec1st, photoperiod_day=dss$photoperiod_day[which(dss$datasetID=="malyshev18")],
      genus=dss$genus[which(dss$datasetID=="malyshev18")])
  library(ggplot2)
  ggplot(malycheck, aes(fieldsample.date, response.time, group=photoperiod_day, col=photoperiod_day))+
      geom_point() + facet_wrap(genus~.)
  ggplot(anothercheck, aes(dayssince, response.time, group=photoperiod_day, col=photoperiod_day))+
      geom_point() + facet_wrap(genus~.)
}

# next steps  ...
# (1) Figure out why the plot from malycheck does not look like Figure 2 (or another check) ... maybe date is not plotting properly?
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
