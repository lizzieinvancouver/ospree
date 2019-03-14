## Started 2 March 2019 ##
## By Lizzie ##

# Start your own R file to make a f(x) to count interactive experiments, ideally for all possible intxns across photoperiod, chilling, forcing, and field sample date (maybe provenance) treatments! #

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

### Libraries
library(dplyr)
library(lubridate)

if(FALSE){
# Build a simple example
nointxn <- data.frame(datasetID=rep("bob12", 8), study=c(rep("exp2", 4),
    rep("exp2", 4)), photoperiod_day=c(8, 8, 8, 8, 8, 8, 12, 12),
    forcetemp=c(15, 15, 18, 18, 15, 15, 15, 15), chilltemp=c(NA,NA,NA,NA,NA,NA,NA,NA),
    chilldays=c(NA,NA,NA,NA,NA,NA,NA,NA), fieldsample.date2=c(NA,NA,NA,NA,NA,NA,NA,NA))
wintxn <- data.frame(datasetID=rep("bob14", 8), study=rep("exp1", 8),
     photoperiod_day=c(8, 8, 8, 8, 12, 12, 12, 12), forcetemp=c(20, 20, 25, 25, 20,
     20, 25, 25), chilltemp=c(NA,NA,NA,NA,NA,NA,NA,NA),
     chilldays=c(NA,NA,NA,NA,NA,NA,NA,NA), fieldsample.date2=c(NA,NA,NA,NA,NA,NA,NA,NA))
somezeros<- data.frame(datasetID=rep("bob12", 8), study=rep("exp3", 8),
                     photoperiod_day=c(8, 8, 8, 8, 12, NA, 12, 12), forcetemp=c(20, 20, 25, 25, 20,
                                                                  20, "ambient", 25), chilltemp=c(NA,NA,NA,NA,NA,NA,NA,NA),
                     chilldays=c(NA,NA,NA,NA,NA,NA,NA,NA), fieldsample.date2=c(NA,NA,NA,NA,NA,NA,NA,NA))
testdat <- rbind(nointxn, wintxn)
testdat <- rbind(testdat, somezeros)
testdat
}

# Ready to switch to read data?
dat <- read.csv("output/ospree_clean.csv",header = TRUE)
dat <- dat[dat$woody=="yes",]
dat$fieldsample.date2<-strptime(strptime(dat$fieldsample.date, format = "%d-%b-%Y"),format = "%Y-%m-%d")

bbdat <- read.csv("output/ospree_clean_withchill_BB_taxon.csv")

### Cat's first stab... works for fake data BUT issue that not all studies are separated by experiment so 
### it is weeding out some studies that should work... have to add in by figure/table column to try and make it work
### but this makes the function VERY slow
countintrxns <- function(xx){

  xx$newphoto <- as.numeric(xx$photoperiod_day)
  xx$newforce <- as.numeric(xx$forcetemp)
  
  xx$newchilltemp <- as.numeric(xx$chilltemp)
  xx$newchillday <- as.numeric(xx$chilldays)
  xx$newfieldsamp <- as.numeric(yday(as.Date(xx$fieldsample.date2)))
  
  
  #### Now count number of different values of each cue for each datasetID & exp
  xx <- within(xx, { numphotos <- as.numeric(ave(xx$newphoto, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numforces <- as.numeric(ave(xx$newforce, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numchilltemps <- as.numeric(ave(xx$newchilltemp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numchilldays <- as.numeric(ave(xx$newchillday, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numfieldsamps <- as.numeric(ave(xx$newfieldsamp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  
  #### Now count number of different forces by photos, photos by forces, etc. for each datasetID & exp
  xx$numphotobyforces <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newforce), as.numeric(ave(xx$newphoto, xx$newforce, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numforcebyphotos <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newforce), as.numeric(ave(xx$newforce, xx$newphoto, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  
      ### all chills with photos
  xx$numphotobychilltemps <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newchilltemp), as.numeric(ave(xx$newphoto, xx$newchilltemp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numchilltempbyphotos <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newchilltemp), as.numeric(ave(xx$newchilltemp, xx$newphoto, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  
  xx$numphotobychilldays <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newchillday), as.numeric(ave(xx$newphoto, xx$newchillday, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numchilldaybyphotos <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newchillday), as.numeric(ave(xx$newchillday, xx$newphoto, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  
  xx$numphotobyfieldsamps <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newfieldsamp), as.numeric(ave(xx$newphoto, xx$newfieldsamp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numfieldsampbyphotos <- ifelse(!is.na(xx$newphoto) & !is.na(xx$newfieldsamp), as.numeric(ave(xx$newfieldsamp, xx$newphoto, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
      
      ### all chills with forces
  xx$numforcebychilltemps <- ifelse(!is.na(xx$newforce) & !is.na(xx$newchilltemp), as.numeric(ave(xx$newforce, xx$newchilltemp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numchilltempbyforces <- ifelse(!is.na(xx$newforce) & !is.na(xx$newchilltemp), as.numeric(ave(xx$newchilltemp, xx$newforce, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  
  xx$numforcebychilldays <- ifelse(!is.na(xx$newforce) & !is.na(xx$newchillday), as.numeric(ave(xx$newforce, xx$newchillday, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numchilldaybyforces <- ifelse(!is.na(xx$newforce) & !is.na(xx$newchillday), as.numeric(ave(xx$newchillday, xx$newforce, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  
  xx$numforcebyfieldsamps <- ifelse(!is.na(xx$newforce) & !is.na(xx$newfieldsamp), as.numeric(ave(xx$newforce, xx$newfieldsamp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)
  xx$numfieldsampbyforces <- ifelse(!is.na(xx$newforce) & !is.na(xx$newchilltemp), as.numeric(ave(xx$newfieldsamp, xx$newforce, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE))), 1)

  cantequal <- c(0,1) ### remove non-numerics
  
  #### Now, add up the number of interactions to get rid of the ones! 
      ### e.g. Need more than one force per photo and more than one photo per force
  
  xx$photoandforces <- ifelse(!xx$numphotos%in%cantequal & !xx$numforces%in%cantequal & xx$numphotobyforces>1 & xx$numforcebyphotos>1, TRUE, FALSE)
  xx$photoandchilltemps <- ifelse(!xx$numphotos%in%cantequal & !xx$numchilltemps%in%cantequal & xx$numphotobychilltemps>1 & xx$numchilltempbyphotos>1, TRUE, FALSE)
  xx$forceandchilltemps <- ifelse(!xx$numforces%in%cantequal & !xx$numchilltemps%in%cantequal & xx$numforcebychilltemps>1 & xx$numchilltempbyforces>1, TRUE, FALSE)
  
  xx$photoandchilldays <- ifelse(!xx$numphotos%in%cantequal & !xx$numchilldays%in%cantequal & xx$numphotobychilldays>1 & xx$numchilldaybyphotos>1, TRUE, FALSE)
  xx$forceandchilldays <- ifelse(!xx$numforces%in%cantequal & !xx$numchilldays%in%cantequal & xx$numforcebychilldays>1 & xx$numchilldaybyforces>1, TRUE, FALSE)
  
  xx$photoandfieldsamps <- ifelse(!xx$numphotos%in%cantequal & !xx$numfieldsamps%in%cantequal & xx$numphotobyfieldsamps>1 & xx$numfieldsampbyphotos>1, TRUE, FALSE)
  xx$forceandfieldsamps <- ifelse(!xx$numforces%in%cantequal & !xx$numfieldsamps%in%cantequal & xx$numforcebyfieldsamps>1 & xx$numfieldsampbyforces>1, TRUE, FALSE)

  
  return(xx)
}


#### For ospree_clean.csv
newdat <- countintrxns(dat) ## takes a bit ~40 seconds

osp.fp <- newdat
osp.fp$fp <- ifelse(ave(osp.fp$photoandforces, osp.fp$datasetID, osp.fp$study, osp.fp$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.fp <- osp.fp[(osp.fp$fp=="yes"),]
osp.fp <- subset(osp.fp, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "fp"))
#osp.fp <- osp.fp[!duplicated(osp.fp),]
#osp.fp <- osp.fp[(osp.fp$photoandforces==TRUE),]
unique(osp.fp$datasetID) ### [1] "basler14"     "heide05"      "heide08"      "heide11"      "heide93"      "heide93a"     "okie11"      
                         ### [8] "partanen98"   "pettersen71"  "Sanz-Perez09" "sogaard08"


osp.pctemp <- newdat
osp.pctemp$pctemp <- ifelse(ave(osp.pctemp$photoandchilltemps, osp.pctemp$datasetID, osp.pctemp$study, osp.pctemp$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.pctemp <- osp.pctemp[(osp.pctemp$pctemp=="yes"),]
osp.pctemp <- subset(osp.pctemp, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "pctemp"))
#osp.pctemp <- osp.pctemp[!duplicated(osp.pctemp),]
#osp.pctemp <- osp.pctemp[(osp.pctemp$photoandchilltemps==TRUE),]
unique(osp.pctemp$datasetID) ### "myking95"


osp.fctemp <- newdat
osp.fctemp$fctemp <- ifelse(ave(osp.fctemp$forceandchilltemps, osp.fctemp$datasetID, osp.fctemp$study, osp.fctemp$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.fctemp <- osp.fctemp[(osp.fctemp$fctemp=="yes"),]
osp.fctemp <- subset(osp.fctemp, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "fctemp"))
#osp.fctemp <- osp.fctemp[!duplicated(osp.fctemp),]
#osp.fctemp <- osp.fctemp[(osp.fctemp$forceandchilltemps==TRUE),]
unique(osp.fctemp$datasetID)  ### [1] "skuterud94" 


osp.pcday <- newdat
osp.pcday$pcday <- ifelse(ave(osp.pcday$photoandchilldays, osp.pcday$datasetID, osp.pcday$study, osp.pcday$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.pcday <- osp.pcday[(osp.pcday$pcday=="yes"),]
osp.pcday <- subset(osp.pcday, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "pcday"))
#osp.pcday <- osp.pcday[!duplicated(osp.pcday),]
#osp.pcday <- osp.pcday[(osp.pcday$photoandchilldays==TRUE),]
unique(osp.pcday$datasetID) ### [1] "basler14"     "caffarra11b"  "laube14a"     "myking95"     "nienstaedt66" "okie11"       "partanen98"


osp.fcday <- newdat
osp.fcday$fcday <- ifelse(ave(osp.fcday$forceandchilldays, osp.fcday$datasetID, osp.fcday$study, osp.fcday$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.fcday <- osp.fcday[(osp.fcday$fcday=="yes"),]
osp.fcday <- subset(osp.fcday, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "fcday"))
#osp.fcday <- osp.fcday[!duplicated(osp.fcday),]
#osp.fcday <- osp.fcday[(osp.fcday$forceandchilldays==TRUE),]
unique(osp.fcday$datasetID) ### [1] "basler14"   "campbell75" "falusi97"   "junttila12" "karlsson03" "okie11"     "partanen98" "pop2000"


osp.pfield <- newdat
osp.pfield$pfield <- ifelse(ave(osp.pfield$photoandfieldsamps, osp.pfield$datasetID, osp.pfield$study, osp.pfield$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.pfield <- osp.pfield[(osp.pfield$pfield=="yes"),]
osp.pfield <- subset(osp.pfield, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "pfield"))
#osp.pfield <- osp.pfield[!duplicated(osp.pfield),]
#osp.pfield <- osp.pfield[(osp.pfield$forceandchilldays==TRUE),]
unique(osp.pfield$datasetID) ### [1] "ashby62"      "basler14"     "caffarra11b"  "ghelardini10" "heide93"      "heide93a"     "partanen05"  
                             ### [8] "partanen98"   "zohner16" 


osp.ffield <- newdat
osp.ffield$ffield <- ifelse(ave(osp.ffield$forceandfieldsamps, osp.ffield$datasetID, osp.ffield$study, osp.ffield$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.ffield <- osp.ffield[(osp.ffield$ffield=="yes"),]
osp.ffield <- subset(osp.ffield, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "ffield"))
#osp.ffield <- osp.ffield[!duplicated(osp.ffield),]
#osp.ffield <- osp.ffield[(osp.ffield$forceandchilldays==TRUE),]
unique(osp.ffield$datasetID) ## no studies



#### For ospree_clean_withchill_BB.csv
newdat <- countintrxns(bbdat)

osp.fp <- newdat
osp.fp$fp <- ifelse(ave(osp.fp$photoandforces, osp.fp$datasetID, osp.fp$study, osp.fp$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.fp <- osp.fp[(osp.fp$fp=="yes"),]
osp.fp <- subset(osp.fp, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "fp"))
#osp.fp <- osp.fp[!duplicated(osp.fp),]
#osp.fp <- osp.fp[(osp.fp$photoandforces==TRUE),]
unique(osp.fp$datasetID) ### [1] "heide93"      "Sanz-Perez09" "basler14"     "gianfagna85"  "heide05"      "heide08"      "heide11"     
                         ### [8] "heide93a"     "morin10"      "pettersen71"  "skuterud94"


osp.pctemp <- newdat
osp.pctemp$pctemp <- ifelse(ave(osp.pctemp$photoandchilltemps, osp.pctemp$datasetID, osp.pctemp$study, osp.pctemp$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.pctemp <- osp.pctemp[(osp.pctemp$pctemp=="yes"),]
osp.pctemp <- subset(osp.pctemp, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "pctemp"))
#osp.pctemp <- osp.pctemp[!duplicated(osp.pctemp),]
#osp.pctemp <- osp.pctemp[(osp.pctemp$photoandchilltemps==TRUE),]
unique(osp.pctemp$datasetID) ### [1] "jones12"    "myking95"   "skuterud94"


osp.fctemp <- newdat
osp.fctemp$fctemp <- ifelse(ave(osp.fctemp$forceandchilltemps, osp.fctemp$datasetID, osp.fctemp$study, osp.fctemp$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.fctemp <- osp.fctemp[(osp.fctemp$fctemp=="yes"),]
osp.fctemp <- subset(osp.fctemp, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "fctemp"))
#osp.fctemp <- osp.fctemp[!duplicated(osp.fctemp),]
#osp.fctemp <- osp.fctemp[(osp.fctemp$forceandchilltemps==TRUE),]
unique(osp.fctemp$datasetID)  ### [1] "skuterud94" 


osp.pcday <- newdat
osp.pcday$pcday <- ifelse(ave(osp.pcday$photoandchilldays, osp.pcday$datasetID, osp.pcday$study, osp.pcday$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.pcday <- osp.pcday[(osp.pcday$pcday=="yes"),]
osp.pcday <- subset(osp.pcday, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "pcday"))
#osp.pcday <- osp.pcday[!duplicated(osp.pcday),]
#osp.pcday <- osp.pcday[(osp.pcday$photoandchilldays==TRUE),]
unique(osp.pcday$datasetID) ### [1] "jones12"      "basler14"     "caffarra11b"  "laube14a"     "myking95"     "nienstaedt66"


osp.fcday <- newdat
osp.fcday$fcday <- ifelse(ave(osp.fcday$forceandchilldays, osp.fcday$datasetID, osp.fcday$study, osp.fcday$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.fcday <- osp.fcday[(osp.fcday$fcday=="yes"),]
osp.fcday <- subset(osp.fcday, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "fcday"))
#osp.fcday <- osp.fcday[!duplicated(osp.fcday),]
#osp.fcday <- osp.fcday[(osp.fcday$forceandchilldays==TRUE),]
unique(osp.fcday$datasetID) ### [1] "junttila12" "basler14"   "campbell75" "karlsson03" "partanen98" "skuterud94"


osp.pfield <- newdat
osp.pfield$pfield <- ifelse(ave(osp.pfield$photoandfieldsamps, osp.pfield$datasetID, osp.pfield$study, osp.pfield$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.pfield <- osp.pfield[(osp.pfield$pfield=="yes"),]
osp.pfield <- subset(osp.pfield, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "pfield"))
#osp.pfield <- osp.pfield[!duplicated(osp.pfield),]
#osp.pfield <- osp.pfield[(osp.pfield$forceandchilldays==TRUE),]
unique(osp.pfield$datasetID) ### [1] "ashby62"      "basler14"     "caffarra11b"  "ghelardini10" "gianfagna85"  "heide93"      "heide93a"    
                             ### [8] "laube14a"     "yazdaniha64"  "zohner16" 


osp.ffield <- newdat
osp.ffield$ffield <- ifelse(ave(osp.ffield$forceandfieldsamps, osp.ffield$datasetID, osp.ffield$study, osp.ffield$figure.table..if.applicable., FUN = function(x) all(x==TRUE)), "yes", "no")
osp.ffield <- osp.ffield[(osp.ffield$ffield=="yes"),]
osp.ffield <- subset(osp.ffield, select=c("datasetID", "study", "figure.table..if.applicable.", "genus", "species", "ffield"))
#osp.ffield <- osp.ffield[!duplicated(osp.ffield),]
#osp.ffield <- osp.ffield[(osp.ffield$forceandchilldays==TRUE),]
unique(osp.ffield$datasetID) ## no studies
