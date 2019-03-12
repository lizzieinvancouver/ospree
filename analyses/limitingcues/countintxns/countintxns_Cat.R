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

if(FALSE){
# Build a simple example
nointxn <- data.frame(datasetID=rep("bob12", 8), study=c(rep("exp2", 4),
    rep("exp2", 4)), photo=c(8, 8, 8, 8, 8, 8, 12, 12),
    force=c(15, 15, 18, 18, 15, 15, 15, 15))
wintxn <- data.frame(datasetID=rep("bob14", 8), study=rep("exp1", 8),
     photo=c(8, 8, 8, 8, 12, 12, 12, 12), force=c(20, 20, 25, 25, 20,
     20, 25, 25))
somezeros<- data.frame(datasetID=rep("bob12", 8), study=rep("exp2", 8),
                     photo=c(8, 8, 8, 8, 12, NA, 12, 12), force=c(20, 20, 25, 25, 20,
                                                                  20, "ambient", 25))
testdat <- rbind(nointxn, wintxn)
testdat <- rbind(testdat, somezeros)
testdat
}

# Ready to switch to read data?
dat <- read.csv("output/ospree_clean.csv",header = TRUE)
dat <- dat[dat$woody=="yes",]

bbdat <- read.csv("output/ospree_clean_withchill_BB_taxon.csv")

### Cat's first stab... works for fake data but not at all for real data... need to keep thinking
countintrxns <- function(xx){

  xx$newphoto <-as.numeric(xx$photoperiod_day)
  xx$newforce <-as.numeric(xx$forcetemp)
  
  xx$newchilltemp <-as.numeric(xx$chilltemp)
  xx$newchillday <-as.numeric(xx$chilldays)
  xx$newfieldsamp <-as.numeric(xx$fieldsample.date)
  
  xx <- within(xx, { numphotos <- as.numeric(ave(xx$newphoto, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numforces <- as.numeric(ave(xx$newforce, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numchilltemps <- as.numeric(ave(xx$newchilltemp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numchilldays <- as.numeric(ave(xx$newchillday, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  xx <- within(xx, { numfieldsamps <- as.numeric(ave(xx$newfieldsamp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)))})
  
  cantequal <- c(0,1)
  xx$photobyforce_calc <- ifelse(!xx$numphotos%in%cantequal & !xx$numforces%in%cantequal, ave(xx$newphoto, xx$newforce, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)), NA)
  xx$photobychilltemp_calc <- ifelse(!xx$numphotos%in%cantequal & !xx$numchilltemps%in%cantequal, ave(xx$newphoto, xx$newchilltemp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)), NA)
  xx$forcebychilltemp_calc <- ifelse(!xx$numforces%in%cantequal & !xx$numchilltemps%in%cantequal, ave(xx$newforce, xx$newchilltemp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)), NA)
  
  xx$photobychillday_calc <- ifelse(!xx$numphotos%in%cantequal & !xx$numchilldays%in%cantequal, ave(xx$newphoto, xx$newchillday, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)), NA)
  xx$forcebychillday_calc <- ifelse(!xx$numforces%in%cantequal & !xx$numchilldays%in%cantequal, ave(xx$newforce, xx$newchillday, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)), NA)
  
  xx$photobyfieldsamp_calc <- ifelse(!xx$numphotos%in%cantequal & !xx$numfieldsamps%in%cantequal, ave(xx$newphoto, xx$newfieldsamp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)), NA)
  xx$forcebyfieldsamp_calc <- ifelse(!xx$numforces%in%cantequal & !xx$numfieldsamps%in%cantequal, ave(xx$newforce, xx$newfieldsamp, xx$datasetID, xx$study, FUN=function(x) n_distinct(x, na.rm=TRUE)), NA)
  
  
  return(xx)
}


#### For ospree_clean.csv
newdat <- countintrxns(dat)

checkintrxns_fp <- subset(newdat, newdat$photobyforce_calc!=0) 
unique(checkintrxns_fp$datasetID) ### basler14"     "heide05"      "heide08"      "heide11"      "heide93"      "heide93a"    
                                  ### "okie11"       "partanen98"   "pettersen71"  "Sanz-Perez09" "sogaard08"    "worrall67"   

checkintrxns_pctemp <- subset(newdat, newdat$photobychilltemp_calc!=0) 
unique(checkintrxns_pctemp$datasetID) ### "myking95"
checkintrxns_fctemp <- subset(newdat, newdat$forcebychilltemp_calc!=0) 
unique(checkintrxns_fctemp$datasetID) ### "skuterud94"

checkintrxns_pcday <- subset(newdat, newdat$photobychillday_calc!=0) 
unique(checkintrxns_pcday$datasetID) ### [1] "basler14"     "caffarra11a"  "caffarra11b"  "laube14a"     "myking95"     "nienstaedt66"
                                    ### [7] "okie11"       "partanen98"   "sogaard08"    "worrall67"
checkintrxns_fcday <- subset(newdat, newdat$forcebychillday_calc!=0) 
unique(checkintrxns_fcday$datasetID) ### [1] "basler14"   "campbell75" "falusi97"   "junttila12" "karlsson03" "okie11"     "partanen98"
                                     ### [8] "pop2000"    "skuterud94" "sogaard08"  "worrall67" 

checkintrxns_pcfield <- subset(newdat, newdat$photobychillfield_calc!=0) 
unique(checkintrxns_pcfield$datasetID) ### no studies
checkintrxns_fcfield <- subset(newdat, newdat$forcebychillfield_calc!=0) 
unique(checkintrxns_fcfield$datasetID) ### no studies

checkintrxns_all <- subset(newdat, (newdat$photobyforce_calc != 0) |
                             
                             (newdat$photobychilltemp_calc != 0) |
                             (newdat$photobychillday_calc != 0) |
                             (newdat$photobyfieldsamp_calc != 0) |
                              (newdat$forcebychilltemp_calc != 0) |
                              (newdat$forcebychillday_calc != 0) |
                              (newdat$forcebyfieldsamp_calc != 0) )


#### For ospree_clean_withchill_BB.csv
newdat <- countintrxns(bbdat)

checkintrxns_fp <- subset(newdat, newdat$photobyforce_calc!=0) 
unique(checkintrxns_fp$datasetID) ### [1] "heide93"      "Sanz-Perez09" "basler12"     "basler14"     "gianfagna85"  "gomory15"    
                                  ### [7] "heide05"      "heide08"      "heide11"      "heide93a"     "laube14a"     "morin10"     
                                  ### [13] "myking97"     "nishimoto95"  "partanen98"   "pettersen71"  "schnabel87"   "skuterud94"  
                                  ### [19] "worrall67"

checkintrxns_pctemp <- subset(newdat, newdat$photobychilltemp_calc!=0) 
unique(checkintrxns_pctemp$datasetID) ### "jones12"    "myking95"   "skuterud94"
checkintrxns_fctemp <- subset(newdat, newdat$forcebychilltemp_calc!=0) 
unique(checkintrxns_fctemp$datasetID) ### "skuterud94"

checkintrxns_pcday <- subset(newdat, newdat$photobychillday_calc!=0) 
unique(checkintrxns_pcday$datasetID) ### [1] "jones12"      "rinne97"      "basler14"     "caffarra11a"  "caffarra11b"  "laube14a"    
                                     ### [7] "myking95"     "nienstaedt66" "partanen98"   "skuterud94"   "sogaard08"    "worrall67"   
                                    ### [13] "yazdaniha64" 
checkintrxns_fcday <- subset(newdat, newdat$forcebychillday_calc!=0) 
unique(checkintrxns_fcday$datasetID) ### [1] "falusi97"   "junttila12" "basler14"   "campbell75" "karlsson03" "laube14a"   "partanen98"
                                     ### [8] "skuterud94" "worrall67"

checkintrxns_pcfield <- subset(newdat, newdat$photobychillfield_calc!=0) 
unique(checkintrxns_pcfield$datasetID) ### no studies
checkintrxns_fcfield <- subset(newdat, newdat$forcebychillfield_calc!=0) 
unique(checkintrxns_fcfield$datasetID) ### no studies


checkintrxns_all <- subset(newdat, (newdat$photobyforce_calc != 0) |
                              
                            (newdat$photobychilltemp_calc != 0) |
                            (newdat$photobychillday_calc != 0) |
                            (newdat$photobyfieldsamp_calc != 0) |
                            
                            (newdat$forcebychilltemp_calc != 0) |
                            (newdat$forcebychillday_calc != 0) |
                            (newdat$forcebyfieldsamp_calc != 0) )

