## Started 2 March 2019 ##
## By Lizzie ##

# Start your own R file to make a f(x) to count interactive experiments, ideally for all possible intxns across photoperiod, chilling, forcing, and field sample date (maybe provenance) treatments! #

## See also: https://github.com/lizzieinvancouver/ospree/issues/235

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

source("misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("misc/gettreatdists.R") # f(x) counts up treatment interactions, and more!

###################
# All OSPREE data #
###################
dat <- read.csv("output/ospree_clean.csv", header = TRUE)
dat <- dat[dat$woody=="yes",]
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")
dat$doy <- format(dat$fieldsample.date, "%j")

# Get the number of field sampling dates that are 14 or more weeks apart, first for each datasetIDx study ...
ddatefx.all <- subset(dat, select=c("datasetID", "study", "fieldsample.date"))
ddatefx <- ddatefx.all[!duplicated(ddatefx.all), ]
ddatefx$datasetIDstudy <- paste(ddatefx$datasetID, ddatefx$study)

## Change main DF so only relevant dates are included ... not pretty, but should work
# First get the unique dates in a df
dates2weeks.count <- countfieldsample(ddatefx, 14)
uniquedates.df <- fieldsample.getuniquedates(ddatefx, 14)
uniquedates.df$selectcolumn <- paste(uniquedates.df$datasetIDstudy, uniquedates.df$date)
# Now subset to sane # of columnns
datsm <- subset(dat, select=c("datasetID", "study", "genus", "species", "forcetemp","forcetemp_night", "photoperiod_day", 
    "fieldsample.date", "chilltemp", "chillphotoperiod", "chilldays"))
datsm$study
head(datsm)

## Okay, formatting to look at intxns
datsm$force <- as.numeric(datsm$forcetemp)
datsm$photo <- as.numeric(datsm$photoperiod_day)

datsm.noNA <- subset(datsm, is.na(force)==FALSE & is.na(photo)==FALSE)

osp.fp <- get.treatdists(datsm.noNA, "photo", "force")
osp.fpintxn <- subset(osp.fp, intxn>=2)
osp.fpintxn[order(osp.fpintxn$datasetID),]

osp.ctf <- get.treatdists(datsm.noNA, "chilltemp", "force")
osp.ctfintxn <- subset(osp.ctf, intxn>=2)

osp.cdf <- get.treatdists(datsm.noNA, "chilldays", "force")
osp.cdfintxn <- subset(osp.cdf, intxn>=2)

lookatunique <- get.uniquetreats(datsm.noNA, "photo", "force")

# Now (not pretty part) we'll take all NA dates ...
datsm$selectcolumn <- paste(datsm$datasetID, datsm$study, datsm$fieldsample.date)
datsm14d <- datsm[which(datsm$selectcolumn %in% uniquedates.df$selectcolumn),]

dim(datsm)
dim(datsm14d) # not such a huge loss of rows

# Check we don't lose any datasets!
setdiff(unique(paste(datsm$datasetID, datsm$study)), unique(paste(datsm14d$datasetID, datsm14d$study)))

datsm14d.noNA <- subset(datsm14d, is.na(force)==FALSE & is.na(photo)==FALSE)

# Repeat of the above but correcting for field sampling date repetition
osp14d.fp <- get.treatdists(datsm14d.noNA, "photo", "force")
nrow(osp14d.fp) #107 experiments from
unique(osp14d.fp$datasetID) ### 64 studies manipulate photo and force

osp14d.fpintxn <- subset(osp14d.fp, intxn>=2) # 15experiments from 
osp14d.fpintxn[order(osp14d.fpintxn$datasetID),]
nrow(osp14d.fpintxn) 
unique(osp14d.fpintxn$datasetID) #from 10 studies

###now indentfy which of the above might have periodicity issues
thermop<-dplyr::filter(datsm14d, datasetID %in%unique(osp14d.fpintxn$datasetID))

moreforcinginfo <- get.treatdists.daynight(thermop, "forcetemp", "forcetemp_night")



forcingvaried <- subset(moreforcinginfo, treatinfo!="forcing does not vary")
studiesinclconstantforce <- subset(forcingvaried, numconstantforce>0) # some studies have both
studiesinclforceperiodicity <- subset(forcingvaried, numdiffforce>0) 
nrow(studiesinclforceperiodicity) #7 out of 15 might have this issue
unique(studiesinclforceperiodicity$datasetID) #4

osp14d.ctf <- get.treatdists(datsm14d.noNA, "chilltemp", "force")
osp14d.ctfintxn <- subset(osp14d.ctf, intxn>=2) # 2 studies

osp14d.cdf <- get.treatdists(datsm14d.noNA, "chilldays", "force")
osp14d.cdfintxn <- subset(osp14d.cdf, intxn>=2) # same 2 studies # skuterud94  exp1  &  heide12  exp2

osp14d.daysf <- get.treatdists(datsm14d.noNA, "fieldsample.date", "force")
osp14d.daysfintxn <- subset(osp14d.daysf, intxn>=2) # 9 studies

osp14d.daysp <- get.treatdists(datsm14d.noNA, "fieldsample.date", "photo")
osp14d.dayspintxn <- subset(osp14d.daysp, intxn>=2) # 11 studies

length(unique(paste(datsm14d$datasetID, datsm14d$study)))

##For DAN
head(datsm.noNA)
class(datsm.noNA$photo)
datsm.noNA %>% dplyr::group_by(datasetID,study) %>% 
dplyr::mutate(count = n(photo)) %>% 
  unique()
##################
# BB OSPREE data #
##################
## Tried to ref bbstandleadin.R here but a pain ... ##
## So below I just read in dataframes I built in bbstanleadin.R ##

mainmodelbb <- TRUE # set to FALSE for larger species set

if(mainmodelbb){
bb <- read.csv("output/bbstan_mainmodel_utah_allsppwcrops_allfp_allchill.csv", header=TRUE) 
}
if(!mainmodelbb){
bb <- read.csv("output/bbstan_allsppmodel_utahzscore_wcrops_allfp_allchill.csv", header=TRUE)
}

# decoder:
# MM (main model)
# FM (full model)

###################################
# Dealing with field sample dates #
###################################
bb$fieldsample.date <- as.Date(bb$fieldsample.date, format="%d-%b-%Y")

bb.ddatefx.all <- subset(bb, select=c("datasetID", "study", "fieldsample.date"))
bb.ddatefx <- bb.ddatefx.all[!duplicated(bb.ddatefx.all), ]
bb.ddatefx$datasetIDstudy <- paste(bb.ddatefx$datasetID, bb.ddatefx$study)

## Change bb DF so only relevant dates are included ... again, not pretty, but should work
# First get the unique dates in a df
bb.dates2weeks.count <- countfieldsample(bb.ddatefx, 14)
bb.uniquedates.df <- fieldsample.getuniquedates(bb.ddatefx, 14)
# Now (not pretty part) we'll take all NA dates ...
# Note that for some reason we don't have to do thee gymnastics above
bbselectNA <- subset(bb.uniquedates.df, is.na(date)==TRUE)
bb$fieldsample.date.merge <- bb$fieldsample.date
bb$fieldsample.date.merge[paste(bb$datasetID, bb$study) %in% unique(bbselectNA$datasetIDstudy)] <- NA
bb$selectcolumn <- paste(bb$datasetID, bb$study, bb$fieldsample.date.merge)
bb.uniquedates.df$selectcolumn <- paste(bb.uniquedates.df$datasetIDstudy, bb.uniquedates.df$date)
bb14d <- bb[which(bb$selectcolumn %in% unique(bb.uniquedates.df$selectcolumn)),]
# Check we don't lose any datasets!
setdiff(unique(paste(bb$datasetID, bb$study)), unique(paste(bb14d$datasetID, bb14d$study)))

dim(bb)
dim(bb14d) # this loses ~200 rows

bb14d.noNA <- subset(bb14d, is.na(force)==FALSE & is.na(photo)==FALSE)

###################################
# Now, calculate interaction nums #
###################################
bbintxnsdf <- data.frame(treat1=character(), treat2=character(), n=numeric())

# Repeat of the what we did above for all OSPREE data correcting for field sampling date repetition
# Note that the notes on numbers may not be all updated

# Simple one-by-one interactions
bb14d.fp <- get.treatdists(bb14d.noNA, "photo", "force")
bb14d.fpintxn <- subset(bb14d.fp, intxn>=2) 
bb14d.fpintxn[order(bb14d.fpintxn$datasetID),]
length(unique(paste(bb14d.fpintxn$datasetID, bb14d.fpintxn$study))) 
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="photo", treat2="force",
    n=length(unique(paste(bb14d.fpintxn$datasetID, bb14d.fpintxn$study)))))

bb14d.ctf <- get.treatdists(bb14d.noNA, "chilltemp", "force")
bb14d.ctfintxn <- subset(bb14d.ctf, intxn>=2) 
length(unique(paste(bb14d.ctfintxn$datasetID, bb14d.ctfintxn$study))) # MM and FM: 1 study: skuterud94 exp1
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="chilltemp", treat2="force",
    n=length(unique(paste(bb14d.ctfintxn$datasetID, bb14d.ctfintxn$study)))))

bb14d.cdf <- get.treatdists(bb14d.noNA, "chilldays", "force")
bb14d.cdfintxn <- subset(bb14d.cdf, intxn>=2)  
length(unique(paste(bb14d.cdfintxn$datasetID, bb14d.cdfintxn$study))) # MM and FM: 5 studies (does not include skuterud94 exp1)
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="chilldays", treat2="force",
    n=length(unique(paste(bb14d.cdfintxn$datasetID, bb14d.cdfintxn$study)))))

bb14d.ctp <- get.treatdists(bb14d.noNA, "chilltemp", "photo")
bb14d.ctpintxn <- subset(bb14d.ctp, intxn>=2) 
length(unique(paste(bb14d.ctpintxn$datasetID, bb14d.ctpintxn$study))) # MM and FM: 1 study: myking95  exp1
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="chilltemp", treat2="photo",
    n=length(unique(paste(bb14d.ctpintxn$datasetID, bb14d.ctpintxn$study)))))

bb14d.cdp <- get.treatdists(bb14d.noNA, "chilldays", "photo")
bb14d.cdpintxn <- subset(bb14d.cdp, intxn>=2)  
length(unique(paste(bb14d.cdpintxn$datasetID, bb14d.cdpintxn$study))) # MM and FM: 7 studies (includes myking95  exp1)
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="chilldays", treat2="photo",
    n=length(unique(paste(bb14d.cdpintxn$datasetID, bb14d.cdpintxn$study)))))

bb14d.daysf <- get.treatdists(bb14d.noNA, "fieldsample.date", "force")
bb14d.daysfintxn <- subset(bb14d.daysf, intxn>=2) # 
length(unique(paste(bb14d.daysfintxn$datasetID, bb14d.daysfintxn$study))) # MM: 7 studies; FM: 9 studies
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="fieldsample.date", treat2="force",
    n=length(unique(paste(bb14d.daysfintxn$datasetID, bb14d.daysfintxn$study)))))

bb14d.daysp <- get.treatdists(bb14d.noNA, "fieldsample.date", "photo")
bb14d.dayspintxn <- subset(bb14d.daysp, intxn>=2) 
length(unique(paste(bb14d.dayspintxn$datasetID, bb14d.dayspintxn$study))) # MM: 10 studies; FM: 10
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="fieldsample.date", treat2="photo",
    n=length(unique(paste(bb14d.dayspintxn$datasetID, bb14d.dayspintxn$study)))))

length(unique(paste(bb14d$datasetID, bb14d$study))) # MM: 42; FM: 66 studies
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-datasetIDstudies", treat2="NA",
    n=length(unique(paste(bb14d$datasetID, bb14d$study))) ))
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-datasetIDs", treat2="NA",
    n=length(unique(bb14d$datasetID))))
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-species", treat2="NA",
    n=length(unique(paste(bb14d$genus, bb14d$species))) ))


# Now, which studies manipulate chill (and interact it with photo or force)?
# Note that chilldays may be field-sample-date based, so this is probably an overestimate
union.expchill1 <- union(unique(paste(bb14d.ctfintxn$datasetID, bb14d.ctfintxn$study)),
    unique(paste(bb14d.cdfintxn$datasetID, bb14d.cdfintxn$study)))
union.expchill2 <- union(union.expchill1, unique(paste(bb14d.ctpintxn$datasetID, bb14d.ctpintxn$study)))
union.expchill3 <- union(union.expchill2, unique(paste(bb14d.cdpintxn$datasetID, bb14d.cdpintxn$study))) # MM and FM: 9 unique studies

# Which manipulate field-sample dates (and interact it with photo or force)? 
union.fs <- union(unique(paste(bb14d.daysfintxn$datasetID, bb14d.daysfintxn$study)),
    unique(paste(bb14d.dayspintxn$datasetID, bb14d.dayspintxn$study))) # MM: 12 unique studies; FM: 13

## Okay, now dig into what the above identified as manipulating chilling but we called 'field-estimated'
checkme.expchill <- bb14d[which(paste(bb14d$datasetID, bb14d$study) %in% union.expchill3),]
checkme.expchill.fldchill <- subset(checkme.expchill, chill_type=="fldest")
checkme.expchill.exp <- subset(checkme.expchill, chill_type=="exp" | chill_type=="bothest")
unique(paste(checkme.expchill.fldchill$datasetID, checkme.expchill.fldchill$study)) # field-chill studies
sort(unique(paste(checkme.expchill.exp$datasetID, checkme.expchill.exp$study))) # 6 studies manipulate chilling
# caffarra11b exp2, basler14 exp1, laube14a exp1 show up as both exp chill and field sample dates ... (partanen98 exp1 is fldest only)

## did manipulate chilling (based on checking the data):
# checkdat <- subset(bb, datasetID=="caffarra11b" & study=="exp2")
# caffarra11a exp3, myking95 exp1, worrall67 exp 3, "skuterud94 exp1", campbell75 exp3
# caffarra11b exp2 DID NOT alter chill (the 'bothest' is for no chilling only)
# laube14a exp1 DID NOT alter chill (state clearly in the paper chilling is based on time in field)
# Add this info below, subtract 1 for caffarra11b exp2 ... see below for final #s
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-exp-chill-fpintxns", treat2="NA",
    n=length(unique(paste(checkme.expchill.exp$datasetID, checkme.expchill.exp$study)))-1))
 
checkme.fieldchill <- bb14d[which(paste(bb14d$datasetID, bb14d$study) %in% union.fs),]
subset(checkme.fieldchill, chill_type!="fldest")
# heide93 exp3 -- no chilldays or chilltemp so assume all field-sample-date
sort(union.fs)

bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-fielddate-chill-fpintxns", treat2="NA",
    n=length(union.fs)))

## The above is all about interactive treatments ... let's now check against single treatments
forcetreats <- get.treatdists.singletreatment(bb14d.noNA, "force")
phototreats <- get.treatdists.singletreatment(bb14d.noNA, "photo")
# new and I can confirm it is a chilling treatment (from checkdat)
# mainmodel: Li05, thielges75 exp1 (from above: caffarra11a exp3, myking95 exp1, worrall67 exp 3, skuterud94 exp1, campbell75 exp3)
# allsppmodel: chavarria09 exp1, jones12 exp1 and exp2, sonsteby14 exp2 and exp3
# pagter15 exp1 is ambient warming (not exp chill)
chilltemptreats <- get.treatdists.singletreatment(bb14d.noNA, "chilltemp")
chilldaystreats <- get.treatdists.singletreatment(bb14d.noNA, "chilldays")
fsdatestreats <- get.treatdists.singletreatment(bb14d.noNA, "fieldsample.date")
sort(union(unique(paste(checkme.expchill.exp$datasetID, checkme.expchill.exp$study)), paste(chilltemptreats$datasetID,
   chilltemptreats$study)))
expchillist <- union(unique(paste(checkme.expchill.exp$datasetID, checkme.expchill.exp$study)), paste(chilltemptreats$datasetID,
   chilltemptreats$study))
expchillist <- expchillist[which(!expchillist %in% c("caffarra11b exp2", "pagter15 exp1", "laube14a exp1"))]
# removing caffarra11b exp2, laube14a exp1, pagter15 exp1
# if we assume the field-sample number is correct we should just rm the studies that are exp chilling ...
setdiff(paste(fsdatestreats$datasetID, fsdatestreats$study), expchillist)
# but check the new studies by reviewing the data
# mainmodel: falusi97 exp2, calme94 exp 1, myking98 exp 1, spiers74 exp2, webb78 exp 2 all look correct)
# allsp model: pagter15 exp1, biasi12 exp1, cook00b exp1, gianfagna85 exp1, gomory15 exp1, guerriero90 exp3, ramos99 exp1, schnabel87 exp1
setdiff(setdiff(paste(fsdatestreats$datasetID, fsdatestreats$study), expchillist), union.fs)
# And overwrite with the better info the expchill #s
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-exp-chill", treat2="NA",
    n=length(expchillist)))
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-fielddate-chill", treat2="NA",
    n=length(setdiff(paste(fsdatestreats$datasetID, fsdatestreats$study), expchillist))))
## Done with single treatments

## Now get thermo-photoperiodicity
# Note that I (Lizzie) did NOT go back to the data and check these ...
moreforcinginfo <- get.treatdists.daynight(bb14d.noNA, "forcetemp", "forcetemp_night")
forcingvaried <- subset(moreforcinginfo, treatinfo!="forcing does not vary")
studiesinclconstantforce <- subset(forcingvaried, numconstantforce>0) # some studies have both
studiesinclforceperiodicity <- subset(forcingvaried, numdiffforce>0) # some studies have both
# And add this info too!
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-varied-forcing", treat2="NA",
    n=nrow(forcingvaried)))
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-wsometreats-constant-forcing", treat2="NA",
    n=nrow(studiesinclconstantforce)))
bbintxnsdf <- rbind(bbintxnsdf, data.frame(treat1="total-variedwsometreats-varydaynight-forcing", treat2="NA",
    n=nrow(studiesinclforceperiodicity)))
# Enough already?

if(mainmodelbb){
names(bbintxnsdf) <- c("Treatment 1", "Treatment 2", "n studies")
write.csv(bbintxnsdf, "limitingcues/output/bbstan_mainmodel_countinxns.csv", row.names=FALSE)
}

if(!mainmodelbb){
names(bbintxnsdf) <- c("Treatment 1", "Treatment 2", "n studies")
write.csv(bbintxnsdf, "limitingcues/output/bbstan_allsppmodel_countinxns.csv", row.names=FALSE)
}

