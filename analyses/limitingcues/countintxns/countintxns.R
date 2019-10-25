## Started 2 March 2019 ##
## By Lizzie ##

# Start your own R file to make a f(x) to count interactive experiments, ideally for all possible intxns across photoperiod, chilling, forcing, and field sample date (maybe provenance) treatments! #

## See also: https://github.com/lizzieinvancouver/ospree/issues/235

################
## To do here ##
################
# (*) Figure out why my numbers differ from Cat's (search for *** below)
# (*) Decide how to deal with forcing having varying night and day treatments
# (*) Make heatmaps ...

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
dat$latbi <- paste(dat$genus, dat$species)

# Some basic counts
sort(unique(dat$latbi)) # 227 species
table(dat$continent)
spbycontinentfull <- subset(dat, select=c("continent", "latbi"))
spbycontinent <- spbycontinentfull[!duplicated(spbycontinentfull), ]
table(spbycontinent$continent)
studybycontinentfull <- subset(dat, select=c("continent", "datasetID"))
studybycontinent <- studybycontinentfull[!duplicated(studybycontinentfull), ]
table(studybycontinent$continent)

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
datsm <- subset(dat, select=c("datasetID", "study", "genus", "species", "forcetemp", "forcetemp_night", 
    "photoperiod_day", "fieldsample.date", "chilltemp", "chillphotoperiod", "chilldays")) 
head(datsm)

## If forcetemp_night is empty, we assume it is the same as forcetemp
datsm$forcetemp_night[which(datsm$forcetemp_night=="")] <- datsm$forcetemp[which(datsm$forcetemp_night=="")]
datsm$forcetemp_night[which(is.na(datsm$forcetemp_night)==TRUE)] <- datsm$forcetemp[which(is.na(datsm$forcetemp_night)==TRUE)]

## Okay, formatting to look at intxns
datsm$force <- as.numeric(datsm$forcetemp)
datsm$forcemean <- (datsm$force + as.numeric(datsm$forcetemp_night))/24
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

# datsm14d <- subset(datsm14d, is.na(force)==FALSE & is.na(photo)==FALSE) to write out ospree_countinxns.noNA.csv

datsm14d.noNA <- subset(datsm14d, is.na(force)==FALSE & is.na(photo)==FALSE)

# Start gathering data ... 
ospcounts <- data.frame(treat1=character(), treat2=character(), n=numeric())

## Single treatments
forceosp <- get.treatdists.singletreatment(datsm14d, "force")
forceospnight <- get.treatdists.singletreatment(datsm14d, "forcetemp_night")
photoosp <- get.treatdists.singletreatment(datsm14d, "photo")
chilltemposp <- get.treatdists.singletreatment(datsm14d, "chilltemp")
chilldaysosp <- get.treatdists.singletreatment(datsm14d, "chilldays")
fsdatesosp <- get.treatdists.singletreatment(datsm14d, "fieldsample.date")

# Check how many studies did forcing day or night ...
forceall <- union(unique(paste(forceosp$datasetID, forceosp$study, sep="_")), paste(forceospnight$datasetID,
   forceospnight$study, sep="_")) # 46... so I need to deal with this somehow

# chilldays includes experiments and field sample dates ... so we need to check and udpate ...
checkchillist <- setdiff(unique(paste(chilldaysosp$datasetID, chilldaysosp$study, sep="_")), paste(fsdatesosp$datasetID,
   fsdatesosp$study, sep="_")) # these have chilldays but not multiple field sampled dates; need to check the papers!
chilldaysosp.prep <- data.frame(datasetID=rep(NA, length(checkchillist)), study=rep(NA, length(checkchillist)))
chilldaysosp.prep$datasetID <- unlist(lapply(strsplit(as.character(checkchillist), "_", fixed=TRUE), function(x) x[1]))
chilldaysosp.prep$study <- unlist(lapply(strsplit(as.character(checkchillist), "_", fixed=TRUE), function(x) x[2]))
chilldaysospuse <- merge(chilldaysosp.prep, chilldaysosp, by=c("datasetID", "study"), all.x=TRUE)

# combine chilldays and chilltemp (so, experimental chilling)
allchill <- union(checkchillist, paste(chilltemposp$datasetID, chilltemposp$study, sep="_"))

ospcounts <- rbind(ospcounts, data.frame(treat1="force", treat2="single treat",
    n=nrow(forceosp)), data.frame(treat1="photo", treat2="single treat",
    n=nrow(photoosp)), data.frame(treat1="chilltemp", treat2="single treat",
    n=nrow(chilltemposp)), data.frame(treat1="chilldays", treat2="single treat",
    n=length(checkchillist)), data.frame(treat1="chill days or temp", treat2="single treat",
    n=length(allchill)), data.frame(treat1="fieldsample.date", treat2="single treat",
    n=nrow(fsdatesosp)))

## From here we can ask about which studies did two factors ...
# (but not neccessarily interactively) ##
osp14d.fpall <- intersect(unique(paste(forceosp$datasetID, forceosp$study, sep="_")), 
   paste(photoosp$datasetID, photoosp$study, sep="_"))
osp14d.ctfall <- intersect(unique(paste(forceosp$datasetID, forceosp$study, sep="_")), 
   paste(chilltemposp$datasetID, chilltemposp$study, sep="_"))
osp14d.ctpall <- intersect(unique(paste(photoosp$datasetID, photoosp$study, sep="_")), 
   paste(chilltemposp$datasetID, chilltemposp$study, sep="_"))
osp14d.daysf <- intersect(unique(paste(forceosp$datasetID, forceosp$study, sep="_")), 
   paste(fsdatesosp$datasetID, fsdatesosp$study, sep="_"))
osp14d.daysp <- intersect(unique(paste(photoosp$datasetID, photoosp$study, sep="_")), 
   paste(fsdatesosp$datasetID, fsdatesosp$study, sep="_"))
osp14d.cdf <- intersect(unique(paste(forceosp$datasetID, forceosp$study, sep="_")), 
   checkchillist)
osp14d.cdp <- intersect(unique(paste(photoosp$datasetID, photoosp$study, sep="_")), 
   checkchillist)
osp14d.cddays <- intersect(unique(paste(fsdatesosp$datasetID, fsdatesosp$study, sep="_")), 
   checkchillist) # do any studies manipulating chilling days also manipulate field sample date? (No.)
osp14d.cdct <- intersect(unique(paste(fsdatesosp$datasetID, fsdatesosp$study, sep="_")), 
   paste(chilltemposp$datasetID, chilltemposp$study, sep="_")) # do any studies manipulating chill temp also manipulate field sample date? ("pagter15_exp1" "skre08_exp1")
osp14d.fc <- intersect(unique(paste(forceosp$datasetID, forceosp$study, sep="_")), 
   allchill)
osp14d.pc <- intersect(unique(paste(photoosp$datasetID, photoosp$study, sep="_")), 
   allchill)

ospcounts <- rbind(ospcounts, data.frame(treat1="force", treat2="photo ALL",
    n=length(osp14d.fpall)), data.frame(treat1="force", treat2="chill temp ALL",
    n=length(osp14d.ctfall)), data.frame(treat1="photo", treat2="chill temp ALL",
    n=length(osp14d.ctpall)), data.frame(treat1="force", treat2="fieldsample days ALL",
    n=length(osp14d.daysf)), data.frame(treat1="photo", treat2="fieldsample ALL",
    n=length(osp14d.daysp)), data.frame(treat1="force", treat2="chill days ALL",
    n=length(osp14d.cdf)), data.frame(treat1="photo", treat2="chill days ALL",
    n=length(osp14d.cdp)), data.frame(treat1="fieldsample", treat2="chill days ALL",
    n=length(osp14d.cddays)), data.frame(treat1="fieldsample", treat2="chill temp ALL",
    n=length(osp14d.cdct)), data.frame(treat1="force", treat2="chill days or temp ALL",
    n=length(osp14d.fc)), data.frame(treat1="photo", treat2="chill day or temp ALL",
    n=length(osp14d.pc)))

## Now ask which studies did three factors ...
# (but not neccessarily interactively) ##
fpct <- intersect(osp14d.fpall, paste(chilltemposp$datasetID, chilltemposp$study, sep="_"))
fpcd <- intersect(osp14d.fpall, checkchillist)
fpfs <- intersect(osp14d.fpall, paste(fsdatesosp$datasetID, fsdatesosp$study, sep="_"))
fpc <- intersect(osp14d.fpall, allchill)

ospcounts <- rbind(ospcounts, data.frame(treat1="force", treat2="photo and chill temp ALL",
    n=length(fpct)), data.frame(treat1="force", treat2="photo and chill days ALL",
    n=length(fpcd)), data.frame(treat1="photo", treat2="force and fieldsample ALL",
    n=length(fpfs)), data.frame(treat1="photo", treat2="force and chill days or temp ALL",
    n=length(fpfs)))

## Two-way interactive treatments 
osp14d.fp <- get.treatdists(datsm14d, "photo", "force")
osp14d.fpintxn <- subset(osp14d.fp, intxn>=2) # 14 studies
osp14d.fpintxn[order(osp14d.fpintxn$datasetID),]

osp14d.ctf <- get.treatdists(datsm14d, "chilltemp", "force")
osp14d.ctfintxn <- subset(osp14d.ctf, intxn>=2) # 2 studies

osp14d.ctp <- get.treatdists(datsm14d, "chilltemp", "photo")
osp14d.ctpintxn <- subset(osp14d.ctp, intxn>=2) # 3 studies

# get chill days but subset to ones that are no fieldsample date
osp14d.cdf <- get.treatdists(datsm14d, "chilldays", "force")
osp14d.cdfintxn.all <- subset(osp14d.cdf, intxn>=2) # same 2 studies # skuterud94  exp1  &  heide12  exp2
osp14d.cdfintxn <- osp14d.cdfintxn.all[which(paste(osp14d.cdfintxn.all$datasetID,
   osp14d.cdfintxn.all$study, sep="_") %in% paste(chilldaysospuse$datasetID, chilldaysospuse$study, sep="_")),]
# how many of the single chill studies are interactive?
intersect(unique(paste(chilldaysospuse$datasetID, chilldaysospuse$study, sep="_")), paste(osp14d.cdfintxn$datasetID,
   osp14d.cdfintxn$study, sep="_"))

# repeat of just above, but for photo only
osp14d.cdp <- get.treatdists(datsm14d, "chilldays", "photo")
osp14d.cdpintxn.all <- subset(osp14d.cdp, intxn>=2) 
osp14d.cdpintxn <- osp14d.cdpintxn.all[which(paste(osp14d.cdpintxn.all$datasetID,
   osp14d.cdpintxn.all$study, sep="_") %in% paste(chilldaysospuse$datasetID, chilldaysospuse$study, sep="_")),]
intersect(unique(paste(chilldaysospuse$datasetID, chilldaysospuse$study, sep="_")), paste(osp14d.cdpintxn$datasetID,
   osp14d.cdpintxn$study, sep="_"))

osp14d.daysf <- get.treatdists(datsm14d, "fieldsample.date", "force")
osp14d.daysfintxn <- subset(osp14d.daysf, intxn>=2) # 9 studies

osp14d.daysp <- get.treatdists(datsm14d, "fieldsample.date", "photo")
osp14d.dayspintxn <- subset(osp14d.daysp, intxn>=2) # 11 studies

osp14d.allchillf <- union(paste(osp14d.cdfintxn$datasetID, osp14d.cdfintxn$study, sep="_"),
   paste(osp14d.ctfintxn$datasetID, osp14d.ctfintxn$study, sep="_"))

osp14d.allchillp <- union(paste(osp14d.cdpintxn$datasetID, osp14d.cdpintxn$study, sep="_"),
   paste(osp14d.ctpintxn$datasetID, osp14d.ctpintxn$study, sep="_"))

length(unique(paste(datsm14d$datasetID, datsm14d$study)))

ospcounts <- rbind(ospcounts, data.frame(treat1="force", treat2="photo INTERACTIVE",
    n=length(unique(paste(osp14d.fpintxn$datasetID, osp14d.fpintxn$study)))),
    data.frame(treat1="chilltemp", treat2="force INTERACTIVE",
    n=length(unique(paste(osp14d.ctfintxn$datasetID, osp14d.ctfintxn$study)))),
    data.frame(treat1="chilltemp", treat2="photo INTERACTIVE",
    n=length(unique(paste(osp14d.ctpintxn$datasetID, osp14d.ctpintxn$study)))),
    data.frame(treat1="chilldays", treat2="force INTERACTIVE",
    n=length(unique(paste(osp14d.cdfintxn$datasetID, osp14d.cdfintxn$study)))),
    data.frame(treat1="chilldays", treat2="photo INTERACTIVE",
    n=length(unique(paste(osp14d.cdpintxn$datasetID, osp14d.cdpintxn$study)))),
    data.frame(treat1="fieldsample.date", treat2="force INTERACTIVE",
    n=length(unique(paste(osp14d.daysfintxn$datasetID, osp14d.daysfintxn$study)))),
    data.frame(treat1="fieldsample.date", treat2="photo INTERACTIVE",
    n=length(unique(paste(osp14d.dayspintxn$datasetID, osp14d.dayspintxn$study)))),
    data.frame(treat1="chill days or temp", treat2="force INTERACTIVE",
    n=length(osp14d.allchillf)),
    data.frame(treat1="chill days or temp", treat2="photo INTERACTIVE",
    n=length(osp14d.allchillp)))

# thermoperiodicity
osp14d.moreforcinginfo <- get.treatdists.daynight(datsm14d, "forcetemp", "forcetemp_night")
osp14d.forcingvaried <- subset(osp14d.moreforcinginfo, treatinfo!="forcing does not vary")
osp14d.studiesinclconstantforce <- subset(osp14d.forcingvaried, numconstantforce>0) # some studies have both
osp14d.studiesinclforceperiodicity <- subset(osp14d.forcingvaried, numdiffforce>0) # some studies have both

ospcounts <- rbind(ospcounts, data.frame(treat1="total-varied-forcing", treat2="NA",
    n=nrow(osp14d.forcingvaried)), data.frame(treat1="total-wsometreats-constant-forcing", treat2="NA",
    n=nrow(osp14d.studiesinclconstantforce)),  data.frame(treat1="total-variedwsometreats-varydaynight-forcing", treat2="NA",
    n=nrow(osp14d.studiesinclforceperiodicity)), data.frame(treat1="ALL studies", treat2="NA",
    n=length(unique(paste(datsm14d$datasetID, datsm14d$study)))))

write.csv(ospcounts, "limitingcues/output/ospree_countinxns.csv", row.names=FALSE)

###############
## OKAY! Next...
## ***
## (1) [Check again once we deal with numeric versus character issue!] Why is total forcing different higher in thermo versus force code? Some studies vary night forcing only, and then you get MORE identified if you use datsm14d instead of datsm14d.noNA ... but still different (46 versus 48) 
## (2) Why is my number of studies that manipulated forcing so much less than Cat's? For forcing I am missing all the ambient and ambient + X studies (except that Cat has gomory15 and I don't think that has multiple forcing temps)
## (3) Once all sorted, working on heatmaps ...

gooall <- read.csv("output/studytype_table.csv")
goo <- gooall[which(paste(gooall$datasetID, gooall$study) %in% unique(paste(datsm14d$datasetID, datsm14d$study))),] # makes no difference, we're using the same studies!
goober <- subset(goo, force>1)
length(unique(goober$datasetID))
length(unique(forceosp$datasetID))

setdiff(unique(goober$datasetID), unique(forceosp$datasetID))

goober[order(unique(goober$datasetID)),]
forceosp[order(unique(forceosp$datasetID)),] # I don't have (not complete list): caffarra11a exp3 or exp1

photoosp[order(photoosp$datasetID),]

if(FALSE){ # checking part of my code ...
# forceosp <- get.treatdists.singletreatment(datsm14d.noNA, "force")
df <- datsm14d
treatcol <- "force"

did <- "caffarra11a"
studyid <- "exp3" # ONE forcing treat of 18; exp1 looks like ONE forcing treat of 22 
    
get.treatdists.singletreatment <- function(df, treatcol){
    dfbuild <- data.frame(datasetID=character(), study=character(), ntreats=numeric())
    for (did in unique(df[["datasetID"]])){
     subbydid <- subset(df, datasetID==did)
       for (studyid in unique(subbydid$study)){
          subbydidexp.allcols <- subset(subbydid, study==studyid)
          subbydidexp <- subbydidexp.allcols[,c(treatcol)]
          dfbuildadd <- data.frame(datasetID=did, study=studyid, ntreats=length(unique(subbydidexp)))
          dfbuild <- rbind(dfbuild, dfbuildadd)
          dfbuild.multi <- subset(dfbuild, ntreats>1)
          }
     }
  return(dfbuild.multi)
}    
# osp14d.moreforcinginfo <- get.treatdists.daynight(datsm14d.noNA, "forcetemp", "forcetemp_night")
}
# in Cat's some show up more than once, e.g.,   caffarra11a  exp2 or  falusi97  exp1





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
# Note that for some reason we don't have to do these gymnastics above (CHECK WHY before finishing limitingcues!)
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

# bb14d.noNA <- bb14d # Below versus shown here, does not seem to matter, even to full model
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

## First, if forcetemp_night is empty, we assume it is the same as forcetemp
bb14d.noNA$forcetemp_night[which(bb14d.noNA$forcetemp_night=="")] <-
    bb14d.noNA$forcetemp[which(bb14d.noNA$forcetemp_night=="")]
bb14d.noNA$forcetemp_night[which(is.na(bb14d.noNA$forcetemp_night)==TRUE)] <-
    bb14d.noNA$forcetemp[which(is.na(bb14d.noNA$forcetemp_night)==TRUE)]

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

