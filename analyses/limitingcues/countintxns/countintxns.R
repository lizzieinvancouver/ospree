## Started 2 March 2019 ##
## By Lizzie ##

# This tries to count *interactive experiments*, ideally for all possible intxns across photoperiod, chilling, forcing, and field sample date (maybe provenance) treatments! #
# I don't recommend it for counting the number of cues. For that, I think Cat's code studydesign_numcues.R is much saner. #

## See also: https://github.com/lizzieinvancouver/ospree/issues/235

################
## To do here ##
################
# (*) Decide how to deal with forcing having varying night and day treatments (do I mean check that it is NOT what they varied? I think if I take forcemean I should capture this)
# (*) Make heatmaps ...
# (*) Deal with 'CHECK WHY' below

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
 } else if(length(grep("ailene", getwd())>0)){  setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses")
} else setwd("~/Documents/git/ospree/analyses")

source("misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("misc/gettreatdists.R") # f(x) counts up treatment interactions, and more!
print('Code also relies on sourcing limitingcues/source/countintxns_cleanosp.R (see below)')

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

# Studies by species ...
spbydatasetIDfull <- subset(dat, select=c("datasetID", "latbi"))
spbydatasetID <- spbydatasetIDfull[!duplicated(spbydatasetIDfull), ]
spbydatasetID.df <- aggregate(spbydatasetID["datasetID"], spbydatasetID["latbi"], FUN=length)
spbydatasetID.df <- spbydatasetID.df[order(spbydatasetID.df$datasetID),]
nrow(spbydatasetID.df)
nrow(subset(spbydatasetID.df, datasetID>1))
nrow(subset(spbydatasetID.df, datasetID>2))

# Get the number of field sampling dates that are 14 or more weeks apart, first for each datasetIDx study ...
# This is just to correct for studies that truly meant to have muultiple field sample dates (versus, for example, when ...
# we go to a site and it takes us three days to sample) #
ddatefx.all <- subset(dat, select=c("datasetID", "study", "fieldsample.date"))
ddatefx <- ddatefx.all[!duplicated(ddatefx.all), ]
ddatefx$datasetIDstudy <- paste(ddatefx$datasetID, ddatefx$study)

## START: Change main DF so only relevant dates are included ... not pretty, but should work (and goes on for a while)
# First get the unique dates in a df
dates2weeks.count <- countfieldsample(ddatefx, 14)
uniquedates.df <- fieldsample.getuniquedates(ddatefx, 14)
uniquedates.df$selectcolumn <- paste(uniquedates.df$datasetIDstudy, uniquedates.df$date)
# Now subset to sane # of columnns
datsm <- subset(dat, select=c("datasetID", "study", "genus", "species", "forcetemp", "forcetemp_night", 
    "photoperiod_day", "fieldsample.date", "chilltemp", "chillphotoperiod", "chilldays")) 
head(datsm)

# Some photoperiods labelled as 'constant' ... should check why (from my quick look at karlsson03 I could not see why; others were "cronje03"   and "devries82
# constantphoto <- subset(datsm, photoperiod_day=="constant")

## Okay, since code effectively relies on numeric cues for force and photo we clean these here
# It's ugly and inaccurate (in terms of numbers assigned) but works *for this code* (don't use the numbers assigned as treatments here for other analyses)
source("limitingcues/source/countintxns_cleanosp.R")

## If forcetemp_night is empty, we assume it is the same as forcetemp
datsm$forcetemp_night[which(datsm$forcetemp_night=="")] <- datsm$forcetemp[which(datsm$forcetemp_night=="")]
datsm$forcetemp_night[which(is.na(datsm$forcetemp_night)==TRUE)] <- datsm$forcetemp[which(is.na(datsm$forcetemp_night)==TRUE)]

## Okay, formatting to look at intxns
datsm$force <- as.numeric(datsm$forcetemp)
datsm$forcemean <- (datsm$force + as.numeric(datsm$forcetemp_night))/24
datsm$photo <- as.numeric(datsm$photoperiod_day)

datsm.noNA <- subset(datsm, is.na(force)==FALSE & is.na(photo)==FALSE)
dim(datsm.noNA)
dim(datsm) # Good, we should not lose any rows here!

osp.fp <- get.treatdists(datsm.noNA, "photo", "force")
osp.fpintxn <- subset(osp.fp, intxn>=2)
osp.fpintxn[order(osp.fpintxn$datasetID),]

osp.ctf <- get.treatdists(datsm.noNA, "chilltemp", "force")
osp.ctfintxn <- subset(osp.ctf, intxn>=2)

osp.cdf <- get.treatdists(datsm.noNA, "chilldays", "force")
osp.cdfintxn <- subset(osp.cdf, intxn>=2)

lookatunique <- get.uniquetreats(datsm.noNA, "photo", "force")

# Back to the subsetting by field sample dates
# Now (not pretty part) we'll take all NA dates ...
datsm$selectcolumn <- paste(datsm$datasetID, datsm$study, datsm$fieldsample.date)
datsm14d <- datsm[which(datsm$selectcolumn %in% uniquedates.df$selectcolumn),]

dim(datsm)
dim(datsm14d) # not such a huge loss of rows

# Check we don't lose any datasets!
setdiff(unique(paste(datsm$datasetID, datsm$study)), unique(paste(datsm14d$datasetID, datsm14d$study)))

datsm14d.noNA <- subset(datsm14d, is.na(force)==FALSE & is.na(photo)==FALSE)
dim(datsm14d.noNA)
dim(datsm14d) # Good, again we should not lose any rows here
## END: Change main DF so only relevant dates are included


# Start gathering data ...
# My whole approach is to count studies, so set up empty DF
ospcounts <- data.frame(treat1=character(), treat2=character(), n=numeric())

## Let's start with single treatments ....
# get.treatdists.singletreatment f(x) counts based on one treatment (ignoring the other treatments)
forceosp <- get.treatdists.singletreatment(datsm14d, "force")
forceospnight <- get.treatdists.singletreatment(datsm14d, "forcetemp_night")
photoosp <- get.treatdists.singletreatment(datsm14d, "photo")
chilltemposp <- get.treatdists.singletreatment(datsm14d, "chilltemp")
chilldaysosp <- get.treatdists.singletreatment(datsm14d, "chilldays")
fsdatesosp <- get.treatdists.singletreatment(datsm14d, "fieldsample.date")

# Check how many studies did forcing day or night ...
forceall <- union(unique(paste(forceosp$datasetID, forceosp$study, sep="_")), paste(forceospnight$datasetID,
   forceospnight$study, sep="_")) # 46... so I need to deal with this somehow

# The column chilldays includes chilldays due to experiments and due field sample dates ...
# So we need to check that we count these accurately! To do so ... 
# Lizzie worked through many of these in 2020 for her studydesignplots.R code
# Definitely check out studydesignplots.R code for some relevant cleaning... 
# She wrote notes in https://github.com/lizzieinvancouver/ospree/issues/249 on many of the studies here

# But for here ... below I grab studies that have chilldays but not multiple field sampled dates; need to check the papers!
checkchillist <- setdiff(unique(paste(chilldaysosp$datasetID, chilldaysosp$study, sep="_")), paste(fsdatesosp$datasetID,
   fsdatesosp$study, sep="_")) 
chilldaysosp.prep <- data.frame(datasetID=rep(NA, length(checkchillist)), study=rep(NA, length(checkchillist)))
chilldaysosp.prep$datasetID <- unlist(lapply(strsplit(as.character(checkchillist), "_", fixed=TRUE), function(x) x[1]))
chilldaysosp.prep$study <- unlist(lapply(strsplit(as.character(checkchillist), "_", fixed=TRUE), function(x) x[2]))
chilldaysospuse <- merge(chilldaysosp.prep, chilldaysosp, by=c("datasetID", "study"), all.x=TRUE)
# There are a LOT of studies in checkchillist, so I will pull in some of the info from BB analysis in case that helps
dfbb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
dfbbchill <- dfbb[which(paste(dfbb$datasetID, dfbb$study, sep="_") %in% checkchillist),]
dfbbchillsm <- subset(dfbbchill, select=c("datasetID", "study", "chill_type"))
checkchillbb <- dfbbchillsm[!duplicated(dfbbchillsm),]
checkchillbyhand <- checkchillist[!which(checkchillist %in% paste(dfbb$datasetID, dfbb$study, sep="_"))]
# So, a few rows in falusi90 exp1, cannell83 (exp 1-2), granhus09 (exp 1) and worrall67 exp 2 still show as NA -- the rest are a mix

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
# But not neccessarily interactively! We're just using the intersect command to see....
# which studies have >1 cue manipulated in multiple cues ##
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
# (but, again, not neccessarily interactively) ##
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
# get.treatdists f(x) is designed to look for interactions!
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

# Now! A dataframe of how each study was categorized -- IMPT! a study can get into multiple categories, so I manually fix it below (ugly code) so each study is one row
ospintxnstudies <- data.frame(datasetID=character(), study=character(), intxn=character())

ospintxnstudies <- rbind(ospintxnstudies,
    data.frame(datasetID=osp14d.fpintxn$datasetID, study=osp14d.fpintxn$study, intxn=rep("force x photo", nrow(osp14d.fpintxn))),
    data.frame(datasetID=osp14d.ctfintxn$datasetID, study=osp14d.ctfintxn$study, intxn=rep("force x chilltemp", nrow(osp14d.ctfintxn))),
    data.frame(datasetID=osp14d.ctpintxn$datasetID, study=osp14d.ctpintxn$study, intxn=rep("photo x chilltemp", nrow(osp14d.ctpintxn))),
    data.frame(datasetID=osp14d.cdfintxn$datasetID, study=osp14d.cdfintxn$study, intxn=rep("force x chilldays", nrow(osp14d.cdfintxn))),
    data.frame(datasetID=osp14d.cdpintxn$datasetID, study=osp14d.cdpintxn$study, intxn=rep("photo x chilldays", nrow(osp14d.cdpintxn))),
    data.frame(datasetID=osp14d.daysfintxn$datasetID, study=osp14d.daysfintxn$study, intxn=rep("force x fieldsample dates",
        nrow(osp14d.daysfintxn))),
    data.frame(datasetID=osp14d.dayspintxn$datasetID, study=osp14d.dayspintxn$study, intxn=rep("photo x fieldsample dates",
        nrow(osp14d.dayspintxn)))) # Does not include osp14d.allchillf and osp14d.allchillp work

ospintxnstudiesall <- ospintxnstudies[order(ospintxnstudies$datasetID),]
ospintxnstudies.count <- aggregate(ospintxnstudies["intxn"], ospintxnstudies[c("datasetID",
    "study")], FUN=length)
ospintxnstudies.rm <- subset(ospintxnstudies.count, intxn>1)
ospintxnstudies <- ospintxnstudiesall[!duplicated(paste(ospintxnstudiesall$datasetID, ospintxnstudiesall$study)),]

# Remove and re-label the datasetID-study in ospintxnstudies.count with intxn>1
# How did I check these to relabel them?
# (1) I looked at the data if it was short enough to quickly look through after subsetting datsm14d...
# (2) Otherwise I checked the paper
# (3) Then I crossed-checked against ospintxnstudiesall: three-way interactions should have 3 rows, and otherwise what intxns are found should match to my notes
# (4) I checked any suspicious ones using code below
if(FALSE){
test <- subset(datsm14d, datasetID=="okie11" & study=="exp2")
testsm <- subset(test, select=c("forcetemp", "forcetemp_night", "photoperiod_day",
    "fieldsample.date", "chilltemp", "chilldays"))
testtreats <- testsm[!duplicated(testsm),]
}

ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="worrall67" &
    ospintxnstudies$study=="exp 3")] <-
    "photo/force x exp chilldays (force and photo are NOT interactive)"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="basler14" &
    ospintxnstudies$study=="exp1")] <- "photo x force x field sample dates"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="falusi90" &
    ospintxnstudies$study=="exp1")] <- "photo x exp chilling (temp and days)"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="heide12" &
    ospintxnstudies$study=="exp1")] <- "photo x exp chilling (x different sized plants)"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="heide93" &
    ospintxnstudies$study=="exp1")] <- "photo x force x field sample dates"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="heide93" &
    ospintxnstudies$study=="exp2")] <- "photo x force x field sample dates"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="myking95" &
    ospintxnstudies$study=="exp1")] <- "photo x chilltemp (with also change in chilldays)"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="partanen98" &
    ospintxnstudies$study=="exp1")] <-
    "photo/force x field sample dates (force and photo are NOT full factorial)"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="schnabel87" &
    ospintxnstudies$study=="exp1")] <-
    "force/photo x field sample (but mainly field sample dates)"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="skuterud94" &
    ospintxnstudies$study=="exp1")] <- "force x field sample x chilltemp (x provenance)"
ospintxnstudies$intxn[which(ospintxnstudies$datasetID=="okie11" &
    ospintxnstudies$study=="exp2")] <- "photo x force x chill days"

write.csv(ospcounts, "limitingcues/output/ospree_countinxns.csv", row.names=FALSE)
write.csv(ospintxnstudies, "limitingcues/output/ospree_studyinxns.csv", row.names=FALSE)
write.csv(datsm14d, "limitingcues/output/osp14d_forheatmaps.csv", row.names=FALSE)

#### Cat adding in some code to fill in MS with info. Using code from studydesing_numcues.R
## 8 Feb 2021
## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
osp <- read.csv("output/ospree_clean.csv")
#osp<-read.csv("output/ospree_clean_withchill_BB.csv")
studfile <- read.csv("output/studytype_table.csv", header=TRUE)
#studfile <- read.csv("output/studytype_withBB.csv", header=TRUE)

osp <- osp[osp$woody=="yes",]
osp$fieldsample.date <- as.Date(osp$fieldsample.date, format="%d-%b-%Y")
#head(d)

###
###
lookupstudyyr <- aggregate(osp[c("year")], osp[c("datasetID", "study")], FUN=mean)
stud <- merge(studfile, lookupstudyyr, by=c("datasetID", "study"), all.x=TRUE, all.y=TRUE)

cues<-subset(stud, select=c("study", "datasetID", "force", "photo", "chill", "chilltime", "year", "prov.lat", "spp", "field.sample"))
cues<-cues[!duplicated(cues),]

cues$force<-ifelse(cues$force<=1, 0, 1)
cues$photo<-ifelse(cues$photo<=1, 0, 1)
cues$chill<-ifelse(cues$chill<=1, 0, 1)
cues$chilltime<-ifelse(cues$chilltime<=1, 0, 1)
cues$chill<-ifelse(cues$chill==1 | cues$chilltime==1, 1, 0)

cues$numcues<-cues$force + cues$photo + cues$chill

cues<-cues[!(cues$datasetID=="sogaard08" & cues$numcues==0),]
cues<-cues[!(cues$datasetID=="guerriero90" & cues$numcues==0 & cues$field.sample==1),]
cues<-cues[!(cues$datasetID=="spiers74" & cues$numcues==0 & cues$field.sample==1),]
cues<-cues[!(cues$datasetID=="worrall67" & cues$numcues==0 & cues$prov.lat==1),]
cues<-cues[!(cues$datasetID=="falusi97" & cues$numcues==0),]
cues<-cues[!(cues$datasetID=="jones12" & cues$numcues==0),]
cues<-cues[!(cues$datasetID=="sonsteby14" & cues$numcues==0),]
cues<-cues[!(cues$datasetID=="thielges" & cues$numcues==0),]
cues<-cues[!(cues$datasetID=="granhus09" | cues$datasetID=="rinne94" | cues$datasetID=="spann04"),]

cues$year<-round(cues$year, digits=0)
cues<-cues[!duplicated(cues),]

cues <- subset(cues, select=c("datasetID", "study", "force", "photo", "chill", "chilltime", "year", "spp",
                              "field.sample", "numcues"))

ospintxnstudiesall$interaction <- 1
ospstudies_cues <- merge(cues, ospintxnstudiesall, by=c("datasetID", "study"), all.x=TRUE, all.y=TRUE)

ospstudies_cues$interaction <- ifelse(is.na(ospstudies_cues$interaction), 0, ospstudies_cues$interaction)
lookupintxns<- aggregate(ospstudies_cues[c("interaction")], ospstudies_cues[c("datasetID")], FUN=sum)
lookupintxns$foo <- ifelse(lookupintxns$interaction>=1, 1, 0)

## Okay now, to bring in the info on cues manipulated:
ospcues <- within(osp, { force <- as.numeric(ifelse(forcetemp!=0, ave(forcetemp, datasetID, FUN=function(x) length(unique(x))), 0))}) # mult forcetemp
ospcues <- within(ospcues, { photo <- as.numeric(ifelse(photoperiod_day!=0, ave(photoperiod_day, datasetID, FUN=function(x) length(unique(x))), 0))}) # mult photoperiod_day
ospcues <- within(ospcues, { chill <- as.numeric(ifelse(chilltemp!=0, ave(chilltemp, datasetID, FUN=function(x) length(unique(x))), 0))}) # mult studychill
ospcues <- within(ospcues, { chilltime <- as.numeric(ifelse(chilldays!=0, ave(chilldays, datasetID, FUN=function(x) length(unique(x))), 0))}) # mult studychill

ospcues <- subset(ospcues, select=c("datasetID", "force", "photo", "chill", "chilltime"))
ospcues <- ospcues[!duplicated(ospcues),]

lookupcues <- aggregate(ospcues[c("force", "photo", "chill", "chilltime")], ospcues[c("datasetID")], FUN=sum)

### Look into single cue studies a little ...
singlecues <- lookupintxns[(lookupintxns$interaction==0),]
lookupcues[which(lookupcues$datasetID %in% singlecues$datasetID),]

#### List of numbers in manuscript:
numspps <- nrow(spbydatasetID.df)
eurstudies <- nrow(studybycontinent[(studybycontinent=="europe"),])
allstudies <- nrow(studybycontinent)
namstudies <- nrow(studybycontinent[(studybycontinent=="north america"),])

photocue <- round(nrow(lookupcues[(lookupcues$photo>1),])/nrow(lookupcues) * 100, digits=0)
forcecue <- round(nrow(lookupcues[(lookupcues$force>1),])/nrow(lookupcues) * 100, digits=0)
chillcue <- round(nrow(lookupcues[(lookupcues$chill>1 | lookupcues$chilltime>1),])/nrow(lookupcues) * 100, digits=0)

numsinglestudies <- round(nrow(lookupintxns[(lookupintxns$interaction==0),])/nrow(lookupintxns) *100, digits=0)

num.fp <- nrow(ospintxnstudiesall[(ospintxnstudiesall$intxn=="force x photo"),])
num.pw <- nrow(ospintxnstudiesall[(ospintxnstudiesall$intxn=="photo x fieldsample dates"),])
num.fw <- nrow(ospintxnstudiesall[(ospintxnstudiesall$intxn=="force x fieldsample dates"),])

num.cf <- nrow(ospintxnstudiesall[(ospintxnstudiesall$intxn=="force x chilldays" | ospintxnstudiesall$intxn=="force x chilltemp"),])
num.cp <- nrow(ospintxnstudiesall[(ospintxnstudiesall$intxn=="photo x chilldays"| ospintxnstudiesall$intxn=="photo x chilltemp"),])


###############
## This is just a side-bar where I check some of my results versus what
## Cat got in her studytype_table.csv ##

if(FALSE){
altver.all <- read.csv("output/studytype_table.csv")
altver <- altver.all[which(paste(altver.all$datasetID, altver.all$study) %in%
    unique(paste(datsm14d$datasetID, datsm14d$study))),] # just a check that we're using the same studies
altverf <- subset(altver, force>1)
length(unique(altverf$datasetID))
length(unique(forceosp$datasetID))

# May use to see which single cue studies had multiple provenances
multiprov <- subset(altver.all,  prov.lat>1)
unique(paste(multiprov$datasetID, multiprov$study))

setdiff(unique(paste(altverf$datasetID, altverf$study)), unique(paste(forceosp$datasetID,
   forceosp$study))) # Cat has gomory15 and I don't think that has multiple forcing temps; that's because the study co-varies forcing temp with sampling dates that are <2 weeks apart (check out issue 249 and my notes from 23 Sep 2020)

altverp <- subset(altver, photo>1)
length(unique(altverp$datasetID))
length(unique(photoosp$datasetID))
setdiff(unique(paste(altverp$datasetID, altverp$study)), unique(paste(photoosp$datasetID,
   photoosp$study)))
}
###############


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
# Note that for some reason we don't have to do these gymnastics above (CHECK WHY before finishing limitingcues! Assume it is due to the fact that we removed NAs for the budburst analyses ... )
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

# Get the study names (tried to pull all the 'Simple one-by-one interactions')
bbintxnsdfstudnames <- rbind(bb14d.fpintxn[order(bb14d.fpintxn$datasetID),], bb14d.ctfintxn[order(bb14d.ctfintxn$datasetID),],
    bb14d.cdfintxn[order(bb14d.cdfintxn$datasetID),], bb14d.ctpintxn[order(bb14d.ctpintxn$datasetID),],
    bb14d.cdpintxn[order(bb14d.cdpintxn$datasetID),], bb14d.daysfintxn[order(bb14d.daysfintxn$datasetID),],
    bb14d.dayspintxn[order(bb14d.dayspintxn$datasetID),])
bbintxnsdfstudnames <- bbintxnsdfstudnames[!duplicated(bbintxnsdfstudnames), ]


if(mainmodelbb){
names(bbintxnsdf) <- c("Treatment 1", "Treatment 2", "n studies")
write.csv(bbintxnsdf, "limitingcues/output/bbstan_mainmodel_countinxns.csv", row.names=FALSE)
}


if(mainmodelbb){
# names(bbintxnsdf) <- c("Treatment 1", "Treatment 2", "n studies")
write.csv(bbintxnsdfstudnames, "limitingcues/output/bbstan_mainmodel_countinxns_datasetIDs.csv", row.names=FALSE)
}



if(!mainmodelbb){
names(bbintxnsdf) <- c("Treatment 1", "Treatment 2", "n studies")
write.csv(bbintxnsdf, "limitingcues/output/bbstan_allsppmodel_countinxns.csv", row.names=FALSE)
}

