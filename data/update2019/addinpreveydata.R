## 19 July 2019 - Cat
# Sorting through data to get raw means and standard errors for each tx and species for OSPREE

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set Working directory
setwd("~/Documents/git/ospree/data/update2019") 

prevflo <- read.csv("prevey_raw.csv", header=TRUE)

prevflo$meanflo <- ave(prevflo$DOY, prevflo$Treatment, prevflo$Sex, FUN=function(x) mean(x, na.rm=T))
prevflo$sdflo <- ave(prevflo$DOY, prevflo$Treatment, prevflo$Sex, FUN=function(x) sd(x, na.rm=T))
prevflo$nflo <- ave(prevflo$DOY, prevflo$Treatment, prevflo$Sex, FUN=length)

meanprev <- subset(prevflo, select=c("species", "Treatment", "Sex",  "meanflo", "sdflo", "nflo"))
meanprev <- meanprev[!duplicated(meanprev),]

meanprev$Treatment <- ifelse(meanprev$Treatment=="Ambient", "ambient_5.7", meanprev$Treatment)
meanprev$Treatment <- ifelse(meanprev$Treatment=="Ambient/greenhouse", "ambientgreenhouse_4.4", meanprev$Treatment)
meanprev$Treatment <- ifelse(meanprev$Treatment=="Webster", "webster_4.5", meanprev$Treatment)
meanprev$Treatment <- ifelse(meanprev$Treatment=="Webster/greenhouse", "webstergreenhouse_4.3", meanprev$Treatment)

### Can we make an excel sheet that matches ospree?...
osp.df <- data.frame(datasetID="prevey18", study="exp1", Entered.By="CJC", 
                  genus="Alnus", species="rubra",
                  varetc="", woody="yes", population="Webster Nursery", population.detail="Washington", provenance.lat=38.898556,
                  provenance.long=-77.037852,
                  population.altitude.m="", 
                  continent="North America", year=2016, material="cuttings", fieldchill="yes", 
                  fieldsample.date="",
                  chilltemp=meanprev$Treatment, chillphotoperiod="ambient", 
                  chilldays="", 
                  forcetemp="",
                  forcetemp_night="",
                  photoperiod_day="ambient",
                  photoperiod_night="ambient", 
                  respvar="daystoflower",
                  response=1, response.time="", n=meanprev$nflo, error.type="standard deviation", resp.error=meanprev$sdflo,
                  figure.table..if.applicable.="", growing.lat="", growing.long="", 
                  field.chill.units="NA", 
                  cu.model="")

latefieldsamps <- c("webstergreenhouse_4.3", "webster_4.5")
osp.df$fieldsample.date <- NA
osp.df$fieldsample.date <- ifelse(!osp.df$chilltemp%in%latefieldsamps, "01-Nov-2016", "12-Jan-2017")

nochilldays <- c("ambient_5.7", "webster_4.5")
osp.df$chilldays <- NA
osp.df$chilldays <- ifelse(!osp.df$chilltemp%in%nochilldays, 90, "")

highforces <- c("4", "9", "ambientgreenhouse")
osp.df$forcetemp <- NA
osp.df$forcetemp <- ifelse(meanprev$Treatment%in%highforces, 16, osp.df$forcetemp)
osp.df$forcetemp <- ifelse(meanprev$Treatment==c("ambient_5.7"), 5.7, osp.df$forcetemp)
osp.df$forcetemp <- ifelse(meanprev$Treatment==c("webstergreenhouse_4.3"), 14.4, osp.df$forcetemp)
osp.df$forcetemp <- ifelse(meanprev$Treatment==c("webster_4.5"), 4.3, osp.df$forcetemp)


osp.df$forcetemp_night <- osp.df$forcetemp

write.csv(osp.df, "~/Documents/git/ospree/data/update2019/prevey18_data.csv", row.names=FALSE)
