## Started 28 August 2017 ##
## By Lizzie ##

## Working on figures for OSPREE concept paper on interactive cues ##

## TO DO! ##
# Should check/merge with studydesignplots_more.R so they use same data and counts #

## housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else
  print("set your working directory")

# Get packages
library(ggplot2)
library(rgdal)
library(RColorBrewer) 
library(plyr)
library(dplyr)

source("misc/getfielddates.R") # counts up field sample dates separated by a number of days you specify

## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
d <- read.csv("output/ospree_clean.csv")
studfile <- read.csv("output/studytype_table.csv", header=TRUE)

d <- d[d$woody=="yes",]
d$fieldsample.date <- as.Date(d$fieldsample.date, format="%d-%b-%Y")
head(d)

###
###
lookupstudyyr <- aggregate(d[c("year")], d[c("datasetID", "study")], FUN=mean)
stud <- merge(studfile, lookupstudyyr, by=c("datasetID", "study"), all.x=TRUE, all.y=TRUE)

goo <-  aggregate(stud[c("year")], stud[c("field.sample")], FUN=mean)
plot(field.sample~year, data=goo)

###
###
yr <- as.numeric(stud$year)

pdf("limitingcues/figures/pubyear.pdf", width = 8, height = 5)
hist(yr, breaks = 50, xaxt="n", col = "lightblue", main = "Years of Publication")
axis(1, at = seq(min(yr, na.rm=TRUE), max(yr, na.rm=TRUE), by = 3))
dev.off()

# Counting
d$latbi <- paste(d$genus, d$species)
length(unique(d$latbi)) # unique species
d$datasetIDstudy <- paste(d$datasetID, d$study)
length(unique(d$datasetIDstudy)) # unique studies

# ranges
range(as.numeric(as.character(d$forcetemp)), na.rm=TRUE) 
range(as.numeric(as.character(d$chilltemp)), na.rm=TRUE)
range(as.numeric(as.character(d$photoperiod_day)), na.rm=TRUE)

# count field.sample dates ... not currently used in plots
ddatefx.all <- subset(d, select=c("datasetID", "study", "fieldsample.date"))
ddatefx <- ddatefx.all[!duplicated(ddatefx.all), ]
ddatefx$datasetIDstudy <- paste(ddatefx$datasetID, ddatefx$study)

dates2weeks <- countfieldsample(ddatefx, 14)
dates2weeks$count[is.na(dates2weeks$count)==TRUE] <- 0
names(dates2weeks)[names(dates2weeks)=="count"] <- "fs.date.countby14d"

# species number and mean cues ... 
sppsumyr <-
      ddply(d, c("datasetID", "study"), summarise,
      sppn=length(unique(latbi)),
      photo=mean(as.numeric(photoperiod_day), na.rm=TRUE),
      force=mean(as.numeric(forcetemp), na.rm=TRUE),
      chill=mean(as.numeric(chilltemp), na.rm=TRUE),
      year=mean(year))

sppsumyr <- sppsumyr[order(sppsumyr$year),]

colz <- c("darkseagreen", "firebrick1", "gold", "deepskyblue")

pdf("limitingcues/figures/pubstudyyear.pdf", width = 8, height = 5)
plot(sqrt(sppn)~year, data=sppsumyr, ylim=c(-1,40), type="n")
lines(sqrt(sppn)~year, data=sppsumyr, col=colz[1])
points(force~year, data=sppsumyr, col=colz[2])
points(photo~year, data=sppsumyr, col=colz[3])
points(chill~year, data=sppsumyr, col=colz[4])
dev.off()

names(stud)[names(stud)=="photo"] <- "photo.count"
names(stud)[names(stud)=="force"] <- "force.count"
names(stud)[names(stud)=="chill"] <- "chill.count"
names(stud)[names(stud)=="field.sample"] <- "fs.count"
names(stud)[names(stud)=="prov.long"] <- "long.count"
names(stud)[names(stud)=="prov.lat"] <- "lat.count"

studyr <- merge(stud, sppsumyr, by=c("datasetID", "study", "year"))
dim(stud)
dim(studyr)
studyr <- subset(studyr, select=c("datasetID", "study", "year",
    "fs.count", "sppn", "photo.count", "force.count",
     "chill.count", "lat.count", "long.count"))

studyr$exp <- paste(studyr$datasetID, studyr$study)
studyr$yearint <- as.integer(studyr$year)

expperyr <- ddply(studyr, c("yearint"), summarise,
      ndat=length(unique(datasetID)),
      nexp=length(unique(exp)))

plot(ndat~yearint, data=expperyr, ylim=c(-5,30), type="l")
points(fs.count~year, data=studyr, ylim=c(-5,30), col="green")

## counting how many manip two cues
## need to work on this ....
howmany <- subset(studyr, chill.count>1 & photo.count >1)
length(unique(paste(howmany$datasetID, howmany$study)))
length(unique(howmany$datasetID))
# force x photo: 59 / 24
# force x chill: 25 / 8
# chill x photo: 25 / 9
howmany.prov <- subset(studyr,  lat.count>1 & photo.count>1)
length(unique(paste(howmany.prov$datasetID, howmany.prov$study)))
length(unique(howmany.prov$datasetID))
# prov overall: 61 / 33
# prov x force: 36 / 15
# prov x photo: 40 / 18
# prov x chill: 21 / 10
##


##
## get map shape file (see map_notes.R for details)
##

wmap <- readOGR("..//..//..//..//general/maps/ne_110m_land") # on Lizzie's computer
wmap.df <- fortify(wmap)

##
## maps
##

length(unique(d$datasetID))
length(unique(paste(d$datasetID, d$study)))

length(unique(paste(d$genus, d$species)))
sort(summary(factor(paste(d$genus, d$species))), decreasing = TRUE)

unique(d$respvar)
sort(summary(d$respvar))

# summary data by study, with n responses (from Dan F.)
nd <- data.frame(n=tapply(d$datasetID, d$datasetID, length))
nd$lat <- as.numeric(as.character(d[match(rownames(nd), d$datasetID), "provenance.lat"]))
nd$long <- as.numeric(as.character(d[match(rownames(nd), d$datasetID), "provenance.long"]))
nd$resp <- d[match(rownames(nd), d$datasetID), "respvar"]

sppsummmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      n=length(unique(latbi)),
      photo=mean(as.numeric(photoperiod_day)),
      year=mean(year))

theme.tanmap <- list(theme(panel.grid.minor = element_blank(),
                        # panel.grid.major = element_blank(),
                        panel.background = element_rect(fill = "white",colour = NA),
                        # plot.background = element_rect(fill=NA),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22),
                        legend.position = "left"))

pdf("limitingcues/figures/maps/map_photo_sppn.pdf", width=10, height=4)

ggplot() + 
    geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
    # coord_cartesian(ylim=c(5, 70), xlim=c(-120, 40)) +
    geom_point(data=sppsummmap, 
        aes(x=provenance.long, y=provenance.lat, size=sqrt(n), fill=photo), 
        colour="black", pch=21) +
        scale_size(name=expression(paste('species ', italic('n')))) +
            # range = c(1, 12), breaks=c(10,100,200,400
            scale_fill_gradient(name="mean photoperiod",
            low="thistle2", high="dodgerblue4", guide="colorbar", na.value="NA") +
  theme.tanmap
# low: thistle2 or pink or papayawhip khaki1?
# high: darkred
dev.off()

## map with study ID
pdf(paste("limitingcues/figures/maps/map_datasetID.pdf", sep=""), width=18, height=7)

ggplot() + 
    geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
    # coord_cartesian(ylim=c(5, 70), xlim=c(-120, 40)) +
    geom_point(data=sppsummmap, 
        aes(x=provenance.long, y=provenance.lat, size=n, shape=datasetID, fill=datasetID), # not sure why my shape command does not work!
        colour="black", pch=21) +
    scale_size(name=expression(paste('species ', italic('n'))), 
            range = c(1, 12), breaks=c(1, 5, 10, 50, 100)) +
  theme.tanmap
dev.off()


## Below is a sort of cheap way to not copy the map code
## You can pick one of three options THEN run the map code at the bottom

# OPTION photo 
name <- "fs.photo"
namesize <- "field sample n"
namecol <- "mean photoperiod"
summmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      size=length(unique(fieldsample.date)),
      fillcolor=mean(as.numeric(photoperiod_day)),
      year=mean(year))
#

# OPTION chilltemp
name <- "fs.chilltemp"
namesize <- "field sample n"
namecol <- "mean chilltemp"
summmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      size=length(unique(fieldsample.date)),
      fillcolor=mean(as.numeric(chilltemp)),
      year=mean(year))
#

# OPTION forcetemp
name <- "fs.forcetemp"
namesize <- "field sample n"
namecol <- "mean force"
summmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      size=length(unique(fieldsample.date)),
      fillcolor=mean(as.numeric(forcetemp)),
      year=mean(year))
#

## MAP code
theme.tanmap <- list(theme(panel.grid.minor = element_blank(),
                        # panel.grid.major = element_blank(),
                        panel.background = element_rect(fill = "grey90",colour = NA),
                        # plot.background = element_rect(fill=NA),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22),
                        legend.position = "left"))

pdf(paste("limitingcues/figures/maps/", name, ".pdf", sep=""), width=10, height=4)

ggplot() + 
    geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
    # coord_cartesian(ylim=c(5, 70), xlim=c(-120, 40)) +
    geom_point(data=summmap, 
        aes(x=provenance.long, y=provenance.lat, size=size, fill=fillcolor), 
        colour="black", pch=21) +
        scale_size(name=namesize) +
            # range = c(1, 12), breaks=c(10,100,200,400
            scale_fill_gradient(name=namecol,
            low="thistle2", high="dodgerblue4", guide="colorbar", na.value="NA") +
  theme.tanmap
dev.off()
##



if(FALSE){
# An aside to make map of BB data (June 2018)

setwd("bb_analysis")
source("source/bbstanleadin.R")

summmap.bb <- summmap[which(summmap$datasetID %in% unique(bb.stan$datasetID)),]
pdf("figures/map_bbdata.pdf", width=8, height=4)

ggplot() + 
    geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
    # coord_cartesian(ylim=c(5, 70), xlim=c(-120, 40)) +
    geom_point(data=summmap.bb, 
        aes(x=provenance.long, y=provenance.lat), 
        colour="black", pch=21) +
  theme.tanmap
dev.off()

}
