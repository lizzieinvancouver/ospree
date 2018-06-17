## Started 28 August 2017 ##
## By Lizzie ##

## Working on figures for OSPREE concept paper on interactive cues ##

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

## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
d <- read.csv("output/ospree_clean.csv")
studfile <- read.csv("output/studytype_table.csv", header=TRUE)

d <- d[d$woody=="yes",]
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
plot(sqrt(n)~year, data=sppsumyr, ylim=c(-1,40), type="n")
lines(sqrt(n)~year, data=sppsumyr, col=colz[1])
points(force~year, data=sppsumyr, col=colz[2])
points(photo~year, data=sppsumyr, col=colz[3])
points(chill~year, data=sppsumyr, col=colz[4])
dev.off()

studyr <- merge(stud, sppsumyr, by=c("datasetID", "study"))
dim(stud)
dim(studyr)
studyr <- subset(studyr, select=c("datasetID", "study", "genus.species", "woody",
    "samplingdates.count", "species.count", "photoperiods.count", "forcetemps.count",
     "expchill.count", "latitude.count", "longitude.count", "year"))

studyr$exp <- paste(studyr$datasetID, studyr$study)
studyr$yearint <- as.integer(studyr$year)

expperyr <- ddply(studyr, c("yearint"), summarise,
      ndat=length(unique(datasetID)),
      nexp=length(unique(exp)))

plot(ndat~yearint, data=expperyr, ylim=c(-5,30), type="l")
points(samplingdates.count~year, data=studyr, ylim=c(-5,30), col="green")


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

pdf("limitingcues/figures/maps/mappy.pdf", width=10, height=4)

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

##
name <- "fs.photo"
namesize <- "field sample n"
namecol <- "mean photoperiod"

summmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      size=length(unique(fieldsample.date)),
      fillcolor=mean(as.numeric(photoperiod_day)),
      year=mean(year))


name <- "fs.chilltemp"
namesize <- "field sample n"
namecol <- "mean chilltemp"

summmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      size=length(unique(fieldsample.date)),
      fillcolor=mean(as.numeric(chilltemp)),
      year=mean(year))



name <- "fs.chilltemp"
namesize <- "field sample n"
namecol <- "mean force"

summmap <-
      ddply(d, c("datasetID", "provenance.lat", "provenance.long"), summarise,
      size=length(unique(fieldsample.date)),
      fillcolor=mean(as.numeric(forcetemp)),
      year=mean(year))

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
