## Started 31 July 2018 - Cat
# Goal: make a figure that has year on the x axis and frequency on the y axis (like we have already 'pubyear.pdf') 
# and then color code the bars by number of cues the studies manipulated

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

## a bunch of this code is taken from cleaning/cleanup_checksmaps.R
# Get packages
#d <- read.csv("output/ospree_clean.csv")
d2<-read.csv("output/ospree_clean_withchill_BB.csv")
#studfile <- read.csv("output/studytype_table.csv", header=TRUE)
studfile2 <- read.csv("output/studytype_withBB.csv", header=TRUE)

#d <- d[d$woody=="yes",]
d2 <- d2[d2$woody=="yes",]
#d$fieldsample.date <- as.Date(d$fieldsample.date, format="%d-%b-%Y")
d2$fieldsample.date <- as.Date(d2$fieldsample.date, format="%d-%b-%Y")
#head(d)

###
###
#lookupstudyyr <- aggregate(d[c("year")], d[c("datasetID", "study")], FUN=mean)
lookupstudyyr2 <- aggregate(d2[c("year")], d2[c("datasetID", "study")], FUN=mean)
#stud <- merge(studfile, lookupstudyyr, by=c("datasetID", "study"), all.x=TRUE, all.y=TRUE)
stud2 <- merge(studfile2, lookupstudyyr2, by=c("datasetID", "study"), all.x=TRUE, all.y=TRUE)

cues<-dplyr::select(stud2, study, datasetID, force, photo, chill, chilltime, year, prov.lat, spp, field.sample)
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

cues$yr<-round(cues$year, digits=0)
studies<-as.data.frame(table(cues$yr))
studies<-studies%>%rename(yr=Var1)%>%rename(studies=Freq)
studies$yr<-as.numeric(as.character(studies$yr))
cues<-cues[!duplicated(cues),]

cues<-inner_join(cues, studies)
cues$cols<-NA
cues$cols<-ifelse(cues$numcues==1, "blue", cues$cols)
cues$cols<-ifelse(cues$numcues==2, "green", cues$cols)
cues$cols<-ifelse(cues$numcues==3, "red", cues$cols)
cues$cols<-ifelse(cues$numcues==0 & cues$field.sample>1, "violet", cues$cols)
cues$cols<-ifelse(cues$numcues==0 & cues$field.sample<=1, "white", cues$cols)

#ggplot(cues, aes(x=yr, fill=cols)) + geom_histogram()
mecolors<-colorRampPalette(brewer.pal(11,"Spectral"))(5)
hist<-ggplot(cues, aes(x=yr)) + geom_histogram(aes(fill=cols), size=0.3) +
  xlab("Year") + ylab("Number of Studies") + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=mecolors, name="Number of Cues",
                    labels=c("blue"="1", "green"="2", "red"="3", "violet"="Multiple Field Sample Dates", "white"="Multiple Provenance Latitudes \nand/or Species")) + scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10), legend.text.align = 0)

quartz()
hist
