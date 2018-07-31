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

cues<-dplyr::select(stud, datasetID, force, photo, chill, year)
cues<-cues[!duplicated(cues),]

cues$force<-ifelse(cues$force==1, 0, 1)
cues$photo<-ifelse(cues$photo==1, 0, 1)
cues$chill<-ifelse(cues$chill==1, 0, 1)

cues$numcues<-cues$force + cues$photo + cues$chill

cues<-na.omit(cues)

cues$yr<-round(cues$year, digits=0)
studies<-as.data.frame(table(cues$yr))
studies<-studies%>%rename(yr=Var1)%>%rename(studies=Freq)
studies$yr<-as.numeric(as.character(studies$yr))

cues<-inner_join(cues, studies)
cues$cols<-NA
cues$cols<-ifelse(cues$numcues==1, "blue", cues$cols)
cues$cols<-ifelse(cues$numcues==2, "red", cues$cols)
cues$cols<-ifelse(cues$numcues==3, "green", cues$cols)

#ggplot(cues, aes(x=yr, fill=cols)) + geom_histogram()

hist<-ggplot(cues, aes(x=yr)) + geom_histogram(aes(fill=cols), color="gray30", size=0.3) +
  xlab("Year") + ylab("Number of Studies") + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"), name="Number of Cues",
                    labels=c("1","2", "3")) + scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10))

quartz()
hist
