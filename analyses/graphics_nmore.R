### Started 20 September 2016 ###
### By Lizzie ###

## Some quick code to look at data ##
## And some model stuff ##

## Updated on 3 February 2017 ##

## To do! ##
# So much ...

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(ggplot2)
library(dplyr)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
  } else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
setwd("~/Documents/git/ospree/analyses")
# get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
labgroups <- read.csv("output/labgroups.csv", header=TRUE)
studytype <- read.csv("output/studytype_withBB.csv", header=TRUE)

# merge in labgroup (we could do this elsewhere someday) and first adjust a couple datasetids so they match
bb$datasetID[bb$datasetID=="Sanz-Perez09"] <- "sanzperez10"
bb$datasetID[bb$datasetID=="Heide03"] <- "heide03"
# note that lamb37, nienstaedt66, webb78 will not match (see authors.R for more)
bb.wlab <- merge(bb, labgroups, by="datasetID", all.x=TRUE, all.y=FALSE)

# how much data?
dim(bb.wlab)

columnstokeep <- c("datasetID", "study", "genus", "species", "varetc", "woody", "forcetemp",
    "photoperiod_day", "respvar", "respvar.simple", "response", "response.time", 
    "Total_Chilling_Hours","Total_Utah_Model", "Total_Chill_portions",
    "Exp_Chilling_Hours",  "Exp_Utah_Model","Exp_Chill_portions", "cat")
    
bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)

# 93 total studies
unique(paste(bb.wlab.sm$datasetID, bb.wlab.sm$study))

# get studies with more than one photoperiod
photo2 <- subset(studytype, photo>1) # 36 studies
force2 <- subset(studytype, force>1) # 39 studies
dates2 <- subset(studytype, field.sample>1) # 39 studies

# make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)
bb.wlab.sm$chillhrs <- as.numeric(bb.wlab.sm$Total_Chilling_Hours)
bb.wlab.sm$chillpor <- as.numeric(bb.wlab.sm$Total_Chill_portions)
bb.wlab.sm$utah <- as.numeric(bb.wlab.sm$Total_Utah_Model)
bb.wlab.sm$expchillhrs <- as.numeric(bb.wlab.sm$Exp_Chilling_Hours)
bb.wlab.sm$expchillpor <- as.numeric(bb.wlab.sm$Exp_Chill_portions)
bb.wlab.sm$exputah <- as.numeric(bb.wlab.sm$Exp_Utah_Model)

# where do we lose data
dim(subset(bb.wlab.sm, is.na(force)==FALSE))
dim(subset(bb.wlab.sm, is.na(photo)==FALSE))
dim(subset(bb.wlab.sm, is.na(chillhrs)==FALSE))
dim(subset(bb.wlab.sm, is.na(chillhrs)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE))

bb.wlab.sm$latbi <- paste(bb.wlab.sm$genus, bb.wlab.sm$species)
sort(unique(bb.wlab.sm$latbi))

##
## plotting
ospr.plot <- bb.wlab.sm

# not working ...
pdf(file="figures/goobergoo.pdf")
for (i in c(1:length(unique(ospr.plot$latbi)))){
    subby <- subset(ospr.plot, latbi==unique(ospr.plot$latbi)[i])
    ggplot(subby,
        aes(x=force, y=response.time, color=photo)) +
        scale_x_discrete(name="Temperature") + scale_y_continuous(name="Days to BB") +
        facet_wrap(~latbi, nrow=6) + 
        geom_point()
}
dev.off()

ggplot(ospr.plot,
    aes(x=force, y=response.time, color=photo)) +
    scale_x_discrete(name="Temperature") + scale_y_continuous(name="Days to BB") +
    facet_wrap(~genus, nrow=6) + 
    geom_point()


ggplot(ospr.plot,
    aes(x=chillhrs, y=response.time, color=force)) + 
    scale_x_discrete(name="Chilling hours") + scale_y_continuous(name="Days to BB") +
    facet_wrap(~genus, nrow=6) + 
    geom_point()

ggplot(ospr.plot,
     aes(x=force, y=response.time, color=genus)) +
     scale_x_discrete(name="Temperature") + scale_y_continuous(name="Days to BB") +
     facet_wrap(~cat, nrow=6) + 
     geom_point()



## Look at photo in studies that manipulated it
ospr.ph <- ospr.plot[which(ospr.plot$datasetID %in% photo2$datasetID),]

ggplot(ospr.ph,
    aes(x=photo, y=response.time, color=force)) +
    facet_wrap(~genus, nrow=6) + 
    geom_point()

summary(lm(response.time~photo*force*genus, data=ospr.ph))

## Special Fagus analysis
fagsyl <- subset(ospr.plot, latbi=="Fagus sylvatica")
dim(fagsyl)
table(fagsyl$datasetID)

unique(fagsyl$photo)
table(fagsyl$photo)

fagsyl$photocat <- NA
fagsyl$photocat[fagsyl$photo>12] <- "morethan12"
fagsyl$photocat[fagsyl$photo<=12] <- "lessthaneq12"
fagsyl<-fagsyl%>%filter(datasetID !="falusi96") # does not manipulate anything?..
fagsyl<-fagsyl%>%filter(datasetID !="falusi90") # doesn't manipulate chilling

ggplot(fagsyl,
       aes(x=chillhrs, y=response.time, color=force)) +
  facet_wrap(~photocat, nrow=4) + 
  geom_point()

ggplot(fagsyl,
    aes(x=chillhrs, y=response.time, color=datasetID)) +
    facet_wrap(~force, nrow=4) + 
    geom_point(aes(shape=photocat))

ggplot(fagsyl,
       aes(x=chillhrs, y=response.time, color=force)) +
  facet_wrap(~datasetID, nrow=4) + 
  geom_point(aes(shape=photocat))

resps<-c("daystobudburst", "percentbudburst")
fagus.sm<-filter(fagsyl, respvar.simple %in% resps)
ggplot(fagus.sm,
       aes(x=chillhrs, y=response.time, color=force)) +
  facet_wrap(~datasetID, nrow=4) + 
  geom_point(aes(shape=photocat))


#### Another way to look at the data - Cat 
# 24 July 2017
ggplot((ospr.plot), aes(x=chillpor, y=response.time)) + xlab("Chilling") + ylab("Days to Budburst") +
  geom_point(aes(col=as.factor(latbi))) + 
  #geom_smooth(aes(col=as.factor(latbi)),method="lm", se=FALSE) + 
  theme(legend.position="none")

View(table(bb$genus))
View(filter(bb,genus=="Betula "))
