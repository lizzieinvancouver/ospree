## Started 7 July 2016 ##
## By Ailene, after modifying Dan and Lizzie's code ##
##Try fitting the model for percbb in lme4 since Stan is not working great for me right now
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(lme4)
library(car)
setwd("~/GitHub/ospree")


ospree <- read.csv("input/ospree_clean1_withchill.csv", header=TRUE)

percbbvars <- c("percentbudburst", "percentstage01", "percentstage02","percentstage03","percentstage04")

ospree.percbb <- ospree[which(ospree$respvar %in% percbbvars),]

dim(ospree)
dim(ospree.percbb) # 4003 rows

ospree.percbb$spp <- paste(ospree.percbb$genus, ospree.percbb$species, sep=".")

ospree.percbb <- subset(ospree.percbb, is.na(spp)==FALSE)
ospree.percbb<-subset(ospree.percbb,as.numeric(ospree.percbb$response)<800)#remove the weird value for perc budburst

# deal with response vs. responsetime (quick fix for now)
resp1 <- subset(ospree.percbb, response==1) # most of are data is like this
resp1.timeNA <- subset(resp1, is.na(response.time)==TRUE) # about 20 rows have this

ospree.percbb$responsedays <- ospree.percbb$response.time
ospree.percbb$responsedays[which(ospree.percbb$response>1 & ospree.percbb$response.time=="")] <- ospree.percbb$response

ospree.percbb$totalchill <- as.numeric(ospree.percbb$Total_Chilling_Hours)
ospree.percbb$forcetemp <- as.numeric(ospree.percbb$forcetemp)
ospree.percbb$photoperiod_day <- as.numeric(ospree.percbb$photoperiod_day)
ospree.percbb$spp <- as.factor(ospree.percbb$spp)
ospree.percbb$response <- as.numeric(ospree.percbb$response)#this is percbudburst
ospree.percbb$datasetID <- as.factor(ospree.percbb$datasetID)
ospree.percbb$responsedays <- as.numeric(ospree.percbb$responsedays)#this is percbudburst

###Try fitting model with lmer:
##First a simple model:
mod<-lmer(response~responsedays+(responsedays|datasetID/spp),data=ospree.percbb)
#this model does not converge...get a warning message
#difficult to put any random slopes
#try with just species as random effect and crossed random effect of dataset ID
mod2<-lmer(response~responsedays+(responsedays|spp)+(1|datasetID),data=ospree.percbb)
summary(mod2)#as days to budburst increases, perc budburst increases! but 
ranef(mod2)
##Now add chilling, forcing, and photoperiod interactions with days: can't do random slopes- doesn't converge
mod.cfp<-lmer(response~responsedays+responsedays:totalchill+responsedays:forcetemp+responsedays:photoperiod_day+(1|spp)+(1|datasetID),data=ospree.percbb)
summary(mod.cfp)#as days to budburst increases, perc budburst increases
ospree.percbb$chill.cat<-NA
ospree.percbb[which(ospree.percbb$totalchill<=1020),]$chill.cat<-"1low"
ospree.percbb[which(ospree.percbb$totalchill>1020),]$chill.cat<-"2high"
Anova(mod.cfp)
ranef(mod.cfp)
quartz()
plot.ospree<-subset(ospree.percbb, select=c("response","responsedays", "chill.cat"))
plot.ospree <- plot.ospree[complete.cases(plot.ospree),]
colors<-c("red","blue")[factor(plot.ospree$chill.cat)]
plot(as.numeric(plot.ospree$responsedays),as.numeric(plot.ospree$response),pch=16, col=colors)
