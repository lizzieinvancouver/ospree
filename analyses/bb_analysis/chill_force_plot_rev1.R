#Plot the days over which chilling and forcing occur in our data
#Goal is to address Rev 1 comments
#STarted July 2020 by Ailene Ettinger
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load libraries
library(scales)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

## set up the flags
use.chillports = FALSE
use.zscore =FALSE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

##
source("source/bbstanleadin.R")
##Get chilling start day and end day (referenced to start of forcing)
bb.stan$chillstart<-0
bb.stan$chillend<-bb.stan$chillstart + as.numeric(bb.stan$chilldays)  

#Now, for field chilling studies: the end chill date is the field sample date.
#To calculate how many days this is, calculate from sept 1 to the field sample date
bb.stan$fieldsample.date2<-strptime(strptime(bb.stan$fieldsample.date, format = "%d-%b-%Y"),format = "%Y-%m-%d")
bb.stan$fieldsample.date2<-as.character(as.Date(bb.stan$fieldsample.date2,"%m/%d/%y")) #needed for new version
yr<-as.numeric(substr(bb.stan$fieldsample.date2,1,4))
bb.stan$chillstday[bb.stan$chill_type=="fldest" & as.numeric(substr(bb.stan$fieldsample.date2,6,7))>=9] <- as.character(as.Date(strptime(paste(yr, "09-01", sep="-"),"%Y-%m-%d", tz="GMT"))[bb.stan$chill_type=="fldest" & as.numeric(substr(bb.stan$fieldsample.date2,6,7))>=9])#start day for chilling is september 1
bb.stan$chillstday[bb.stan$chill_type=="fldest" & as.numeric(substr(bb.stan$fieldsample.date2,6,7))<9] <- as.character(as.Date(strptime(paste(yr-1, "09-01", sep="-"),"%Y-%m-%d", tz="GMT"))[bb.stan$chill_type=="fldest" & as.numeric(substr(bb.stan$fieldsample.date2,6,7))<9])#start day for chilling is september 1

#Check that all sites are either exp chilling or have a field sample date:
#bb.stan[which(is.na(bb.stan$fieldsample.date2) & bb.stan$chill_type=="fldest"),]
#they do! 
#unique(bb.stan$fieldsample.date2)
# and all field sample dates are earlier than april 1
#get days of chilling
bb.stan$chillend[which(is.na(bb.stan$chillend) & bb.stan$chill_type=="fldest")]<-
  as.Date(bb.stan$fieldsample.date2[which(is.na(bb.stan$chillend) & bb.stan$chill_type=="fldest")])-as.Date(bb.stan$chillstday[which(is.na(bb.stan$chillend) & bb.stan$chill_type=="fldest")])

bb.stan$forcestart<-bb.stan$chillend
bb.stan$bbday<-bb.stan$forcestart+bb.stan$response.time
bb.stan<-bb.stan[order(bb.stan$datasetID,bb.stan$chillend),]
pdf("figures/chilldaysforcedays2.pdf", width=8,height=25)


  #quartz(height=12,width=8)#for pc you replace "quartz" with X11
  par(mai=c(1,1.5,.1,.1), omi=c(.5,.1,.1,.2))
  plot(10,15, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Number of Days",ylab=" ", ylim=c(1,dim(bb.stan)[1]), yaxt='n',xlim=c(1,200),las=1)
 
   y<-rev(seq(from =1, to = dim(bb.stan)[1], by = 1))
   bb.stan<-bb.stan[-which(is.na(bb.stan$forcestart)),]
   for(i in 1:dim(bb.stan)[1]){
     lines(c(bb.stan$chillstart[i],bb.stan$chillend[i]),c(y[i],y[i]), col="lightblue",lwd=2)
     lines(c(bb.stan$forcestart[i],bb.stan$bbday[i]),c(y[i],y[i]), col="darkred",lwd=2)
     
        }
   #Make new label for y-axis that is has species name only once
   namelocs<-table(bb.stan$datasetID)
   namelocs<-cumsum(namelocs[order(names(namelocs),decreasing = TRUE)])
   n=length(names(namelocs))
   axis(side=2,at=namelocs[seq(1,n,2)],labels=names(namelocs)[seq(1,n,2)],las=1, tick=FALSE, cex.axis=1)
   axis(side=2,at=namelocs[seq(2,n,2)],labels=names(namelocs)[seq(2,n,2)],las=1, tick=FALSE, cex.axis=1)
dev.off()
       
#Other reviewer issue: find % difference in estimates from utah and chill model
#Forcing estimate- change betwen utah and chillportions mean posteriorestimates
(-4.35- -4.81)/-4.35#-0.11
#chilling estimate- change betwen utah and chillportions estimates
(-8.35- -7.44)/-8.35#-0.11
#photoperiod estimate- change betwen utah and chillportions estimates
(-2.95- -3.07)/-2.95#-0.04

 