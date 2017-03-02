## Started 21 Feb 2017 ##
## By Nacho, Lizzie, Dan and others ##

## Plotting results from Stan models ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

source('stan/savestan.R')

# Load fitted stan model
m1.bb<-readRDS("stan/M1_daysBB_2level.stan.rds")

# explore results in shinystan
#launch_shinystan(m1.bb)

# Data to plot from model
str(m1.bb@sim$samples[[1]])

hist(m1.bb@sim$samples[[1]]$a_0,30)
hist(m1.bb@sim$samples[[1]]$b_force_0,30)
abline(v=median(m1.bb@sim$samples[[1]]$b_force_0),col='red')

hist(m1.bb@sim$samples[[1]]$b_chill_0,30)
abline(v=median(m1.bb@sim$samples[[1]]$b_chill_0),col='red')
names(m1.bb@sim$samples[[1]])

hist(y,30)
max(y)
cols <- adjustcolor("indianred3", alpha.f = 0.3) 

par(mar=c(5,7,3,2))
plot(x=NULL,y=NULL,xlim=c(-150,150),yaxt='n',ylim=c(0,8),
     xlab="MOdel estimate change in days to BB",ylab="",main="M1_daysBB_2level.stan")
axis(2, at=1:7, labels=rownames(summary(m1.bb)$summary)[7:1],las=1)
abline(v=0,lty=2,col="darkgrey")
for(i in 1:7){
  pos.y<-(7:1)[i]
  pos.x<-summary(m1.bb)$summary[i,"mean"]
  lines(summary(m1.bb)$summary[i,c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:22){
  pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(m1.bb)$summary),fixed=T))[1:7]
  jitt<-runif(1,0.05,0.4)
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(m1.bb)$summary[pos.sps.i[i],"mean"]
  lines(summary(m1.bb)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),col=cols)
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8,pch=19,col=cols)
  
}
}


## reading and plotting models from 
load(file="output/m1.nolabgroup.Rdata")
summary(m1.nolabgroup)

## loading data.frame
bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
labgroups <- read.csv("output/labgroups.csv", header=TRUE)

# merge in labgroup (we could do this elsewhere someday
bb.wlab <- merge(bb, labgroups, by="datasetID", all.x=TRUE)

columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", "cat")

bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)
genera<-unique(bb.wlab.sm$genus)
which(grep(genera[12],rownames(summary(m1.nolabgroup))))
start.pos<-seq(8,161,7)

# Data to plot from model

cols <- adjustcolor("indianred3", alpha.f = 0.3) 

par(mar=c(5,9,3,2))
plot(x=NULL,y=NULL,xlim=c(-150,150),yaxt='n',ylim=c(0,8),
     xlab="MOdel estimate change in days to BB",ylab="",main="m1.nolabgroup.rstanarm")
axis(2, at=1:7, labels=rownames(summary(m1.nolabgroup))[7:1],las=1)
abline(v=0,lty=2,col="darkgrey")
for(i in 1:7){
  pos.y<-(7:1)[i]
  pos.x<-summary(m1.nolabgroup)[i,"mean"]
  lines(summary(m1.nolabgroup)[i,c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  for(spsi in 1:22){
    
    pos.sps.i<-start.pos[spsi]+(i-1)
    jitt<-runif(1,0.05,0.4)
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-summary(m1.nolabgroup)[pos.sps.i,"mean"]
    lines(summary(m1.nolabgroup)[pos.sps.i,c("25%","75%")],rep(pos.y.sps.i,2),col=cols)
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8,pch=19,col=cols)
    
  }
}




