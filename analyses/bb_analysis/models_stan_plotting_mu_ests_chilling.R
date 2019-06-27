## Started 27 Feb 2019 ##
## By Ailene  ##

## Marginal effects from Stan models ##
############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#libraries
library(RColorBrewer)
library(geosphere)
library(rstan)
library(chillR)
library(rgl)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis") 
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


figpath <- "figures"

## set up the flags
use.chillports = FALSE
use.zscore = FALSE
use.allspp =TRUE # for the main model this is false
use.multcuespp = FALSE
use.cropspp = TRUE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE


##
source("source/bbstanleadin.R")
##

# Set up colors (more than used currently ...

cols <- adjustcolor(c("maroon4", "lightskyblue","purple4"), alpha.f = 0.8) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

# non-z-scored models
if(use.zscore==FALSE & use.chillports == TRUE){
  load("stan/output/m2lni_spcompexprampfpcp_nonz.Rda") # m2l.ni
  #load("stan/output/m2lnib_spcompexprampfp_nonz.Rda") # m2l.nib
  modelhere <- m2l.ni
  fit <- m2l.ni
}
if(use.zscore==FALSE & use.chillports == FALSE){
  load("stan/output/m2lni_spcompexprampfputah_nonz.Rda") # m2l.ni
  modelhere <- m2l.ni
  fit <- m2l.ni
}

fit.sumz <- summary(fit)$summary

rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")
#For main effects of model:
#Make a figure showing effects of forcing and chilling on budburst
#at different photoperiods



## Plotting
# First, we estimate the posteriors for each thing we want to plot...

list_of_draws <- extract(fit)
#print(names(list_of_draws))
#str(list_of_draws$mu_a_sp)


getest.bb <- function(fit, forcetemp, chill, daylength1,daylength2){
  listofdraws <- extract(fit)
  avgbbdl1 <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*forcetemp + 
    listofdraws$mu_b_photo_sp*daylength1 + listofdraws$mu_b_chill_sp*chill
  avgbbdl2 <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*forcetemp + 
    listofdraws$mu_b_photo_sp*daylength2 + listofdraws$mu_b_chill_sp*chill
  
   yebbest <- list(avgbbdl1, avgbbdl2)
  return(yebbest)
}

forcetemps<-seq(round(min(bb.stan$force), digits=0), round(max(bb.stan$force), digits=0), by=1)
chilltemps<-seq(min(as.numeric(bb.stan$chilltemp), na.rm=TRUE),max(as.numeric(bb.stan$chilltemp), na.rm=TRUE), by=1)
#chilldays<-as.integer(mean(as.numeric(bb.stan$chilldays), na.rm=TRUE))
#instead of using mean, we will vary this
#in addition to chilltemp, vary the duration of chilling
#range(round(as.numeric(bb.stan.expramptypes$chilldays), digits=0), na.rm=TRUE)#ranges from 0-235 days
chilldays<-seq(from=0, to=240,by=20)

temps<-seq(min(c(chilltemps,forcetemps)),max(c(chilltemps,forcetemps)), by=1)
dl1<-8
dl2<-16
chillport<-mean(bb.stan$chill.ports)
chillutah<-mean(bb.stan$chill)
if(use.chillports==TRUE){chill=chillport}
if(use.chillports==FALSE){chill=chillutah}

#make blank dataframe to fill with estimates
predicts <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
predicts.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
predicts.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))

colnames(predicts)<-colnames(predicts.25per) <-colnames(predicts.75per) <-
  c("forcetemp","dl1","dl2")

for (i in 1:length(temps)){
  bbposteriors <- getest.bb(fit,temps[i], chill, dl1,dl2)
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))

  predicts[i,]<-c(temps[i],meanz)
  predicts.25per[i,]<-c(temps[i],quant25per)
  predicts.75per[i,]<-c(temps[i],quant75per)
}

#now calculate with chilling with different experimental chilling treatments
#create a dataframe of chilling, using different temperatures and durations
chillests<-as.data.frame(matrix(NA,ncol=10,nrow=length(temps)*length(chilldays)))

Year<-2014
for(d in 1:length(chilldays)){
JDay<-seq(1:chilldays[d])
  for(i in 1:length(temps)){
    Tmean<- temps[i]
    meandaily<-data.frame(JDay,Year,Tmean)
    #convert mean daily temperature data to hourly data
    hrly.temp =
      data.frame(
        Temp = c(rep(meandaily$Tmean, times = 24)),
        Year = c(rep(meandaily$Year, times = 24)),
        JDay = sort(c(rep(seq(1:length(JDay)), times = 24)))
        )
    chillests[i+((d-1)*length(temps)),]<-c(temps[i],chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp)]))
  }
}
colnames(chillests)<- c("temp","Season","End_year","Season_days","Data_days","Perc_complete","Chilling_Hours","Utah_Model","Chill_portions","GDH")
chillests$Utah_Model240<-chillests$Utah_Model/240
meanchillests<-aggregate(chillests$Utah_Model240,by=list(chillests$temp), mean)
meanchillests2<-aggregate(chillests$Chill_portions,by=list(chillests$temp), mean)
meanchillests<-cbind(meanchillests,meanchillests2$x)
colnames(meanchillests)<-c("temp","Utah_Model240","Chill-portions")
#make blank dataframe to fill with estimates
chillpredicts <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
chillpredicts.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
chillpredicts.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))

colnames(chillpredicts)<-colnames(chillpredicts.25per) <-colnames(chillpredicts.75per) <-
  c("chilltemp","dl1","dl2")
mnforce<-mean(as.numeric(bb.stan$forcetemp), na.rm=TRUE)
for (i in 1:length(temps)){
  if(use.chillports==TRUE){bbposteriors <- getest.bb(fit,mnforce, meanchillests$Chill_portions[i], dl1,dl2)}
  if(use.chillports==FALSE){bbposteriors <- getest.bb(fit,mnforce, meanchillests$Utah_Model240[i], dl1,dl2)}
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))

  chillpredicts[i,]<-c(temps[i],meanz)
  chillpredicts.25per[i,]<-c(temps[i],quant25per)
  chillpredicts.75per[i,]<-c(temps[i],quant75per)
}

#chilling and forcing simultanesouly altered
bothpredicts <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
bothpredicts.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
bothpredicts.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))

colnames(bothpredicts)<-colnames(bothpredicts.25per) <-colnames(bothpredicts.75per) <-
  c("temp","dl1","dl2")
for (i in 1:length(temps)){
  if(use.chillports==TRUE){bbposteriors <- getest.bb(fit,temps[i], meanchillests$Chill_portions[i], dl1,dl2)}
  if(use.chillports==FALSE){bbposteriors <- getest.bb(fit,temps[i], meanchillests$Utah_Model240[i], dl1,dl2)}

  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))

  bothpredicts[i,]<-c(temps[i],meanz)
  bothpredicts.25per[i,]<-c(temps[i],quant25per)
 bothpredicts.75per[i,]<-c(temps[i],quant75per)
}


xlim = c(range(temps))
ylim = c(range(c(predicts[,2:3],chillpredicts[,2:3],bothpredicts[,2:3])))
if(use.chillports==TRUE){figname<-paste("mupredictschill_chillport.pdf", sep="_")}
if(use.chillports==FALSE){figname<-paste("mupredictschill_utah.pdf", sep="_")}

pdf(file.path(figpath,figname), width = 9, height = 6)

#quartz()
par(mar=c(8,7,3,5))
plot(predicts$forcetemp,predicts$dl1, xlim=xlim, xlab="Temperature (°C)", ylim=ylim,
     ylab="Days to budburst", type="l",bty="l", lty=1, lwd=2, col="darkred")
#Add shading around line for credible intervals
polygon(c(rev(predicts$forcetemp),predicts$forcetemp), c(rev(predicts.75per$dl1), predicts.25per$dl1), col = alpha("darkred", 0.2), border = NA)
polygon(c(rev(chillpredicts$chilltemp),chillpredicts$chilltemp), c(rev(chillpredicts.75per$dl1), chillpredicts.25per$dl1), col = alpha("blue", 0.2), border = NA)

lines(chillpredicts$chilltemp,chillpredicts$dl1,lty=1, lwd=2, col="blue")
polygon(c(rev(bothpredicts$temp),bothpredicts$temp), c(rev(bothpredicts.75per$dl1), bothpredicts.25per$dl1), col = alpha("purple", 0.2), border = NA)
lines(bothpredicts$temp,bothpredicts$dl1,lty=1, lwd=2, col="purple")

if(use.chillports==FALSE){legend("topleft",legend=c("forcing","chilling","both"), lty=1, bty="n",col=c("darkred","blue","purple"),lwd=2)}

dev.off()

#Make a 2-d plot using PEP data to estimate chilling instead of above
#Now make the same figure but using PEP field data to get chilling estimates for particular temperatures
#rather than assuming constant durations

pepests<-read.csv("..//output/betpen_for3dplot/betpen.forecast.forheatmap.csv", header=TRUE)
#hist(pepests$winT.forecast[pepests$warming_C==0])#will need to use forecasting data to encompass the range
#add a column with winter tepmerature rounded to the nearest whol number
pepests$chilltemp.int<-round(pepests$winT.forecast, digits=0)
#get utah chilling instead of utha/240
#pepests$chill.utah<-pepests$chill.forecast*240
meanpepchillests<-aggregate(pepests$chill.forecast,by=list(pepests$chilltemp.int), mean)
colnames(meanpepchillests)<-c("chilltemp","chill.utah")
pepchilltemps<-meanpepchillests$chilltemp
#limit experimental chilling and forcing to same range as pep

minpep<-which(predicts$forcetem==min(pepchilltemps))
maxpep<-which(predicts$forcetem==max(pepchilltemps))
predicts.pep<-predicts[minpep:maxpep,]
predicts.pep.75per<-predicts.75per[minpep:maxpep,]
predicts.pep.25per<-predicts.25per[minpep:maxpep,]

chillpredicts.pep <- as.data.frame(matrix(NA,ncol=3,nrow=length(pepchilltemps)))
chillpredicts.pep.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(pepchilltemps)))
chillpredicts.pep.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(pepchilltemps)))

colnames(chillpredicts.pep)<-colnames(chillpredicts.pep.25per) <-colnames(chillpredicts.pep.75per) <-
  c("chilltemp","dl1","dl2")

for (i in 1:length(pepchilltemps)){
  if(use.chillports==FALSE){bbposteriors <- getest.bb(fit,mnforce, meanpepchillests$chill.utah[i], dl1,dl2)}
  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
  
  chillpredicts.pep[i,]<-c(pepchilltemps[i],meanz)
  chillpredicts.pep.25per[i,]<-c(pepchilltemps[i],quant25per)
  chillpredicts.pep.75per[i,]<-c(pepchilltemps[i],quant75per)
}

#chilling and forcing simultanesouly altered
bothpredicts.pep <- as.data.frame(matrix(NA,ncol=3,nrow=length(pepchilltemps)))
bothpredicts.pep.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(pepchilltemps)))
bothpredicts.pep.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(pepchilltemps)))

colnames(bothpredicts.pep)<-colnames(bothpredicts.pep.25per) <-colnames(bothpredicts.pep.75per) <-
  c("temp","dl1","dl2")
for (i in 1:length(pepchilltemps)){
  if(use.chillports==FALSE){bbposteriors <- getest.bb(fit,pepchilltemps[i], meanpepchillests$chill.utah[i], dl1,dl2)}

  meanz <- unlist(lapply(bbposteriors, mean))
  quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
  quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
  quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))
  
  bothpredicts.pep[i,]<-c(pepchilltemps[i],meanz)
  bothpredicts.pep.25per[i,]<-c(pepchilltemps[i],quant25per)
  bothpredicts.pep.75per[i,]<-c(pepchilltemps[i],quant75per)
}



if(use.chillports==FALSE){pepfigname<-"mupredictschill_utah_pep.pdf"}

pdf(file.path(figpath,pepfigname), width = 9, height = 6)

#quartz()
par(mar=c(8,7,3,5))
plot(predicts.pep$forcetemp,predicts.pep$dl1, xlim=c(min(pepchilltemps),max(pepchilltemps)), xlab="Temperature (°C)", ylim=ylim,
     ylab="Days to budburst", type="l",bty="l", lty=1, lwd=2, col="darkred")
#Add shading around line for credible intervals
polygon(c(rev(predicts.pep$forcetemp),predicts.pep$forcetemp), c(rev(predicts.pep.75per$dl1), predicts.pep.25per$dl1), col = alpha("darkred", 0.2), border = NA)
polygon(c(rev(chillpredicts.pep$chilltemp),chillpredicts.pep$chilltemp), c(rev(chillpredicts.pep.75per$dl1), chillpredicts.pep.25per$dl1), col = alpha("blue", 0.2), border = NA)

lines(chillpredicts.pep$chilltemp,chillpredicts.pep$dl1,lty=1, lwd=2, col="blue")
polygon(c(rev(bothpredicts.pep$temp),bothpredicts.pep$temp), c(rev(bothpredicts.pep.75per$dl1), bothpredicts.pep.25per$dl1), col = alpha("purple", 0.2), border = NA)
lines(bothpredicts.pep$temp,bothpredicts.pep$dl1,lty=1, lwd=2, col="purple")

if(use.chillports==FALSE){legend("topleft",legend=c("forcing","chilling","both"), lty=1, bty="n",col=c("darkred","blue","purple"),lwd=2)}

dev.off()

#Make the above as a 3D plot

#new function (for just one daylength)
getest.bb2 <- function(fit, forcetemp, chill, daylength){
  listofdraws <- extract(fit)
  avgbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*forcetemp + 
    listofdraws$mu_b_photo_sp*daylength + listofdraws$mu_b_chill_sp*chill
  yebbest <- list(avgbb)
  return(yebbest)
}

z.matrix.dl1 <- matrix(NA,ncol=length(temps),nrow=length(temps))
z.matrix.dl2 <- matrix(NA,ncol=length(temps),nrow=length(temps))
#Fill matrix row by row
dl1<-8
dl2<-16
#the below takes a while to run.if you want to avoid running the loop
z.matrix.dl1<-read.csv("../output/bbmodests_for3dplot_8hr_utah.csv")
# for (i in 1:length(temps)){#i=chilling
#  print(temps[i]);
#   
#   for(j in 1:length(temps)){
#     if(use.chillports==TRUE){
#     bbposteriors <- getest.bb(fit,temps[j], meanchillests$Chill_portions[i], dl1,dl2)}
#   if(use.chillports==FALSE){
#     bbposteriors <- getest.bb(fit,temps[j], meanchillests$Utah_Model240[i], dl1,dl2)}
# 
#     meanz <- unlist(lapply(bbposteriors, mean))#returns  avgbb, warmsprbb, warmwinbb, warmsprwinbb)
#     z.matrix.dl1[i,j]<-meanz[1]#8 hour daylength only for now
#     z.matrix.dl2[i,j]<-meanz[2]#16 hour daylength only for now
#     
#   }
# }
# 
# colnames(z.matrix.dl1)<-colnames(z.matrix.dl2)<-paste("sprtemp",temps, sep=".")
# rownames(z.matrix.dl1)<-colnames(z.matrix.dl2)<-paste("wintemp",temps, sep=".")
# if(use.chillports==TRUE){
#   write.csv(z.matrix.dl1,"..//output/bbmodests_for3dplot_8hr_cp.csv")
#   write.csv(z.matrix.dl2,"..//output/bbmodests_for3dplot_16hr_cp.csv")}
# if(use.chillports==FALSE){
#   write.csv(z.matrix.dl1,"..//output/bbmodests_for3dplot_8hr_utah.csv")
#   write.csv(z.matrix.dl2,"..//output/bbmodests_for3dplot_16hr_utah.csv")} 
rownames(z.matrix.dl1)<-z.matrix.dl1$X
z.matrix.dl1<-z.matrix.dl1[,-1]
colnames(z.matrix.dl1)[1:10]<-paste("sprtemp.",seq(-10,-1, by=1), sep="")
z.matrix.dl1<-as.matrix(z.matrix.dl1)
mincol<-which(as.numeric(substr(colnames(z.matrix.dl1),9,nchar(colnames(z.matrix.dl1))))==min(forcetemps))
maxcol<-which(as.numeric(substr(colnames(z.matrix.dl1),9,nchar(colnames(z.matrix.dl1))))==max(forcetemps))
minrow<-which(as.numeric(substr(rownames(z.matrix.dl1),9,nchar(rownames(z.matrix.dl1))))==min(chilltemps))
maxrow<-which(as.numeric(substr(rownames(z.matrix.dl1),9,nchar(rownames(z.matrix.dl1))))==max(chilltemps))
z=z.matrix.dl1[minrow:maxrow,mincol:maxcol]
z[z<0]<-0#remove negative estimates!
x=chilltemps
y=forcetemps
zlim <- c(0,70)#c(range(c(predicts[,2:3],chillpredicts[,2:3],bothpredicts[,2:3])))

zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point
#need to work on setting it up so that it looks good without tweaking by hand...
open3d() 
plot3d(z,
       xlim = range(chilltemps), ylim = range(forcetemps), zlim = range(z), 
       xlab = '', 
       ylab = '', zlab = '', axes=FALSE) 
aspect3d(2,2,2)
axes3d(edges=c("x--", "y+-", "z--"), box=TRUE, tick=TRUE, labels=TRUE)

#axis3d(edge="x", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
#       pos = NULL) 
axis3d(edge="y+-", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
       pos = NULL,box=TRUE)
axis3d(edge="z--", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
       pos = NULL,box=TRUE)

axes3d(edges="bbox", labels=FALSE, tick = FALSE, box=TRUE)

surface3d(x,y,z, col=col, back = "lines", xlim = range(chilltemps), ylim = range(forcetemps), zlim = range(z),labels=FALSE, tick = FALSE)
rgl.snapshot("figures/bbmod_3dplot_utah.png")
#add mean winter temp observed at sites in PEP
#alltemps<-read.csv("../output/tempsumsforplotting.csv", header=TRUE)
#rgl.lines(x=range(alltemps$mnwint), y = c(-8,-8), z = c(20,20), col="lightblue", lwd=15)
#add mean spring temp observed at sites in PEP
#rgl.lines(x=c(30,30), y = range(alltemps$mnsprt), z = c(20,20), col="salmon", lwd=15)

#rgl.snapshot("figures/bbmod_3dplot_utah.png")
#if(use.chillports==FALSE){rgl.postscript("figures/forecasting/bbmod_3dplot_utah.pdf", "pdf")}
#if(use.chillports==TRUE){rgl.postscript("figures/forecasting/bbmod_3dplot_cp.pdf", "pdf")}


z.matrix.dl1.pep <- matrix(NA,ncol=length(temps),nrow=length(pepchilltemps))

for (i in 1:length(pepchilltemps)){#i=chilling
  print(pepchilltemps[i]);
  
  for(j in 1:length(temps)){#j=forcing
    if(use.chillports==FALSE){
      bbposteriors <- getest.bb(fit,temps[j], meanpepchillests$chill.utah[i], dl1,dl2)}
    
    meanz <- unlist(lapply(bbposteriors, mean))#returns  avgbb, warmsprbb, warmwinbb, warmsprwinbb)
    z.matrix.dl1.pep[i,j]<-meanz[1]#8 hour daylength only for now

  }
}
colnames(z.matrix.dl1.pep)<-paste("sprtemp",temps, sep=".")
rownames(z.matrix.dl1.pep)<-paste("wintemp",pepchilltemps, sep=".")

if(use.chillports==FALSE){
     write.csv(z.matrix.dl1.pep,"..//output/bbmodests_for3dplot_8hr_utah_pep.csv")}

mincol<-which(as.numeric(substr(colnames(z.matrix.dl1.pep),9,nchar(colnames(z.matrix.dl1.pep))))==min(forcetemps))
maxcol<-which(as.numeric(substr(colnames(z.matrix.dl1.pep),9,nchar(colnames(z.matrix.dl1.pep))))==max(forcetemps))
minrow<-which(as.numeric(substr(rownames(z.matrix.dl1.pep),9,nchar(rownames(z.matrix.dl1.pep))))==min(pepchilltemps))
maxrow<-which(as.numeric(substr(rownames(z.matrix.dl1.pep),9,nchar(rownames(z.matrix.dl1.pep))))==max(pepchilltemps))
z=z.matrix.dl1.pep[minrow:maxrow,mincol:maxcol]


x=pepchilltemps
y=forcetemps
zlim <- c(0,70)#c(range(c(predicts[,2:3],chillpredicts[,2:3],bothpredicts[,2:3])))

zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point



plot3d(zlen,
       xlim = range(pepchilltemps), ylim = range(forcetemps), zlim = range(z), 
       xlab = '', 
       ylab = '', zlab = '', axes=FALSE) 

aspect3d(2,2,2)
axes3d(edges=c("x--", "y+-", "z--"), box=TRUE, tick=TRUE, labels=TRUE)

#axis3d(edge="x", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
#       pos = NULL) 
axis3d(edge="y+-", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
       pos = NULL,box=TRUE)
axis3d(edge="z--", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
       pos = NULL,box=TRUE)

axes3d(edges="bbox", labels=FALSE, tick = FALSE, box=TRUE)

surface3d(x,y,z, col=col, back = "lines")


rgl.snapshot("figures/bbmod_3dplot_utah_withPEP.png")
if(use.chillports==FALSE){rgl.postscript("figures/bbmod_3dplot_utah_withPEP.pdf", "pdf")}


# #The below is NOT what we want (it just adds the points for PEP field conditions)
# ##Add plot with Betula using PEP field conditions
# 
# plot3d(zlen,
#        xlim = range(temps), ylim = range(temps), zlim = range(z), 
#        xlab = '', 
#        ylab = '', zlab = '', axes=FALSE) 
# 
# aspect3d(2,2,2)
# axes3d(edges=c("x--", "y+-", "z--"), box=TRUE, tick=TRUE, labels=TRUE)
# 
# #axis3d(edge="x", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
# #       pos = NULL) 
# axis3d(edge="y+-", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
#        pos = NULL,box=TRUE)
# axis3d(edge="z--", at = NULL, labels = TRUE, tick = TRUE, line = 0, 
#        pos = NULL,box=TRUE)
# 
# axes3d(edges="bbox", labels=FALSE, tick = FALSE, box=TRUE)
# 
# #read in file with different lat/longs from PEP and chiling estimates
# betests<-read.csv("..//output/betpen_for3dplot/betpen.forecast.forheatmap.csv", header=TRUE)
# 
# chilltemps.bet<-as.numeric(betests$winT.forecast[betests$warming_C==0])
# forcetemps.bet<-as.numeric(betests$sprT.forecast[betests$warming_C==0])
# bb.bet<-betests$bb.sprtemp.0[betests$warming_C==0]
# temps.bet<-range(c(chilltemps.bet,forcetemps.bet))
# x=chilltemps.bet
# y=forcetemps.bet
# z=bb.bet
# #need to work on setting it up so that it looks good without tweaking by hand...
# 
# spheres3d(x,y,z, radius = 2, col="darkgreen")
# #add mean winter temp observed at sites in PEP
# #alltemps<-read.csv("../output/tempsumsforplotting.csv", header=TRUE)
# #rgl.lines(x=range(alltemps$mnwint), y = c(-8,-8), z = c(20,20), col="lightblue", lwd=15)
# #add mean spring temp observed at sites in PEP
# #rgl.lines(x=c(30,30), y = range(alltemps$mnsprt), z = c(20,20), col="salmon", lwd=15)
# 
# 
