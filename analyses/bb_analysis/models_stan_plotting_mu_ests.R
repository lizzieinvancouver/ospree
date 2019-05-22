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
quartz()
hist(bb.stan$chill.ports)
hist(bb.stan$chill)#=utah

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

forcetemps<-seq(min(bb.stan$force), max(bb.stan$force), by=1)
chilltemps<-seq(min(as.numeric(bb.stan$chilltemp), na.rm=TRUE),max(as.numeric(bb.stan$chilltemp), na.rm=TRUE), by=1)
chilldays<-as.integer(mean(as.numeric(bb.stan$chilldays), na.rm=TRUE))
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

#now calculate with chilling effects
#create a vactor of chillportions, using different temperatures
chillests<-as.data.frame(matrix(NA,ncol=10,nrow=length(temps)))
JDay<-seq(1:chilldays)
Year<-2014
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
  chillests[i,]<-c(temps[i],chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp)]))
}
colnames(chillests)<- c("temp","Season","End_year","Season_days","Data_days","Perc_complete","Chilling_Hours","Utah_Model","Chill_portions","GDH")  
chillests$Utah_Model<-chillests$Utah_Model/240
#make blank dataframe to fill with estimates
chillpredicts <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
chillpredicts.25per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))
chillpredicts.75per <- as.data.frame(matrix(NA,ncol=3,nrow=length(temps)))

colnames(chillpredicts)<-colnames(chillpredicts.25per) <-colnames(chillpredicts.75per) <-
  c("chilltemp","dl1","dl2")
mnforce<-mean(as.numeric(bb.stan$forcetemp), na.rm=TRUE)
for (i in 1:length(temps)){
  if(use.chillports==TRUE){bbposteriors <- getest.bb(fit,mnforce, chillests$Chill_portions[i], dl1,dl2)}
  if(use.chillports==FALSE){bbposteriors <- getest.bb(fit,mnforce, chillests$ Utah_Model[i], dl1,dl2)}
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
  c("chilltemp","dl1","dl2")
for (i in 1:length(temps)){
  if(use.chillports==TRUE){bbposteriors <- getest.bb(fit,temps[i], chillests$Chill_portions[i], dl1,dl2)}
  if(use.chillports==FALSE){bbposteriors <- getest.bb(fit,temps[i], chillests$Utah_Model[i], dl1,dl2)}
  
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
if(use.chillports==TRUE){figname<-paste("mupredicts_chillport_",min(temps),max(temps),".pdf", sep="_")}
if(use.chillports==FALSE){figname<-paste("mupredicts_utah_",min(temps),max(temps),".pdf", sep="_")}

pdf(file.path(figpath,figname), width = 9, height = 6)

#quartz()
par(mar=c(8,7,3,5))
plot(predicts$forcetemp,predicts$dl1, xlim=xlim, xlab="Temperature (C)", ylim=ylim,
     ylab="Days to BB", type="l",bty="l", lty=1, lwd=2, col="darkred")
lines(predicts$forcetemp,predicts$dl2,lty=2, lwd=2,col="darkred")
lines(chillpredicts$chilltemp,chillpredicts$dl1,lty=1, lwd=2, col="blue")
lines(chillpredicts$chilltemp,chillpredicts$dl2,lty=2, lwd=2, col="blue")
lines(bothpredicts$chilltemp,bothpredicts$dl1,lty=1, lwd=2, col="purple")
lines(bothpredicts$chilltemp,bothpredicts$dl2,lty=2, lwd=2, col="purple")


if(use.chillports==TRUE){legend("topright",
      legend=c("8 hr-forcing","16 hr-forcing","chilling","both"), 
      lty=c(1,2,1,1), col=c("darkred","darkred","blue","purple"),lwd=2)}
if(use.chillports==FALSE){legend("topleft",
                                legend=c("8 hr-forcing","16 hr-forcing","chilling","both"), 
                                lty=c(1,2,1,1), col=c("darkred","darkred","blue","purple"),lwd=2)}

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
#z.matrix.dl1<-read.csv("../output/bbmodests_for3dplot_8hr_utah.csv")
for (i in 1:length(temps)){#i=chilling
 print(temps[i]);
  
  for(j in 1:length(temps)){
    if(use.chillports==TRUE){
    bbposteriors <- getest.bb(fit,temps[j], chillests$Chill_portions[i], dl1,dl2)}
  if(use.chillports==FALSE){
    bbposteriors <- getest.bb(fit,temps[j], chillests$Utah_Model[i], dl1,dl2)}

    meanz <- unlist(lapply(bbposteriors, mean))#returns  avgbb, warmsprbb, warmwinbb, warmsprwinbb)
    z.matrix.dl1[i,j]<-meanz[1]#8 hour daylength only for now
    z.matrix.dl2[i,j]<-meanz[2]#16 hour daylength only for now
    
  }
}

colnames(z.matrix.dl1)<-colnames(z.matrix.dl2)<-paste("sprtemp",temps, sep=".")
rownames(z.matrix.dl1)<-colnames(z.matrix.dl2)<-paste("wintemp",temps, sep=".")
if(use.chillports==TRUE){
  write.csv(z.matrix.dl1,"..//output/bbmodests_for3dplot_8hr_cp.csv")
  write.csv(z.matrix.dl2,"..//output/bbmodests_for3dplot_16hr_cp.csv")}
if(use.chillports==FALSE){
  write.csv(z.matrix.dl1,"..//output/bbmodests_for3dplot_8hr_utah.csv")
  write.csv(z.matrix.dl2,"..//output/bbmodests_for3dplot_16hr_utah.csv")} 
    
z=z.matrix.dl1[1:length(temps),1:length(temps)]
x=temps
y=temps
zlim <- c(0,70)#c(range(c(predicts[,2:3],chillpredicts[,2:3],bothpredicts[,2:3])))

zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point
#need to work on setting it up so that it looks good without tweaking by hand...
open3d() 
plot3d(z,
       xlim = c(-8,30), ylim = c(-8,30), zlim = range(z), 
       xlab = 'Winter temperature (C)', 
       ylab = 'Spring temperature (C)', zlab = 'Days to BB', axes=FALSE) 
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

#add mean winter temp observed at sites in PEP
alltemps<-read.csv("../output/tempsumsforplotting.csv", header=TRUE)
rgl.lines(x=range(alltemps$mnwint), y = c(-8,-8), z = c(20,20), col="lightblue", lwd=15)
#add mean spring temp observed at sites in PEP
rgl.lines(x=c(30,30), y = range(alltemps$mnsprt), z = c(20,20), col="salmon", lwd=15)

rgl.snapshot("figures/bbmod_3dplot_utah.png")
rgl.postscript("figures/bbmod_3dplot_utah_obs.pdf", "pdf")



#######################
####Below not used...yet
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

rgl_init()
plot3d(z.matrix, type = 'n', 
       xlim = range(x), ylim = range(y), zlim = range(z), 
       xlab = 'Winter warming (C)', 
       ylab = 'Spring warming (C)', zlab = 'Days to BB') 
aspect3d(1,1,1)
#aspect3d("iso")

axes3d(edges=c("x--", "y+-", "z--"), box=TRUE)
surface3d(x,y, z,
          col=col, back = "lines")
aspect3d(1,1,1)
aspect3d("iso")

