## Started 23 October 2018 ##
## By Lizzie ##

# Runs from models_stan_plotting.R #
# Built from bb_muplot_m2l.winsp #

# model without interactions (no pooling on interactions)
# currently set up for m2l.winsp
# and for PEP 725 spp just now... 
    
# with COLORS and SYMBOLS for each species #

# muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
# spnum <- length(unique(traitors.sp))
# pdf(file.path(figpath, paste("muplot", nameforfig, figpathmore, ".pdf", sep="")),
#     width = width, height = height)
# par(xpd=FALSE)
# par(mar=c(5,7,3,10))
# plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim= c(0.5,3),
#      xlab="Model estimate change in days to budburst", ylab="", main=nameforfig)
# axis(2, at=1:3, labels=rev(c("Forcing", "Photoperiod", "Chilling")), las=1)
# abline(v=0, lty=2, col="darkgrey")
# rownameshere <- c("betaForceSp", "betaChillSp", "betaPhotoSp")
# ppeffects <- c("betaForceSp", "betaChillSp", "betaPhotoSp") # or 1:4 here...
# for(i in 1:3){
#   pos.y<-(3:1)[i]
#   pos.x<-summary(modelhere)$summary[rownameshere[i],"mean"]
#   lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
#   points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
# for(spsi in 1:spnum){
#   pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[2:4]
#   jitt<-runif(1,0.05,0.4)
#   pos.y.sps.i<-pos.y-jitt
#   pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i[i],"mean"]
#   lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
#       col=alpha(my.pal[spsi], alphahere))
#   points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
#   
# }
# }
# par(xpd=TRUE) # so I can plot legend outside
# legend(leg1, leg2, sort(unique(gsub("_", " ", traitors.sp))), pch=my.pch[1:spnum],
#    col=alpha(my.pal[1:spnum], alphahere),
#    cex=0.75, bty="n", text.font=3)
# dev.off()
# 
# }



spnum <- length(unique(traitors.sp))
#pdf(file.path(figpath, paste("muplot", nameforfig, figpathmore, ".pdf", sep="")),width = width, height = height)
par(xpd=FALSE)
par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim= c(0.5,3),
     xlab="Model estimate change in days to budburst", ylab="", main=nameforfig)
axis(2, at=1:3, labels=rev(c("Forcing", "Photoperiod", "Chilling")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("muForceSp", "muChillSp", "muPhotoSp")
beffects <- c("betaForceSp", "betaChillSp", "betaPhotoSp") # 
aeffects <- c("alphaForceSp", "alphaChillSp", "alphaPhotoSp") # 

for(i in 1:3){
  pos.y<-(3:1)[i]
  pos.x<-summary(modelhere)$summary[rownameshere[1],"mean"]
  lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
  pos.sps.i<-summary(modelhere)$summary[paste(beffects[1], "[",spsi,"]",sep=""),"mean"]
  jitt<-runif(1,0.05,0.4)
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(modelhere)$summary[paste(beffects[1], "[",spsi,"]",sep=""),"mean"]
  lines(summary(modelhere)$summary[paste(beffects[1], "[",spsi,"]",sep=""),c("25%","75%")],rep(pos.y.sps.i,2),
        col=alpha(my.pal[spsi], alphahere))
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[1], col=alpha(my.pal[spsi], alphahere))
}
}

par(xpd=TRUE) # so I can plot legend outside
legend(leg1, leg2, sort(unique(gsub("_", " ", traitors.sp))), pch=my.pch[1:spnum],
       col=alpha(my.pal[1:spnum], alphahere),
       cex=0.75, bty="n", text.font=3)


#############################################################################
traitmean <- 2
par(xpd=FALSE)
par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim= c(-30, 30), yaxt='n', ylim= c(0.5,3),
     xlab="Model estimate change in days to budburst", ylab="", main=nameforfig)
axis(2, at=1:3, labels=rev(c("Forcing", "Photoperiod", "Chilling")), las=1)
abline(v=0, lty=2, col="darkgrey")

for(i in 1:3){
  pos.y<-(3:1)[i]
 
  
for(spsi in 1:spnum){
  jitt<-runif(1,0.05,0.4)
  pos.y.sps.i<-pos.y-jitt
  
alpah.sps.i<-summary(modelhere)$summary[paste(aeffects[1], "[",spsi,"]",sep=""),"mean"]
musp.sps.i<-summary(modelhere)$summary[paste("muSp", "[",spsi,"]",sep=""),"mean"]
mugrand.sps.i<-summary(modelhere)$summary[paste("mu_grand_sp", "[",spsi,"]",sep=""),"mean"]

betaForceSp.sps.i = alpah.sps.i + traitmean * (musp.sps.i + mugrand.sps.i)

points(betaForceSp.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[1], col=alpha(my.pal[1], alphahere))
}
}
