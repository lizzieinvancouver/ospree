## Started 13 July 2018 ##
## By Lizzie ##

# Runs from models_stan_plotting.R #

# model with interactions (no pooling on interactions)
# currently set up for m2l.winsp
spnum <- 38
par(mar=c(5,7,3,2))
plot(x=NULL,y=NULL, xlim=c(-13, 2), yaxt='n', ylim=c(0,6),
     xlab="Model estimate change in days to BB", ylab="", main="M1_daysBB_2level.stan")
axis(2, at=1:6, labels=rev(c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "b_cf","b_cp","b_fp")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "b_cf","b_cp","b_fp")
ppeffects <- c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp") # or 1:4 here...
for(i in 1:6){
  pos.y<-(6:1)[i]
  pos.x<-summary(m1.bb)$summary[rownameshere[i],"mean"]
  lines(summary(m1.bb)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
  pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(m1.bb)$summary),fixed=TRUE))[2:4]
  jitt<-runif(1,0.05,0.4)
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(m1.bb)$summary[pos.sps.i[i],"mean"]
  lines(summary(m1.bb)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),col=cols)
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8,pch=19,col=cols)
  
}
}


if(FALSE){
    
# with COLORS and SYMBOLS for each species #

library(RColorBrewer)
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

spnum <- 38
par(xpd=FALSE)
par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim=c(-13, 2), yaxt='n', ylim=c(0,6),
     xlab="Model estimate change in days to BB", ylab="", main="M1_daysBB_2level")
axis(2, at=1:6, labels=rev(c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "b_cf","b_cp","b_fp")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp", "b_cf","b_cp","b_fp")
ppeffects <- c("mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp") # or 1:4 here...
for(i in 1:6){
  pos.y<-(6:1)[i]
  pos.x<-summary(m1.bb)$summary[rownameshere[i],"mean"]
  lines(summary(m1.bb)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
  pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(m1.bb)$summary),fixed=TRUE))[2:4]
  jitt<-runif(1,0.05,0.4)
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(m1.bb)$summary[pos.sps.i[i],"mean"]
  lines(summary(m1.bb)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
      col=alpha(my.pal[spsi], alphahere))
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
  
}
}
par(xpd=TRUE) # so I can plot legend outside
legend(3, 6.4, unique(bb.stan$complex.wname), pch=my.pch[1:38], col=alpha(my.pal[1:38], alphahere),
   cex=0.75, bty="n")
}
