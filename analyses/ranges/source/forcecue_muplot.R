## Started 23 October 2018 ##
## By Lizzie ##

# Runs from models_stan_plotting.R #
# Built from bb_muplot_m2l.winsp #

# model without interactions (no pooling on interactions)
# currently set up for m2l.winsp
# and for PEP 725 spp just now... 
    
# with COLORS and SYMBOLS for each species #

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
spnum <- length(unique(simpheno$sp))
pdf(file.path(figpath, paste("muplot", nameforfig, figpathmore, ".pdf", sep="")),
    width = width, height = height)
par(xpd=FALSE)
par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
     xlab="Model estimate change in forcing cue", ylab="", main=nameforfig)
axis(2, at=1:3, labels=rev(c("Intercept", "Minimum \nLatitude", "Maximum \nLatitude")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("a_force", "a_mins_sp", "a_maxs_sp")
ppeffects <- c("a_force", "a_mins_sp", "a_maxs_sp") # or 1:4 here...
for(i in 1:3){
  f <- NA
  f <- ifelse(i==1, "a_force", f)
  f <- ifelse(i==2, "a_mins_sp", f)
  f <- ifelse(i==3, "a_maxs_sp", f)
  pos.y<-(3:1)[i]
  pos.x<-summary(modelhere)$summary[mean(grep(f, rownames(summary(modelhere)$summary))),"mean"]
  lines(summary(modelhere)$summary[mean(grep(f, rownames(summary(modelhere)$summary))),c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
  pos.sps.i<-which(grepl(paste(f, "[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[1]
  jitt<-(spsi/36) + 0.01
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i,"mean"]
  lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
      col=alpha(my.pal[spsi], alphahere))
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
  
}
}
par(xpd=TRUE) # so I can plot legend outside
legend(leg1, leg2, sort(unique(gsub("_", " ", simpheno$sp))), pch=my.pch[1:spnum],
   col=alpha(my.pal[1:spnum], alphahere),
   cex=0.75, bty="n", text.font=3)
dev.off()

}
