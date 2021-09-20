## Started 23 October 2018 ##
## By Lizzie ##

# Runs from models_stan_plotting.R #
# Built from bb_muplot_m2l.winsp #

# model without interactions (no pooling on interactions)
# currently set up for m2l.winsp
# and for PEP 725 spp just now... 
    
# with COLORS and SYMBOLS for each species #
muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
spnum <- length(unique(bb.stan$spps))
pdf(file.path(figpath, paste("muplot", nameforfig, figpathmore, ".pdf", sep="")),
    width = width, height = height)
par(xpd=FALSE)
par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
     xlab="Model estimate change in days to budburst", ylab="", main=nameforfig)
axis(2, at=1:3, labels=rev(c("Forcing", "Photoperiod", "Chilling")), las=1)
abline(v=0, lty=2, col="darkgrey")
slopeshere <- c("b_force", "b_photo", "b_chill")
for(i in 1:3){
    pos.y<-(3:1)[i]
    pos.x<-mean(summary(modelhere)$summary[which(grepl(slopeshere[i] ,rownames(summary(modelhere)$summary),fixed=TRUE),)])
    # need to add uncertainty intervals
    points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:spnum){
    pos.sps.i <- which(grepl(paste(slopeshere[i], "[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))
    jitt <- runif(1,0.05,0.4)
    pos.y.sps.i <- pos.y-jitt
    pos.x.sps.i <- summary(modelhere)$summary[pos.sps.i,"mean"]
    lines(summary(modelhere)$summary[pos.sps.i,c("25%","75%")],rep(pos.y.sps.i,2),
        col=alpha(my.pal[spsi], alphahere))
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
}
}
par(xpd=TRUE) # so I can plot legend outside
legend(leg1, leg2, sort(unique(gsub("_", " ", bb.stan$spps))), pch=my.pch[1:spnum],
   col=alpha(my.pal[1:spnum], alphahere),
   cex=0.5, bty="n", text.font=3)
dev.off()

}



## modified version of the above function, tweaked to plot results from
## phylogenetic models once a fit object has been renamed to be used by the
## stan utility
muplotfx_phylo<-function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
    #width = 7 
    #height = 8 
    #ylim =  c(0,3) 
    #xlim = c(-25, 15)  
    #leg1 = 12 
    #leg2 = 3.5
    #posspsindata <- list(10:28,30:48,50:68)
    spnum <- length(unique(bb.stan$spps))
    par(xpd=FALSE)
    par(mar=c(5,7,3,10))
    plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
         xlab="Model estimate change in days to budburst", ylab="", main="")
    axis(2, at=1:3, labels=rev(c("Forcing", "Chilling", "Photoperiod")), las=1)
    abline(v=0, lty=2, col="darkgrey")
    slopeshere <- c("b_zf","b_zc","b_zp")
    
    
    for(i in 1:3){#i=1
        pos.y<-(3:1)[i]
        pos.x<-mean(summary(modelhere)$summary[which(grepl(slopeshere[i] ,rownames(summary(modelhere)$summary),fixed=TRUE),)])
        # need to add uncertainty intervals
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){#spsi=1
            pos.sps.i <- posspsindata[[i]][spsi]
            jitt <- runif(1,0.05,0.4)
            pos.y.sps.i <- pos.y-jitt
            pos.x.sps.i <- summary(modelhere)$summary[pos.sps.i,"mean"]
            lines(summary(modelhere)$summary[pos.sps.i,c("25%","75%")],rep(pos.y.sps.i,2),
                  col=alpha(my.pal[spsi], alphahere))
            points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        }
    }
    
    
    par(xpd=TRUE) # so I can plot legend outside
    legend(leg1, leg2, sort(unique(gsub("_", " ", bb.stan$spps))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=0.8, bty="n", text.font=3)
    #dev.off()
    
}