## Started 3 April 2019 ##
## By Cat - based off code from Lizzie's OSPREE plots ##

# Runs from models_stan_plotting.R #

# with COLORS for each species #

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
  spnum <- unique(df$species)
  pdf(file.path(figpath, paste("", nameforfig, figpathmore, ".pdf", sep="")),
      width = width, height = height)
  par(xpd=FALSE)
  par(mar=c(5,7,3,10))
  plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
       xlab=xlab, ylab="", main=nameforfig)
  axis(2, at=1, labels=c("Forcing"), las=1)
  abline(v=0, lty=2, col="darkgrey")
  rownameshere <- c("b_force")
  ppeffects <- c("b_force") # or 1:4 here...
  for(i in 1){#i=1
    pos.y<-(1)[i]
    pos.x<-modoutput[(modoutput$term==rownameshere[i]),"estimate"]
    lines(modoutput[(modoutput$term==rownameshere[i]),c("lower","upper")],rep(pos.y,2),col="darkgrey")
    lines(modoutput[(modoutput$term==rownameshere[i]),c("low50","high50")],rep(pos.y,2),col="black", lwd=2)
    points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
    for(spsi in 1:length(spnum)){#spsi=1
      pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),mod.ranef$parameter,fixed=TRUE))
      jitt<-(spsi/20) + 0.02
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-mod.ranef[pos.sps.i[i],"mean"]
      lines(mod.ranef[pos.sps.i[i],c("10%","90%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere))
      lines(mod.ranef[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere+0.5))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, col=alpha(my.pal[spsi], alphahere))
      
    }
  }
  par(xpd=TRUE) # so I can plot legend outside
  legend(leg1, leg2, sort(unique(gsub("_", " ", df$species.name))), pch=19, pch=my.pch[1:npop],
         col=alpha(my.pal[1:length(spnum)], alphahere),
         cex=1, bty="n", text.font=3)
  dev.off()
  
}
