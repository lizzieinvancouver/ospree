## Started 3 April 2019 ##
## By Cat - based off code from Lizzie's OSPREE plots ##

# Runs from models_stan_plotting.R #

# with COLORS for each species #

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
  spnum <- unique(mod.ranefpop$species)
  popnum <- unique(mod.ranefpop$pop)
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
    for(spsi in 1:length(spnum)){#spsi=2
      sps <- spnum[spsi]
      pos.sps.i<-which(grepl(paste0(sps),mod.ranefpop$species,fixed=TRUE))
      jitt<-(spsi/15) + 0.02
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-mod.ranefpop[pos.sps.i[i],"mean"]
      lines(mod.ranefpop[pos.sps.i[i],c("10%","90%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere))
      lines(mod.ranefpop[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere+0.5))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, col=alpha(my.pal[spsi], alphahere))
      
      for(popi in 1:length(popnum)){#popi=1
        pop <- popnum[popi]
        pos.pop.i<-which(grepl(paste0(pop),mod.ranefpop$pop,fixed=TRUE))
        jitt2<-(popi*0.01)
        pos.y.pop.i<-pos.y.sps.i-jitt2
        pos.x.pop.i<-mod.ranefpop[pos.pop.i[i],"mean"]
        lines(mod.ranefpop[pos.pop.i[i],c("10%","90%")],rep(pos.y.pop.i,2),
              col=alpha(my.pal[spsi], alphahere))
        lines(mod.ranefpop[pos.pop.i[i],c("25%","75%")],rep(pos.y.pop.i,2),
              col=alpha(my.pal[spsi], alphahere+0.5))
        points(pos.x.pop.i,pos.y.pop.i,cex=0.8, col=alpha(my.pal[spsi], alphahere), pch=my.pch[popi])
        
      }
      
    }
  }
  par(xpd=TRUE) # so I can plot legend outside
  legend(leg1, leg2, sort(unique(gsub("_", " ", df$species))), pch=my.pch[1:length(popnum)],
         col=alpha(my.pal[1:length(spnum)], alphahere),
         cex=1, bty="n", text.font=3)
  dev.off()
  
}
