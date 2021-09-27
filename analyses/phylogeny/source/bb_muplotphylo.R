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
muplotfx_phylo<-function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2, posspsindata){
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
            jitt <- runif(1,0.05,0.8)
            pos.y.sps.i <- pos.y-jitt
            pos.x.sps.i <- summary(modelhere)$summary[pos.sps.i,"mean"]
            lines(summary(modelhere)$summary[pos.sps.i,c("25%","75%")],rep(pos.y.sps.i,2),
                  col=alpha(my.pal[spsi], alphahere))
            points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        }
    }
    
    if(agiosponly){cexleg = 0.4}
    if(gymnosonly){cexleg = 0.8}
    
    par(xpd=TRUE) # so I can plot legend outside
    legend(leg1, leg2, sort(unique(gsub("_", " ", bb.stan$spps))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=cexleg, bty="n", text.font=3)
    #dev.off()
    
}



## aux function from https://github.com/joelnitta/jntools/blob/master/R/plots.R
get_tips_in_ape_plot_order <- function (tree) {
    assertthat::assert_that(inherits(tree, "phylo"))
    # First filter out internal nodes
    # from the the second column of the edge matrix
    is_tip <- tree$edge[,2] <= length(tree$tip.label)
    ordered_tips <- tree$edge[is_tip, 2]
    # Use this vector to extract the tips in the right order
    tree$tip.label[ordered_tips]
}


## modified version of the above function, tweaked to plot results from
## phylogenetic models once a fit object has been renamed to be used by the
## stan utility
muplotfx_phylo_contmap<-function(modelhere, nameforfig, width, height, 
                         ylim, xlim, leg1, leg2, posspsindata,cue,to.rem){
    #width = 7 
    #height = 8 
    #ylim =  c(0,194) 
    #xlim = c(-20, 5)  
    #leg1 = 12 
    #leg2 = 3.5
    spnum <- length(unique(bb.stan$spps))
    par(xpd=FALSE)
    
    
    slopeshere <- c("b_zf","b_zc","b_zp")
    
    
    for(i in cue){#cue=1;i=cue
        
        ## forcing
        cueresponse = summary(modelhere)$summary[posspsindata[[i]],"mean"]
        phylo=multi2di(phylo)
        obj.cue<-contMap(phylo,cueresponse,plot=FALSE)
        obj.cue<-setMap(obj.cue,invert=TRUE)
        obj.cue<-setMap(obj.cue,colors=c("yellow","darkcyan","purple"))
       
        dev.off()
        
        
        if(agiosponly){
            par(mfrow=c(1,2),mar=c(0.5,0.5,1,0.1))
            plot.contMap(obj.cue,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
                         fsize = c(0.45, 0.7),ftype="off", 
                         xlim=c(-0.1,1)*max(nodeHeights(phylo)),
                         outline=FALSE,lwd=2,mar = c(1,1,2,0))
            
        par(mar=c(5,6.5,2,1))
        
        plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
             xlab="Model estimate change in days to budburst", ylab="", main="")
        #axis(2, at=1.1, labels=c("Forcing", "Chilling", "Photoperiod")[cue], las=1)
        abline(v=0, lty=2, col="darkgrey")
        
        #pos.y<-1
        #pos.x<-mean(summary(modelhere)$summary[which(grepl(slopeshere[i] ,rownames(summary(modelhere)$summary),fixed=TRUE),)])
        # need to add uncertainty intervals
        #points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        tips.order = get_tips_in_ape_plot_order(phylo)
        par(xpd=TRUE) # so I can plot legend outside
        }
        
        if(gymnosonly){
            par(mfrow=c(1,2),mar=c(0.5,1,1,0))
            plot.contMap(obj.cue,type = "phylogram",legend = 0.6*max(nodeHeights(phylo)),
                         fsize = c(0.45, 0.7),ftype="off", 
                         xlim=c(-0.1,1)*max(nodeHeights(phylo)),
                         outline=FALSE,lwd=2,mar = c(1,1,2,0))
            
            #ylim =  c(1,19) 
            #xlim = c(-20, 2)  
            par(mar=c(5,6,2,1))
            
            plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
                 xlab="Model estimate change in days to budburst", ylab="", main="")
            #axis(2, at=1.1, labels=c("Forcing", "Chilling", "Photoperiod")[cue], las=1)
            abline(v=0, lty=2, col="darkgrey")
            
            #pos.y<-1
            #pos.x<-mean(summary(modelhere)$summary[which(grepl(slopeshere[i] ,rownames(summary(modelhere)$summary),fixed=TRUE),)])
            # need to add uncertainty intervals
            #points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
            tips.order = get_tips_in_ape_plot_order(phylo)
            par(xpd=TRUE) # so I can plot legend outside
        }
        
        
        
        for(spsi in 1:spnum){#spsi=1
           
            spsiname = phylo$tip.label[spsi]
            
            pos.sps.i <- posspsindata[[i]][spsi]
            pos.y.sps.i <- which(tips.order==spsiname)
            pos.x.sps.i <- summary(modelhere)$summary[pos.sps.i,"mean"]
            
            jj<-which(obj.cue$tree$tip.label==spsiname)
            kk<-which(obj.cue$tree$edge[,2]==jj)
            col.sps.i <- setNames(obj.cue$cols[names(obj.cue$tree$maps[[kk]])[length(obj.cue$tree$maps[[kk]])]],
                         NULL)
            
             
            lines(summary(modelhere)$summary[pos.sps.i,c("25%","75%")],rep(pos.y.sps.i,2),
                  col=col.sps.i)
            points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], 
                   col=col.sps.i)
            
            
            
        text(xlim[1]-to.rem,pos.y.sps.i,tips.order[pos.y.sps.i],cex=0.5,las=1,pos=4)
            #axis(2,at=1:194,labels=,las=1,cex=0.5)

                    }  #plot(obj.force,fsize=c(0.5,1),outline=FALSE,lwd=c(3,7),leg.txt="forcing")
        par(xpd=F) # so I can plot legend outside
        
       
       
    }
    
    if(agiosponly){cexleg = 0.4}
    if(gymnosonly){cexleg = 0.8}
    
    legend(leg1, leg2, sort(unique(gsub("_", " ", bb.stan$spps))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=cexleg, bty="n", text.font=3)
    
    
    
    
    #dev.off()
    
}

