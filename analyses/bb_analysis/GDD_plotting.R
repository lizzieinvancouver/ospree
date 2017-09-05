##############################################################################################################
# Script and functions to:
#' * Plotting GDD against chilling metrics based on different photoperiod treatments 
#' 
#'  Ospree
#'  started 17 August 2017
#'  
##############################################################################################################


## to start
rm(list=ls())
options(stringsAsFactors=FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
} else 
  setwd("~/Documents/git/ospree/analyses")

## loading packages and setting themes
library(ggplot2)
library(cowplot)

theme_set(theme_bw())  # pre-set the bw theme.

## function to plot multiple panel ggplots from http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 


## load saved data with gdd
bb.wstart<-read.csv("output/bbdata_wgdd.csv")
bb.wstart<-bb.wstart[,-1]
head(bb.wstart)

## plotting gdd vs different metrics of chilling (subsetted to gdd up to 3500, does not plot Malus)
chills<-c(21,22,23,37,65,70,72,76,78,79,81)
par(mfrow=c(3,4))
for(i in chills){
  print(i)  
  
  plot(bb.wstart[,i],bb.wstart[,89],ylab="GDD",xlab=names(bb.wstart)[i],
       ylim=c(0,3500),col=terrain.colors(as.factor(bb.wstart$species)))
}



## plotting gdd vs. chilldays, exp_chilling_hours, Field chilling hours, Total hiclling hours
chills<-c(23,70,76,79)

gdds<-list()
gdds.nomalus<-list()
gdds.partial<-list()
gdds.legends<-list()
for (i in chills){#i=70
chills.i<-names(bb.wstart)[i]  
bb.wstart[,chills.i]<-as.numeric(bb.wstart[,chills.i])
bb.plot<-subset(bb.wstart,!is.na(bb.wstart[,chills.i]))
bb.plot$chillmetric<-bb.plot[,chills.i]

# Scatterplot all
gdds[[which(chills==i)]] <- ggplot(bb.plot, aes(x=chillmetric, y=gdd)) + 
  geom_point(aes(col=factor(genus)),alpha=0.25,size=3) + 
  geom_smooth(method="lm", se=T) + 
  #xlim(c(0, 200)) + 
  #ylim(c(0, 7500)) + 
  theme(legend.position="none")+
  labs( 
    y="GDD", 
    x=chills.i
    #title="Scatterplot", 
  )

# Scatterplot without malus 
gdds.nomalus[[which(chills==i)]] <- ggplot(bb.plot, aes(x=chillmetric, y=gdd)) + 
  geom_point(aes(col=factor(genus)),alpha=0.25,size=3) + 
  geom_smooth(method="lm", se=T) + 
  #xlim(c(0, 200)) + 
  ylim(c(0, 3500)) + 
  theme(legend.position="none")+
  labs( 
    y="GDD", 
    x=chills.i
    #title="Scatterplot", 
  )

# Scatterplot without malus and each generic fitted line
gdds.partial[[which(chills==i)]] <- ggplot(bb.plot, aes(x=chillmetric, y=gdd)) + 
  geom_point(aes(col=factor(genus)),alpha=0.25,size=3) + 
  geom_smooth(aes(col=factor(genus)),method="lm", se=F) + 
  #xlim(c(0, 200)) + 
  ylim(c(0, 3500)) + 
  theme(legend.position="none")+
  labs( 
    y="GDD", 
    x=chills.i
    #title="Scatterplot", 
  )

# legends
gdds.legends[[which(chills==i)]]<-get_legend( ggplot(bb.plot, aes(x=chillmetric, y=gdd)) + 
                                              geom_point(aes(col=factor(genus))))

}



# Now plots are aligned vertically with the legend to the right
ggdraw(plot_grid(plot_grid(gdds[[1]],gdds.nomalus[[1]],gdds.partial[[1]], ncol=3, align='h'),
                 plot_grid(gdds.legends[[1]], ncol=1,axis="t"),
                 rel_widths=c(1, 0.2)))
ggdraw(plot_grid(plot_grid(gdds[[2]],gdds.nomalus[[2]],gdds.partial[[2]], ncol=3, align='h'),
                 plot_grid(gdds.legends[[2]], ncol=1,axis="t"),
                 rel_widths=c(1, 0.2)))
ggdraw(plot_grid(plot_grid(gdds[[3]],gdds.nomalus[[3]],gdds.partial[[3]], ncol=3, align='h'),
                 plot_grid(gdds.legends[[3]], ncol=1,axis="t"),
                 rel_widths=c(0.8, 0.5)))
ggdraw(plot_grid(plot_grid(gdds[[4]],gdds.nomalus[[4]],gdds.partial[[4]], ncol=3, align='h'),
                 plot_grid(gdds.legends[[4]], ncol=1,axis="t"),
                 rel_widths=c(0.8, 0.5)))


## replicate plotting accounting for photoperiod treatments
gdds.photo<-list()
gdds.nomalus.photo<-list()
gdds.partial.photo<-list()
gdds.legends.photo<-list()
photos<-sort(as.numeric(unique(bb.wstart$photoperiod_day)))
photo.i<-c(1,2,3,4)
for (i in chills){#i=70
  
  chills.i<-names(bb.wstart)[i]  
  bb.wstart[,chills.i]<-as.numeric(bb.wstart[,chills.i])
  bb.plot<-subset(bb.wstart,!is.na(bb.wstart[,chills.i]))
  bb.plot$chillmetric<-bb.plot[,chills.i]
  gdds.photo[[which(chills==i)]]<-list(NULL,NULL,NULL,NULL)
  gdds.nomalus.photo[[which(chills==i)]]<-list(NULL,NULL,NULL,NULL)
  gdds.partial.photo[[which(chills==i)]]<-list(NULL,NULL,NULL,NULL)
  gdds.legends.photo[[which(chills==i)]]<-list(NULL,NULL,NULL,NULL)
  
  for (j in photo.i){
    print(paste(i,j))
    
  if(j==1){
    bb.plot.photo<-subset(bb.plot,photoperiod_day%in%as.character(photos[1:7]))
  }
  if(j==2){
      bb.plot.photo<-subset(bb.plot,photoperiod_day%in%as.character(photos[8:16]))
  }
  if(j==3){
      bb.plot.photo<-subset(bb.plot,photoperiod_day%in%as.character(photos[7:25]))
  }
  if(j==4){
      bb.plot.photo<-subset(bb.plot,photoperiod_day%in%as.character(photos[26:34]))
    }
    
      
  # Scatterplot all
    gdds.photo[[which(chills==i)]][[j]] <- ggplot(bb.plot.photo, aes(x=chillmetric, y=gdd)) + 
    geom_point(aes(col=factor(genus)),alpha=0.25,size=3) + 
    geom_smooth(method="lm", se=T) + 
    #xlim(c(0, 200)) + 
    #ylim(c(0, 7500)) + 
    theme(legend.position="none")+
    labs( 
      y="GDD", 
      x=chills.i
      #title="Scatterplot", 
    )
  
  # Scatterplot without malus 
  gdds.nomalus.photo[[which(chills==i)]][[j]] <- ggplot(bb.plot.photo, aes(x=chillmetric, y=gdd)) + 
    geom_point(aes(col=factor(genus)),alpha=0.25,size=3) + 
    geom_smooth(method="lm", se=T) + 
    #xlim(c(0, 200)) + 
    ylim(c(0, 3500)) + 
    theme(legend.position="none")+
    labs( 
      y="GDD", 
      x=chills.i
      #title="Scatterplot", 
    )
  
  # Scatterplot without malus and each generic fitted line
  gdds.partial.photo[[which(chills==i)]][[j]] <- ggplot(bb.plot.photo, aes(x=chillmetric, y=gdd)) + 
    geom_point(aes(col=factor(genus)),alpha=0.25,size=3) + 
    geom_smooth(aes(col=factor(genus)),method="lm", se=F) + 
    #xlim(c(0, 200)) + 
    ylim(c(0, 3500)) + 
    theme(legend.position="none")+
    labs( 
      y="GDD", 
      x=chills.i
      #title="Scatterplot", 
    )
  
  # legends
  if(nrow(bb.plot.photo)>2){
  gdds.legends.photo[[which(chills==i)]][[j]]<-get_legend( ggplot(bb.plot.photo, aes(x=chillmetric, y=gdd)) + 
                                                  geom_point(aes(col=factor(genus))))
  }
}

}

i=3;j=1
ggdraw(plot_grid(plot_grid(plot_grid(gdds.photo[[i]][[j]],gdds.nomalus.photo[[i]][[j]],gdds.partial.photo[[i]][[j]], ncol=3, align='h'),
                            plot_grid(gdds.legends.photo[[1]][[j]], ncol=1,axis="t"),
                            rel_widths=c(1, 0.2)),
                 plot_grid(plot_grid(gdds.photo[[i]][[j+1]],gdds.nomalus.photo[[i]][[j+1]],gdds.partial.photo[[i]][[j+1]], ncol=3, align='h'),
                           plot_grid(gdds.legends.photo[[1]][[j+1]], ncol=1,axis="t"),
                           rel_widths=c(1, 0.2)),
                 plot_grid(plot_grid(gdds.photo[[i]][[j+2]],gdds.nomalus.photo[[i]][[j+2]],gdds.partial.photo[[i]][[j+2]], ncol=3, align='h'),
                           plot_grid(gdds.legends.photo[[1]][[j+2]], ncol=1,axis="t"),
                           rel_widths=c(1, 0.2)),
                 plot_grid(plot_grid(gdds.photo[[i]][[j+3]],gdds.nomalus.photo[[i]][[j+3]],gdds.partial.photo[[i]][[j+3]], ncol=3, align='h'),
                           plot_grid(gdds.legends.photo[[i]][[j+3]], ncol=1,axis="t"),
                           rel_widths=c(1, 0.2))))
