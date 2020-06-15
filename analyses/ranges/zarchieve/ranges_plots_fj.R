#### Plotting range data ####
#Faith Jones, started April 7th, 2020, 
#based partly on code in clean_rangesclimatedata.R by Cat 



# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Set working directory
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else setwd("~/Documents/git/ospree/analyses/ranges") 
setwd("~/Documents/github/ospree/analyses/ranges")

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(tidyr)

# Set up colors
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pch <- rep(15:18, each=12)

df <- read.csv("ranges_climclean.csv")
head(df)


#plots by Cat and dan 
#-------------------------


# Set up colors
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pch <- rep(15:18, each=12)

df <- read.csv("ranges_climclean.csv")
head(df)

allgdd <- ggplot(df, aes(x=year, y=gdd, col=complex_name, shape=complex_name)) + geom_point() + geom_line(aes(linetype=continent)) +
  theme_classic() + coord_cartesian(ylim=c(50, 850)) +
  scale_color_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pal) +
  scale_shape_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pch) # + guides(linetype=FALSE)

allutah <- ggplot(df, aes(x=year, y=utah, col=complex_name, shape=complex_name)) + geom_point() + geom_line(aes(linetype=continent)) +
  theme_classic() + coord_cartesian(ylim=c(450, 2500)) +
  scale_color_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pal) +
  scale_shape_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pch)


if(FALSE){
  eurdat <- df[(df$continent=="europe"),]
  eurgdd <- ggplot(eurdat, aes(x=year, y=gdd, col=complex_name, shape=complex_name)) + geom_point() + geom_line() +
    theme_classic() + coord_cartesian(ylim=c(50, 850)) +
    scale_color_manual(name="Species", labels=sort(unique(eurdat$complex_name)), values=my.pal) +
    scale_shape_manual(name="Species", labels=sort(unique(eurdat$complex_name)), values=my.pch) + guides(linetype=FALSE)
  
  namdat <- df[(df$continent=="north america"),]
  namgdd <- ggplot(namdat, aes(x=year, y=gdd, col=complex_name, shape=complex_name)) + geom_point() + geom_line(aes(linetype=continent)) +
    theme_classic() + coord_cartesian(ylim=c(50, 850)) +
    scale_color_manual(name="Species", labels=sort(unique(namdat$complex_name)), values=my.pal) +
    scale_shape_manual(name="Species", labels=sort(unique(namdat$complex_name)), values=my.pch) + guides(linetype=FALSE)
}


quartz()
allgdd
allutah
#eurgdd
#namgdd


#### Thinking about some barplots now...
eurdat$gddmean <- ave(eurdat$gdd, eurdat$year)
eurdat$gddmean.sd <- ave(eurdat$gdd, eurdat$year, FUN=sd)
namdat$gddmean <- ave(namdat$gdd, namdat$year, FUN=sd)
namdat$gddmean.sd <- ave(namdat$gdd, namdat$year, FUN=sd)

eurdat.bar <- subset(eurdat, select=c(year, continent, gddmean, gddmean.sd))
namdat.bar <- subset(namdat, select=c(year, continent, gddmean, gddmean.sd))

all.bar <- dplyr::full_join(eurdat.bar, namdat.bar)
all.bar <- all.bar[!duplicated(all.bar),]

all.bar$ymin <- all.bar$gddmean-all.bar$gddmean.sd
all.bar$ymax <- all.bar$gddmean+all.bar$gddmean.sd

cont.pal <- brewer.pal(n = 8, name = "Dark2")

gddbar<- ggplot(all.bar, aes(x=as.factor(year), y=gddmean, fill=continent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = c(0.1, .95),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("GDD mean") + 
  scale_fill_manual(name="Continent", values=cont.pal,
                    labels=sort(unique(all.bar$continent))) 

quartz()
gddbar

#### Now for utah chill
eurdat$utahmean <- ave(eurdat$utah, eurdat$year)
eurdat$utahmean.sd <- ave(eurdat$utah, eurdat$year, FUN=sd)
namdat$utahmean <- ave(namdat$utah, namdat$year, FUN=sd)
namdat$utahmean.sd <- ave(namdat$utah, namdat$year, FUN=sd)

eurdat.utahbar <- subset(eurdat, select=c(year, continent, utahmean, utahmean.sd))
namdat.utahbar <- subset(namdat, select=c(year, continent, utahmean, utahmean.sd))

all.barutah <- dplyr::full_join(eurdat.utahbar, namdat.utahbar)
all.barutah <- all.barutah[!duplicated(all.barutah),]

all.barutah$ymin <- all.barutah$utahmean-all.barutah$utahmean.sd
all.barutah$ymax <- all.barutah$utahmean+all.barutah$utahmean.sd

cont.pal <- brewer.pal(n = 8, name = "Dark2")

utahbar<- ggplot(all.barutah, aes(x=as.factor(year), y=utahmean, fill=continent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = c(0.1, .95),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("GDD mean") + 
  scale_fill_manual(name="Continent", values=cont.pal,
                    labels=sort(unique(all.bar$continent))) 

quartz()
utahbar


####### now let's try something wild... I want to somehow show all of the data we have
eur.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 3)

eurdat$gddymax <- eurdat$gdd + eurdat$gdd.sd
eurdat$gddymin <- eurdat$gdd - eurdat$gdd.sd

quartz()
ggplot(eurdat, aes(x=year, y=gdd, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=gddymin, ymax=gddymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(eurdat$complex_name)), 
                     values=eur.pal)


eurdat$utahymax <- eurdat$utah + eurdat$utah.sd
eurdat$utahymin <- eurdat$utah - eurdat$utah.sd

quartz()
ggplot(eurdat, aes(x=year, y=utah, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=utahymin, ymax=utahymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(eurdat$complex_name)), 
                     values=eur.pal)


nam.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 2)

namdat$gddymax <- namdat$gdd + namdat$gdd.sd
namdat$gddymin <- namdat$gdd - namdat$gdd.sd

quartz()
ggplot(namdat, aes(x=year, y=gdd, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=gddymin, ymax=gddymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(namdat$complex_name)), 
                     values=nam.pal)


namdat$utahymax <- namdat$utah + namdat$utah.sd
namdat$utahymin <- namdat$utah - namdat$utah.sd

quartz()
ggplot(namdat, aes(x=year, y=utah, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=utahymin, ymax=utahymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(namdat$complex_name)), 
                     values=nam.pal)

#Faith's plots
 #---------------------------------
wd <- getwd()
#Do species with large ragne variation also experience large interannual variation?

#get the mean amount of standard deviation in a given year 
meandf <- df %>% group_by(complex_name)%>%
	 summarise_at(c("gdd.sd", "utah.sd", "ports.sd"), mean) 
names(meandf)[2:4] <- c("range.sd.gdd", "range.sd.utah", "range.sd.ports")

#get the standard deviation around the year mean temperature (sd between years)
sddf <- df %>% group_by(complex_name)%>%
	summarise_at(c("gdd", "utah", "ports"), sd) 
names(sddf)[2:4] <- c("year.sd.gdd", "year.sd.utah", "year.sd.ports")

#combine
meanSDData <- merge(meandf, sddf, by = "complex_name")

forcePlot <- ggplot(data = meanSDData, aes(x = range.sd.gdd, y = year.sd.gdd))
forcePlot2 <- forcePlot + geom_point(aes(colour = complex_name)) +
	xlab ("within range mean sd ggd") +
	ylab("between year sd of mean ggd") +
	theme_classic() + 
	ggtitle("Forcing") +
	theme(text = element_text(size=20),
		legend.position = "none")

ChillPlot <- ggplot(data = meanSDData, aes(x = range.sd.utah, y = year.sd.utah))
ChillPlot2 <- ChillPlot + geom_point(aes(colour = complex_name))+
	xlab ("within range mean sd utah") +
	ylab("between year sd of mean utah") +
	theme_classic() + 
	ggtitle("Chilling") +
	theme(text = element_text(size=20),
		legend.position = "none")

photoPlot  <- ggplot(data = meanSDData, aes(x = range.sd.ports, y = year.sd.ports))
photoPlot2 <- photoPlot + geom_point(aes(colour = complex_name))+
	xlab ("within range mean sd ports") +
	ylab("between year sd of mean ports") +
	theme_classic() + 
	ggtitle("Photoperiod") +
	theme(text = element_text(size=20))

png(paste(wd, "/figures/meantimeVsrangeSD.png", sep = ""), width = 1800, height = 600, units = "px")
grid.arrange(forcePlot2, ChillPlot2,photoPlot2, nrow = 1, widths = c(0.5, 0.5,1))
dev.off()

#how do mean values relate to sd? (relates to H2 Chilling.)
head(df)

forcemeanPlot <- ggplot(data = df, aes(x = gdd, y = gdd.sd))
forcemeanPlot2 <- forcemeanPlot + geom_point(aes(colour = continent))+
	theme_classic()+ 
	ggtitle("Forcing") + 
	theme(text = element_text(size=20),
		legend.position = "none")

chillmeanPlot <- ggplot(data = df, aes(x = utah, y = utah.sd))
chillmeanPlot2 <- chillmeanPlot + geom_point(aes(colour = continent))+
	theme_classic()+ 
	ggtitle("Chilling") + 
	theme(text = element_text(size=20),
		legend.position = "none")

photomeanPlot <- ggplot(data = df, aes(x = ports, y = ports.sd))
photomeanPlot2 <- photomeanPlot + geom_point(aes(colour = continent))+
	theme_classic()+ 
	ggtitle("Photoperiod") + 
	theme(text = element_text(size=20))

png(paste(wd, "/figures/meanAganistSD.png", sep = ""), width = 1200, height = 400, units = "px")
grid.arrange(forcemeanPlot2, chillmeanPlot2,photomeanPlot2, nrow = 1, widths = c(1, 1,1.3))
dev.off()

#How does the sd around chilling relate to teh sd around forcing?

forceChillsdPlot <- ggplot(data = df, aes(x = utah.sd, y = gdd.sd))
forceChillsdPlot2 <- forceChillsdPlot + geom_point(aes(colour = continent))+
	theme_classic()+ 
	theme(text = element_text(size=20))

forceChillsdPlot <- ggplot(data = df, aes(x = utah.sd, y = gdd.sd))
forceChillsdPlot3 <- forceChillsdPlot + geom_point(aes(colour = complex_name))+
	theme_classic()+ 
	theme(text = element_text(size=20))

png(paste(wd, "/figures/focesdchillsd.png", sep = ""), width = 1200, height = 400, units = "px")
grid.arrange(forceChillsdPlot2, forceChillsdPlot3, nrow = 1, widths = c(1.2, 2))
dev.off()