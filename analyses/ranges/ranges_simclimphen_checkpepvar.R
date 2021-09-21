## Started 21 Sept 2021 ##
## By Ailene  ##
## Modified from https://github.com/lizzieinvancouver/decsens/blob/master/analyses/pep_analyses/plotpepdailyclim.R ##

## Look at annual and daily variance in temperature from PEP data ##
## (Annual and daily variance is used in ranges_simclimphen.R)

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
require(plyr)
require(dplyr)
require(ggplot2)

# Set working directory
setwd("~/GitHub/ospree/analyses/ranges/")

# From Cat: daily temps from Jan 1 to Apr 30. Let me know if you'd like me to make a figure! I used the 17 sites from the BETPEN 20-year window
bp <- read.csv("../output/dailyclim/dailytemps_jantoapr.csv")
bp$date <- as.Date(bp$Date, format="%Y-%m-%d")
bp$doy <- format(bp$date, "%j")
bp$mon <- format(bp$date, "%m")
bp$lat.round <- round(bp$lat, 2)
bp$lon.round <- round(bp$lon, 2)
bp$latlon.round <- paste(bp$lat.round, "N", bp$lon.round, "E", sep=" ")
length(unique(bp$lat.long))
length(unique(bp$latlon.round))

#Focus on winter temp and ,calculate daily variance in Tavg, across sites and years
bpwin <- subset(bp, as.numeric(doy)<32)
dim(bpwin)

varbpwinT<-aggregate(bpwin$Tavg,by=list(bpwin$latlon.round,bpwin$year), var)
mnbpwinT<-aggregate(bpwin$Tavg,by=list(bpwin$latlon.round,bpwin$year), mean)
varmn<-cbind(mnbpwinT,varbpwinT$x)
colnames(varmn)<-c("latlon.round","year","winTavg","varwinTavg")

ggplot(varmn, aes(x=as.numeric(year), y=varwinTavg)) +
  geom_point() +
  # geom_smooth(method="lm") + 
  facet_wrap(.~as.factor(latlon.round)) +
  ggtitle("Variance in daily Tavg, Jan")

hist(varmn$varwinTavg)

#looks like variance of 5-15 are quite common

#Now variance in mean annual temperature for January


intervar<-aggregate(varmn$winTavg,by=list(varmn$latlon.round), var)

intervar

#make plot of both intra annual and interannual variance

par(mfrow=c(1,2))
hist(varmn$varwinTavg, main = "Intra-annual var",xlab="Var,Jan daily Tavg (C)")
hist(intervar$x, main = "Inter-annual",xlab="Var, mean Jan Tavg (C)")


#Spring linear increase in temp (visualization from decsens)
bpsm <- subset(bp, as.numeric(doy)>45)

bpsm.select <- subset(bpsm, year==2007)
ggplot(bpsm.select, aes(x=as.numeric(doy), y=Tavg, group=as.factor(year), colour=as.factor(year))) +
    geom_point() +
   # geom_smooth(method="lm") + 
    facet_wrap(.~as.factor(latlon.round))

bpsm$decade <- NA
bpsm$decade[which(bpsm$year<1971)] <- "1951-1970"
bpsm$decade[which(bpsm$year>1990)] <- "1991-2010"
bpsm$decade[which(is.na(bpsm$decade)==TRUE)] <- "1971-1990"

bpsummdec <-
      ddply(bpsm, c("latlon.round", "decade", "doy"), summarise,
      temp = mean(Tavg))

ggplot(bpsummdec, aes(x=as.numeric(doy), y=temp, group=as.factor(decade), color=as.factor(decade))) +
    geom_line() +
    # geom_smooth(method="lm") +
    facet_wrap(.~as.factor(latlon.round))

bpsumm <-
      ddply(bpsm, c("latlon.round", "doy"), summarise,
      temp = mean(Tavg))

getrsq <- function(df, x, y, group){
    dfnew <- data.frame(group=character(), r2=numeric(), slope=numeric())
    getgroups <- unlist(unique(df[group]), use.names=FALSE)
    for(agroup in getgroups){
    subby <- df[which(df[group]==agroup),]
    m <- lm(subby[[y]] ~ as.numeric(subby[[x]]))
    r2 = format(summary(m)$r.squared, digits = 3)
    dfhere <-  data.frame(group=agroup, r2=r2, slope=coef(m)[2])
    dfnew <- rbind(dfnew, dfhere)
}
return(dfnew)
}

bpsummr2 <- getrsq(bpsumm, "doy", "temp", "latlon.round")
names(bpsummr2)[names(bpsummr2)=="group"] <- "latlon.round"

# Jonathan's suggested visualization: https://statmodeling.stat.columbia.edu/2014/04/10/small-multiples-lineplots-maps-ok-always-yes-case/
ggplot(bpsumm, aes(x=as.numeric(doy), y=temp)) +
    geom_line(color="dodgerblue") +
    geom_smooth(method = "lm", linetype = 2, lwd=0.5, color="darkgray", se = FALSE) +
    facet_wrap(.~as.factor(latlon.round)) +
    geom_text(color="dodgerblue", size=3, data=bpsummr2, aes(x = 57, y = 12, label = r2)) +
    xlab("Day of year") +
    ylab(expression(paste("Daily temperature (", degree, "C)"), sep="")) +
    theme_minimal()


# add photoperiod for Ailene's paper
library(geosphere)

bpsumm2 <- bpsumm
bpsites <- subset(bp, select=c("latlon.round", "lat.round"))
bpsites <- bpsites[!duplicated(bpsites),]
bpsumm.wp <- merge(bpsumm2, bpsites, by="latlon.round", all.x=TRUE)
bpsumm.wp$daylength <- NA

for (i in c(1:nrow(bpsumm.wp))){
    photoper <- daylength(bpsumm.wp$lat.round[i], as.numeric(bpsumm.wp$doy[i]))
    bpsumm.wp$daylength[i] <- photoper
    }

ggplot(bpsumm.wp, aes(x=as.numeric(doy))) +
    geom_line(aes(y=temp), color="dodgerblue") +
    geom_line(aes(y=daylength), color="darkorchid") +
    facet_wrap(.~as.factor(latlon.round)) +
    xlab("Day of year") +
    ylab("Photoperiod") +
    theme_minimal()

onesite <- subset(bpsumm.wp, lat.round==52.4)
plot(daylength~as.numeric(doy), data=onesite, col="red", ylim=c(6,18), pch=16)
points(temp+8~as.numeric(doy), data=onesite, pch=16)


