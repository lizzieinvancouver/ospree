### Prepare all data for climate data...
### 7 June 2019 - Cat
## based off of betpen_chillandgdd_tg.R but using Tmin and Tmax now to find Tmean

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(chillR)
library(egg)
library(raster)
library(RColorBrewer)

setwd("~/Documents/git/ospree/analyses/bb_analysis/PEP_climate")

full.site <- read.csv("output/betpen_allchillsandgdds_45sites_tntx_forsims.csv", header = TRUE)

r<-brick("~/Desktop/tg_0.25deg_reg_v19.0.nc", varname="tg", sep="")
rn<-brick("~/Desktop/tn_0.25deg_reg_v16.0.nc", varname="tn", sep="")
rx<-brick("~/Desktop/tx_0.25deg_reg_v16.0.nc", varname="tx", sep="")

period <- c(1951:1960, 2001:2010)
sites<-subset(full.site, select=c(lat, long, lat.long))
sites<-sites[!duplicated(sites$lat.long),]
sites$x<-sites$long
sites$y<-sites$lat
Coords<-subset(sites, select=c(x, y))
nsites<-length(sites$lat.long)
tmin <- rn
tmax <- rx
tmean <- r

points.min <- SpatialPoints(Coords, proj4string = rn@crs)
points.max <- SpatialPoints(Coords, proj4string = rx@crs)
points.mean <- SpatialPoints(Coords, proj4string = r@crs)

yearsinclim<-as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
yearsinperiod<-which(yearsinclim%in%period)
climsubmin<-subset(tmin,yearsinperiod)
climsubmax<-subset(tmax,yearsinperiod)
climsubmean<-subset(tmean,yearsinperiod)

## subset climate days
monthsinclim<-as.numeric(format(as.Date(names(climsubmin),format="X%Y.%m.%d"),"%m"))
mstmonths<-c(4:5)
monthsinmst<-which(monthsinclim%in%mstmonths)
mstsubmin<-subset(climsubmin,monthsinmst)
mstsubmax<-subset(climsubmax,monthsinmst)
mstsubmean<-subset(climsubmean,monthsinmst)

valuesmin <- raster::extract(mstsubmin,points.min)
valuesmax <- raster::extract(mstsubmax,points.max)
valuesmean <- raster::extract(mstsubmean,points.mean)

dclimmin <- cbind.data.frame(coordinates(points.min),valuesmin)
dclimmax <- cbind.data.frame(coordinates(points.max),valuesmax)
dclimmean <- cbind.data.frame(coordinates(points.mean),valuesmean)

library(reshape2)
dxmin<-melt(dclimmin, id.vars=c("x","y"))
dxmax<-melt(dclimmax, id.vars=c("x","y"))
dxmean<-melt(dclimmean, id.vars=c("x","y"))

dxmin<-dxmin%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tmin=value)

dxmax<-dxmax%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tmax=value)

dxmean<-dxmean%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tmean=value)

dx <- data.frame(lat=dxmin$lat, long=dxmean$long, date=dxmean$date, tmin=dxmin$Tmin, tmax=dxmax$Tmax,
                 dxmean$Tmean)

foo <- full_join(dxmin, dxmax)
goo <- full_join(foo, dxmean)
goo$Tavg <- (dx$tmin+dx$tmax)/2

goo$date<-substr(goo$date, 2,11)
goo$Date<- gsub("[.]", "-", goo$date)
goo$year <- substr(goo$Date, 0, 4)
goo$cc <- ifelse(goo$year>1980, "post", "pre")
goo$date <- NULL

#### Very different for January especially but they become closer together further along in the season.
## But below, shows that the means and ranges even out!

goober <- subset(goo, select=c("Date", "Tmean", "Tavg", "lat", "long", "cc"))
names(goober) <- c("date", "tg", "tntx", "lat", "long", "cc")
goober <- gather(goober, method, temp, -date, -lat, -long, -cc)
goober <- goober[!duplicated(goober),]

cols <- colorRampPalette(brewer.pal(3,"Dark2"))(3)
climdiff<- ggplot(goober, aes(x=method, y=temp, alpha=cc)) + geom_boxplot(aes(alpha=as.factor(cc), fill=as.factor(method), col=as.factor(method))) +
  scale_fill_manual(name="Method", values=cols,
                    labels=c("tmean", "tmin plus tmax")) + 
  scale_color_manual(name="Method", values=cols,
                     labels=c("tmean", "tmin plus tmax")) + 
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0,
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines")) + # top, right, bottom, left
  #scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("post" = "Post CC", "pre" = "Pre CC")) +
  ylab("Temperature") + #coord_cartesian(ylim=c(50,165)) + 
  scale_alpha_manual(name="Climate Change", values=c(0.2, 0.7),
                     labels=c("pre"="1951-1960", "post"="2001-2010")) +
  guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)))), col=FALSE, fill=FALSE)

quartz()
climdiff

mean(goo$Tmean) # 4.796564 - tg months 1:5
                # 10.4391 - tg months 4:5
range(goo$Tmean) # -26.47 to 25.85 - tg months 1:5
                # -6.36 to 25.85 - tg months 4:5

mean(goo$Tavg) # 4.858375 - tntx months 1:5
               # 10.47201 - tntx months 4:5

range(goo$Tavg) # -25.62 to 25.50 - tntx months 1:5
                # -5.265 to 25.5 - tntx months 4:5 


