### Prepare all data for climate data...
### 20 November 2018

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(raster)
library(reshape2)
library(data.table)
library(chillR)
library(egg)

setwd("~/Documents/git/ospree/analyses/bb_analysis/PEP_climate")
#d<-read.csv("/n/wolkovich_lab/Lab/Cat/PEP725_DE_Betpen.csv", header=TRUE)
gersites<-read.csv("input/PEP725_DE_stations_fagsyl.csv", header=TRUE)
d<-read.csv("input/bbch_fagsyl.csv", header=TRUE)

peps<-as.vector(gersites$PEP_ID)
d<-subset(d, d$PEP_ID %in% peps)

df<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950 & YEAR<1980)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df<-dplyr::select(df, year, PEP_ID, lat, long, lo)

df<-df[!duplicated(df),]

sites<-sample(unique(df$PEP_ID), 200)
df<-df[(df$PEP_ID %in% sites),]
x<-paste(df$year, df$lo)
df$date<-as.Date(strptime(x, format="%Y %j"))
df$Date<- as.character(df$date)

df.post<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1980& YEAR<2017)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df.post<-dplyr::select(df.post, year, PEP_ID, lat, long, lo)

df.post<-df.post[!duplicated(df.post),]

#sites<-read.csv("output/betpen_sites.csv", header=TRUE)
df.post<-df.post[(df.post$PEP_ID %in% sites),]
x<-paste(df.post$year, df.post$lo)
df.post$date<-as.Date(strptime(x, format="%Y %j"))
df.post$Date<- as.character(df.post$date)

df$lat.long<-paste(df$lat, df$long)
peps.pre<-df[!duplicated(df$lat.long),]
df.post$lat.long<-paste(df.post$lat, df.post$long)
peps.post<-df.post[!duplicated(df.post$lat.long),] ### subset down to overlapping sites


#r<-brick("/n/wolkovich_lab/Lab/Cat/Big Data Items/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")
r<-brick("~/Desktop/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")

#bb<-df
#bb$lat.long<-paste(bb$lat, bb$long, sep=",")
#bb<-bb[!duplicated(bb$lat.long),]
lats <- peps.post$lat ## fewer sites so using peps.post
lons <- peps.post$long

coords <- data.frame(x=lons,y=lats)

coords<- na.omit(coords)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- raster::extract(r,points)

dclim <- cbind.data.frame(coordinates(points),values)

dx<-melt(dclim, id.vars=c("x","y"))

dx<-dx%>%
  rename(long=x)%>%
  rename(lat=y)%>%
  rename(date=variable)%>%
  rename(Tavg=value)

dx$date<-substr(dx$date, 2,11)
dx$Date<- gsub("[.]", "-", dx$date)

df$date<-NULL
df.post$date<-NULL
dx$date<-NULL

dx$month<-substr(dx$Date, 6, 7)
dx$month<-as.numeric(dx$month)

dx$winter<-ifelse(dx$month>=9 | dx$month<=3, "winter", 0)
winter<-dx[(dx$winter=="winter"),]
winter<-winter[!is.na(winter$Tavg),]

dx$spring<-ifelse(dx$month>=4 & dx$month<=6, "spring", 0)
ddx<-dx[(dx$spring=="spring"),]
ddx<-ddx[!is.na(ddx$Tavg),]

ddx$year<-as.numeric(substr(ddx$Date, 0, 4))
ddx$lat.long<-paste(ddx$lat, ddx$long)
ddx$mat<-ave(ddx$Tavg, ddx$year, ddx$lat.long)

mst<-ddx%>%dplyr::select(-Tavg, -Date, -spring, -month)
mst<-mst[!duplicated(mst),]

#mst$num.years<-ave(mst$mat, mst$lat.long, FUN=length)

preCC <- mst[(mst$year>=1950 & mst$year<=1960), ]
postCC <- mst[(mst$year>=2000 & mst$year<=2010), ]

#df$lat.long<-paste(df$lat, df$long)
bb<-df
bb$PEP_ID<-NULL
bb$lat<-NULL
bb$long<-NULL
bb$Date<-NULL

preCC<-full_join(preCC, bb)
preCC<-preCC[!is.na(preCC$lo),]
preCC<-preCC[!is.na(preCC$mat),]
preCC<-preCC[(preCC$year>=1950 & preCC$year<=1960),]
preCC$num.years<-ave(preCC$mat, preCC$lat.long, FUN=length)


#df.post$lat.long<-paste(df.post$lat, df.post$long)
bb.post<-df.post
bb.post$PEP_ID<-NULL
bb.post$lat<-NULL
bb.post$long<-NULL
bb.post$Date<-NULL


postCC<-full_join(postCC, bb.post)
postCC<-postCC[!is.na(postCC$lo),]
postCC<-postCC[!is.na(postCC$mat),]
postCC<-postCC[(postCC$year>=2000 & postCC$year<=2010),]
postCC$num.years<-ave(postCC$mat, postCC$lat.long, FUN=length)


##### Let's make some plots! #####
mat<-full_join(preCC, postCC)
mat$cc<-ifelse(mat$year>=1950 & mat$year<=1960, "apre", "post")
foo<-mat[(mat$num.years>=5),]
tt<-as.data.frame(table(foo$cc, foo$lat.long))
tt<-tt[!(tt$Freq==0),]
bestsites<-as.data.frame(table(tt$Var2))
bestsites<-bestsites[(bestsites$Freq>1),]

mat<-foo[(foo$lat.long %in% bestsites$Var1),]

#write.csv(mat, file="output/fagsyl_mat.csv", row.names=FALSE)
check<-read.csv("output/fagsyl_mat.csv", header=TRUE)

#osp<-read.csv("..//..//output/ospree_clean_withchill_BB.csv", header=TRUE)
#osp.bp<-subset(osp, osp$genus=="Betula" & osp$species=="pendula")
#osp.bp<-subset(osp.bp, select=c(year, forcetemp,response.time, respvar.simple))
#osp.bp$forcetemp<-as.numeric(osp.bp$forcetemp)
#osp.bp<-na.omit(osp.bp)
#osp.bp<-osp.bp[(osp.bp$respvar.simple=="daystobudburst"),]

#osp.bp$mat<-osp.bp$forcetemp
#osp.bp$lo<-osp.bp$response.time

#mat<-full_join(mat, osp.bp)
#mat$cc<-ifelse(is.na(mat$cc), "ospree", mat$cc)


xlab <- "Mean Spring Temperature (°C)"

quartz()
mat.plot<-ggplot(mat, aes(x=mat, y=lo, col=cc)) + geom_line(aes(col=cc), stat="smooth", method="lm") + 
  theme_classic() + labs(x=xlab, y="Day of Leafout") + theme(legend.position="none") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) + geom_point(aes(col=cc), alpha=0.3)

ggplot(mat, aes(y=mat)) + geom_boxplot(aes(y=mat, x=cc, col=cc)) + 
  theme_classic() + labs(y=xlab, x="") + theme(legend.position = "none") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) +
  scale_x_discrete(labels=c("apre" = "1950 - 1960",
                            "post" = "2000 - 2010"))


##### Now to calculate chilling using Chill portions based on Ailene's code `chillcode_snippet.R' #####
mat <- read.csv("output/fagsyl_mat.csv", header=TRUE)
period<-1950:1960
#period<-2000:2010
sites<-subset(mat, select=c(lat, long, lat.long))
sites<-sites[!duplicated(sites$lat.long),]
sites$x<-sites$long
sites$y<-sites$lat
Coords<-subset(sites, select=c(x, y))
nsites<-length(sites$lat.long)
sites$siteslist<-1:10
tavg<-r

leaps<-c(1952, 1956, 1960, 2000, 2004, 2008)

## set function
#extractchillpre<-function(tavg,period){
extractchillpost<-function(tavg,period){
  
  ## define array to store results
  nyears<-length(period)
  chillingyears<-array(NA,dim=c(nyears, 3, nsites))
  row.names(chillingyears)<-period
  colnames(chillingyears)<-c("Mean.Chill", "SD.Chill", "Site Num.")
  #dimnames(chillforcespsyears)<-spslist
  
  ## subset climate years
  yearsinclim<-as.numeric(format(as.Date(names(tavg),format="X%Y.%m.%d"),"%Y"))
  yearsinperiod<-which(yearsinclim%in%period)
  climsub<-subset(tavg,yearsinperiod)
  
  ## subset climate days
  monthsinclim<-as.numeric(format(as.Date(names(climsub),format="X%Y.%m.%d"),"%m"))
  chillmonths<-c(9:12,1:3)
  monthsinchill<-which(monthsinclim%in%chillmonths)
  chillsub<-subset(climsub,monthsinchill)
  
  ## commence loop  
  for (i in 1:nsites){#i=2
    print(i)
    sitesi<-sites$siteslist[i]
    
    
    #values <- raster::extract(r,points)
    
    
    ## load shape
    if(sitesi==sites$siteslist[i])
      Coords<-data.frame(sites$x, sites$y)
    points <- SpatialPoints(Coords, proj4string = tavg@crs)
    #spsshapeproj<-spTransform(points,proj4string(chillsub[[1]]))
    
    
    ## loop across years to extract each years averages and stddevs
    # save first an array to store results
    yearlyresults<-array(NA,dim=c(length(period),3))
    
    for(j in period){#j=1980
      print(paste(i,j))
      
      # select year's layer
      chillyears<-which(as.numeric(format(as.Date(
        names(chillsub),format="X%Y.%m.%d"),"%Y"))==j)
      
      yearschill<-subset(chillsub,chillyears)
      
      # extract values and format to compute means and sdevs
      tempschills<-raster::extract(yearschill,points)
      
      #turn into data frame and remove NAs
      ch<-as.data.frame(tempschills)
      ch<-subset(ch,!is.na(rowSums(ch)))
      
      
      ## calculate chilling (Utah)
      chillunitseachcelleachday<-apply(ch,2,function(x){
        #tlow=-5 ## not sure about which parameteres we are using
        #thigh=5
        Tmean<-x
        #Tmean<-ifelse(minns>0,minns,0)
        return(Tmean)})
      meandaily<-chillunitseachcelleachday[(as.numeric(rownames(chillunitseachcelleachday))==i)]
      #hist(utahssum)
      
      
      if(j %in% leaps){
        meandaily<-head(meandaily, -1)
      } 
      
      if(j %in% leaps){
        chillunitseachcelleachday<-chillunitseachcelleachday[,-213]
      }
      
      
      hrly.temp =
        data.frame(
          Temp = c(rep(meandaily, times = 24)),
          Year = as.numeric(substr(colnames(chillunitseachcelleachday), 2, 5)),
          JDay = sort(c(rep(seq(1:length(colnames(meandaily))), times = 24)))
        )
      #hrly.temp<-hrly.temp[!(hrly.temp$Year==24),]
      
      chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[i], hrly.temp$JDay[nrow(hrly.temp[i])])
      #chillcalc.mn<-chillcalc.mn[!(chillcalc.mn$End_year==24),]
      
      #yearlyresults[which(period==j),1]<-mean(utahssum,na.rm=T)
      #yearlyresults[which(period==j),2]<-sd(utahssum,na.rm=T)
      
      yearlyresults[which(period==j),1]<-chillcalc.mn$Chill_portions[which(chillcalc.mn$End_year==j)]
      #yearlyresults[which(period==j),2]<-sd(chillcalc.mn,na.rm=T)
      
      yearlyresults[which(period==j),3]<-sites$siteslist[i]
      
    }
    
    chillingyears[,,i]<-yearlyresults
    
  } 
  
  return(chillingyears)
  
}


## apply function
chill_pre<-extractchillpre(tavg,period)
chill_post<-extractchillpost(tavg,period) ## rerun from top but change period to 2000:2010 and create function extractchillpost

pre<-as.data.frame(chill_pre)
post<-as.data.frame(chill_post)

predata<-data.frame(chillutah = c(pre$Mean.Chill.1, pre$Mean.Chill.2,
                                  pre$Mean.Chill.3, pre$Mean.Chill.4,
                                  pre$Mean.Chill.5, pre$Mean.Chill.6,
                                  pre$Mean.Chill.7, pre$Mean.Chill.8,
                                  pre$Mean.Chill.9, pre$Mean.Chill.10,
                                  pre$Mean.Chill.11, pre$Mean.Chill.12),
                                  #pre$Mean.Chill.13, pre$Mean.Chill.14,
                                  #pre$Mean.Chill.15),
                    siteslist = c(pre$`Site Num..1`, pre$`Site Num..2`,
                                  pre$`Site Num..3`, pre$`Site Num..4`,
                                  pre$`Site Num..5`, pre$`Site Num..6`,
                                  pre$`Site Num..7`, pre$`Site Num..8`,
                                  pre$`Site Num..9`, pre$`Site Num..10`,
                                  pre$`Site Num..11`, pre$`Site Num..12`),
                                  #pre$`Site Num..13`, pre$`Site Num..14`,
                                  #pre$`Site Num..15`),
                    year = rownames(pre))

port<-full_join(predata, sites)
port$x<-NULL
port$y<-NULL


postdata<-data.frame(chillutah = c(post$Mean.Chill.1, post$Mean.Chill.2,
                                   post$Mean.Chill.3, post$Mean.Chill.4,
                                   post$Mean.Chill.5, post$Mean.Chill.6,
                                   post$Mean.Chill.7, post$Mean.Chill.8,
                                   post$Mean.Chill.9, post$Mean.Chill.10,
                                   post$Mean.Chill.11, post$Mean.Chill.12),
                                   #post$Mean.Chill.13, post$Mean.Chill.14,
                                   #post$Mean.Chill.15),
                     siteslist = c(post$`Site Num..1`, post$`Site Num..2`,
                                   post$`Site Num..3`, post$`Site Num..4`,
                                   post$`Site Num..5`, post$`Site Num..6`,
                                   post$`Site Num..7`, post$`Site Num..8`,
                                   post$`Site Num..9`, post$`Site Num..10`,
                                   post$`Site Num..11`, post$`Site Num..12`),
                                   #post$`Site Num..13`, post$`Site Num..14`,
                                   #post$`Site Num..15`),
                     year = rownames(post))

port.post<-full_join(postdata, sites)
port.post$x<-NULL
port.post$y<-NULL

allchills<-full_join(port, port.post)
allchills$year<-as.numeric(allchills$year)
allchills<-full_join(allchills, mat)
allchills$mat<-NULL
allchills$num.years<-NULL
allchills<-na.omit(allchills)

xlab <- "Total Utah Chill"

quartz()
chill.plot<-ggplot(allchills, aes(x=chillutah, y=lo, col=cc)) + geom_line(aes(col=cc), stat="smooth", method="lm") + 
  theme_classic() + labs(x=xlab, y="Day of Leafout") + theme(legend.position="none") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) + geom_point(aes(col=cc), alpha=0.3)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(chill.plot)
quartz()
grid.arrange(mat.plot, chill.plot.port, chill.plot, mylegend, ncol=4, widths=c(2, 2, 2, 0.5))

#write.csv(mat, file="output/fagsyl_mat.csv", row.names = FALSE)
#write.csv(sites, file="output/betpen_sites.csv", row.names = FALSE)

quartz()
bb50 <- ggplot(mat[(mat$year<=1960),], aes(x=year, y=lo, col=cc)) + geom_line(aes(col=cc), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Year", y="Day of Leafout") + theme(legend.position="none") + coord_cartesian(ylim=c(95, 140)) +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) + geom_point(aes(col=cc), alpha=0.3) +
  scale_x_continuous(breaks=seq(1950, 1960, 2), limits=c(1950, 1960))

bb00 <- ggplot(mat[(mat$year>=2000),], aes(x=year, y=lo, col=cc)) + geom_line(aes(col=cc), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Year", y="Day of Leafout") + theme(legend.position="none") + coord_cartesian(ylim=c(95, 140)) +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) + geom_point(aes(col=cc), alpha=0.3) +
  scale_x_continuous(breaks=seq(2000, 2010, 2), limits=c(2000, 2010))

grid.arrange(bb50, bb00, ncol=2, widths=c(2, 2))




