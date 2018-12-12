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

setwd("~/Documents/git/ospree/analyses/bb_analysis/PEP_climate")
#d<-read.csv("/n/wolkovich_lab/Lab/Cat/PEP725_DE_Betpen.csv", header=TRUE)
gersites<-read.csv("input/PEP725_DE_stations.csv", header=TRUE)
d<-read.csv("input/bbch_region_betpen.csv", header=TRUE)

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
r<-brick("~/Desktop/Big Data Items/tg_0.25deg_reg_v16.0.nc", varname="tg", sep="")

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

dx$winter<-ifelse(dx$month>=9 | dx$month<=4, "winter", 0)
winter<-dx[(dx$winter=="winter"),]
winter<-winter[!is.na(winter$Tavg),]

dx$spring<-ifelse(dx$month>=2 & dx$month<=4, "spring", 0)
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
ggplot(mat, aes(x=mat, y=lo, col=cc)) + geom_line(aes(col=cc), stat="smooth", method="lm") + 
  theme_classic() + labs(x=xlab, y="Day of Leafout") +
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


# define period
#period<-1950:1960
period<-2000:2010
sites<-subset(mat, select=c(lat, long, lat.long))
sites<-sites[!duplicated(sites$lat.long),]
sites$x<-sites$long
sites$y<-sites$lat
Coords<-subset(sites, select=c(x, y))
nsites<-length(sites$lat.long)
sites$siteslist<-1:12
tavg<-r

## set function
extractchillpre<-function(tavg,period){
#extractchillpost<-function(tavg,period){
  
  ## define array to store results
  nyears<-length(period)
  chillingyears<-array(NA,dim=c(nyears, 3, nsites))
  row.names(chillingyears)<-period
  colnames(chillingyears)<-c("Mean.Chill","SDev.Chill", "Site Num.")
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
          tlow=-5 ## not sure about which parameteres we are using
          thigh=5
          minns<-ifelse((thigh-x)>(thigh-tlow),thigh-tlow,thigh-x)
          utah<-ifelse(minns>0,minns,0)
          return(utah)})
        utahssum<-rowSums(chillunitseachcelleachday)
        #hist(utahssum)
    
    yearlyresults[which(period==j),1]<-mean(utahssum,na.rm=T)
    yearlyresults[which(period==j),2]<-sd(utahssum,na.rm=T)
    yearlyresults[which(period==j),3]<-sites$siteslist[i]
    
  }
  
  chillingyears[,,i]<-yearlyresults
 
  } 
  
  return(chillingyears)
  
}


## apply function
chill_pre<-extractchillpre(tavg,period)
chill_post<-extractchillpost(tavg,period)

colnames(chill_pre) <- c("utahchill","sd")
chill_pre<-as.data.frame(chill_pre)
chill_pre$cc<-"apre"
colnames(chill_post) <- c("utahchill","sd")
chill_post<-as.data.frame(chill_post)
chill_post$cc<-"post"

utahs<-full_join(chill_pre, chill_post)

xlab <- "Total Utah Chilling"

quartz()
ggplot(utahs, aes(x=utahchill, y=lo, col=cc)) + geom_line(aes(col=cc), stat="smooth", method="lm") + 
  theme_classic() + labs(x=xlab, y="Day of Leafout") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) + geom_point(aes(col=cc), alpha=0.3)


#write.csv(mat, file="output/betpen_mat.csv", row.names = FALSE)
#write.csv(sites, file="output/betpen_sites.csv", row.names = FALSE)


