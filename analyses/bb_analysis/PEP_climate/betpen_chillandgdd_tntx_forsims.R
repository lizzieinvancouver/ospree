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
d<-read.csv("input/pep_betpen_all.csv", header=TRUE)

df<-d%>%
  filter(BBCH==11)%>%
  filter(YEAR>=1950 & YEAR<=2010)%>%
  dplyr::select(YEAR, DAY, BBCH, PEP_ID, LAT, LON, species)%>%
  rename(year=YEAR)%>%
  rename(lo=DAY)%>%
  rename(lat=LAT)%>%
  rename(long=LON)
## Hmm... can we sequence from budburst to leafout to find the number of freezes between?
df<-dplyr::select(df, year, PEP_ID, lat, long, lo)

df<-df[!duplicated(df),]

x<-paste(df$year, df$lo)
df$date<-as.Date(strptime(x, format="%Y %j"))
df$Date<- as.character(df$date)
df$lat.long <- paste(df$lat, df$long)
allpeps <- df[(df$year>=1951 & df$year<=1960) | (df$year>=2001 & df$year<=2010),]

allpeps$cc<-ifelse(allpeps$year>=1950 & allpeps$year<=1960, "apre", "post")
allpeps$num.years<-ave(allpeps$year, allpeps$lat.long, FUN=length)
mostdata<-allpeps[(allpeps$num.years>=20),]
tt<-as.data.frame(table(mostdata$cc, mostdata$lat.long))
tt<-tt[!(tt$Freq==0),]
bestsites<-as.data.frame(table(tt$Var2))
bestsites<-bestsites[(bestsites$Freq>1),]
bestsites <- bestsites$Var1

allpeps.subset<-mostdata[(mostdata$lat.long %in% bestsites),]

rn<-brick("~/Desktop/tn_0.25deg_reg_v16.0.nc", sep="")
rx<-brick("~/Desktop/tx_0.25deg_reg_v16.0.nc", sep="")

##### Now to calculate chilling using Chill portions based on Ailene's code `chillcode_snippet.R' #####
## Adjust the period you are using below to match the function you want to use (i.e. extractchillpre or extractchillpost)
#period<-1951:1960
period<-2001:2010
sites<-subset(allpeps.subset, select=c(lat, long, lat.long))
sites<-sites[!duplicated(sites$lat.long),]
badsites<-c("54.5 11.1", "49.7667 11.55", "47.8 11.0167") 
sites<-sites[!(sites$lat.long%in%badsites),]
sites$x<-sites$long
sites$y<-sites$lat
nsites<-length(sites$lat.long)
sites$siteslist<-1:45
tmin<-rn
tmax<-rx

lositeyear <- subset(allpeps.subset, select=c("lo", "lat", "long", "lat.long", "year"))
lositeyear <- lositeyear[!duplicated(lositeyear),]
lositeyear <- left_join(lositeyear, sites)
lositeyear<-na.omit(lositeyear)

leaps<-c(1952, 1956, 1960, 2000, 2004, 2008)

## set function - depending on the period you are using
#extractclimpre<-function(tmin,period){
extractclimpost<-function(tmin,period){
  
  ## define array to store results
  nyears<-length(period)
  climateyears<-array(NA,dim=c(nyears, 5, nsites))
  row.names(climateyears)<-period
  colnames(climateyears)<-c("Mean.Utah", "Mean.Port", "Mean.GDD", "Spring.Temp", "Site Num.")
  
  ## subset climate years
  yearsinclimmin<-as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
  yearsinclimmax<-as.numeric(format(as.Date(names(tmax),format="X%Y.%m.%d"),"%Y"))
  yearsinperiodmin<-which(yearsinclimmin%in%period)
  yearsinperiodmax<-which(yearsinclimmax%in%period)
  climsubmin<-subset(tmin,yearsinperiodmin)
  climsubmax<-subset(tmax,yearsinperiodmax)
  
  ## subset climate days
  monthsinclimmin<-as.numeric(format(as.Date(names(climsubmin),format="X%Y.%m.%d"),"%m"))
  monthsinclimmax<-as.numeric(format(as.Date(names(climsubmax),format="X%Y.%m.%d"),"%m"))
  chillmonths<-c(9:12,1:3)
  monthsinchillmin<-which(monthsinclimmin%in%chillmonths)
  monthsinchillmax<-which(monthsinclimmax%in%chillmonths)
  chillsubmin<-subset(climsubmin,monthsinchillmin)
  chillsubmax<-subset(climsubmax,monthsinchillmax)
  
  warmmonths<-c(1:5)
  monthsinwarmmin<-which(monthsinclimmin%in%warmmonths)
  monthsinwarmmax<-which(monthsinclimmax%in%warmmonths)
  warmsubmin<-subset(climsubmin,monthsinwarmmin)
  warmsubmax<-subset(climsubmax,monthsinwarmmax)
  
  ## commence loop  
  for (i in 1:nsites){#i=2
    print(i)
    sitesi<-sites$siteslist[i]
    
    ## load shape
    if(sitesi==sites$siteslist[i])
      Coords<-data.frame(sites$x, sites$y)
    points.min <- SpatialPoints(Coords, proj4string = rn@crs)
    points.max <- SpatialPoints(Coords, proj4string = rx@crs)

    ## loop across years to extract each years averages
    # save first an array to store results
    yearlyresults<-array(NA,dim=c(length(period),5))
    
    for(j in period){#j=2001
      print(paste(i,j))
      
      # select year's layer
      chillyearsmin<-which(as.numeric(format(as.Date(
        names(chillsubmin),format="X%Y.%m.%d"),"%Y"))==j)
      chillyearsmax<-which(as.numeric(format(as.Date(
        names(chillsubmax),format="X%Y.%m.%d"),"%Y"))==j)
      
      yearschillmin<-subset(chillsubmin,chillyearsmin)
      yearschillmax<-subset(chillsubmax,chillyearsmax)
      
      # extract values and format to compute means
      tempschillsmin<-raster::extract(yearschillmin,points.min)
      tempschillsmax<-raster::extract(yearschillmax,points.max)
      
      #turn into data frame and remove NAs
      chmin<-as.data.frame(tempschillsmin)
      chmin<-subset(chmin,!is.na(rowSums(chmin)))
      chmax<-as.data.frame(tempschillsmax)
      chmax<-subset(chmax,!is.na(rowSums(chmax)))
 
      ## calculate chilling
      chillunitseachcelleachdaymin<-apply(chmin,2,function(x){
        Tmin<-x
        return(Tmin)})
      tminchill<-chillunitseachcelleachdaymin[(as.numeric(rownames(chillunitseachcelleachdaymin))==i)]
      
      chillunitseachcelleachdaymax<-apply(chmax,2,function(x){
        Tmax<-x
        return(Tmax)})
      tmaxchill<-chillunitseachcelleachdaymax[(as.numeric(rownames(chillunitseachcelleachdaymax))==i)]
      
      meandaily <- (tminchill + tmaxchill)/2
      
      x <- as.Date(substr(colnames(chillunitseachcelleachdaymin), 2, 11),format="%Y.%m.%d")
      
      hrly.temp=
        data.frame(
          Temp = c(rep(meandaily, each = 24)),
          Year = c(rep(as.numeric(substr(colnames(chillunitseachcelleachdaymin), 2, 5)), times=24)),
          #JDay = sort(c(rep(seq(1:length(colnames(meandaily))), times = 24)))
          JDay = sort(c(rep(yday(x), times=24)))
        )
      
      
      # select year's layer
      warmyearsmin<-which(as.numeric(format(as.Date(
        names(warmsubmin),format="X%Y.%m.%d"),"%Y"))==j)
      warmyearsmax<-which(as.numeric(format(as.Date(
        names(warmsubmax),format="X%Y.%m.%d"),"%Y"))==j)
      
      yearswarmmin<-subset(warmsubmin,warmyearsmin)
      yearswarmmax<-subset(warmsubmax,warmyearsmax)
      
      # extract values and format to compute means and sdevs
      tempswarmsmin<-raster::extract(yearswarmmin,points.min)
      tempswarmsmax<-raster::extract(yearswarmmax,points.max)
      
      #turn into data frame and remove NAs
      wamin<-as.data.frame(tempswarmsmin)
      wamin<-subset(wamin,!is.na(rowSums(wamin)))
      wamax<-as.data.frame(tempswarmsmax)
      wamax<-subset(wamax,!is.na(rowSums(wamax)))
      
      
      ## calculate forcing (GDD)
      warmunitseachcelleachdaymin<-apply(wamin,2,function(x){
        Tmin.warm<-x
        return(Tmin.warm)})
      tminwarm<-warmunitseachcelleachdaymin[(as.numeric(rownames(warmunitseachcelleachdaymin))==i)]
      
      warmunitseachcelleachdaymax<-apply(wamax,2,function(x){
        Tmax.warm<-x
        return(Tmax.warm)})
      tmaxwarm<-warmunitseachcelleachdaymax[(as.numeric(rownames(warmunitseachcelleachdaymax))==i)]
      
      meandaily.warm <- (tminwarm + tmaxwarm)/2
 
      
      x.warm <- as.Date(substr(colnames(warmunitseachcelleachdaymin), 2, 11),format="%Y.%m.%d")
      
      hrly.temp.warm =
        data.frame(
          Temp = c(rep(meandaily.warm, each = 24)),
          Year = c(rep(as.numeric(substr(colnames(warmunitseachcelleachdaymin), 2, 5)), times=24)),
          JDay = sort(c(rep(yday(x.warm), times = 24)))
        )
      
      lopersite <- lositeyear[(lositeyear$siteslist==i & lositeyear$year==j),]
      lo <- as.numeric(lopersite$lo)
      hrly.temp.warm <- hrly.temp.warm[(hrly.temp.warm$JDay<=lo & hrly.temp.warm$JDay>=lo-30),]
      
      chillcalc.mn<-chilling(hrly.temp, hrly.temp$JDay[1], hrly.temp$JDay[nrow(hrly.temp[1])])
      warmcalc.mn<-chilling(hrly.temp.warm, hrly.temp.warm$JDay[1], hrly.temp.warm$JDay[nrow(hrly.temp.warm[1])])
      
      
      yearlyresults[which(period==j),1]<-chillcalc.mn$Utah_Model[which(chillcalc.mn$End_year==j)]
      yearlyresults[which(period==j),2]<-chillcalc.mn$Chill_portions[which(chillcalc.mn$End_year==j)]
      yearlyresults[which(period==j),3]<-(warmcalc.mn$GDH[which(warmcalc.mn$End_year==j)])/24
      yearlyresults[which(period==j),4]<-mean(hrly.temp.warm$Temp, na.rm=TRUE)
      
      yearlyresults[which(period==j),5]<-sites$siteslist[i]
      
    }
    
    climateyears[,,i]<-yearlyresults
    
  } 
  
  return(climateyears)
  
}


## apply function
clim_pre<-extractclimpre(tmin,period)
clim_post<-extractclimpost(tmin,period) ## rerun from top but change period to 2000:2010 and create function extractchillpost

pre<-as.data.frame(clim_pre)
post<-as.data.frame(clim_post)

predata<-data.frame(chillutah = c(pre$Mean.Utah.1, pre$Mean.Utah.2,
                                  pre$Mean.Utah.3, pre$Mean.Utah.4,
                                  pre$Mean.Utah.5, pre$Mean.Utah.6,
                                  pre$Mean.Utah.7, pre$Mean.Utah.8,
                                  pre$Mean.Utah.9, pre$Mean.Utah.10,
                                  pre$Mean.Utah.11, pre$Mean.Utah.12,
                                  pre$Mean.Utah.13, pre$Mean.Utah.14,
                                  pre$Mean.Utah.15, pre$Mean.Utah.16,
                                  pre$Mean.Utah.17, pre$Mean.Utah.18,
                                  pre$Mean.Utah.19, pre$Mean.Utah.20,
                                  pre$Mean.Utah.21, pre$Mean.Utah.22,
                                  pre$Mean.Utah.23, pre$Mean.Utah.24,
                                  pre$Mean.Utah.25, pre$Mean.Utah.26,
                                  pre$Mean.Utah.27, pre$Mean.Utah.28,
                                  pre$Mean.Utah.29, pre$Mean.Utah.30,
                                  pre$Mean.Utah.31, pre$Mean.Utah.32,
                                  pre$Mean.Utah.33, pre$Mean.Utah.34,
                                  pre$Mean.Utah.35, pre$Mean.Utah.36,
                                  pre$Mean.Utah.37, pre$Mean.Utah.38,
                                  pre$Mean.Utah.39, pre$Mean.Utah.40,
                                  pre$Mean.Utah.41, pre$Mean.Utah.42,
                                  pre$Mean.Utah.43, pre$Mean.Utah.44,
                                  pre$Mean.Utah.45),
                    
                    chillports = c(pre$Mean.Port.1, pre$Mean.Port.2,
                                   pre$Mean.Port.3, pre$Mean.Port.4,
                                   pre$Mean.Port.5, pre$Mean.Port.6,
                                   pre$Mean.Port.7, pre$Mean.Port.8,
                                   pre$Mean.Port.9, pre$Mean.Port.10,
                                   pre$Mean.Port.11, pre$Mean.Port.12,
                                   pre$Mean.Port.13, pre$Mean.Port.14,
                                   pre$Mean.Port.15, pre$Mean.Port.16,
                                   pre$Mean.Port.17, pre$Mean.Port.18,
                                   pre$Mean.Port.19, pre$Mean.Port.20,
                                   pre$Mean.Port.21, pre$Mean.Port.22,
                                   pre$Mean.Port.23, pre$Mean.Port.24,
                                   pre$Mean.Port.25, pre$Mean.Port.26,
                                   pre$Mean.Port.27, pre$Mean.Port.28,
                                   pre$Mean.Port.29, pre$Mean.Port.30,
                                   pre$Mean.Port.31, pre$Mean.Port.32,
                                   pre$Mean.Port.33, pre$Mean.Port.34,
                                   pre$Mean.Port.35, pre$Mean.Port.36,
                                   pre$Mean.Port.37, pre$Mean.Port.38,
                                   pre$Mean.Port.39, pre$Mean.Port.40,
                                   pre$Mean.Port.41, pre$Mean.Port.42,
                                   pre$Mean.Port.43, pre$Mean.Port.44,
                                   pre$Mean.Port.45),
                    
                    
                    gdd = c(pre$Mean.GDD.1, pre$Mean.GDD.2,
                            pre$Mean.GDD.3, pre$Mean.GDD.4,
                            pre$Mean.GDD.5, pre$Mean.GDD.6,
                            pre$Mean.GDD.7, pre$Mean.GDD.8,
                            pre$Mean.GDD.9, pre$Mean.GDD.10,
                            pre$Mean.GDD.11, pre$Mean.GDD.12,
                            pre$Mean.GDD.13, pre$Mean.GDD.14,
                            pre$Mean.GDD.15, pre$Mean.GDD.16,
                            pre$Mean.GDD.17, pre$Mean.GDD.18,
                            pre$Mean.GDD.19, pre$Mean.GDD.20,
                            pre$Mean.GDD.21, pre$Mean.GDD.22,
                            pre$Mean.GDD.23, pre$Mean.GDD.24,
                            pre$Mean.GDD.25, pre$Mean.GDD.26,
                            pre$Mean.GDD.27, pre$Mean.GDD.28,
                            pre$Mean.GDD.29, pre$Mean.GDD.30,
                            pre$Mean.GDD.31, pre$Mean.GDD.32,
                            pre$Mean.GDD.33, pre$Mean.GDD.34,
                            pre$Mean.GDD.35, pre$Mean.GDD.36,
                            pre$Mean.GDD.37, pre$Mean.GDD.38,
                            pre$Mean.GDD.39, pre$Mean.GDD.40,
                            pre$Mean.GDD.41, pre$Mean.GDD.42,
                            pre$Mean.GDD.43, pre$Mean.GDD.44,
                            pre$Mean.GDD.45),
                    
                    mat.lo = c(pre$Spring.Temp.1, pre$Spring.Temp.2,
                            pre$Spring.Temp.3, pre$Spring.Temp.4,
                            pre$Spring.Temp.5, pre$Spring.Temp.6,
                            pre$Spring.Temp.7, pre$Spring.Temp.8,
                            pre$Spring.Temp.9, pre$Spring.Temp.10,
                            pre$Spring.Temp.11, pre$Spring.Temp.12,
                            pre$Spring.Temp.13, pre$Spring.Temp.14,
                            pre$Spring.Temp.15, pre$Spring.Temp.16,
                            pre$Spring.Temp.17, pre$Spring.Temp.18,
                            pre$Spring.Temp.19, pre$Spring.Temp.20,
                            pre$Spring.Temp.21, pre$Spring.Temp.22,
                            pre$Spring.Temp.23, pre$Spring.Temp.24,
                            pre$Spring.Temp.25, pre$Spring.Temp.26,
                            pre$Spring.Temp.27, pre$Spring.Temp.28,
                            pre$Spring.Temp.29, pre$Spring.Temp.30,
                            pre$Spring.Temp.31, pre$Spring.Temp.32,
                            pre$Spring.Temp.33, pre$Spring.Temp.34,
                            pre$Spring.Temp.35, pre$Spring.Temp.36,
                            pre$Spring.Temp.37, pre$Spring.Temp.38,
                            pre$Spring.Temp.39, pre$Spring.Temp.40,
                            pre$Spring.Temp.41, pre$Spring.Temp.42,
                            pre$Spring.Temp.43, pre$Spring.Temp.44,
                            pre$Spring.Temp.45),
                    
                    siteslist = c(pre$`Site Num..1`, pre$`Site Num..2`,
                                  pre$`Site Num..3`, pre$`Site Num..4`,
                                  pre$`Site Num..5`, pre$`Site Num..6`,
                                  pre$`Site Num..7`, pre$`Site Num..8`,
                                  pre$`Site Num..9`, pre$`Site Num..10`,
                                  pre$`Site Num..11`, pre$`Site Num..12`,
                                  pre$`Site Num..13`, pre$`Site Num..14`,
                                  pre$`Site Num..15`, pre$`Site Num..16`,
                                  pre$`Site Num..17`, pre$`Site Num..18`,
                                  pre$`Site Num..19`, pre$`Site Num..20`,
                                  pre$`Site Num..21`, pre$`Site Num..22`,
                                  pre$`Site Num..23`, pre$`Site Num..24`,
                                  pre$`Site Num..25`, pre$`Site Num..26`,
                                  pre$`Site Num..27`, pre$`Site Num..28`,
                                  pre$`Site Num..29`, pre$`Site Num..30`,
                                  pre$`Site Num..31`, pre$`Site Num..32`,
                                  pre$`Site Num..33`, pre$`Site Num..34`,
                                  pre$`Site Num..35`, pre$`Site Num..36`,
                                  pre$`Site Num..37`, pre$`Site Num..38`,
                                  pre$`Site Num..39`, pre$`Site Num..40`,
                                  pre$`Site Num..41`, pre$`Site Num..42`,
                                  pre$`Site Num..43`, pre$`Site Num..44`,
                                  pre$`Site Num..45`),
                    year = rownames(pre))

site<-full_join(predata, sites)
site$x<-NULL
site$y<-NULL  

postdata<-data.frame(chillutah = c(post$Mean.Utah.1, post$Mean.Utah.2,
                                   post$Mean.Utah.3, post$Mean.Utah.4,
                                   post$Mean.Utah.5, post$Mean.Utah.6,
                                   post$Mean.Utah.7, post$Mean.Utah.8,
                                   post$Mean.Utah.9, post$Mean.Utah.10,
                                   post$Mean.Utah.11, post$Mean.Utah.12,
                                   post$Mean.Utah.13, post$Mean.Utah.14,
                                   post$Mean.Utah.15, post$Mean.Utah.16,
                                   post$Mean.Utah.17, post$Mean.Utah.18,
                                   post$Mean.Utah.19, post$Mean.Utah.20,
                                   post$Mean.Utah.21, post$Mean.Utah.22,
                                   post$Mean.Utah.23, post$Mean.Utah.24,
                                   post$Mean.Utah.25, post$Mean.Utah.26,
                                   post$Mean.Utah.27, post$Mean.Utah.28,
                                   post$Mean.Utah.29, post$Mean.Utah.30,
                                   post$Mean.Utah.31, post$Mean.Utah.32,
                                   post$Mean.Utah.33, post$Mean.Utah.34,
                                   post$Mean.Utah.35, post$Mean.Utah.36,
                                   post$Mean.Utah.37, post$Mean.Utah.38,
                                   post$Mean.Utah.39, post$Mean.Utah.40,
                                   post$Mean.Utah.41, post$Mean.Utah.42,
                                   post$Mean.Utah.43, post$Mean.Utah.44,
                                   post$Mean.Utah.45),
                     
                     chillports = c(post$Mean.Port.1, post$Mean.Port.2,
                                    post$Mean.Port.3, post$Mean.Port.4,
                                    post$Mean.Port.5, post$Mean.Port.6,
                                    post$Mean.Port.7, post$Mean.Port.8,
                                    post$Mean.Port.9, post$Mean.Port.10,
                                    post$Mean.Port.11, post$Mean.Port.12,
                                    post$Mean.Port.13, post$Mean.Port.14,
                                    post$Mean.Port.15, post$Mean.Port.16,
                                    post$Mean.Port.17, post$Mean.Port.18,
                                    post$Mean.Port.19, post$Mean.Port.20,
                                    post$Mean.Port.21, post$Mean.Port.22,
                                    post$Mean.Port.23, post$Mean.Port.24,
                                    post$Mean.Port.25, post$Mean.Port.26,
                                    post$Mean.Port.27, post$Mean.Port.28,
                                    post$Mean.Port.29, post$Mean.Port.30,
                                    post$Mean.Port.31, post$Mean.Port.32,
                                    post$Mean.Port.33, post$Mean.Port.34,
                                    post$Mean.Port.35, post$Mean.Port.36,
                                    post$Mean.Port.37, post$Mean.Port.38,
                                    post$Mean.Port.39, post$Mean.Port.40,
                                    post$Mean.Port.41, post$Mean.Port.42,
                                    post$Mean.Port.43, post$Mean.Port.44,
                                    post$Mean.Port.45),
                     
                     
                     gdd = c(post$Mean.GDD.1, post$Mean.GDD.2,
                             post$Mean.GDD.3, post$Mean.GDD.4,
                             post$Mean.GDD.5, post$Mean.GDD.6,
                             post$Mean.GDD.7, post$Mean.GDD.8,
                             post$Mean.GDD.9, post$Mean.GDD.10,
                             post$Mean.GDD.11, post$Mean.GDD.12,
                             post$Mean.GDD.13, post$Mean.GDD.14,
                             post$Mean.GDD.15, post$Mean.GDD.16,
                             post$Mean.GDD.17, post$Mean.GDD.18,
                             post$Mean.GDD.19, post$Mean.GDD.20,
                             post$Mean.GDD.21, post$Mean.GDD.22,
                             post$Mean.GDD.23, post$Mean.GDD.24,
                             post$Mean.GDD.25, post$Mean.GDD.26,
                             post$Mean.GDD.27, post$Mean.GDD.28,
                             post$Mean.GDD.29, post$Mean.GDD.30,
                             post$Mean.GDD.31, post$Mean.GDD.32,
                             post$Mean.GDD.33, post$Mean.GDD.34,
                             post$Mean.GDD.35, post$Mean.GDD.36,
                             post$Mean.GDD.37, post$Mean.GDD.38,
                             post$Mean.GDD.39, post$Mean.GDD.40,
                             post$Mean.GDD.41, post$Mean.GDD.42,
                             post$Mean.GDD.43, post$Mean.GDD.44,
                             post$Mean.GDD.45),
                     
                     mat.lo = c(post$Spring.Temp.1, post$Spring.Temp.2,
                             post$Spring.Temp.3, post$Spring.Temp.4,
                             post$Spring.Temp.5, post$Spring.Temp.6,
                             post$Spring.Temp.7, post$Spring.Temp.8,
                             post$Spring.Temp.9, post$Spring.Temp.10,
                             post$Spring.Temp.11, post$Spring.Temp.12,
                             post$Spring.Temp.13, post$Spring.Temp.14,
                             post$Spring.Temp.15, post$Spring.Temp.16,
                             post$Spring.Temp.17, post$Spring.Temp.18,
                             post$Spring.Temp.19, post$Spring.Temp.20,
                             post$Spring.Temp.21, post$Spring.Temp.22,
                             post$Spring.Temp.23, post$Spring.Temp.24,
                             post$Spring.Temp.25, post$Spring.Temp.26,
                             post$Spring.Temp.27, post$Spring.Temp.28,
                             post$Spring.Temp.29, post$Spring.Temp.30,
                             post$Spring.Temp.31, post$Spring.Temp.32,
                             post$Spring.Temp.33, post$Spring.Temp.34,
                             post$Spring.Temp.35, post$Spring.Temp.36,
                             post$Spring.Temp.37, post$Spring.Temp.38,
                             post$Spring.Temp.39, post$Spring.Temp.40,
                             post$Spring.Temp.41, post$Spring.Temp.42,
                             post$Spring.Temp.43, post$Spring.Temp.44,
                             post$Spring.Temp.45),
                     
                     siteslist = c(post$`Site Num..1`, post$`Site Num..2`,
                                   post$`Site Num..3`, post$`Site Num..4`,
                                   post$`Site Num..5`, post$`Site Num..6`,
                                   post$`Site Num..7`, post$`Site Num..8`,
                                   post$`Site Num..9`, post$`Site Num..10`,
                                   post$`Site Num..11`, post$`Site Num..12`,
                                   post$`Site Num..13`, post$`Site Num..14`,
                                   post$`Site Num..15`, post$`Site Num..16`,
                                   post$`Site Num..17`, post$`Site Num..18`,
                                   post$`Site Num..19`, post$`Site Num..20`,
                                   post$`Site Num..21`, post$`Site Num..22`,
                                   post$`Site Num..23`, post$`Site Num..24`,
                                   post$`Site Num..25`, post$`Site Num..26`,
                                   post$`Site Num..27`, post$`Site Num..28`,
                                   post$`Site Num..29`, post$`Site Num..30`,
                                   post$`Site Num..31`, post$`Site Num..32`,
                                   post$`Site Num..33`, post$`Site Num..34`,
                                   post$`Site Num..35`, post$`Site Num..36`,
                                   post$`Site Num..37`, post$`Site Num..38`,
                                   post$`Site Num..39`, post$`Site Num..40`,
                                   post$`Site Num..41`, post$`Site Num..42`,
                                   post$`Site Num..43`, post$`Site Num..44`,
                                   post$`Site Num..45`),
                     year = rownames(post))

site.post<-full_join(postdata, sites)
site.post$x<-NULL
site.post$y<-NULL


full.site<-full_join(site, site.post)
full.site$year<-as.numeric(full.site$year)
full.site$cc <- ifelse(full.site$year>1960, "2000-2010", "1950-1960")
lodata <- subset(allpeps.subset, select=c("year", "lat", "long", "lo"))
full.site <- left_join(full.site, lodata)
full.site.nonas <- full.site[!is.na(full.site$lo),]

if(FALSE){
  allchillsgdds<-rbind(full.site1, full.site2)
  allchillsgdds<-rbind(allchillsgdds, full.site3)
  allchillsgdds<-rbind(allchillsgdds, full.site4)
  allchillsgdds<-rbind(allchillsgdds, full.site5)
}
#write.csv(full.site.nonas, file="output/betpen_allchillsandgdds_45sites_tntx_forsims.csv", row.names = FALSE)

##################################################################################################
############################### MEAN TEMP instead of GDD #########################################
##################################################################################################
full.site <- read.csv("output/betpen_allchillsandgdds_45sites_tntx_forsims.csv", header = TRUE)

period <- c(1951:1960, 2001:2010)
sites<-subset(full.site, select=c(lat, long, lat.long))
sites<-sites[!duplicated(sites$lat.long),]
sites$x<-sites$long
sites$y<-sites$lat
Coords<-subset(sites, select=c(x, y))
nsites<-length(sites$lat.long)
tmin <- rn
tmax <- rx

points.min <- SpatialPoints(Coords, proj4string = rn@crs)
points.max <- SpatialPoints(Coords, proj4string = rx@crs)

yearsinclim<-as.numeric(format(as.Date(names(tmin),format="X%Y.%m.%d"),"%Y"))
yearsinperiod<-which(yearsinclim%in%period)
climsubmin<-subset(tmin,yearsinperiod)
climsubmax<-subset(tmax,yearsinperiod)

## subset climate days
monthsinclim<-as.numeric(format(as.Date(names(climsubmin),format="X%Y.%m.%d"),"%m"))
mstmonths<-c(3:4)
monthsinmst<-which(monthsinclim%in%mstmonths)
mstsubmin<-subset(climsubmin,monthsinmst)
mstsubmax<-subset(climsubmax,monthsinmst)

valuesmin <- raster::extract(mstsubmin,points.min)
valuesmax <- raster::extract(mstsubmax,points.max)

dclimmin <- cbind.data.frame(coordinates(points.min),valuesmin)
dclimmax <- cbind.data.frame(coordinates(points.max),valuesmax)

library(reshape2)
dxmin<-melt(dclimmin, id.vars=c("x","y"))
dxmax<-melt(dclimmax, id.vars=c("x","y"))

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

dx <- data.frame(lat=dxmin$lat, long=dxmin$long, date=dxmin$date, tmin=dxmin$Tmin, tmax=dxmax$Tmax)
dx$Tavg <- (dx$tmin+dx$tmax)/2

dx$date<-substr(dx$date, 2,11)
dx$Date<- gsub("[.]", "-", dx$date)

dx$tmin <- NULL
dx$tmax <- NULL
dx$date<-NULL

dx$year<-as.numeric(substr(dx$Date, 0, 4))
dx$lat.long<-paste(dx$lat, dx$long)
dx$mat<-ave(dx$Tavg, dx$year, dx$lat.long)

mst<-dx%>%dplyr::select(-Tavg, -Date)
mst<-mst[!duplicated(mst),]

fullsites45 <- left_join(full.site, mst)

write.csv(fullsites45, file="output/betpen_allchillsandgdds_45sites_mat_tntx_forsims.csv", row.names = FALSE)

##################################################################################################
################################# Now for some plots! ############################################
##################################################################################################
fullsites45 <- read.csv("output/betpen_allchillsandgdds_45sites_mat_tntx_forsims.csv", header=TRUE)
somesites <- sample(unique(fullsites45$lat.long), 9)

tensites<-fullsites45[(fullsites45$lat.long %in% somesites),]

post.cols <- colorRampPalette(brewer.pal(9,"Reds"))(9)
pre.cols <- colorRampPalette(brewer.pal(9,"Blues"))(9)

peppre <- tensites[(tensites$cc=="1950-1960"),]
peppost <- tensites[(tensites$cc=="2000-2010"),]


chill.utah.pre<-ggplot(tensites, aes(x=chillutah, y=lo, col=as.factor(lat.long))) + geom_line(data=peppre, aes(col=as.factor(lat.long)), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Total Utah Chill", y="Day of Leafout") + theme(legend.position="none") + ylim(65, 150) + xlim(1000, 3000) +
  geom_point(data=tensites[(tensites$cc=="1950-1960"),],
             aes(col=lat.long), alpha=0.3) + 
  scale_color_manual(name="Years", values=pre.cols,
                     labels=c("1950-1960" = "1950-1960")) 

chill.utah.post<-ggplot(tensites, aes(x=chillutah, y=lo, col=as.factor(lat.long))) + geom_line(data=peppost, aes(col=as.factor(lat.long)), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Total Utah Chill", y="Day of Leafout") + theme(legend.position="none") + ylim(65, 150) + xlim(1000, 3000) +
  geom_point(data=tensites[(tensites$cc=="2000-2010"),],
             aes(col=lat.long), alpha=0.3) + 
  scale_color_manual(name="Years", values=post.cols,
                     labels=c("2000-2010" = "2000-2010"))  


chill.ports.pre<-ggplot(tensites, aes(x=chillports, y=lo, col=as.factor(lat.long))) + geom_line(data=peppre, aes(col=as.factor(lat.long)), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Total Chill Portions", y="Day of Leafout") + theme(legend.position="none") + ylim(65, 150) +
  geom_point(data=tensites[(tensites$cc=="1950-1960"),],
             aes(col=lat.long), alpha=0.3) + xlim(85,150) + 
  annotate("text", x = 95, y = 145, label = "1950-1960", fontface = "bold") +
  scale_color_manual(name="Years", values=pre.cols,
                     labels=c("1950-1960" = "1950-1960")) 

chill.ports.post<-ggplot(tensites, aes(x=chillports, y=lo, col=as.factor(lat.long))) + geom_line(data=peppost, aes(col=as.factor(lat.long)), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Total Chill Portions", y="Day of Leafout") + theme(legend.position="none") + 
  ylim(65, 150) + xlim(85,150) +
  geom_point(data=tensites[(tensites$cc=="2000-2010"),],
             aes(col=as.factor(lat.long)), alpha=0.3) + 
  annotate("text", x = 95, y = 145, label = "2000-2010", fontface = "bold") +
  scale_color_manual(name="Sites", values=post.cols,
                     labels=c("2000-2010" = "2000-2010")) 

if(FALSE){
  quartz()
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-ggplot(g_legend(chill.ports.post))
  
  quartz()
  g1<-ggarrange(chill.utah.pre, chill.utah.post,
                chill.ports.pre, chill.ports.post, nrow=2, ncol=2)
  grid.arrange(g1, mylegend, ncol=3, widths = c(1.5, 0.1, 0.35), layout_matrix=rbind(c(1,NA,2)))
  
}




gdd.pre<-ggplot(tensites, aes(x=gdd, y=lo, col=as.factor(lat.long))) + geom_line(data=peppre, aes(col=as.factor(lat.long)), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Growing Degree Days", y="Day of Leafout") + theme(legend.position="none") + ylim(65, 150) + 
  geom_point(data=tensites[(tensites$cc=="1950-1960"),],
             aes(col=as.factor(lat.long)), alpha=0.3) + xlim(0, 40) +
  scale_color_manual(name="Years", values=pre.cols,
                     labels=c("1950-1960" = "1950-1960")) 

gdd.post<-ggplot(tensites, aes(x=gdd, y=lo, col=as.factor(lat.long))) + geom_line(data=peppost, aes(col=as.factor(lat.long)), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Growing Degree Days", y="Day of Leafout") + theme(legend.position="none") + 
  ylim(65, 150) + 
  geom_point(data=tensites[(tensites$cc=="2000-2010"),],
             aes(col=as.factor(lat.long)), alpha=0.3) + xlim(0, 40) +
  scale_color_manual(name="Years", values=post.cols,
                     labels=c("2000-2010" = "2000-2010")) 

mat.pre<-ggplot(tensites, aes(x=mat, y=lo, col=as.factor(lat.long))) + geom_line(data=peppre, aes(col=as.factor(lat.long)), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Mean Spring Temperature", y="Day of Leafout") + theme(legend.position="none") + ylim(65, 150) +
  geom_point(data=tensites[(tensites$cc=="1950-1960"),],
             aes(col=as.factor(lat.long)), alpha=0.3) + xlim(-1, 8) +
  scale_color_manual(name="Years", values=pre.cols,
                     labels=c("1950-1960" = "1950-1960")) 

mat.post<-ggplot(tensites, aes(x=mat, y=lo, col=lat.long)) + geom_line(data=peppost, aes(col=lat.long), stat="smooth", method="lm") + 
  theme_classic() + labs(x="Mean Spring Temperature", y="Day of Leafout") + theme(legend.position="none") + 
  ylim(65, 150) +
  geom_point(data=tensites[(tensites$cc=="2000-2010"),],
             aes(col=lat.long), alpha=0.3) + xlim(-1, 8) +
  scale_color_manual(name="Years", values=post.cols,
                     labels=c("2000-2010" = "2000-2010"))

quartz()
grid.arrange(chill.utah.pre, chill.utah.post,
             gdd.pre, gdd.post, 
             mat.pre, mat.post, nrow=3, ncol=2)

if(FALSE){
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(mat.post)
  g1<-ggarrange(chill.plot.utah, chill.plot.ports, gdd,mat.post, ncol=2, nrow=2)
  grid.arrange(g1, mylegend, ncol=3, widths = c(1.5, 0.1, 0.35), layout_matrix=rbind(c(1,NA,2)))
  
}




quartz()
leafout<- ggplot(full.site, aes(x=cc, y=lo, col=cc)) + geom_boxplot(aes(col=as.factor(cc))) +
  theme_classic() + labs(x="",y="Day of Leafout") + theme(legend.position = "none") +
  scale_color_manual(name="Climate Change", labels=c("1950-1960"="1950-1960","2000-2010" ="2000-2010"),
                     values=c("navyblue", "red4"))

