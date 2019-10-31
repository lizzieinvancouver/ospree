### Source file to generate fake data using simulations of +1 to +4 temperature shifts
## 31 October 2019 started by Cat

simgenerate <- function(precctemp, postcctemp, sigmatemp, preccbb, postccbb, sigmabb){
  x<-seq(as.Date("1960-01-01"),as.Date("2018-12-31"),by="day")
  
  # Step 1: create budburst dataframe 
  #Columns are: Year, bb_date (in character format (%Y-%d-%m)), bb_mean (essentially day of year of budburst), doy95 (day of year where 95% bursted bud)
  bb <- data.frame(cbind(Year=as.numeric(substr(x,1,4))))
  bb$cc <- ifelse(bb$Year>1985, "postcc", "precc")
  bb <- bb[!duplicated(bb),]
  
  bbpre <- c() 
  bbpost <- c()
  for (i in c(1:nrow(bb))){
    if (bb$cc[i]=="precc") {
      bbpre <- rpois(n = nrow(bb[(bb$cc=="precc"),]), preccbb)
    } else {
      bbpost <- rpois(n = nrow(bb[(bb$cc=="postcc"),]), postccbb)
    }
    bb_mean <- c(bbpre, bbpost)
  }
  
  bb <- data.frame(cbind(bb, bb_mean))
  bbsw <- bb[(bb$Year>=1961 & bb$Year<=2018),]
  bbsw$bb_date <- as.character(as.Date(bbsw$bb_mean, origin=as.Date(paste0(bbsw$Year, "-01-01"))))
  bbsw$doy95 <- bbsw$bb_mean - 4
  
  bbdata <- subset(bbsw, select=c("Year", "bb_date", "bb_mean", "doy95"))
  
  # Step 2: create climate data
  df <- data.frame(cbind(date=as.character(x), yday=yday(x), year=substr(x, 1, 4), cc=rep(c("precc"))))
  df$cc <- ifelse(df$year>1985, "postcc", df$cc)
  dailytemp <- c()
  dailytemppre <- c() 
  dailytemppost <- c()
  
  for (i in c(1:nrow(df))){
    if (df$cc[i]=="precc") {
      dailytemppre <- rnorm(nrow(df[(df$cc=="precc"),]), precctemp, sigmatemp)
    } else {
      dailytemppost <- rnorm(nrow(df[(df$cc=="postcc"),]), postcctemp, sigmatemp)
    }
    dailytemp <- c(dailytemppre, dailytemppost)
  }
  
  clim <- data.frame(cbind(df, dailytemp))
  
  # Columns are; date, year, yday (day of year), day (day of month), month, temp (daily mean temperature in degrees celsius)
  climdata <- clim
  climdata$day <- as.numeric(substr(climdata$date, 9, 10))
  climdata$month <- as.numeric(substr(climdata$date, 6, 7))
  climdata$temp <- climdata$dailytemp
  
  climate.data <- subset(climdata, select=c("date", "year", "yday", "day", "month", "temp")) 
  
  return(list(bbdata, climate.data))
}