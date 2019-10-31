### Source file to generate fake data using simulations of +1 to +4 temperature shifts
## 31 October 2019 started by Cat

simgenerate <- function(precctemp, postcctemp, sigmatemp, preccbb, postccbb, sigmabb){
  x<-seq(as.Date("1950-01-01"),as.Date("2018-12-31"),by="day")
  yrs <- substr(x, 1, 4)
  #doy<-rep(c(1:100), times=69)
  #yrs <- rep(c(1950:2018), each=100)
  #dates <- as.Date(doy, origin=paste0(yrs, "-01-01"))
  
  # Step 1: create climate data
  df <- data.frame(cbind(date=as.character(x), yday=yday(x), year=yrs, cc=rep(c("precc"))))
  df$cc <- ifelse(df$year>=1985, "postcc", df$cc)
  dailytemp <- c()
  dailytemppre <- c() 
  dailytemppost <- c()
  gdd <- c()
  
  dailytemppre <- rnorm(nrow(df[(df$year<1985),]), precctemp, sigmatemp)
  dailytemppost <- rnorm(nrow(df[(df$year>=1985),]), postcctemp, sigmatemp)
  
  dailytemp <- c(dailytemppre, dailytemppost)
  tempyr <- data.frame(cbind(dailytemp, yrs))
  tempyr$gdd <- ave(tempyr$dailytemp, tempyr$yrs,  FUN=cumsum)
  gdd <- tempyr$gdd
  
  clim <- data.frame(cbind(df, dailytemp))
  
  # Columns are; date, year, yday (day of year), day (day of month), month, temp (daily mean temperature in degrees celsius)
  climdata <- clim
  climdata$day <- as.numeric(substr(climdata$date, 9, 10))
  climdata$month <- as.numeric(substr(climdata$date, 6, 7))
  climdata$temp <- climdata$dailytemp
  
  ## Step 2: finalize climate data frame
  climate.data <- subset(climdata, select=c("date", "year", "yday", "day", "month", "temp")) 
  
  # Step 3: Make a data frame to find bb date using GDDs. Can adjust by changing the fstar. 
  bb <- data.frame(cbind(date=as.character(x), Year=yrs, doy=yday(x), dailytemp, gdd))
  bb$gdd <- as.numeric(bb$gdd)
  
  # Step 4: Using code from Lizzie's pep_sims/pepvarsimfxs.R. "Now, in a very slow, painful way I get the BB date"
  bb$bb.YN <- ifelse(bb$gdd<fstar, "N", "Y")
  
  bbdata <- bb[(bb$bb.YN=="Y"),]
  bbdata$bb_mean <- as.numeric(ave(bbdata$doy, bbdata$Year, FUN=min))
  bbdata$bb_date <- ifelse(bbdata$bb_mean==bbdata$doy, bbdata$date, NA)
  bbdata <- bbdata[!is.na(bbdata$bb_date),]
  bbdata <- bbdata[(bbdata$Year>=1951),]
  
  # Step 5: create budburst dataframe 
  #Columns are: Year, bb_date (in character format (%Y-%d-%m)), bb_mean (essentially day of year of budburst), doy95 (day of year where 95% bursted bud)
  bbdata$doy95 <- bbdata$bb_mean - 4 ### not sure if this column is necessary so I just made it up... 
  
  bbsw <- subset(bbdata, select=c("Year", "bb_date", "bb_mean", "doy95"))
  bbsw$bb_date <- as.character(bbsw$bb_date)
  
  return(list(bbdata, climate.data))
}