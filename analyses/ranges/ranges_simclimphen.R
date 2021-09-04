## Started 4 September 2021 ##
## By Lizzie so far ##

## We want to try some simulations for the rangers project ... ##
## See https://github.com/lizzieinvancouver/ospree/issues/410
## So we need:
# generate climate data with vary inter and intra-annual variation
# generate phenology with forcing and chilling (or photoperiod) of varying strength ##

## To start, Lizzie is jut grabbing decsens code ##
# Using decsensSimsMultCues.R so far only  

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)

## NEXT STEPS ...
# 1) Step back and build and check climate data ... check values of inter and intra SD
# 2) Add in leafout via GDD
# 3) then add in chilling effect (or photoperiod) 
# Check for TO DO items  throughout code

# Some attempts ...

dayswinter <- 120
daysspring <- 90
wintertemp <- 1
springtemp <- 2
springtempinc <- 0.1
fstar <- 200
cstar <- 0
fstaradjforchill <- 3 # how much more GDD to you need based on diff from cstar at end of daystostart
yearz <- 50 # used to be 20, adjusted to 50 to compare to Jonathan's code 
sitez <- 45 # sites, basically useful as replicates
simsnum <- 10
# things we may want to vary?
sigma <- 4 # let's treat this as intra-annual ...
sigmainter <- seq(0, 10, length.out=simsnum) # inter-annual, let's start by varying this ... 

## Step 2: Now I put together the seasonal temps, varying fstar (increases when chill is low) and calculate the sensitivities    
df <- data.frame(sigmainter=numeric(), rep=numeric(), chill=numeric(), fstar=numeric())
    
for (i in sigmainter){
   for (j in 1:sitez){
       daily_temp <- sapply(rep(NA, yearz), function(x) c(rnorm(dayswinter, wintertemp + i, sigma),
           (rnorm(daysspring, springtemp + i , sigma) + c(1:daysspring)*springtempinc)))
       daily_temp <- daily_temp + rnorm(1, 0, i) # add same value to WHOLE year ... is this best way to do it?
       # TO DO ... add a switch at the + so that we multiple by -1 or 1 and thus do not always ADD the variation
       chill <- daily_temp
       chill[(chill)<0] <- 0
       chill[(chill)>5] <- 0
       gdd <- daily_temp
       gdd[(gdd)<0] <- 0
       gddreq <- c()
       leafout_date <- c()
       for (k in 1:ncol(chill)){
           chillsum <- sapply(1:ncol(chill), function(x) (cumsum(chill[,x])))
           gddsum <- sapply(1:ncol(gdd), function(x) (cumsum(gdd[dayswinter:nrow(gdd),x])))
           if (chillsum[dayswinter,k]>cstar) {
           gddreq[k] <- fstar
           } else {
           gddreq[k] <- fstar + (cstar-chillsum[dayswinter,k])*fstaradjforchill
           }
           leafout_date[k] <- min(which(gddsum[,k] > gddreq[k])) # leafout_date unit of days *after* dayswinter
           meanchill <- mean(chillsum[dayswinter,])
           meanfstar <- mean(gddreq)
           }
           yearly_temp <- colMeans(daily_temp)
           dfadd <- data.frame(sigmainter=i, rep=j, chill=meanchill, fstar=meanfstar)
           df <- rbind(df, dfadd)     
       }
   }

# has some warnings sometimes? Last run I did worked... 

# code chunks we may want ...


# Original code from decsensSimsMultCues.R, leaving here for now
if(FALSE){ 
# Step 1: Set up years, days per year, temperatures, required GDD (fstar), required chill (cstar) and how much higher fstar is when cstar is not met
dayswinter <- 120
daysspring <- 90
wintertemp <- 1
springtemp <- 2
springtempinc <- 0.1
sigma <- 4 
fstar <- 200
cstar <- 110
fstaradjforchill <- 3 # how much more GDD to you need based on diff from cstar at end of daystostart
yearz <- 50 # used to be 20, adjusted to 50 to compare to Jonathan's code 
sitez <- 45
simsnum <- 30
degreez <- seq(0, 7, length.out=simsnum) # warming -- applied evenly across `winter' and `spring'

## Step 2: Now I put together the seasonal temps, varying fstar (increases when chill is low) and calculate the sensitivities    
df <- data.frame(degwarm=numeric(), rep=numeric(), chill=numeric(), fstar=numeric(), simplelm=numeric())
    
for (i in degreez){
   for (j in 1:sitez){
       daily_temp <- sapply(rep(NA, yearz), function(x) c(rnorm(dayswinter, wintertemp + i, sigma),
           (rnorm(daysspring, springtemp + i , sigma)+ c(1:daysspring)*springtempinc)))
       chill <- daily_temp
       chill[(chill)<0] <- 0
       chill[(chill)>5] <- 0
       gdd <- daily_temp
       gdd[(gdd)<0] <- 0
       gddreq <- c()
       leafout_date <- c()
       for (k in 1:ncol(chill)){
           chillsum <- sapply(1:ncol(chill), function(x) (cumsum(chill[,x])))
           gddsum <- sapply(1:ncol(gdd), function(x) (cumsum(gdd[dayswinter:nrow(gdd),x])))
           if (chillsum[dayswinter,k]>cstar) {
           gddreq[k] <- fstar
           } else {
           gddreq[k] <- fstar + (cstar-chillsum[dayswinter,k])*fstaradjforchill
           }
           leafout_date[k] <- min(which(gddsum[,k] > gddreq[k])) # leafout_date unit of days *after* dayswinter
           meanchill <- mean(chillsum[dayswinter,])
           meanfstar <- mean(gddreq)
           }
           yearly_temp <- colMeans(daily_temp)
           dfadd <- data.frame(degwarm=i, rep=j, chill=meanchill, fstar=meanfstar,     
               simplelm=coef(lm(leafout_date~yearly_temp))[2],
               loglm=coef(lm(log(leafout_date)~log(yearly_temp)))[2])
           df <- rbind(df, dfadd)     
       }
   }

}
