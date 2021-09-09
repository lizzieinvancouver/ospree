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


frostdamage <- 0
fstar <- 200
yearz <- 20 # 
sitez <- 20 # sites, basically useful as *replicates*
simsnum <- 5#
# START inter/intra-annual variation parameter edits here
dayswinter <- 120
daysspring <- 90

wintemp_mean <- 1
#sprtemp_mean <- 2#not used in this version- instead spring temp is 1 degree more than winter temp
springtempinc <- 0.1
sigma <- seq(0, 4, length.out=simsnum) # let's treat this as intra-annual (i.e., daily variation)- 
sigmainter <- seq(0, 10, length.out=simsnum) # inter-annual, let's start by varying this ... 

# END inter/intra-annual variation parameter edits here

if(FALSE){ # not using just now ... as it's chilling
   cstar <- 0
   fstaradjforchill <- 3 # how much more GDD to you need based on diff from cstar at end of daystostart
}

## Step 2: Now I put together the seasonal temps, varying fstar (increases when chill is low) and calculate the sensitivities    
df <- data.frame(intervar=numeric(),
                 intravar=numeric(),
                 meanwinter=numeric(),
                 meanspring=numeric(),
                 meantemp=numeric(),
                 rep=numeric(),
                 year=numeric(),
                 fstar=numeric(),
                 frostevents=numeric()) 

for (i in sigmainter){
   
   for (j in 1:sitez){
      wintertemp <- rnorm(yearz, wintemp_mean,i)
      springtemp <- wintertemp+1
      
      for (l in sigma){
         
         # TO DO ... inter-annual variation ... and then intra-annual within it
         # If you're working on that edit here ... START within looop edits
         daily_temp <- sapply(rep(NA, yearz), function(x) c(rnorm(dayswinter, wintertemp, l),
                                                            (rnorm(daysspring, springtemp, l) + c(1:daysspring)*springtempinc)))
         # END within loop edits here for inter/intra-annual variation
         
         gdd <- daily_temp
         gdd[(gdd)<0] <- 0
         leafout_date <- c()
         frostevents <- c()
         for (k in 1:ncol(daily_temp)){
            gddsum <- sapply(1:ncol(gdd), function(x) (cumsum(gdd[dayswinter:nrow(gdd),x])))
            leafout_date[k] <- min(which(gddsum[,k] > fstar)) # leafout_date unit of days *after* dayswinter
            colddays <- which(daily_temp[,k] < frostdamage) # Cat, please check all my edits!
            frostz <- which(colddays > leafout_date[k]) 
            frostevents[k] <- length(frostz)
         }
         yearly_temp <- colMeans(daily_temp)
         dfadd <- data.frame(intervar=rep(i, yearz),
                             intravar=rep(l, yearz),
                             meanwinter=rep(wintertemp, yearz),
                             meanspring=rep(springtemp, yearz),
                             meantemp=yearly_temp,
                             rep=rep(j, yearz),
                             year=c(1:yearz),
                             fstar=rep(fstar),
                             frostevents=frostevents)
         df <- rbind(df, dfadd)     
      }
   }
}
# has some warnings sometimes? Last run I did worked... 

#Make some simple plots to see if the code is working
plot(df$intervar,df$intravar)#seems to be fitting every combination of intra and inter
par(mfrow=c(2,4))
boxplot(meanwinter~intervar,data=df)
boxplot(meanspring~intervar,data=df)
boxplot(meantemp~intervar,data=df)
boxplot(frostevents~intervar,data=df)
boxplot(meanwinter~intravar,data=df)
boxplot(meanspring~intravar,data=df)
boxplot(meantemp~intravar,data=df)
boxplot(frostevents~intravar,data=df)

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
