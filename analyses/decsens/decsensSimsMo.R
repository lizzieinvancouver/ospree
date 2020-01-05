## Started 28 December 2019 ##
## By Lizzie ##

## Simulations of winter to spring temperatures with fstar (GDD) required increasing when chilling is low ##

# This code requires higher forcing (aka GDD, aka fstar) for leafout when chilling is low #
# It does not vary when GDD accumulates though, it always just starts accumulating on a certain day (daystostart) #

## To do! ##
# Check the code, in particular:
# (a) Make sure daily temp is working correctly
# (b) Look more at the intermediate output to make sure it makes sense


# Step 1: Set up years, days per year, temperatures, required GDD (fstar), required chill (cstar) and how much higher fstar is when cstar is not met
daysperseason <- 100
daysperinterseason <- 25
daystostart <- daysperseason+daysperinterseason # this defines the break between 'winter' and 'spring,' functionally we accumulate chill only in 1:daystostart and GDD only in daystostart:end(df)
yearz <- 30
sitez <- 45 # aka reps
degreez <- c(0, 0.5, 1, 2, 4, 6, 7) # warming -- applied evenly across whole period
sigma <- 4
fstar <- 200
cstar <- 110
fstaradjforchill <- 3 # how much more GDD to you need based on diff from cstar at end of daystostart

if(FALSE){
# Saving some intermediate progress .... setting up the seasonal temps and looking at chill and GDD calculated from them 
df <- data.frame(degwarm=numeric(), rep=numeric(), chill=numeric(), gdd=numeric())

for (i in degreez){
   for (j in 1:sitez){
       yearly_expected_temp <- rep(6, yearz)
       daily_temp <- sapply(yearly_expected_temp, function(x) c(rnorm(daysperseason, 0 + i, sigma),
           rnorm(daysperinterseason, 2 + i , sigma), rnorm(daysperinterseason, 4 + i, sigma),
           rnorm(daysperseason, 6 + i, sigma)))
       chill <- daily_temp
       chill[(chill)<0] <- 0
       chill[(chill)>5] <- 0
       gdd <- daily_temp
       gdd[(gdd)<0] <- 0
       dfadd <- data.frame(degwarm=i, rep=j, chill=mean(colSums(chill[1:daystostart,], na.rm = TRUE)),
           gdd=mean(colSums(gdd[daystostart:nrow(gdd),], na.rm = TRUE)))
       df <- rbind(df, dfadd)
    }
}

plot(daily_temp[,1]~c(1:nrow(daily_temp)), type="l")
par(mfrow=c(1,2))
plot(chill~degwarm, df)
plot(gdd~degwarm, df)
par(mfrow=c(1,1))

# Saving some more intermediate progress ... this sets up a varying fstar when chilling is below cstar
gddreq <- c()
leafout_date <- c()
for (k in 1:ncol(chill)){
    chillsum <- sapply(1:ncol(chill), function(x) (cumsum(chill[,x])))
    gddsum <- sapply(1:ncol(gdd), function(x) (cumsum(gdd[daystostart:nrow(gdd),x])))
    if (chillsum[daystostart,k]>cstar) {
        gddreq[k] <- fstar
        } else {
        gddreq[k] <- fstar + (cstar-chillsum[daystostart,k])*fstaradjforchill
        }
    leafout_date[k] <- min(which(gddsum[,k] > gddreq[k]))
    }

plot(leafout_date~colMeans(daily_temp))
plot(leafout_date~gddreq)
}

## Step 2: Now I put together the seasonal temps, varying fstar (increases when chill is low) and calculate the sensitivities

df <- data.frame(degwarm=numeric(), rep=numeric(), chill=numeric(), fstar=numeric(), simplelm=numeric(),
    loglm=numeric(), perlm=numeric())

# yearlytemp <- "postwinter"
yearlytemp <- "alltemps"

for (i in degreez){
   for (j in 1:sitez){
       yearly_expected_temp <- rep(6, yearz)
       daily_temp <- sapply(yearly_expected_temp, function(x) c(rnorm(daysperseason, 0 + i, sigma),
           rnorm(daysperinterseason, 2 + i , sigma), rnorm(daysperinterseason, 4 + i, sigma),
           rnorm(daysperseason, 6 + i, sigma)))
       chill <- daily_temp
       chill[(chill)<0] <- 0
       chill[(chill)>5] <- 0
       gdd <- daily_temp
       gdd[(gdd)<0] <- 0
       gddreq <- c()
       leafout_date <- c()
       for (k in 1:ncol(chill)){
           chillsum <- sapply(1:ncol(chill), function(x) (cumsum(chill[,x])))
           gddsum <- sapply(1:ncol(gdd), function(x) (cumsum(gdd[daystostart:nrow(gdd),x])))
           if (chillsum[daystostart,k]>cstar) {
           gddreq[k] <- fstar
           } else {
           gddreq[k] <- fstar + (cstar-chillsum[daystostart,k])*fstaradjforchill
           }
           leafout_date[k] <- min(which(gddsum[,k] > gddreq[k]))
           meanchill <- mean(chillsum[daystostart,])
           meanfstar <- mean(gddreq)
           }
           yearly_tempall <- colMeans(daily_temp)
           yearly_temppostwinter <- colMeans(daily_temp[daystostart:nrow(daily_temp),])
           if(yearlytemp=="postwinter") {
               yearly_temp <- yearly_temppostwinter
               } else {
               yearly_temp <- yearly_tempall
               }
           per_leafout_date <- leafout_date/mean(leafout_date)
           per_yearly_temp <- yearly_temp/mean(yearly_temp)
           plot(yearly_temp, leafout_date, pch=20)
           dfadd <- data.frame(degwarm=i, rep=j, chill=meanchill, fstar=meanfstar,     
               simplelm=coef(lm(leafout_date~yearly_temp))[2],
               loglm=coef(lm(log(leafout_date)~log(yearly_temp)))[2],
               perlm=coef(lm(per_leafout_date~per_yearly_temp))[2])
           df <- rbind(df, dfadd)
       }
   }

plot(simplelm~degwarm, data=df, pch=16, ylab="Sensitivity (days/C or log(days)/log(C)", xlab="Degree warming")
points(loglm~degwarm, data=df, col="dodgerblue")
plot(perlm~degwarm, data=df, col="firebrick")
