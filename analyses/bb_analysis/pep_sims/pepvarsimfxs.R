# Big outside loop ...
pepvariance.sim <- function(drawstotal, precctemp, postcctemp, samplefreq, sigma, fstar){
draws <- c()
precc.sens <- c()
postcc.sens <- c()
var.lo.precc <- c()
var.lo.postcc <- c()

   
for(j in 1:drawstotal){

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 60
yrstotal <- 40
yrs <- rep(1:yrstotal, each=daysperyr)
dayz <- rep(1:daysperyr, 40)
precc <- precctemp
postcc <- postcctemp
sigma <- sigma
fstar <- fstar
samplefreq <- samplefreq

cc <- c(rep("precc", yrstotal/2), rep("postcc", yrstotal/2))

# Step 2: here we go, set up some empty vectors, then fill with random draws based on temps above
dailytemp <- c()
gdd <- c()

for (i in 1:yrstotal){
    if (i<(yrstotal/2+1)) {
        dailytemphere <- rnorm(daysperyr, precc, sigma)
        } else {
        dailytemphere <- rnorm(daysperyr, postcc, sigma)
        }
    dailytemp <- c(dailytemp, dailytemphere)
    gdd <- c(gdd, cumsum(dailytemphere))
    }

# Step 3: Make a data frame and get the mean temp per year (needed later to calculate sensitivities)
df <- data.frame(cbind(yrs, dayz, dailytemp, gdd))
df.meantemp <- aggregate(df["dailytemp"], df[c("yrs")], FUN=mean)
plot(dailytemp ~ yrs, data=df.meantemp)

# Step 4: Now, in a very slow, painful way I get the BB date
df$bb.YN <- NA

for (i in c(1:nrow(df))){ # This loop just makes a Yes/No vector for budburst
    if (df$gdd[i]<fstar) {
        df$bb.YN[i] <- "N"
        } else {
        df$bb.YN[i] <- "Y"
        }
    }

# Step 5: Now we remove rows based on sampling frequency and then calculate the observed BB date
df.sample <- df[seq(1, nrow(df), samplefreq),]

bbdate <- c()

for (i in 1:yrstotal){
    subby <- subset(df.sample, yrs==i & bb.YN=="Y")
    bbdate[i] <- subby$dayz[1]
    }

# Step 6: Whoop! Now we can caculate temperature sensitivities.
dfcalc <- cbind(bbdate, df.meantemp, cc)
estprecc <- lm(bbdate~dailytemp, data=subset(dfcalc, cc=="precc"))
estpostcc <- lm(bbdate~dailytemp, data=subset(dfcalc, cc=="postcc"))

diffbefore.after <- coef(estprecc)[2]-coef(estpostcc)[2]
# negative means a decline in sensitivity AFTER climate change
draws <- rbind(draws, diffbefore.after)

# get a few more things
preccdf <- subset(dfcalc, cc=="precc")
postccdf <- subset(dfcalc, cc=="postcc")
precc.sens <-  rbind(precc.sens, coef(estprecc)[2])
postcc.sens <-  rbind(postcc.sens, coef(estpostcc)[2])
var.lo.precc <- rbind(var.lo.precc, var(preccdf$bbdate, na.rm=TRUE))
var.lo.postcc <- rbind(var.lo.postcc, var(postccdf$bbdate, na.rm=TRUE))


# Okay, now build a df with a few things we want...
df.return <- data.frame(cbind(draws, precc.sens, postcc.sens, var.lo.precc, var.lo.postcc), row.names = NULL) 
names(df.return) <- c("diffbefore.after", "precc.sens", "postcc.sens", "var.lo.precc", "var.lo.postcc")
    
}
return(df.return)
}

