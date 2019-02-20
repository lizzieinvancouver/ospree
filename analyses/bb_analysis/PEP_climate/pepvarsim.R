## Code to see if you can get lower estimated sensitivitie soley due to higher temps and non-constant monitoring... ##
## Started 14 February 2019 ##
## Snow and clouds from the train to Seattle ##
## Started by Lizzie ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Need to simulate data as GDD system; only really need forcing #

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 60
yrstotal <- 40
yrs <- rep(1:yrstotal, each=daysperyr)
dayz <- rep(1:daysperyr, 40)
precc <- 10
postcc <- 12
sigma <- 8
fstar <- 400
samplefreq <- 3

cc <- c(rep("precc", yrstotal/2), rep("postcc", yrstotal/2))

# This controls how many runs of the whole thing you do ...
drawstotal <- 30
draws <- c()

# Big outside loop ... 
for(j in 1:drawstotal){

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
}

mean(draws)
hist(draws, breaks=10)
