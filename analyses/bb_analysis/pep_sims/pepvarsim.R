## Code to see if you can get lower estimated sensitivitie soley due to higher temps and non-constant monitoring... ##
## Started 14 February 2019 ##
## Snow and clouds from the train to Seattle ##
## Started by Lizzie ##

## NOTE: These sims are designed to contrast a BEFORE and AFTER climate change sensitivity etc. ... you would need to adjust the f(x) to do time-series-style change over time approach. But since this was the approach we were taking to the PEP725 data (i.e., look at 10 years before CC and 10 years after) I did it this way.

# Updated on 18 April 2019: Added: Plots of estimated sens versus warming and plots of variance in leafout date versus warming
## TO DO:
# (2) Use realistic daily temps and fstar values #

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Need to simulate data as GDD system; only really need forcing #

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis/pep_sims")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis/pep_sims") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_sims")

figpath <- "figures"

source("pepvarsimfxs.R")

## Below is the original loop that I wrote ... not sure if we need to keep it?

if(FALSE){
# This controls how many runs of the whole thing you do ...
drawstotal <- 50
draws <- c()

# Big outside loop ... 
for(j in 1:drawstotal){

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
}

# Looking at effect of sampling freq and warming
if(FALSE){
sim.1d.2deg <- pepvariance.sim(100, 10, 12, 1, 6, 400)
sim.3d.2deg <- pepvariance.sim(100, 10, 12, 3, 6, 400)
sim.7d.2deg <- pepvariance.sim(100, 10, 12, 7, 6, 400)
# sim.20d.2deg <- pepvariance.sim(100, 10, 12, 20, 6, 400)
sim.3d.0deg <- pepvariance.sim(100, 10, 10, 3, 6, 400)
sim.3d.5deg <- pepvariance.sim(100, 10, 15, 3, 6, 400)

breaks=20

pdf(file.path(figpath, "sims.pdf"), width = 8, height = 6)
par(mfrow=c(2,3))
hist(sim.1d.2deg, xlab="Diff sens (before-after cc)", main="Sample daily", breaks=breaks)
abline(v=mean(sim.1d.2deg), col="red")
hist(sim.3d.2deg, xlab="Diff sens (before-after cc)", main="Sample every 3 days", breaks=breaks)
abline(v=mean(sim.3d.2deg), col="red")
hist(sim.7d.2deg, xlab="Diff sens (before-after cc)", main="Sample every 7 days", breaks=breaks)
abline(v=mean(sim.7d.2deg), col="red")

hist(sim.3d.0deg, xlab="Diff sens (before-after cc)", main="No temp change", breaks=breaks)
abline(v=mean(sim.3d.0deg), col="red")
hist(sim.3d.2deg, xlab="Diff sens (before-after cc)", main="Two deg change", breaks=breaks)
abline(v=mean(sim.3d.2deg), col="red")
hist(sim.3d.5deg, xlab="Diff sens (before-after cc)", main="Five deg change", breaks=breaks)
abline(v=mean(sim.3d.5deg), col="red")
dev.off()
}


# drawstotal, precctemp, postcctemp, samplefreq, sigma, fstar
sim.1deg <- pepvariance.sim(100, 10, 11, 3, 6, 400)
sim.2deg <- pepvariance.sim(100, 10, 12, 3, 6, 400)
sim.3deg <- pepvariance.sim(100, 10, 13, 3, 6, 400)
sim.4deg <- pepvariance.sim(100, 10, 14, 3, 6, 400)
sim.5deg <- pepvariance.sim(100, 10, 15, 3, 6, 400)

sim.1deg$degwarm <- 1
sim.2deg$degwarm <- 2
sim.3deg$degwarm <- 3
sim.4deg$degwarm <- 4
sim.5deg$degwarm <- 5

degwarm.runs <- rbind(sim.1deg, sim.2deg, sim.3deg, sim.4deg, sim.5deg)

plot(postcc.sens~as.factor(degwarm), data=degwarm.runs, xlab="degree warming", ylab="temperature sensitivity")
plot(var.lo.postcc~as.factor(degwarm), data=degwarm.runs, xlab="degree warming", ylab="var(leafout date)")
