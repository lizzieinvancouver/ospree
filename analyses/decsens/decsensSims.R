## Started 4 January 2020 ##
## By Lizzie, see also decsensSimsAuerbach.R and pepvarsim.R ##

## Simulation of the declining sensitivities problem ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

set.seed(113)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/decsens")
} else if(length(grep("lizzie", getwd()))>0) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/decsens")
} else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/decsens") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis/pep_sims")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/decsens")

# Make some data ...

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 60
yearz <- 30
sitez <- 45 # reps
degreez <- c(0, 0.5, 1, 2, 4, 7)
sigma <- 4
basetemp <- 6
fstar <- 150

df <- data.frame(degwarm=numeric(), rep=numeric(), simplelm=numeric(), loglm=numeric())

for (i in degreez){
   for (j in 1:sitez){
       yearly_expected_temp <- rep(6, yearz)
       daily_temp <- sapply(yearly_expected_temp, function(x) rnorm(daysperyr, basetemp + i, sigma)) 
       leafout_date <- sapply(1:ncol(daily_temp), function(x) min(which(cumsum(daily_temp[,x]) > fstar)))
       yearly_temp <- colMeans(daily_temp)
       plot(yearly_temp, leafout_date, pch=20)
       # yearly_temp_trunc <- sapply(1:ncol(daily_temp), function(x) mean(daily_temp[1:leafout_date[x], x]))
       dfadd <- data.frame(degwarm=i, rep=j, simplelm=coef(lm(leafout_date~yearly_temp))[2],
           loglm=coef(lm(log(leafout_date)~log(yearly_temp)))[2])
       df <- rbind(df, dfadd)
    }
}

plot(simplelm~degwarm, data=df, pch=16, ylab="Sensitivity (days/C or log(days)/log(C)", xlab="Degree warming")
points(loglm~degwarm, data=df, col="dodgerblue")

plot(abs(simplelm)~degwarm, data=df, col="lightgrey",
    ylab="Abs(Sensitivity (days/C or log(days)/log(C))", xlab="Degree warming")
df$degwarmJitter <- df$degwarm + 0.05
points(abs(loglm)~degwarmJitter, data=df, col="dodgerblue", cex=0.8)
