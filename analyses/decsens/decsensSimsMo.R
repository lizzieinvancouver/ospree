## Started 28 December 2019 ##
## By Lizzie ##

## Simulations of winter and spring temperature ... 120 day winter + spring season

# 6. Calculate GDD (days 31 onward) and chilling (first 90 days)
# 7. Select some range of high chill where same GDD is needed for budburst, then set up a linear relationship between chill (at lower range) and GDD required (more GDD for lower chill)
# 8. Predict GDD based on simulated weather and 7.

# Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
daysperyr <- 60
yearz <- 30
sitez <- 45 # reps
degreez <- c(0, 0.5, 1, 2, 4, 7)
sigma <- 4
basetemp <- 6
fstar <- 150

# Step 2: Build the data and calculate sensitivities
df <- data.frame(degwarm=numeric(), rep=numeric(), simplelm=numeric(), loglm=numeric(), perlm=numeric())

for (i in degreez){
   for (j in 1:sitez){
       yearly_expected_temp <- rep(6, yearz)
       daily_temp <- sapply(yearly_expected_temp, function(x) rnorm(daysperyr, basetemp + i, sigma)) 
       leafout_date <- sapply(1:ncol(daily_temp), function(x) min(which(cumsum(daily_temp[,x]) > fstar)))
       yearly_temp <- colMeans(daily_temp)
       per_leafout_date <- leafout_date/mean(leafout_date)
       per_yearly_temp <- yearly_temp/mean(yearly_temp)
       plot(yearly_temp, leafout_date, pch=20)
       # yearly_temp_trunc <- sapply(1:ncol(daily_temp), function(x) mean(daily_temp[1:leafout_date[x], x]))
       dfadd <- data.frame(degwarm=i, rep=j, simplelm=coef(lm(leafout_date~yearly_temp))[2],
           loglm=coef(lm(log(leafout_date)~log(yearly_temp)))[2],
           perlm=coef(lm(per_leafout_date~per_yearly_temp))[2])
       df <- rbind(df, dfadd)
    }
}

##
##

# First, build up some simulated climate and leafout (after 200 GDD) data
yearly_expected_temp <- c(rep(0,20), rep(5,20), rep(10,20))

# Temp below is 60 days with base of -5, 15 days at -2.5, 15 days at 2.5 and 30 days at 5
daily_temp <- sapply(yearly_expected_temp, function(x) c(rnorm(60, -5 + x, 4), rnorm(15, -2.5 + x, 4), rnorm(15, 2.5 + x, 4), rnorm(30, 5 + x, 4)))

gdd <- daily_temp[30:nrow(daily_temp),]
gdd[(gdd)<0] <- 0

leafout_date <- sapply(1:ncol(daily_temp), function(x) min(which(cumsum(daily_temp[,x]) > 200)))
yearly_temp <- colMeans(daily_temp)

yearly_temp_trunc <- sapply(1:ncol(daily_temp), function(x) mean(spring_daily_temp[1:leafout_date[x], x]))

plot(yearly_temp, leafout_date, pch=20)
points(yearly_temp_trunc, leafout_date, pch=20, col = "red")

plot(yearly_temp_trunc, leafout_date, pch=20, col = "red")

# Lizzie added below ... 
plot(log(yearly_temp_trunc), log(leafout_date), pch=20, col = "dodgerblue")
