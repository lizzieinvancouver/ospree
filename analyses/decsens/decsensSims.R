## Started 18 December 2019 ##
## Code by Jonathan Auerbach (and Andrew Gelman some) ##
## Notes by Lizzie ##

## Quick simulation of the declining sensitivities problem ##

# First, build up some simulated climate and leafout (after 200 GDD) data
yearly_expected_temp <- c(rep(0,20), rep(5,20), rep(10,20))
daily_temp <- sapply(yearly_expected_temp, function(x) rnorm(30, 10 + x, 4))
# daily_temp <- sapply(yearly_expected_temp, function(x) rnorm(30, 10 + x, (4+x))) # Lizzie added
leafout_date <- sapply(1:ncol(daily_temp), function(x) min(which(cumsum(daily_temp[,x]) > 200)))
yearly_temp <- colMeans(daily_temp)

yearly_temp_trunc <- sapply(1:ncol(daily_temp), function(x) mean(daily_temp[1:leafout_date[x], x]))

plot(yearly_temp, leafout_date, pch=20)
points(yearly_temp_trunc, leafout_date, pch=20, col = "red")

plot(yearly_temp_trunc, leafout_date, pch=20, col = "red")

# Lizzie added below ... 
plot(log(yearly_temp_trunc), log(leafout_date), pch=20, col = "dodgerblue")
