##Started by Dan B on 5 Sept 2019 and by no way opporational yet.
# the goal is to understand the beta distibution and simulate % budburst data with it
# It still seems to be liek % budburst over time is fundamentally different
#than the "coin flip models" the beta distrubution seems to usally deal with.
rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()
library(rstanarm)
library("rBeta2009")

### Poorly commented exaomple from:
#https://cran.r-project.org/web/packages/rstanarm/vignettes/betareg.html
#not sure how we translate these parementers to make the ospreeish
eta <- c(1, -0.2)
gamma <- c(1.8, 0.4)
N <- 200
x <- rnorm(N, 2, 2)
z <- rnorm(N, 0, 2)
mu <- binomial(link = logit)$linkinv(eta[1] + eta[2]*x)
phi <- binomial(link = log)$linkinv(gamma[1] + gamma[2]*z)
y <- rbeta(N, mu * phi, (1 - mu) * phi)
dat <- data.frame(cbind(y, x, z))
hist(dat$y, col = "darkgrey", border = F, main = "Distribution of Outcome Variable", xlab = "y", breaks = 20, freq = F)

## Another more intuative but less computationally rigorous
#Can we take the analogy of baseball?
#http://varianceexplained.org/statistics/beta_distribution_and_baseball/
##lets sat we expect there is a 50% chance a bud will burst acheived by a=10 and b=10
### probablility shouyld increase...but not sure how the b parameter should change

b<-10
a<-10
slope<-2
time<-seq(1:20)

for (i in c(length(time))) {
  goober<-rbeta(1000,a+slope*time[i],b)
}

hist(rbeta(1000,10,10),col="red",xlim=c(0,1))
hist(goober,col="blue", add=TRUE) # here we see the distribution does change for te positive

##below is still broken
df<-data.frame(time=numeric(), y=numeric())
for (i in c(length(time))){
 a<-slope*time[i]+intercept
dfhere <- data.frame(time=time, y=rbeta(length(a),a[i],b))
  df <- rbind(df, dfhere)
                     }



