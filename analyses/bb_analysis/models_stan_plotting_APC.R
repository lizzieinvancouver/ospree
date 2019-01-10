## Started 3 Jan 2019 ##
## By Lizzie (to start) ##

## Marginal effects from Stan models ##
## Based off models_stan_plotting.R ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("~/Documents/Github/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


figpath <- "figures"

## set up the flags
use.chillports = TRUE
use.zscore = FALSE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

##
source("source/bbstanleadin.R")
##

# Set up colors (more than used currently ...
library(RColorBrewer)
cols <- adjustcolor(c("maroon4", "lightskyblue"), alpha.f = 0.8) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

# non-z-scored models
if(use.zscore==FALSE){
load("stan/output/m2lni_spcompexprampfp_nonz.Rda") # m2l.ni
load("stan/output/m2lnib_spcompexprampfp_nonz.Rda") # m2l.nib
modelhere <- m2l.ni
}

hist(bb.stan$chill.ports)
rownameshere <- c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp")

# Let's pretend that average spring temp is 5 and average chill portions is 140
# And, let's pretend 2 deg warming decreases chilling by 30 portions (I made this up)
sprtemp <- 5
chillport <- 140
daylength <- 14
warmspring <- 2
warmwinter <- -30


## Plotting
# First, we estimate the posteriors for each thing we want to plot...
fit <- m2l.ni

list_of_draws <- extract(fit)
print(names(list_of_draws))
str(list_of_draws$mu_a_sp)


getest.bb <- function(fit, sprtemp, daylength, chillport, warmspring, warmwinter,
    daylengthwarmspr, daylengthwarmwin, daylengthwarmsprwin){
    listofdraws <- extract(fit)
    avgbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*sprtemp + 
        listofdraws$mu_b_photo_sp*daylength + listofdraws$mu_b_chill_sp*chillport
    warmsprbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*(sprtemp+warmspring) + 
        listofdraws$mu_b_photo_sp*(daylength + daylengthwarmspr) + listofdraws$mu_b_chill_sp*chillport
    warmwinbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*sprtemp + 
        listofdraws$mu_b_photo_sp*(daylength + daylengthwarmwin) + listofdraws$mu_b_chill_sp*(chillport+warmwinter)
    warmsprwinbb <- listofdraws$mu_a_sp + listofdraws$mu_b_force_sp*(sprtemp+warmspring) +
        listofdraws$mu_b_photo_sp*(daylength + daylengthwarmsprwin) + listofdraws$mu_b_chill_sp*(chillport+warmwinter)
    yebbest <- list(avgbb, warmsprbb, warmwinbb, warmsprwinbb)
    return(yebbest)
}

# NOTE: I believe quantile is fine for extracting probabilities, see https://discourse.mc-stan.org/t/reporting-credible-confidence-intervals/2262
bbposteriors <- getest.bb(m2l.ni, sprtemp, daylength, chillport, warmspring, warmwinter, 0, 0, 0)
meanz <- unlist(lapply(bbposteriors, mean))
quantz <- lapply(bbposteriors, function(x) quantile(x,  c(0.25, 0.5, 0.75)))
quant25per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.25))))
quant75per <- unlist(lapply(bbposteriors, function(x) quantile(x,  c(0.75))))

# Some cheap stuff to add in photo effects
# Assume 14 hr day, which is Apr 17 in Berlin
# I looked up changes in table online, but we could and should AUTOMATE)
meanz[2]-meanz[1] # 1.5 days earlier ... no real change
meanz[3]-meanz[1] # 7 days later ... 0.5 longer day
meanz[4]-meanz[1] # 6 days later ... 0.5 longer day

bbposteriors.wdaylength <- getest.bb(m2l.ni, sprtemp, daylength, chillport, warmspring, warmwinter, 0, 0.5, 0.5)
meanz.wdaylength <- unlist(lapply(bbposteriors.wdaylength, mean))
quant25per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.25))))
quant75per.wdaylength <- unlist(lapply(bbposteriors.wdaylength, function(x) quantile(x,  c(0.75))))

xlim = c(0, 5)
ylim = c(5, 40)
par(mar=c(8,7,3,5))
plot(x=NULL,y=NULL, xlim=xlim, xaxt='n', ylim=ylim,
     ylab="Days to BB", xlab="", main="14 hour photo")
axis(1, at=1:4, labels=c("avg", "spr warming", "winter warming",
    "both warming"), las=2)
  pos.x <- (1:4)
  pos.y <- meanz
  points(pos.x, pos.y, cex=1.5, pch=19, col=cols[2])
for(i in 1:4){
  lines(c(pos.x[i], pos.x[i]), c(quant25per[i], quant75per[i]), 
      col=cols[2], lwd=2)
  }
  pos.y.photo <- meanz.wdaylength
  points(pos.x, pos.y.photo, cex=1.5, pch=19, col=cols[1])
for(i in 1:4){
  lines(c(pos.x[i], pos.x[i]), c(quant25per.wdaylength[i], quant75per.wdaylength[1]), 
      col=cols[1], lwd=1)
  }



