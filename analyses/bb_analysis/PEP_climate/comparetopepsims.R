## Started 22 May 2019 ##
## Outside Philz coffee in Davis ##
## By Lizzie ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis/PEP_climate")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/PEP_climate")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis/PEP_climate") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/PEP_climate")
if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis/PEP_climate") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/PEP_climate")


# get some data
# Betula puendula data from PEP (both have has GDD from 1 Jan to leafout)
# bp has mat from March 1st to June 1st and mat.lo is 30 days before leafout (uses tg -- aka mean -- data from E-OBS)
# bpalt is similar, but calculated uses txtm -- aka min and max (and we caculate the mean ourselves from those values) -- data from E-OBS) ... we don't use this currently 
bp <- read.csv("output/betpen_allchillsandgdds_45sites_mat_forsims.csv", header=TRUE)
bpalt <- read.csv("output/betpen_allchillsandgdds_45sites_mat_tntx_forsims.csv", header=TRUE)

# Sims
pepsims <- read.csv("..//pep_sims/output/degwarmpepsims6CFstar150.csv",header=TRUE)
pepsims$varlodiffper <- pepsims$var.lo.postcc/pepsims$var.lo.precc
mean.pepsims <- aggregate(pepsims[c("diffbefore.after", "precc.sens", "postcc.sens",
    "var.lo.precc", "var.lo.postcc", "varlodiffper")], pepsims["degwarm"], FUN=mean)
median.pepsims <- aggregate(pepsims[c("diffbefore.after", "precc.sens", "postcc.sens",
    "var.lo.precc", "var.lo.postcc", "varlodiffper")], pepsims["degwarm"], FUN=median)
sd.pepsims <- aggregate(pepsims[c("diffbefore.after", "precc.sens", "postcc.sens",
    "var.lo.precc", "var.lo.postcc", "varlodiffper")], pepsims["degwarm"], FUN=sd)

# loop to extract some model estimates
# this takes mean for each time period then allows comparison acrosgs the two resulting values
bpest <- data.frame(siteslist=numeric(), cc=character(), meanmat=numeric(), varmat=numeric(),  
    sdmat=numeric(), meanlo=numeric(), varlo=numeric(), sdlo=numeric(), meanutah=numeric(), meangdd=numeric(), 
    matslope=numeric(), matslopese=numeric(), meanmatlo=numeric(), varmatlo=numeric(), sdmatlo=numeric())

sitez <- unique(bp$siteslist)

for(i in c(1:length(sitez))){ # i <- 1
    subby <- subset(bp, siteslist==sitez[i])
        for(ccstate in c(1:2)){
            subbycc <- subset(subby, cc==unique(bp$cc)[ccstate])
            meanmat <- mean(subbycc$mat, na.rm=TRUE)
            varmat <- var(subbycc$mat, na.rm=TRUE)
            sdmat <- sd(subbycc$mat, na.rm=TRUE)
            meanmatlo <- mean(subbycc$mat.lo, na.rm=TRUE)
            varmatlo <- var(subbycc$mat.lo, na.rm=TRUE)
            sdmatlo <- sd(subbycc$mat.lo, na.rm=TRUE)
            meanlo <- mean(subbycc$lo, na.rm=TRUE)
            varlo <- var(subbycc$lo, na.rm=TRUE)
            sdlo <- sd(subbycc$lo, na.rm=TRUE)
            meanutah <- mean(subbycc$chillutah, na.rm=TRUE)
            meangdd <- mean(subbycc$gdd, na.rm=TRUE)
            lmmat <- lm(lo~mat, data=subbycc)
            lmmatse <- summary(lmmat)$coef[2,2]
            bpestadd <- data.frame(siteslist=sitez[i], cc=unique(bp$cc)[ccstate], meanmat=meanmat, 
                varmat=varmat, sdmat=sdmat, meanlo=meanlo, varlo=varlo, sdlo=sdlo, meanutah=meanutah, 
                meangdd=meangdd, matslope=coef(lmmat)["mat"], matslopese=lmmatse, meanmatlo=meanmatlo,
                varmatlo=varmatlo, sdmatlo=sdmatlo)
            bpest <- rbind(bpest, bpestadd)
        }
}    

meanhere <- aggregate(bpest[c("meanmat", "varmat", "sdmat", "meanmatlo", "varmatlo", "sdmatlo", "meanlo", "varlo", "sdlo", "meanutah", "meangdd",
    "matslope", "matslopese")], bpest["cc"], FUN=mean)
sdhere <- aggregate(bpest[c("meanmat", "varmat", "meanmatlo", "varmatlo", "meanlo", "varlo", "meanutah", "meangdd", "matslope")],
    bpest["cc"], FUN=sd)


#          cc  meanmat   varmat    sdmat meanmatlo varmatlo  sdmatlo   meanlo     varlo     sdlo meanutah  meangdd  matslope matslopese
# 1950-1960 5.365163 3.005094 1.731358  6.814883 1.363054 1.086849 113.8089 110.51111 10.25803 2246.987 68.70881 -4.534630   1.258845
# 2000-2010 6.450939 1.251629 1.111780  6.615273 1.431603 1.152353 106.3356  46.95728  6.57374 2235.493 61.50754 -3.611025   1.579758


## Also get the difference for each site across two time periods
# This is to compare to sims better

bpest.sitediffs <- data.frame(siteslist=numeric(), matdiff=numeric(), matlodiff=numeric(), diffslope=numeric(),
    varlodiff=numeric(), varlodiffper=numeric(), varmatdiffper=numeric())

for(i in c(1:length(sitez))){ # i <- 1
    subby <- subset(bpest, siteslist==sitez[i])
    precc <- subset(subby, cc=="1950-1960")
    postcc <- subset(subby, cc=="2000-2010")
    matdiff <- precc$meanmat-postcc$meanmat
    matlodiff <- precc$meanmatlo-postcc$meanmatlo
    diffslope <- precc$matslope-postcc$matslope
    varlodiff <- precc$varlo-postcc$varlo
    varlodiffper <- postcc$varlo/precc$varlo
    varmatdiffper <- postcc$varmat/precc$varmat
    bpest.sitediffs.add <- data.frame(siteslist=sitez[i], matdiff=matdiff,matlodiff=matlodiff, diffslope=diffslope,
        varlodiff=varlodiff, varlodiffper=varlodiffper, varmatdiffper=varmatdiffper)
    bpest.sitediffs <- rbind(bpest.sitediffs, bpest.sitediffs.add)
    }

bpest.sitediffs$daysperC <- bpest.sitediffs$diffslope/bpest.sitediffs$matdiff

########################
## Stuff in the supp ##
########################

# estimate change in variance
# take the means, then calculates the percent change between pre and post cc 
1-meanhere$varlo[2]/meanhere$varlo[1]
1-mean.pepsims$var.lo.postcc[1]/mean.pepsims$var.lo.precc[1]
1-median.pepsims$var.lo.postcc[1]/median.pepsims$var.lo.precc[1]

# in supp 'we estimated a decline in sensitivity'
mean(bpest.sitediffs$daysperC)
sd(bpest.sitediffs$daysperC)/sqrt(45) # days per C mean SE bp

# in supp 'given X warming'
mean(bpest.sitediffs$matdiff)
mean(bpest.sitediffs$matdiff)/sqrt(45)

# compare to pep sims (in figure, not text in supp currently)
mean.pepsims$diffbefore.after[1]
sd.pepsims$diffbefore.after[1]/(sqrt(45)) # days per C mean SE pepsims

# Utah units
meanhere$meanutah
sdhere$meanutah/sqrt(45)

# gdd and matlo
meanhere$meangdd
sdhere$meangdd/sqrt(45)
meanhere$meanmatlo
sdhere$meanmatlo/sqrt(45)

##############################
## End of stuff in the supp ##
##############################

# variance! here's what you get if you calculate the % change site-by-site then average
1 - mean(bpest.sitediffs$varlodiffper)
sd(bpest.sitediffs$varlodiffper)/sqrt(45)
1 - mean.pepsims$varlodiffper[1]
sd.pepsims$varlodiffper[1]/(sqrt(45))

# closer look at variance differences due to when we take the mean ...
# when we take the diff at each site, we exacerbate outliers, this explains (I think) the difference between the two ways we calculate variance
pepsims.1d <- subset(pepsims, degwarm==1)
hist(pepsims.1d$varlodiff)
hist(pepsims.1d$var.lo.precc)
hist(pepsims.1d$var.lo.postcc)

# other stuff ... (not using currently)
mean(bpest.sitediffs$diffslope)
sd(bpest.sitediffs$diffslope)/sqrt(45)
mean(bpest.sitediffs$matlodiff)
sd(bpest.sitediffs$matdiff)
sd(bpest.sitediffs$matlodiff)
sd(bpest.sitediffs$daysperC)

#####################
## alt dataset ... ##
#####################

# loop to extract some model estimates
bpaltest <- data.frame(siteslist=numeric(), cc=character(), meanmat=numeric(), varmat=numeric(),
    sdmat=numeric(), meanlo=numeric(), varlo=numeric(), sdlo=numeric(), meanutah=numeric(), meangdd=numeric(), 
    matslope=numeric(), matslopese=numeric(), meanmatlo=numeric(), varmatlo=numeric(), sdmatlo=numeric())

sitez <- unique(bpalt$siteslist)

for(i in c(1:length(sitez))){ # i <- 1
    subby <- subset(bpalt, siteslist==sitez[i])
        for(ccstate in c(1:2)){
            subbycc <- subset(subby, cc==unique(bpalt$cc)[ccstate])
            meanmat <- mean(subbycc$mat, na.rm=TRUE)
            varmat <- var(subbycc$mat, na.rm=TRUE)
            sdmat <- sd(subbycc$mat, na.rm=TRUE)
            meanmatlo <- mean(subbycc$mat.lo, na.rm=TRUE)
            varmatlo <- var(subbycc$mat.lo, na.rm=TRUE)
            sdmatlo <- sd(subbycc$mat.lo, na.rm=TRUE)
            meanlo <- mean(subbycc$lo, na.rm=TRUE)
            varlo <- var(subbycc$lo, na.rm=TRUE)
            sdlo <- sd(subbycc$lo, na.rm=TRUE)
            meanutah <- mean(subbycc$chillutah, na.rm=TRUE)
            meangdd <- mean(subbycc$gdd, na.rm=TRUE)
            lmmat <- lm(lo~mat.lo, data=subbycc)
            lmmatse <- summary(lmmat)$coef[2,2]
            bpaltestadd <- data.frame(siteslist=sitez[i], cc=unique(bpalt$cc)[ccstate], meanmat=meanmat, 
                varmat=varmat, sdmat=sdmat, meanlo=meanlo, varlo=varlo, sdlo=sdlo, meanutah=meanutah, 
                meangdd=meangdd, matslope=coef(lmmat)["mat.lo"], matslopese=lmmatse, meanmatlo=meanmatlo,
                varmatlo=varmatlo, sdmatlo=sdmatlo)
            bpaltest <- rbind(bpaltest, bpaltestadd)
        }
}    

meanhere.alt <- aggregate(bpaltest[c("meanmat", "varmat", "sdmat", "meanmatlo", "varmatlo", "sdmatlo", "meanlo", "varlo", "sdlo",  "meanutah", "meangdd", 
    "matslope", "matslopese")], bpaltest["cc"], FUN=mean)
sdhere.alt <- aggregate(bpaltest[c("meanmat", "varmat", "meanmatlo", "varmatlo", "meanlo", "varlo", "meanutah", "meangdd", "matslope")],
    bpaltest["cc"], FUN=sd)

#      cc     meanmat    varmat     sdmat   meanmatlo varmatlo  sdmatlo   meanlo     varlo     sdlo  meanutah  meangdd  matslope matslopese
#1 1950-1960 0.6981628 2.2788545 1.5055267  1.865197 1.105066 1.001733 113.8089 110.51111 10.25803   1710.267 7.467982 0.7854042   3.649336
#2 2000-2010 1.6117745 0.6948032 0.8258134  1.815321 1.357658 1.141949 106.3356  46.95728  6.57374  1935.600 5.639006 2.1208607   1.872305

if(FALSE){
library(ggplot2)
ggplot(bpalt, aes(x=mat.lo, y=lo)) +
   geom_point() +
   geom_smooth(method=lm) + facet_wrap(~siteslist)
}

##############
## Plotting ##
##############

cexhere <- 0.95
pdf(file.path("figures/peprealandsims.pdf"), width = 6, height = 4)
par(xpd=FALSE)
par(mar=c(5,5,2,2))
plot(x=NULL,y=NULL, xlim=c(0.5,4.5), ylim=c(-3.1, -0.1),
     ylab=expression(paste("Change in estimated sensitivity (days/", degree, "C)"), sep=""),
         xlab=expression(paste("Warming (", degree, "C)")), main="")
# abline(h=0, lty=2, col="darkgrey")
for(i in 1:4){
  pos.x <- mean.pepsims$degwarm[i]
  pos.y <- mean.pepsims$diffbefore.after[i]
  sehere <- sd.pepsims$diffbefore.after[i]/(sqrt(45))
  lines(x=rep(pos.x, 2), y=c(pos.y-sehere, pos.y+sehere), col="darkblue")
  points(pos.x, pos.y, cex=cexhere, pch=19, col="darkblue")
  }
realdat.diff <- mean(bpest.sitediffs$diffslope) 
points(abs(mean(bpest.sitediffs$matdiff)), realdat.diff, cex=cexhere, pch=17, col="salmon")
realdatse <- sd(bpest.sitediffs$diffslope)/sqrt(45)
lines(x=rep(abs(mean(bpest.sitediffs$matdiff)), 2), y=c(realdat.diff-realdatse, realdat.diff+realdatse),
    col="salmon")
# par(xpd=TRUE) # so I can plot legend outside
legend("topright", pch=c(17, 19), col=c("salmon", "darkblue"), legend=c("European data (PEP725)", "Simulations with constant cues"),
   cex=1, bty="n")
dev.off()
