## Started 22 May 2019 ##
## Outside Philz coffee in Davis ##
## By Lizzie ##

## TO DO! ##
## I used variance here, but then did var/sqrt(n) for SE ... so need to think/check that!!! ##

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
# bp has mat from March 1st to June 1st
# bpalt  mat from Jan 1 to May 1 (but I thought it had mat until leafout also ...)
bp <- read.csv("output/betpen_allchillsandgdds_45sites_mat_forsims.csv", header=TRUE)
bpalt <- read.csv("output/betpen_allchillsandgdds_45sites_mat_tntx_forsims.csv", header=TRUE)

# Sims
pepsims <- read.csv("..//pep_sims/output/degwarmpepsims6CFstar150.csv",header=TRUE)
mean.pepsims <- aggregate(pepsims[c("diffbefore.after", "precc.sens", "postcc.sens",
    "var.lo.precc", "var.lo.postcc")], pepsims["degwarm"], FUN=mean)
sd.pepsims <- aggregate(pepsims[c("diffbefore.after", "precc.sens", "postcc.sens",
    "var.lo.precc", "var.lo.postcc")], pepsims["degwarm"], FUN=sd)

# loop to extract some model estimates
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
# should also get SE ...

#     cc      meanmat   varmat    sdmat meanmatlo varmatlo  sdmatlo   meanlo     varlo     sdlo meanutah  meangdd  matslope  matslopese
#1 1950-1960 5.365163 3.005094 1.731358  6.814883 1.363054 1.086849 113.8089 110.51111 10.25803 2246.987 68.70881 -4.534630   1.258845
#2 2000-2010 6.450939 1.251629 1.111780  6.615273 1.431603 1.152353 106.3356  46.95728  6.57374 2235.493 61.50754 -3.611025   1.579758

#      cc     meanmat    varmat  meanmatlo  varmatlo   meanlo    varlo meanutah  meangdd matslope
#1 1950-1960 0.8790317 0.3026431 0.8058748 1.191774 5.934599 51.70230 206.2212 17.52128 1.436585
#2 2000-2010 0.8904595 0.2879469 0.6957409 1.003430 4.613378 36.94105 131.0899 13.56985 1.289904


    
# estimate change in variance
1-meanhere$varlo[2]/meanhere$varlo[1]
1-mean.pepsims$var.lo.postcc[1]/mean.pepsims$var.lo.precc[1]

## Also get the diff to compare to sims

bpest.sitediffs <- data.frame(siteslist=numeric(), matdiff=numeric(), matlodiff=numeric(), diffslope=numeric())

for(i in c(1:length(sitez))){ # i <- 1
    subby <- subset(bpest, siteslist==sitez[i])
    precc <- subset(subby, cc=="1950-1960")
    postcc <- subset(subby, cc=="2000-2010")
    matdiff <- precc$meanmat-postcc$meanmat
    matlodiff <- precc$meanmatlo-postcc$meanmatlo
    diffslope <- precc$matslope-postcc$matslope
    bpest.sitediffs.add <- data.frame(siteslist=sitez[i], matdiff=matdiff,matlodiff=matlodiff, diffslope=diffslope)
    bpest.sitediffs <- rbind(bpest.sitediffs, bpest.sitediffs.add)
    }

bpest.sitediffs$daysperC <- bpest.sitediffs$diffslope/bpest.sitediffs$matdiff

mean(bpest.sitediffs$diffslope)
mean(bpest.sitediffs$matdiff)
mean(bpest.sitediffs$daysperC)
mean(bpest.sitediffs$matlodiff)
sd(bpest.sitediffs$diffslope)
sd(bpest.sitediffs$matdiff)
sd(bpest.sitediffs$matlodiff)
sd(bpest.sitediffs$daysperC)
sd(bpest.sitediffs$daysperC)/sqrt(45) # days per C mean SE bp
mean.pepsims$diffbefore.after[1]
sd.pepsims$diffbefore.after[1]/(sqrt(45)) # days per C mean SE pepsims

## alt dataset ...

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

cexhere <- 1.15
pdf(file.path("figures/peprealandsims.pdf"), width = 5, height = 4)
par(xpd=FALSE)
# par(mar=c(5,7,3,10))
plot(x=NULL,y=NULL, xlim=c(0.5,4.5), ylim=c(-3.1, -0.1),
     ylab="Change in estimated temperature sensitivity", xlab="Degree warming", main="")
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
