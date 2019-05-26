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
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis/pep_climate")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_climate")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis/pep_sims") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis/pep_climate")

# get some data
bp <- read.csv("output/betpen_allchillsandgdds_45sites_mat.csv", header=TRUE)

# loop to extract some model estimates
bpest <- data.frame(siteslist=numeric(), cc=character(), meanmat=numeric(), varmat=numeric(),
    meanlo=numeric(), varlo=numeric(), meanutah=numeric(), meangdd=numeric(), matslope=numeric())

sitez <- unique(bp$siteslist)

for(i in c(1:length(sitez))){ # i <- 1
    subby <- subset(bp, siteslist==sitez[i])
        for(ccstate in c(1:2)){
            subbycc <- subset(subby, cc==unique(bp$cc)[ccstate])
            meanmat <- mean(subbycc$mat, na.rm=TRUE)
            varmat <- var(subbycc$mat, na.rm=TRUE)
            meanlo <- mean(subbycc$lo, na.rm=TRUE)
            varlo <- var(subbycc$lo, na.rm=TRUE)
            meanutah <- mean(subbycc$chillutah, na.rm=TRUE)
            meangdd <- mean(subbycc$gdd, na.rm=TRUE)
            lmmat <- lm(lo~mat, data=subbycc)
            bpestadd <- data.frame(siteslist=sitez[i], cc=unique(bp$cc)[ccstate], meanmat=meanmat, 
                varmat=varmat, meanlo=meanlo, varlo=varlo, meanutah=meanutah, meangdd=meangdd,
                matslope=coef(lmmat)["mat"])
            bpest <- rbind(bpest, bpestadd)
        }
}    

meanhere <- aggregate(bpest[c("meanmat", "varmat", "meanlo", "varlo", "meanutah", "meangdd", "matslope")],
    bpest["cc"], FUN=mean)
sdhere <- aggregate(bpest[c("meanmat", "varmat", "meanlo", "varlo", "meanutah", "meangdd", "matslope")],
    bpest["cc"], FUN=sd)
# should also get SE ...

#          cc  meanmat   varmat   meanlo     varlo meanutah  meangdd  matslope
# 1 1950-1960 5.365163 3.005094 113.8089 110.51111 2246.987 81.10503 -4.534630
# 2 2000-2010 6.450939 1.251629 106.3356  46.95728 2235.493 70.27807 -3.611025

unique(bpest$siteslist)

## Also get the diff to compare to sims

bpest.sitediffs <- data.frame(siteslist=numeric(), matdiff=numeric(), diffslope=numeric())

for(i in c(1:length(sitez))){ # i <- 1
    subby <- subset(bpest, siteslist==sitez[i])
    precc <- subset(subby, cc=="1950-1960")
    postcc <- subset(subby, cc=="2000-2010")
    matdiff <- precc$meanmat-postcc$meanmat
    diffslope <- precc$matslope-postcc$matslope
    bpest.sitediffs.add <- data.frame(siteslist=sitez[i], matdiff=matdiff, diffslope=diffslope)
    bpest.sitediffs <- rbind(bpest.sitediffs, bpest.sitediffs.add)
    }
    
mean(bpest.sitediffs$diffslope)
mean(bpest.sitediffs$matdiff)
sd(bpest.sitediffs$diffslope)
sd(bpest.sitediffs$matdiff)

##############
## Plotting ##
##############

pepsims <- read.csv("..//pep_sims/output/degwarmpepsims6CFstar150.csv",header=TRUE)
mean.pepsims <- aggregate(pepsims[c("diffbefore.after", "precc.sens", "postcc.sens",
    "var.lo.precc", "var.lo.postcc")], pepsims["degwarm"], FUN=mean)
sd.pepsims <- aggregate(pepsims[c("diffbefore.after", "precc.sens", "postcc.sens",
    "var.lo.precc", "var.lo.postcc")], pepsims["degwarm"], FUN=sd)


cexhere <- 1.25
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
legend("topright", pch=c(17, 19), col=c("salmon", "darkblue"), legend=c("European data", "Simulations"),
   cex=1, bty="n")
dev.off()
