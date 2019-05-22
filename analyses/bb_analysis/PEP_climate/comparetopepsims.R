## Started 22 May 2019 ##
## Outside Philz coffee in Davis ##
## By Lizzie ##
#

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
