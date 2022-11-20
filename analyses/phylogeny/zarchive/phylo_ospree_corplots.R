## 14 November 2022 ##
## By Lizzie ##

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/phylogeny")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/phylogeny") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")


library(rstan)

mod <- readRDS("output/testme_deirdre_noout.rds")
modpost <- extract(mod)
quantile(modpost$sigma_y, probs = c(0.2, 0.5, 0.8))

# grab the quantiles (giving df cols generic names so we can switch it up)
# extract posteriors for species-level estimates
chillarray <- modpost$b_chill
forcearray <- modpost$b_force
photoarray <- modpost$b_photo
intarray <- modpost$a

# create dataframe to fill (ALERT: goes chill, force, photo, intercept)
lengthhere <- ncol(chillarray)
modquant <- data.frame(spnum=c(paste("sp", c(1:(4*lengthhere)), sep="")),
    cue=rep(c("chill", "force", "photo", "intercept"), each=lengthhere),
    qlow=rep(0, 4*lengthhere), qmid=rep(0, 4*lengthhere),
    qhigh=rep(0, 4*lengthhere))

arrays <- list(chillarray, forcearray, photoarray, intarray)

for(whicharray in c(1:length(arrays))){
    arrayhere <- arrays[[whicharray]]
    for(i in c(1:ncol(arrayhere))){
        quanthere <- quantile(arrayhere[,i], probs = c(0.2, 0.5, 0.8))
        if(whicharray==1){start <- 0}
        if(whicharray==2){start <- lengthhere}
        if(whicharray==3){start <- lengthhere*2}
        if(whicharray==4){start <- lengthhere*3}
        modquant$qlow[start+i] <- quanthere[[1]]
        modquant$qmid[start+i] <- quanthere[[2]]
        modquant$qhigh[start+i] <- quanthere[[3]]
    }
}

# not elegant, but reshaping is ugly too... 
chilldf <- subset(modquant, cue=="chill")
forcedf <- subset(modquant, cue=="force")
photodf <- subset(modquant, cue=="photo")
intdf <- subset(modquant, cue=="intercept")


# plots! First the cue x cue plots... 
par(mfrow=c(1,3))
plot(chilldf$qmid ~ forcedf$qmid)
abline(lm(chilldf$qmid ~ forcedf$qmid))
plot(chilldf$qmid ~ photodf$qmid)
abline(lm(chilldf$qmid ~ photodf$qmid))
plot(forcedf$qmid ~ photodf$qmid)
abline(lm(forcedf$qmid ~ photodf$qmid))

# plots! intercept by cue .. 
par(mfrow=c(1,3))
plot(chilldf$qmid ~ intdf$qmid)
abline(lm(chilldf$qmid ~ intdf$qmid))
plot(forcedf$qmid ~ intdf$qmid)
abline(lm(forcedf$qmid ~ intdf$qmid))
plot(photodf$qmid ~ intdf$qmid)
abline(lm(photodf$qmid ~ intdf$qmid))

# for my own learning, I add the error bars ...
# see also ... confidence intervals example in R.txt
par(mfrow=c(1,1))
myblack <- alpha("black", alpha = 0.25)
plot(chilldf$qmid ~ intdf$qmid, pch=16, col=myblack)
arrows(intdf$qmid, chilldf$qlow, intdf$qmid, chilldf$qhigh,
       code=3, length=0, col=myblack)
arrows(intdf$qlow, chilldf$qmid, intdf$qhigh, chilldf$qmid,
       code=3, length=0, col=myblack)
