## Started 15 February 2021 ##
## By Lizzie ##

## See also models_stan_pepspp.R and betpenexp.R (in decsens) ##
## And GDD_plotting, which has not been updated since 2017 and is missing heide03 ...
# so I did not use (notes about this in issue 398). ##

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(ggplot2)
library(viridis)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else
 setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# get the data from ospree repo
osp <- read.csv("..//output/ospree_clean_withchill_BB.csv", header = TRUE)
dall <- subset(osp, datasetID!="junttila12") # removing junttilla which I noted is a dormancy release study (might be others that I did not remove!)
source("..//source/commoncols.R")
d <- dall[,which(colnames(dall) %in% c(common.cols.wchill, "forcetemp_night", "photoperiod_night"))]
d$datasetstudy <- paste(d$datasetID, d$study)
d$forceday <- as.numeric(d$forcetemp)
d$forcenight <- as.numeric(d$forcetemp_night)
d$photonight <- as.numeric(d$photoperiod_night)

d$photo <- as.numeric(d$photoperiod_day)
d$force <- d$forceday
# Adjust forcing temperature to incorporate day/night temp differentials, if present
d$force[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
    is.na(d$photonight)==FALSE] <-
    (d$forceday[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
    is.na(d$photonight)==FALSE]*
    d$photo[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
    is.na(d$photonight)==FALSE] +
    d$forcenight[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
    is.na(d$photonight)==FALSE]*
    d$photonight[is.na(d$forcenight)==FALSE & is.na(d$photo)==FALSE &
    is.na(d$photonight)==FALSE])/24

# quick calculate GDD
d$resp <- as.numeric(d$response.time)
d$gddreq <- d$force * d$resp

# plot
# wheee!!! this first one is big!
pdf(file.path("..//limitingcues/figures/gddbyutah_all.pdf"), width = 20, height = 10)
ggplot(subset(d, Total_Utah_Model>0), aes(Total_Utah_Model, gddreq, col=as.factor(photo))) +
    geom_point() + facet_wrap(datasetstudy ~ ., scales="free")
dev.off()

relevstud <- c("cronje03 exp1", "sonsteby14 exp1", "thielges75 exp1") # which are apples (color code by chill days?), black currant (color code by prov lat?) and Populus deltoides, respectively

plotme <- d[which(d$datasetstudy %in% relevstud),]
unique(plotme$datasetstudy)

pdf(file.path("..//limitingcues/figures/gddbyutah.pdf"), width = 10, height = 4)
ggplot(plotme, aes(Total_Utah_Model, gddreq, col=as.factor(photo))) +
    geom_point() +
    # geom_smooth(method = "lm", linetype = 2, lwd=0.5, color="darkgray", se = FALSE) +
    facet_wrap(.~as.factor(datasetID), scales="free") +
    # geom_text(color="dodgerblue", size=3, data=bpsummr2, aes(x = 57, y = 12, label = r2)) +
    xlab("total chilling (in Utah units)") +
    ylab(expression(paste("GDD until budburst (in ", degree, "C)"), sep="")) +
    theme_minimal()
dev.off()


plotme1 <- d[which(d$datasetstudy %in% relevstud[1]),] # chilldays is a mess -- not sure it's best to include
plotme2 <- d[which(d$datasetstudy %in% relevstud[2]),]
plotme3 <- d[which(d$datasetstudy %in% relevstud[3]),]

plotme2 <- plotme2[with(plotme2, order(provenance.lat)),]

colz <- viridis(6, alpha = 0.5)
pchhere <- 19
ylabhere <- expression(paste("GDD until budburst (in ", degree, "C)"), sep="")
xlabhere <- "Total chilling (in Utah model units)"

pdf(file.path("..//limitingcues/figures/gddbyutahpretty.pdf"), width = 8, height = 4)
par(mfrow=c(1,2))
plot(gddreq ~ Total_Utah_Model, data=plotme2, xlab=xlabhere, ylab=ylabhere)
mtext(expression(paste("(a)",  italic("Ribes nigrum"), sep=" ")), side=3, adj = 0) 
points(gddreq ~ Total_Utah_Model, data=subset(plotme2, provenance.lat==unique(plotme2$provenance.lat)[1]),
       col=colz[6], pch=pchhere)
points(gddreq ~ Total_Utah_Model, data=subset(plotme2, provenance.lat==unique(plotme2$provenance.lat)[2]),
       col=colz[5], pch=pchhere)
points(gddreq ~ Total_Utah_Model, data=subset(plotme2, provenance.lat==unique(plotme2$provenance.lat)[3]),
       col=colz[4], pch=pchhere)
points(gddreq ~ Total_Utah_Model, data=subset(plotme2, provenance.lat==unique(plotme2$provenance.lat)[4]),
       col=colz[3], pch=pchhere)
points(gddreq ~ Total_Utah_Model, data=subset(plotme2, provenance.lat==unique(plotme2$provenance.lat)[5]),
       col=colz[2], pch=pchhere)
legend(x=2300, y=480, pch=rep(pchhere, 5), col=colz[6:2], legend=unique(round(plotme2$provenance.lat, 0)),
   cex=0.75, bty="n", title="provenance \n latitude")
plot(gddreq ~ Total_Utah_Model, data=plotme3, xlab=xlabhere, ylab=ylabhere)
points(gddreq ~ Total_Utah_Model, data=plotme3, pch=pchhere,  col=colz[1])
mtext(expression(paste("(b)", italic("Populus deltoides"), sep=" ")), side=3, adj = 0) 
dev.off()

