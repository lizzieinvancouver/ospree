### Using data from betpen_climate_prep.R make boxplots showing differences in PEP leafout data across sites
### 20 November 2018

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(egg)
library(RColorBrewer)

setwd("~/Documents/git/ospree/analyses/bb_analysis/PEP_climate")

betpen <- read.csv("output/betpen_matportutah.csv", head=TRUE)

if(FALSE){
se <- function(x,y) (x)/sqrt(y)

betpen$mat.sd <- ave(betpen$mat, betpen$lat.long, FUN=sd)
betpen$mat.y <- ave(betpen$mat, betpen$lat.long, FUN=length)
betpen$mat.se <- se(betpen$mat.sd, betpen$mat.y)
betpen$port.sd <- ave(betpen$chillport, betpen$lat.long, FUN=sd)
betpen$port.y <- ave(betpen$chillport, betpen$lat.long, FUN=length)
betpen$port.se <- se(betpen$port.sd, betpen$port.y)
betpen$utah.sd <- ave(betpen$chillutah, betpen$lat.long, FUN=sd)
betpen$utah.y <- ave(betpen$chillutah, betpen$lat.long, FUN=length)
betpen$utah.se <- se(betpen$utah.sd, betpen$utah.y)
}

mat.boxies <- ggplot(betpen, aes(y=mat)) + geom_boxplot(aes(y=mat, x=as.character(siteslist), col=cc)) + 
  theme_classic() + labs(y="Mean Spring Temperature (°C)", x="Sites") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position="none") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) 

port.boxies <- ggplot(betpen, aes(y=chillport)) + geom_boxplot(aes(y=chillport, x=as.character(siteslist), col=cc)) + 
  theme_classic() + labs(y="Total Chill Portions", x="Sites") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position ="none") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) 

utah.boxies <- ggplot(betpen, aes(y=chillutah)) + geom_boxplot(aes(y=chillutah, x=as.character(siteslist), col=cc)) + 
  theme_classic() + labs(y="Total Utah Chill", x="Sites") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) 


quartz()
grid.arrange(mat.boxies, port.boxies, utah.boxies, ncol=3, widths=c(1,1,1.5))


####################################################################################
######################## NOW TRY DIFFERNT RUNS! ####################################
####################################################################################

## 26 February 2019 - a lot of missing data from the multiple runs dataframe... 

bpmult <- read.csv("output/betpen_allchills.csv", head=TRUE)


### chillport1 and mat1
mat1.boxies <- ggplot(bpmult[!is.na(bpmult$mat),], aes(y=mat)) + geom_boxplot(aes(y=mat, x=as.character(siteslist), col=cc)) + 
  theme_classic() + labs(y="Mean Spring Temperature (°C)", x="Sites") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position="none") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) 

port1.boxies <- ggplot(bpmult[!is.na(bpmult$chillport1),], aes(y=chillport1)) + geom_boxplot(aes(y=chillport1, x=as.character(siteslist), col=cc)) + 
  theme_classic() + labs(y="Total Chill Portions", x="Sites") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) 

quartz()
grid.arrange(mat1.boxies, port1.boxies, ncol=2, widths=c(1,1.5))




bpmult.utah <- read.csv("output/betpen_allchills_utah.csv", head=TRUE)
### chillutah1 and mat1

mat.boxies <- ggplot(betpen, aes(y=mat)) + geom_boxplot(aes(y=mat, x=as.character(siteslist), col=cc)) + 
  theme_classic() + labs(y="Mean Spring Temperature (°C)", x="Sites") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position="none") +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) 

utah.boxies <- ggplot(betpen, aes(y=chillutah)) + geom_boxplot(aes(y=chillutah, x=as.character(siteslist), col=cc)) + 
  theme_classic() + labs(y="Total Utah Chill", x="Sites") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_color_manual(name="Years", values=c(apre = "darkblue", 
                                            post = "darkred"),
                     labels=c(apre = "1950-1960",
                              post = "2000-2010")) 


quartz()
grid.arrange(mat.boxies, port.boxies, utah.boxies, ncol=3, widths=c(1,1,1.5))




