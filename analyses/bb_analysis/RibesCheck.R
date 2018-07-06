## Checking out what's happening with Ribes 
# Cat - 6 July 2018

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

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
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

library(brms)
library(rstan)
library(egg)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source/bbstanleadin.R")

spp<-c("Ribes_nigrum", "Fraxinus_excelsior")
ribs<-subset(bb.stan, complex.wname%in%spp)

hist(bb.stan$force)
hist(ribs$force[ribs$complex.wname=="Ribes_nigrum"])
hist(ribs$force[ribs$complex.wname=="Fraxinus_excelsior"])

dataset<-subset(ribs, select=c(datasetID, force))
dataset<-dataset[!duplicated(dataset),]

f<-ggplot(ribs, aes(x=force, y=resp, col=complex.wname)) + geom_point(data=ribs, aes(col=complex.wname)) +
  scale_color_manual(values=c("red4", "blue3"), labels=c("Ribes_nigrum", "Fraxinus_excelsior"), name="") +
  annotate("text", x=11, y=45, label="basler12", col="red4") +
  annotate("text", x=22, y=50, label="pagter15", col="blue3") +
  annotate("text", x=22, y=17, label="sonsteby14", col="blue3") +
  annotate("text", x=11, y=8, label="heide12", col="blue3") +
  annotate("text", x=25, y=30, label="laube14a", col="red4") +
  annotate("text", x=22, y=42, label="jones12", col="blue3")
  
p<-ggplot(ribs, aes(x=photo, y=resp, col=complex.wname)) + geom_point(data=ribs, aes(col=complex.wname)) +
  scale_color_manual(values=c("red4", "blue3"), labels=c("Ribes_nigrum", "Fraxinus_excelsior"), name="") +
  annotate("text", x=10, y=48, label="basler12", col="red4") +
  annotate("text", x=16, y=60, label="pagter15", col="blue3") +
  annotate("text", x=25, y=16, label="sonsteby14", col="blue3") +
  annotate("text", x=10, y=8, label="heide12", col="blue3") +
  annotate("text", x=11, y=32, label="laube14a", col="red4") +
  annotate("text", x=10, y=41, label="jones12", col="blue3")


c<-ggplot(ribs, aes(x=chill, y=resp, col=complex.wname)) + geom_point(data=ribs, aes(col=complex.wname)) +
  scale_color_manual(values=c("red4", "blue3"), labels=c("Ribes_nigrum", "Fraxinus_excelsior"), name="") +
  annotate("text", x=7, y=48, label="basler12", col="red4") +
  annotate("text", x=4, y=65, label="pagter15", col="blue3") +
  annotate("text", x=2, y=16, label="sonsteby14", col="blue3") +
  annotate("text", x=8, y=3, label="heide12", col="blue3") +
  annotate("text", x=4, y=32, label="laube14a", col="red4") +
  annotate("text", x=15, y=44, label="jones12", col="blue3")

quartz()
ggarrange(f, p, c, nrow=3)



