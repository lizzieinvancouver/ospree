## Script to format Flynn's data and merge into Ospree
# Started by Nacho
# Date: 6th Dec 2018


## to start
rm(list=ls())
options(stringsAsFactors=FALSE)


## load packages




## setwd
setwd("~/GitHub/ospree/")

## load data
bdaymean.hf<-read.csv("analyses/phylogeny/input/bdaymean.hf.csv")
bdaymean.sh<-read.csv("analyses/phylogeny/input/bdaymean.sh.csv")

## info from README
#C: force - 15 C day/5 C night
#W: force - 20 C day/10 C night
#S: photo - 8 hrs
#L: photo - 12 hrs
#0: chill - field chill only -- 814.50 for HF; 599.50 for SH
#1: chill - field chill + weeks at 4 C -- 2062.50 for HF; 1847.50 for SH 
#2: chill - field chill + weeks at 1.5 C -- 1702.50 for HF; 1487.50 SH 














