###Dan is investigating if there is any flowering data to work with in OSPREEE on June 19, 2017
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)

# Setting working directory.
setwd("~/Documents/git/ospree/analyses/output")

###data
ds1<- read.csv("ospree_clean_withchill.csv", header=TRUE)
ds<- read.csv("ospree_clean.csv", header=TRUE)

###which responses deal with flowers?
unique(ds$respvar)
unique(ds$respvar.simple )

####check out "daystoflower"
flo<-filter(ds, respvar.simple=="daystoflower")
dim(flo) ##nrow 180
unique(flo$genus) ##only 3
unique(flo$datasetID) ##only 4

###checl out "percectflower"
flo2<-filter(ds, respvar.simple=="percentflower")
unique(flo2$datasetID) #only 4
unique(flo2$genus) #only 2
dim(flo2) ## 61 0and ~500 are from one study

####last hope "flowernumber"
flo3<-filter(ds, respvar.simple=="flowernumber")
unique(flo3$genus) #1
dim(flo3) #163

###doubtful flowering data is useful for any kind of analysis##############################
