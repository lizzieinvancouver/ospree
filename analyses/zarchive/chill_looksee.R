### Started 21 January 2016 ###
### By Lizzie ###

## This file looks at the current chilling data we have #
# with the aim to quantify what we have and whether to use the data! ##

# safety features 
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# setup stuff
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
# setwd("~/Documents/git")


library(ggplot2)

# f(x)s
percentfilled <- function(dataframe, colname){
    subbyNo <- subset(dataframe, dataframe[colname]=="")
    percentNOTfilled <- nrow(subbyNo)/nrow(dataframe)
    print(1-percentNOTfilled)
}


# get the data 
dater <- read.csv("budreview/growthchambers_litreview.csv", header=TRUE)
dim(dater)
dater.agg <- aggregate(dater["respvar"], dater[c("datasetID", "chilltemp", "chilldays",
    "fieldchill", "chillphotoperiod")], FUN=length)

# chilling fields are:
    # fieldchill
    # chilldays
    # chilltemp
    # chillphotoperiod
table(dater$chilldays)
table(dater$chilltemp)
table(dater$chillphotoperiod)
table(dater$chilltemp)

percentfilled(dater.agg, "chilldays")
percentfilled(dater.agg, "chillphotoperiod")
percentfilled(dater.agg, "chilltemp")
percentfilled(dater.agg, "fieldchill")

percentfilled(dater, "chilldays")
percentfilled(dater, "chillphotoperiod")
percentfilled(dater, "chilltemp")
percentfilled(dater, "fieldchill")

ggplot(dater, aes(x=datasetID, y=as.numeric(chilldays), color=datasetID)) + geom_point()
ggplot(dater, aes(x=datasetID, y=as.numeric(chilltemp), color=datasetID)) + geom_point()
ggplot(dater, aes(x=datasetID, y=as.numeric(chillphotoperiod), color=datasetID)) + geom_point()

ggplot(dater, aes(x=datasetID, y=chilltemp, color=datasetID)) + geom_point()

# Lots of cleanup needed. 

ggplot(dater, 
	aes(x=as.numeric(as.character(dater$chilldays)), 
	y=as.numeric(as.character(dater$chilltemp)), 
	cex=log(as.numeric(as.character(dater$n))))) +
	 xlab("Chilling Days") + ylab("Chilling Temperature °C") + geom_point()

dev.print(device=pdf, file = "budreview/graphs/ChillSummary.pdf")
