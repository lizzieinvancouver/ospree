###Cat to double check forcing columns
##To do:
## Find any columns with 'ambient' or non-numerics and fix
#rm(list=ls()) 
#options(stringsAsFactors=FALSE)

## read data
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
#setwd("~/Documents/git/ospree/analyses")
#d<-read.csv("output/ospree_clean.csv",as.is=T)
if(is.data.frame(d)){

# man10 - fix from 0 ramped up 3 degrees every 6 days
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 28 & response == 100] <- 22)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 18 & response == 0] <- 16)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 19 & response == 20.981] <- 16)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 20 & response == 45.231] <- 16)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 21 & response == 75.684] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 22 & response == 90.156] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 23 & response == 97.813] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 24 & response == 97.813] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 25 & response == 98.167] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 26 & response == 99.267] <- 19)
d <- within(d, forcetemp[datasetID == 'man10' & response.time == 27 & response == 100] <- 19)

# schnabel87 - fix 10 for 2 week, then... to 10
d <- within(d, forcetemp[datasetID == 'schnabel87' & n == 30 & response.time == 35.4] <- 10)

# campbell75 - fix from 18-27 (20 average) to 20
d <- within(d, forcetemp[datasetID == 'campbell75' & study== "exp1"] <- 20)

# howe95 - calculated mean, changed from 22-27 to 24.5
d <- within(d, forcetemp[datasetID == 'howe95'] <- 24.5)

# laube14a - currently listed as 7-27.5, need to calculate thermaltime in order to extract 
# this information. Adding this to Issue #72

# skuterud94 now - is thermal time, does not explicitly say which forcing temp
# for each tx (mean 9, 12, 15)

# basler12 - is another thermal time study, currently "meandaily", cannot be fixed

# guak98: does not specify the "ambient" temperature but increased by 4 
# in other experiments, didn't change anything

# gunderson12: treatments were in relation to "thermal provenance" so I left them at ambient +2, +4																									
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, ambient forcing to days now... getting closer")
#setwd("~Documents/git/ospree/analyses/output")
#write.csv(d,"ospree_clean_withforce.csv")
