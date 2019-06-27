## 27 June 2019 - Cat
# Work on 'What is budburst?' table for the main manuscript

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")


## Load in files
whatis <- read.csv("input/whatisbudburst.csv", header=TRUE)
bb.studies <- read.csv("output/bbstan_allsppmodel_utahzscore_wcrops_allfp_allchill.csv", header=TRUE)


datasets <- unique(bb.studies$datasetID)
whatis <- whatis[(whatis$datasetID%in%datasets),]

whatis$Definition <- ifelse(whatis$Definition=="Not addressed", "Not Defined", whatis$Definition)
whatis$numbudsburst<- whatis$Percent

table(whatis$Definition)
# green tip: 68.8% of studies (33)
# not defined: 8.33% of studies (4)
# leaf emergence: 4.2% (2)
# leaf unfolded: 6.25% (3)
# the rest are 1
#0.5cm new growth   1cm new growth 2 cm leaf growth        bud swell        green tip   leaf emergence 
#       1                1                1                  1               33                2 
#leaf unfolded      Not Defined  open bud scales  petiole emerged 
#     3                4                1                1 


table(whatis$numbudsburst)
# not defined       >= 3 buds    >=10 leaves      1%         10%        100%         20%         50% 
#     3                 1              1          34           1           1           1           6 


whatis <- subset(whatis, select=c("datasetID", "Definition", "numbudsburst"))


write.csv(whatis, "output/whatisbudburst_bbstanallsppmodel.csv", row.names=FALSE)
