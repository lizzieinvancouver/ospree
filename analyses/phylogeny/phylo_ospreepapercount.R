## Started 19 May 2023 ##
## By Lizzie ##

## Estimate paper nums ....

options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

## Get a few things ...
ospclean <- read.csv("output/ospree_clean.csv")
sort(unique(ospclean$genus)) # they look all woody

length(unique(ospclean$datasetID))
length(unique(paste(ospclean$datasetID, ospclean$study)))

# What's new?
dup <- read.csv("output/ospree2019update.csv") # built in merge_update2019.R
unique(dup$datasetID)
unique(paste(dup$datasetID, dup$study))
# And they all stay in ospclean
unique(ospclean$datasetID)[which(unique(ospclean$datasetID) %in% unique(dup$datasetID))]

length(unique(ospclean$datasetID)[which(unique(ospclean$datasetID) %in% unique(dup$datasetID))])
