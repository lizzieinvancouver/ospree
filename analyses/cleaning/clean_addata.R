## Started 29 May 2020 ###
## By Lizzie ##

## A place to add in data that somehow missed the boat before and needs to be effectively pasted in (save for Zohner, Dan did that his special way) ##

### okie11 ... see cleaning/okie_cleaning/okiecleaning_README.txt
### This section by Geoff
### Original ospree file did not include responses over time
### Here I remove all okie11 and replace with data from a cleaned
okie11 <- read.csv("input/okie_merge.csv", header = TRUE) # read okie11 data
d <- subset(d, datasetID != "okie11") # remove old okie11 data
d <- rbind(d, okie11)

# adding the new dantec14 data that never made it into ospree. MG 10 Aug 2020
dantec14 <- read.csv("input/dantec_merge.csv", header = TRUE)
dantec14$X <- NULL
d <- rbind(d,dantec14)
