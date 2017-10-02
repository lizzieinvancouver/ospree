## Started 2 October 2017 ##
## By Lizzie (so far)##

## This code does some basic OSPREE cleaning that didn't fit anywhere else ##
## It is sourced in cleanmerge_all.R ##

# In July 2017 we realized we left year off the Zonher data, but we need it! So we’re adding it in here, it should be year of experiment, which is 2014. 
d$year[which(d$datasetID=="zohner16")] <- 2014
# Fix a table that was mis-referenced
d$figure.table..if.applicable.[which(d$datasetID=="campbell75" & d$figure.table..if.applicable=="table1")] <- "table2"
