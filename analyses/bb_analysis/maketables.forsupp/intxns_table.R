#Make table summarizing interactions
# housekeeping
options(stringsAsFactors = FALSE)

#read in file with different lat/longs from PEP and chiling estimates
intxns<-read.csv("..//..//analyses/limitingcues/output/bbstan_mainmodel_countinxns.csv", header=TRUE)
intxn.table<-rbind(intxns[1:7,])
