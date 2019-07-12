#Make table summarizing datasetID and the reference it goes with
# housekeeping
options(stringsAsFactors = FALSE)

#read in file with different lat/longs from PEP and chiling estimates
moddat<-read.csv("..//..//analyses/output/bbstan_allsppmodel_utahzscore_wcrops_allfp_allchill.csv", header=TRUE)
datasetIDs<-sort(unique(moddat$datasetID))
refs<-c("\\citet{Basler:2012}","\\citet{Basler:2014aa}","\\citet{Biasi:2012}","\\citet{Caffarra:2011a}","\\citet{Caffarra:2011b}","\\citet{Calme:1994aa}","\\citet{Campbell:1975aa}","\\citet{chavarria09}")
