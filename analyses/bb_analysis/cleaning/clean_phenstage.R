## 26 June 2017 - Cat
# What's going on with this phenstage madness?

# Load from bb_cleanmergeall.R

phenstage <- d[which(d$respvar.simple=="phenstage"),]
unique(phenstage$datasetID)
datasets<-unique(phenstage$datasetID)
xx<-d[which(d$datasetID==datasets),] # cannell83, gansert02, gunderson12, pagter15, pettersen71, sonsteby13
unique(xx$respvar.simple)
daysto<-xx%>%filter(respvar.simple=="daystobudburst")

### Let's check...
# cannell83: not a useful daystobudburst conversion

# gansert02: we could use budstages 2-3 (QUESTION: Do you mean 2-7 ... that's what the code does) ... fixing gives 3 observations
d[which(d$datasetID=="gansert02" & d$response>=2 & d$response<7),] # to see what rows it alters ... 
d$respvar.simple[which(d$datasetID=="gansert02" & d$response>=2 & d$response<7)] <- "daystobudburst"
d$response[which(d$datasetID=="gansert02" & d$response>=2 & d$response<7)] <- "timeonly"

# gunderson12: budburst is defined as stage 4 which is plotted in Figure 2 and is already recorded in ospree dataset

# pagter15: Between budstage 1 and 2 is considered budburst - gives 5 observations
d$respvar.simple[which(d$datasetID=="pagter15" & d$response>=1)]<-"daystobudburst"
d$response[which(d$datasetID=="pagter15" & d$response>=1)] <- "timeonly"

# pettersen71: flowers - can't fix

# sonsteby13: flowers - can't fix



