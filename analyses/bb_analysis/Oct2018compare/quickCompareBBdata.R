nowd<- read.csv("~/Documents/git/projects/treegarden/budreview/ospree/analyses/output/ospree_clean_withchill_BB.csv")

nowdsm <- subset(nowd, select=c("datasetID", "ID_chilltreat", "study", "photoperiod_day", "forcetemp", "Total_Chill_portions")) 

befd <- read.csv("~/Documents/git/projects/treegarden/budreview/ospree/analyses/output/older/ospree_clean_withchill_BB8Jun2018.csv")

befdsm <- subset(befd, select=c("datasetID", "ID_chilltreat", "study", "photoperiod_day", "forcetemp", "Total_Chill_portions"))

write.csv(befdsm, "~/Desktop/before.csv")
write.csv(nowdsm, "~/Desktop/now.csv")

# Does not help ...
# try <- merge(nowdsm, befdsm, by=c("datasetID", "ID_chilltreat", "study"), all.x=TRUE, all.y=TRUE, suffixes=c(".now", ".before"))

library(dplyr)
tryagain <- anti_join(nowdsm, befdsm)

unique(tryagain$datasetID)

# Now I write out the simple forms of the data and look at the datasets/rows that tryagain says do not match ... 
write.csv(befdsm, "~/Desktop/before.csv")
write.csv(nowdsm, "~/Desktop/now.csv")
