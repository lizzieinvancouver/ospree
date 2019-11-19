#Exploring some things in OSPREE for response to reviewers.
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#libraries
library(dplyr)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
} else if (length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

#How many studies include error in figs/tables/:
d<-read.csv("../output/ospree_clean_withchill_BB.csv", header=TRUE)
bb.stan<-read.csv("..//output/bbstan_mainmodel_utah_allsppwcrops_allfp_allchill.csv", header=TRUE)
d$forcetemp<-as.numeric(d$forcetemp)
d$forcetemp_night<-as.numeric(d$forcetemp_night)
d$photoperiod_day<-as.numeric(d$photoperiod_day)
d$photoperiod_day<-as.numeric(d$photoperiod_day)
d$chilldays<-as.numeric(d$chilldays)

dmod<-left_join(bb.stan,d)
d<-dmod
#refine this to only studies used in the model, after cleaning....

length(d$resp_error[which(is.na(d$resp_error))])
(length(d$resp_error[d$resp_error=="no response"])+length(d$resp_error[d$resp_error=="0"])+length(d$resp_error[d$resp_error==""])+length(d$resp_error[which(is.na(d$resp_error))]))/length(d$resp_error)
d$nerror.possible<-"yes"
d$nerror.possible[d$resp_error=="no response"|d$resp_error=="0"|d$resp_error==""|d$resp_error=="cant see bars, very small" ]<-"no"

#from need.n.error.csv
d$nerror.possible[d$datasetID=="falusi03"]<-"yes"
d$nerror.possible[d$datasetID=="falusi96"]<-"yes"
d$nerror.possible[d$datasetID=="ghelardini10"]<-"yes"
d$nerror.possible[d$datasetID=="laube14a"]<-"yes"
d$nerror.possible[d$datasetID=="laube14b"]<-"yes"
d$nerror.possible[d$datasetID=="myking95"]<-"yes"
d$nerror.possible[d$datasetID=="partanen01"]<-"yes"

unique(d$resp_error[d$nerror.possible=="yes"])
#how many studies used in the main ospree model include error or it is possible to get error?
unique(d$datasetIDstudy[d$nerror.possible=="no"])
unique(d$datasetIDstudy[d$nerror.possible=="yes"])
unique(d$datasetIDstudy)

#of the studies that do include data on measurement error, how large is it relative to the response itself?
d.wse<-d[d$nerror.possible=="yes",]
d.wse$resp_error<-as.numeric(d.wse$resp_error)
d.wse$response<-as.numeric(d.wse$response)
d.wse<-d.wse[!is.na(d.wse$resp_error),]
d.wse<-d.wse[!is.na(d.wse$response),]
#for SE
d.wse$error.type[d.wse$error.type=="SE across three years of experiments"]<-"SE across three years of experiments"
d.wse$rel.error<-d.wse$resp_error/d.wse$resp#
mean(d.wse$rel.error[d.wse$error.type=="SD"], na.rm=TRUE)
range(d.wse$rel.error[d.wse$error.type=="SD"], na.rm=TRUE)
range(d.wse$rel.error[d.wse$error.type=="SE"])
mean(d.wse$rel.error[d.wse$error.type=="SE"])
#all caffarra11a error estinates are way off, after checking the paper...we did not clean this column so this is difficult...
d.wse<-d.wse[d.wse$datasetID!="caffarra11a",]

#some SE values look super high! 
d.wse$datasetID[d.wse$rel.error>.5]

#what proportion of studies used utah as their reported units?

#what proportion of studies are temperate?
dim(d[as.numeric(d$provenance.lat)>40,])[1]/dim(d)[1]
length(unique(d$datasetID[as.numeric(d$provenance.lat)>40]))
length(unique(d$datasetID))
62/74

length(unique(d$Total_Utah_Model))

length(which(is.na(d$Total_Utah_Model)))#has most data
length(which(is.na(d$Total_Chill_portions)))
length(which(is.na(d$Total_Chilling_Hours)))
unique(d$field.chill.units)
dim(d[d$cu.model=="Utah"|d$cu.model=="Utah model",])
dim(d[d$cu.model=="hours below 5.5",])
dim(d[d$cu.model=="hoursbelow7deg",])
dim(d[d$cu.model=="",])

dim(d[d$datasetID=="zohner16",])[1]/dim(d)[1]
d$genus.species<-paste(d$genus,d$species,sep=".")
length(unique(d$genus.species))
length(unique(d$genus.species[d$datasetID=="zohner16"]))
length(unique(d$genus.species[d$datasetID!="zohner16"]))


#check which papers make it into the final model with use.flags.for.mainmodel
bbstan<-read.csv("..//output/bbstan_mainmodel_utah_allsppwcrops_allfp_allchill.csv", header=TRUE) 
dim(bbstan)
length(unique(bbstan$datasetID))#30
bbstan$ID_study_resp<-paste(bbstan$datasetID,bbstan$study,bbstan$resp,sep="_")
bbstan$ID_study<-paste(bbstan$datasetID,bbstan$study,sep="_")


modstud <- bbstan %>% # start with the data frame
  distinct(ID_study_resp, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(datasetID, study, resp)
modstud2 <- bbstan %>% # start with the data frame
  distinct(ID_study, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(datasetID, study)
d_modstud<-semi_join(d,modstud2)
d_modstud2<-subset(d_modstud,select=c(datasetID,study,response,n,error.type,resp_error,figure.table..if.applicable.))
d_modstud3<-d_modstud2[d_modstud2$n=="" & d_modstud2$resp_error=="",]
write.csv(d_modstud3,"..//output/add.n.error.csv")
length(unique(d_modstud3$datasetID))#12 lack n or error

#how many studies include provenance lat?
head(d_modstud)
unique(d_modstud$provenance.lat)
length(which(is.na(d_modstud$provenance.lat)))
(length(d_modstud$provenance.lat)-length(which(is.na(d_modstud$provenance.lat))))/length(d_modstud$provenance.lat)
length(unique(d_modstud$datasetID[which(is.na(d_modstud$provenance.lat))]))
length(unique(d_modstud$datasetID))
#check whether chilling and forcing are correlated in space/time in nature



