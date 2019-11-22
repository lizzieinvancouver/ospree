library(dplyr)
#How many studies include error in figs/tables/:
d<-read.csv("..//..//analyses/output/ospree_clean_withchill_BB.csv", header=TRUE)
length(d$resp_error[which(is.na(d$resp_error))])
(length(d$resp_error[d$resp_error=="no response"])+length(d$resp_error[d$resp_error=="0"])+length(d$resp_error[d$resp_error==""])+length(d$resp_error[which(is.na(d$resp_error))]))/length(d$resp_error)

#what proportion of studies used utah as their reported units?

#what proportion of studies are temperate?
dim(d[as.numeric(d$provenance.lat)>40,])[1]/dim(d)[1]
length(unique(d$datasetID[as.numeric(d$provenance.lat)>40]))/length(unique(d$datasetID))
#62/74

dim(d[d$cu.model=="Utah"|d$cu.model=="Utah model",])
dim(d[d$cu.model=="hours below 5.5",])
dim(d[d$cu.model=="hoursbelow7deg",])
dim(d[d$cu.model=="",])

dim(d[d$datasetID=="zohner16",])[1]/dim(d)[1]
d$genus.species<-paste(d$genus,d$species,sep=".")
length(unique(d$genus.species))
length(unique(d$genus.species[d$datasetID=="zohner16"]))


#check which papers make it into the final model with use.flags.for.mainmodel
bbstan<-read.csv("..//..//analyses/output/bbstan_mainmodel_utah_allsppwcrops_allfp_allchill.csv", header=TRUE) 
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
# write.csv(d_modstud3,"..//output/add.n.error.csv")
# 

length(unique(d_modstud3$datasetID))#12 lack n or error
colnames(d_modstud3)[7]<-"fig.table"
d_modstud3$ID_study_fig<-paste(d_modstud3$datasetID,d_modstud3$study,d_modstud3$fig.table,sep="_")

#check quantiles of forcing treatments

# ns<-bbstan %>% # start with the data frame
#   distinct(ID_study, .keep_all = TRUE) %>% # establishing grouping variables
#   dplyr::select(datasetID, study, 'n')
# 
# 
# neednerror <- d_modstud3 %>% # start with the data frame
#   distinct(ID_study_fig, .keep_all = TRUE) %>% # establishing grouping variables
#   dplyr::select(datasetID, study, fig.table,n,error.type,resp_error)
# write.csv(neednerror,"..//output/need.n.error.csv")
