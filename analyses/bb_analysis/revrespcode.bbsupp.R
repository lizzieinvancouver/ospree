library(dplyr)
#How many studies do not include error in figs/tables/:
d<-read.csv("..//..//analyses/output/ospree_clean_withchill_BB.csv", header=TRUE)
length(d$resp_error[which(is.na(d$resp_error))])
(length(d$resp_error[d$resp_error=="no response"])+
    length(d$resp_error[d$resp_error=="0"])+
    length(d$resp_error[d$resp_error==""])+
    length(d$resp_error[which(is.na(d$resp_error))]))/length(d$resp_error)

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
d$datasetID<-as.character(d$datasetID)
modstud$datasetID<-as.character(modstud$datasetID)
d$study<-as.character(d$study)
modstud$study<-as.character(modstud$study)
d_modstud<-semi_join(d,modstud)
d_modstud2<-subset(d_modstud,select=c(datasetID,study,Entered.By,response,n,error.type,resp_error,figure.table..if.applicable.))
#Make a list of studies that need n and/or errir
colnames(d_modstud2)[8]<-"fig.table"

non<-d_modstud2[d_modstud2$n=="",]

noerror<-d_modstud2[d_modstud2$resp_error==""| d_modstud2$resp_error=="0"| d_modstud2$resp_error=="cant see bars, very small"| is.na(d_modstud2$resp_error),]
# write.csv(d_modstud3,"..//output/add.n.error.csv")
nonerr<-full_join(non,noerror)
nonerr$ID_study_fig<-paste(nonerr$datasetID,nonerr$study,nonerr$fig.table,sep="_")
neednerror <- nonerr %>% # start with the data frame
     distinct(ID_study_fig, .keep_all = TRUE) %>% # establishing grouping variables
     dplyr::select(datasetID, study, Entered.By,fig.table,n,resp_error,error.type)
 write.csv(neednerror,"..//output/need.n.error.csv")
  
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
