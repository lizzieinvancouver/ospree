
# libraries
library(dplyr)
library(shinystan)
library(brms)

#How many studies do not include error in figs/tables/:
d<-read.csv("..//..//analyses/output/ospree_clean_withchill_BB.csv", header=TRUE)
length(d$resp_error[which(is.na(d$resp_error))])
(length(d$resp_error[d$resp_error=="no response"])+
    length(d$resp_error[d$resp_error=="0"])+
    length(d$resp_error[d$resp_error==""])+
    length(d$resp_error[which(is.na(d$resp_error))]))/length(d$resp_error)
#91.6% do not have resp. error
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
bbstan<-read.csv("..//..//analyses/output/bbstan_mainmodel.csv", header=TRUE) 
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
d_modstud2<-subset(d_modstud,select=c(datasetIDstudy,study,Entered.By,genus,species,response,response.time,n,error.type,resp_error,figure.table..if.applicable.))
#Make a list of studies that need n and/or errir
colnames(d_modstud2)[11]<-"fig.table"

non<-d_modstud2[d_modstud2$n=="",]

noerror<-d_modstud2[d_modstud2$resp_error==""| d_modstud2$resp_error=="0"| d_modstud2$resp_error=="cant see bars, very small"| is.na(d_modstud2$resp_error),]
d_modstud2$haveerror<-"yes"
d_modstud2$haveerror[d_modstud2$resp_error==""| d_modstud2$resp_error=="0"| d_modstud2$resp_error=="cant see bars, very small"| is.na(d_modstud2$resp_error)]<-"no"
table(d_modstud2$haveerror,d_modstud2$datasetIDstudy)
# no  yes 
#2446  364
#364/(364+2446)
#0.1295374 of rows have error
t.test(as.numeric(d_modstud2$n)~d_modstud2$haveerror)
# write.csv(d_modstud3,"..//output/add.n.error.csv")
nonerr<-full_join(non,noerror)
nonerr$ID_study_fig<-paste(nonerr$datasetID,nonerr$study,nonerr$fig.table,sep="_")
neednerror <- nonerr %>% # start with the data frame
     distinct(ID_study_fig, .keep_all = TRUE) %>% # establishing grouping variables
     dplyr::select(datasetIDstudy, Entered.By,fig.table,n,resp_error,error.type)
 #write.csv(neednerror,"..//output/need.n.error.csv")
  
#check quantiles of forcing treatments


#Check the mean and range of experiment length of studies
range(as.numeric(d_modstud$chilldays),na.rm=TRUE)#0 182
mean(as.numeric(d_modstud$chilldays),na.rm=TRUE)#71.41829
median(as.numeric(d_modstud$chilldays),na.rm=TRUE)#73

quantile(as.numeric(d_modstud$chilldays),na.rm=TRUE,.25)#40
quantile(as.numeric(d_modstud$chilldays),na.rm=TRUE,.75)#105
length(as.numeric(d_modstud$chilldays))#n=2810

max(as.numeric(d_modstud$response.time[d_modstud$response.time!="999"]),na.rm=TRUE)#422.52
min(as.numeric(d_modstud$response.time[d_modstud$response.time!="999"]),na.rm=TRUE)#0
quantile(as.numeric(d_modstud$response.time[d_modstud$response.time!="999"]),na.rm=TRUE,0.25)#18
quantile(as.numeric(d_modstud$response.time[d_modstud$response.time!="999"]),na.rm=TRUE,0.75)#42
length(unique(d_modstud$datasetID[which(as.numeric(d_modstud$response.time[d_modstud$response.time!="999"])>365)]))/length(unique(d_modstud$datasetID))#422.52
range(as.numeric(d_modstud$response.time[d_modstud$response.time!="999"]), na.rm=TRUE)
mean(as.numeric(d_modstud$response.time[d_modstud$response.time!="999"]), na.rm=TRUE)
#only one study: "basler12" was greater than 1 year

#range of chilling temp treatments
mean(as.numeric(d_modstud$chilltemp),na.rm=TRUE)#4.37
range(as.numeric(d_modstud$chilltemp),na.rm=TRUE)#0, 16
range(as.numeric(d_modstud$forcetemp),na.rm=TRUE)#5, 32
mean(as.numeric(d_modstud$forcetemp),na.rm=TRUE)#15.67971

#how many studies have no information on error
errortab<-as.data.frame(t(table(d_modstud2$haveerror,d_modstud2$datasetIDstudy)))
colnames(errortab)<-c("datasetID","have.error","numrows")
werror<-dim(errortab[errortab$have.error=="yes" & errortab$numrows==0,])[1]
noerrorst<-dim(errortab[errortab$have.error=="no" & errortab$numrows>0,])[1]
numst<-unique(errortab$datasetID)
#Plot sample size sv error
#can we add in SE

#remove NAs
d_modstud2$n<-as.numeric(d_modstud2$n)
d_modstud2<-d_modstud2[-which(is.na(d_modstud2$n)),]
d_modstud2$resp_error<-as.numeric(d_modstud2$resp_error)
d_modstud2<-d_modstud2[-which(is.na(d_modstud2$resp_error)),]
studynum<-length(unique(d_modstud2$datasetID))
d_modstud2$genus.species<-paste(d_modstud2$genus,d_modstud2$species, sep=".")
spnum <- length(unique(d_modstud2$genus.species))

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 2)
my.pal.sp<-my.pal[1:spnum]
my.pal.sp<-as.data.frame(cbind(my.pal.sp,unique(d_modstud2$genus.species)))
colnames(my.pal.sp)<-c("colsp","genus.species")
my.pal.sp<-my.pal.sp[order(my.pal.sp$genus.species),]
my.pal.st<-my.pal[1:studynum]
my.pal.st<-as.data.frame(cbind(my.pal.st,unique(d_modstud2$datasetIDstudy)))
colnames(my.pal.st)<-c("colst","datasetIDstudy")
my.pal.st<-my.pal.st[order(my.pal.st$datasetIDstudy),]

my.pal.st$datasetIDstudy<-as.character(my.pal.st$datasetIDstudy)
d_modstud3<-left_join(d_modstud2,my.pal.st, copy=TRUE)
d_modstud4<-left_join(d_modstud3,my.pal.sp)
#convert SE to SD
d_modstud4$SD<-NA
d_modstud4$SD[d_modstud4$error.type=="SD"]<-d_modstud4$resp_error[d_modstud4$error.type=="SD"]
d_modstud4$SD[d_modstud4$error.type=="SE"]<-d_modstud4$resp_error[d_modstud4$error.type=="SE"]*sqrt(d_modstud4$n[d_modstud4$error.type=="SE"])

# display.brewer.all()
#my.pch <- rep(15:19, each=12)
#alphahere = 0.4
pdf("..//..//analyses/figures/sdvsn.pdf", width = 7, height = 5)

plot(d_modstud4$n,d_modstud4$SD,pch=16,col=as.character(d_modstud4$colsp),xlim=c(0,28), ylim=c(0,165), bty="l",xlab="Sample size",ylab= "StDev")
mod<-lm(d_modstud4$SD~d_modstud4$n)
summary(mod)
unique(d_modstud$error.type)
legend("topright",legend=paste(my.pal.sp$genus.species),pch=16,col=as.character(my.pal.sp$colsp))
dev.off()
d_modstud4$response.time[d_modstud4$response.time==0]<-0.1
d_modstud4$SD.stand<-d_modstud4$SD/d_modstud4$response.time
range(d_modstud4$SD.stand, na.rm=TRUE)
pdf(".//..//figures/sdvsn_stand.pdf", width = 7, height = 5)
plot(d_modstud4$n,d_modstud4$SD.stand,pch=16,col=as.character(d_modstud4$colsp),xlim=c(0,28), ylim=c(0,25), bty="l",xlab="Sample size",ylab= "StDev")
mod<-lm(d_modstud4$SD.stand~d_modstud4$n)

summary(mod)
unique(d_modstud$error.type)
legend("topright",legend=paste(my.pal.sp$genus.species),pch=16,col=as.character(my.pal.sp$colsp))
dev.off()


##make figure of models with measurement error



#Old code
# ns<-bbstan %>% # start with the data frame
#   distinct(ID_study, .keep_all = TRUE) %>% # establishing grouping variables
#   dplyr::select(datasetID, study, 'n')
# 
# 
# neednerror <- d_modstud3 %>% # start with the data frame
#   distinct(ID_study_fig, .keep_all = TRUE) %>% # establishing grouping variables
#   dplyr::select(datasetID, study, fig.table,n,error.type,resp_error)
# write.csv(neednerror,"..//output/need.n.error.csv")
