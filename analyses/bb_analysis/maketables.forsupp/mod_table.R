#This file makes a table of model summaries with 95 % credible intervalsfor the ospree budburst supplement
#would be good to add n_studies
#utah units, nonz
load("../../analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_nonz.Rda") # m2l.ni
fit.nonz <- summary(m2l.ni)$summary
#summary(fit.nonz)# min n_ef: 1104 
nonztab<-as.data.frame(round(cbind(fit.nonz[1:9,1],fit.nonz[1:9,5],fit.nonz[1:9,7],fit.nonz[1:9,4],fit.nonz[1:9,8]),digits=2))
#add n_sp
nonztab<-rbind(nonztab,c(length(fit.nonz[grep("a_sp", rownames(fit.nonz)),1])-2,"","","","",""))
rownames(nonztab)[10]<-"n_sp"

#utah units, z
load("../../analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_z.Rda") # m2l.ni
fit.z <- summary(m2l.ni)$summary
#summary(fit.z)# min n_ef: 1198 

ztab<-as.data.frame(round(cbind(fit.z[1:9,1],fit.z[1:9,5],fit.z[1:9,7],fit.z[1:9,4],fit.z[1:9,8]),digits=2))
ztab<-rbind(ztab,c(length(fit.z[grep("a_sp", rownames(fit.z)),1])-2,"","","","",""))
rownames(ztab)[10]<-"n_sp"

#cp units, nonz
load("../../analyses/bb_analysis/stan/output/m2lni_spcompexprampfpcp_nonz.Rda") # m2l.ni
fitcp.nonz <- summary(m2l.ni)$summary
#summary(fitcp.nonz)# min n_ef: 1228 

nonzcptab<-as.data.frame(round(cbind(fitcp.nonz[1:9,1],fitcp.nonz[1:9,5],fitcp.nonz[1:9,7],fitcp.nonz[1:9,4],fitcp.nonz[1:9,8]),digits=2))
nonzcptab<-rbind(nonzcptab,c(length(fitcp.nonz[grep("a_sp", rownames(fitcp.nonz)),1])-2,"","","","",""))
rownames(nonzcptab)[10]<-"n_sp"
#cp units, z
load("../../analyses/bb_analysis/stan/output/m2lni_spcompexprampfpcp_z.Rda") # m2l.ni
fitcp.z <- summary(m2l.ni)$summary
#summary(fitcp.z)# min n_ef: 1192 

zcptab<-as.data.frame(round(cbind(fitcp.z[1:9,1],fitcp.z[1:9,5],fitcp.z[1:9,7],fitcp.z[1:9,4],fitcp.z[1:9,8]),digits=2))
zcptab<-rbind(zcptab,c(length(fitcp.z[grep("a_sp", rownames(fitcp.z)),1])-2,"","","","",""))
rownames(zcptab)[10]<-"n_sp"

#add model with crops and all species and all treatments included, nonz
load("../../analyses/bb_analysis/stan/output/m2lni_allsppwcrop_utah_nonz.Rda") # m2l.ni
fitallsp.nonz <- summary(m2l.ni)$summary
#summary(fitallsp.nonz)# min n_ef: 441.8 
nonzallsptab<-as.data.frame(round(cbind(fitallsp.nonz[1:9,1],fitallsp.nonz[1:9,5],fitallsp.nonz[1:9,7],fitallsp.nonz[1:9,4],fitallsp.nonz[1:9,8]),digits=2))
nonzallsptab<-rbind(nonzallsptab,c(length(fitallsp.nonz[grep("a_sp", rownames(fitallsp.nonz)),1])-2,"","","","",""))
rownames(nonzallsptab)[10]<-"n_sp"
#add model with crops and all species and all treatments included, z
load("../../analyses/bb_analysis/stan/output/m2lni_allsppwcrop_utah_z.Rda") # m2l.ni
fitallsp.z <- summary(m2l.ni)$summary
#summary(fitallsp.z)# min n_ef: 432.1 

zallsptab<-as.data.frame(round(cbind(fitallsp.z[1:9,1],fitallsp.z[1:9,5],fitallsp.z[1:9,7],fitallsp.z[1:9,4],fitallsp.z[1:9,8]),digits=2))
zallsptab<-rbind(zallsptab,c(length(fitallsp.z[grep("a_sp", rownames(fitallsp.z)),1])-2,"","","","",""))
rownames(zallsptab)[10]<-"n_sp"

#add column names to all sub tables
# colnames(ztab)<-c("utah.mean","utah.25%", "utah.75%", "utah.2.5%","utah.97.5%")
#   colnames(zcptab)<-c("cp.mean","cp.25%", "cp.75%","cp.2.5%","cp.97.5%")
#   colnames(zallsptab)<-c("allsp.mean","allsp.25%", "allsp.75%","allsp.2.5%","allsp.97.5%")
#   colnames(nonztab)<-c("utah.mean","utah.25%", "utah.75%", "utah.2.5%","utah.97.5%")
#   colnames(nonzcptab)<-c("cp.mean","cp.25%", "cp.75%","cp.2.5%","cp.97.5%")
#   colnames(nonzallsptab)<-c("allsp.mean","allsp.25%", "allsp.75%","allsp.2.5%","allsp.97.5%")
  
  colnames(ztab)<-colnames(zcptab)<-colnames(zallsptab)<-
  colnames(nonztab)<-colnames(nonzcptab)<-colnames(nonzallsptab)<-
    c("mean","25%", "75%","2.5%","97.5%")
zmodtable<-cbind(ztab,zcptab,zallsptab)

row.names(zmodtable)<-c(row.names(fitallsp.z)[1:9],"n_sp")
#next step is to get these in:
#row.names(fitallsp.z)[1]<-c("$\\mu_\\alpha$")

nonzmodtable<-cbind(nonztab,nonzcptab,nonzallsptab)
row.names(zmodtable)<-row.names(nonzmodtable)<-c("$\\mu_{\\alpha}$","$\\mu_{forcing}$","$\\mu_{photoperiod}$",   
                           "$\\mu_{chilling}$","$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$"
                           , "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{y}$","$N_{sp}$") 
write.csv(zmodtable,"../../analyses/output/supptables/zmodetable.csv", row.names = FALSE)
write.csv(nonzmodtable,"../../analyses/output/supptables/nonzmodetable.csv", row.names = FALSE)
#zmodtable<-rbind(c("","Utah.units","","","","","Chill.portions","","","","","All.species","","","",""),zmodtable)

#nonzmodtable<-rbind(c("","Utah.units","","","","","Chill.portions","","","","","All.species","","","",""),nonzmodtable)

#load model fit to data minus zohner

#utah units, z
load("../../analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_nozohner_z.Rda") # m2l.ni
fit.nozohz <- summary(m2l.ni)$summary
#summary(fit.z)# min n_ef: 1198 

ztab<-as.data.frame(round(cbind(fit.z[1:9,1],fit.z[1:9,5],fit.z[1:9,7],fit.z[1:9,4],fit.z[1:9,8]),digits=2))
ztab<-rbind(ztab,c(length(fit.z[grep("a_sp", rownames(fit.z)),1])-2,"","","","",""))
row.names(ztab)<-c("$\\mu_{\\alpha}$","$\\mu_{forcing}$","$\\mu_{photoperiod}$",   
                                                 "$\\mu_{chilling}$","$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$"
                                                 , "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{y}$","$N_{sp}$")
colnames(ztab)<- c("mean","25%", "75%","2.5%","97.5%")
write.csv(ztab,"../../analyses/output/supptables/nozohnzmodtable.csv", row.names = FALSE)
