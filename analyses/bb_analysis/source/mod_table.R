#This file makes a table of model summaries with 95 % credible intervalsfor the ospree budburst supplement
#utah units
load("..//..//analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_z.Rda") # m2l.ni
fit.nonz <- summary(m2l.ni)$summary
nonztab<-as.data.frame(round(cbind(fit.nonz[1:9,1],fit.nonz[1:9,4],fit.nonz[1:9,8]),digits=2))
load("..//..//analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_nonz.Rda") # m2l.ni
fit.z <- summary(m2l.ni)$summary
ztab<-as.data.frame(round(cbind(fit.z[1:9,1],fit.z[1:9,4],fit.z[1:9,8]),digits=2))
#cp units
load("..//..//analyses/bb_analysis/stan/output/m2lni_spcompexprampfpcp_z.Rda") # m2l.ni
fitcp.nonz <- summary(m2l.ni)$summary
nonzcptab<-as.data.frame(round(cbind(fitcp.nonz[1:9,1],fitcp.nonz[1:9,4],fitcp.nonz[1:9,8]),digits=2))
load("..//..//analyses/bb_analysis/stan/output/m2lni_spcompexprampfpcp_nonz.Rda") # m2l.ni
fitcp.z <- summary(m2l.ni)$summary
zcptab<-as.data.frame(round(cbind(fitcp.z[1:9,1],fitcp.z[1:9,4],fitcp.z[1:9,8]),digits=2))

#add model with crops and all species and all treatments included, 
load("..//..//analyses/bb_analysis/stan/output/m2lni_allsppwcrop_utah_z.Rda") # m2l.ni
fitallsp.nonz <- summary(m2l.ni)$summary
nonzallsptab<-as.data.frame(round(cbind(fitallsp.nonz[1:9,1],fitallsp.nonz[1:9,4],fitallsp.nonz[1:9,8]),digits=2))
load("..//..//analyses/bb_analysis/stan/output/m2lni_allsppwcrop_utah_nonz.Rda") # m2l.ni
fitallsp.z <- summary(m2l.ni)$summary
zallsptab<-as.data.frame(round(cbind(fitallsp.z[1:9,1],fitallsp.z[1:9,4],fitallsp.z[1:9,8]),digits=2))
#add column names to all sub tables
colnames(ztab)<-colnames(zcptab)<-colnames(zallsptab)<-
  colnames(nonztab)<-colnames(nonzcptab)<-colnames(nonzallsptab)<-c("est","lci","uci")

ztab$credint<-paste(ztab$lci,ztab$uci, sep="-")
zcptab$credint<-paste(zcptab$lci,zcptab$uci, sep="-")
zallsptab$credint<-paste(zallsptab$lci,zallsptab$uci, sep="-")

zmodtable<-cbind(ztab$est,ztab$credint,zcptab$est,zcptab$credint,zallsptab$est,zallsptab$credint)

nonztab$credint<-paste(nonztab$lci,nonztab$uci, sep="-")
nonzcptab$credint<-paste(nonzcptab$lci,nonzcptab$uci, sep="-")
nonzallsptab$credint<-paste(nonzallsptab$lci,nonzallsptab$uci, sep="-")

nonzmodtable<-cbind(nonztab$est,nonztab$credint,nonzcptab$est,nonzcptab$credint,nonzallsptab$est,nonzallsptab$credint)

colnames(zmodtable)<-colnames(nonzmodtable)<-c("utah.est","utah.ci","cp.est","cp.ci","utahallsp.est","utahallsp.ci")
