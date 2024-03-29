#Based off of Ailene's bb_analysis/maketables.forsupp/mod_table.R
#This file makes a table of model summaries with 95 % credible intervalsfor the ospree budburst supplement
#would be good to add n_studies

#utah, z
load("../../analyses/lat_analysis/stan/m2l.inter.lat.z.Rda") # m2l.inter
fit.z <- summary(m2l.inter)$summary
ztab<-as.data.frame(round(cbind(fit.z[1:13,1],fit.z[1:13,5],fit.z[1:13,7], fit.z[1:13,4],fit.z[1:13,8]),digits=2))
ztab<-rbind(ztab,c(length(fit.z[grep("a_sp", rownames(fit.z)),1]),"","","", ""))
rownames(ztab)[14]<-"n_sp"
if(FALSE){
#utah units, nonz
load("../../analyses/bb_analysis/stan/output/m2l.inter.lat.nonz.Rda") # m2l.inter
fitcp.nonz <- summary(m2l.inter)$summary
nonzcptab<-as.data.frame(round(cbind(fitcp.nonz[1:13,1],fitcp.nonz[1:13,4],fitcp.nonz[1:13,8]),digits=2))
nonzcptab<-rbind(nonzcptab,c(length(fitcp.nonz[grep("a_sp", rownames(fitcp.nonz)),1]),"","",""))
rownames(nonzcptab)[14]<-"n_sp"
}



#add column names to all sub tables
colnames(ztab)<-c("mean","25%", "75%", "2.5%", "97.5%")

row.names(ztab)<-c("$\\mu_{\\alpha}$","$\\mu_{forcing}$","$\\mu_{photoperiod}$", 
                   "$\\mu_{chilling}$", "$\\mu_{latitude}$", "$\\mu_{photo:latitude}$",
                   "$\\sigma_{\\alpha}$", "$\\sigma_{forcing}$", 
                   "$\\sigma_{photoperiod}$","$\\sigma_{chilling}$","$\\sigma_{latitude}$", 
                   "$\\sigma_{photo:latitude}$","$\\sigma_{y}$","$N_{sp}$") 


#ztab$credint<-paste(ztab$`2.5%`, ztab$`2.5%`,ztab$uci, sep="-")

#zmodtable<-cbind(c(row.names(fit.z)[1:13],"n_sp"),ztab$est,ztab$credint)

#nonztab$credint<-paste(nonztab$lci,nonztab$uci, sep="-")

#colnames(zmodtable)<-c("parameter", "cp.est","cp.ci")

write.csv(ztab,"../../analyses/output/supptables/zmodetable_lat.csv", row.names = TRUE)

