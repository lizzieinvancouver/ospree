#This file makes a table of model summaries for the ospree budburst supplement
load("..//..//analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_z.Rda") # m2l.ni
fit.nonz <- summary(m2l.ni)$summary
nonztab<-round(cbind(fit.nonz[1:9,1],fit.nonz[1:9,4],fit.nonz[1:9,8]),digits=2)
load("..//..//analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_nonz.Rda") # m2l.ni
fit.z <- summary(m2l.ni)$summary
ztab<-round(cbind(fit.z[1:9,1],fit.z[1:9,4],fit.z[1:9,8]),digits=2)

