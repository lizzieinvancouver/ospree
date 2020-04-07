## Started 20 November 2018 ##
## By Lizzie ##


## Try to run REAL Ospree data ##
## With Stan! ##

## See also: models_stan_previous.R

## Take 1: This code is based heavily off bbmodel1_stan.R 
## Take 2: February 2017! ##
## Take 3: July 2017! ## New code to run stan models on Ospree (by Nacho, Lizzie and more)
## Take 4: June 2018! Lizzie re-organizes code and adds rstanarm
## Take 5: 4-5 December 2018! Big reorganization (see models_stan_previous.R)

## To do
# (a) subset down to relevant block/transplant treatments for gomory15??
# Impt: not dealing with provenance and material (which mean some treatments show up more than once but the partial pooling should handle this we think)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure 2 in main text) versus other versions (all spp model, chill portions, uncentered predictors, as in supp table and figures 3-4)
use.flags.for.mainmodel <- TRUE
use.flags.for.spcomp.cp <- FALSE
use.flags.for.allspp.utah <- FALSE
use.flags.for.spcomp.utah.nonz <- FALSE
use.flags.for.spcomp.cp.nonz <- FALSE # predictors on natural scale, spcomplex with utah units. Fig 3-4 in main text of budburst ms
use.flags.for.allspp.utah.nonz <- FALSE
use.yourown.flagdesign <- FALSE

source("source/flags.for.models.in.bbms.R")

source("source/bbstanleadin_nvar.R")
bb.stan$resp_error<-as.numeric(bb.stan$resp_error)
bb.stan$n<-as.numeric(bb.stan$n)
bb.stan$SD<-NA
bb.stan$SD[bb.stan$error.type=="SD"]<-bb.stan$resp_error[bb.stan$error.type=="SD"]
bb.stan$SD[bb.stan$error.type=="SE"]<-bb.stan$resp_error[bb.stan$error.type=="SE"]*sqrt(bb.stan$n[bb.stan$error.type=="SE"])
#Get the mean relative SD (SD/resp)- add this if we want to add measurement error to studies that do not include it
bb.stan$SDrel<-bb.stan$SD/(bb.stan$resp)
bb.stan$SDrel[which(bb.stan$SDrel== "Inf")]<-NA
meanSDrel<-mean(bb.stan$SDrel, na.rm=TRUE)
meann<-as.integer(mean(bb.stan$n, na.rm=TRUE))
havensd<-which(!is.na(bb.stan$n) & !is.na(bb.stan$SD))
havenonly<-which(!is.na(bb.stan$n) & is.na(bb.stan$SD))
havenone<-which(is.na(bb.stan$n) & is.na(bb.stan$SD))

#Add sampling error (use mean SD and mean n when these data are not available)
samps<-20#number of samples you want to do of model fitting

modsummn<-c()
for(i in 1:samps){
#create a column of new responses which will be filled by creating distribtions of observations using data about n, resp (=mean), and sd
bb.stan$resp.samp<-NA

#for responses with n and sd, create a distribution and pull one sample from it
for(j in havensd){
  bb.stan$resp.samp[j]<-rnorm(bb.stan$n[j],bb.stan$resp[j],bb.stan$SD[j])[1]
}

#for responses with n but no sd, create a distribution using mean sd and pull one sample from it
for(k in havenonly){
  bb.stan$resp.samp[k]<-rnorm(bb.stan$n[k],bb.stan$resp[k],meanSDrel*bb.stan$resp[k])[1]
}
#for responses with n but no sd, create a distribution using mean sd and pull one sample from it
for(l in havenone){
  bb.stan$resp.samp[l]<-rnorm(meann,bb.stan$resp[l],meanSDrel*bb.stan$resp[l])[1]
}

datalist.bb <- with(bb.stan, 
                      list(y = resp.samp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
)


m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
              iter = 3500, warmup=2500)

m2lni.sum <- summary(m2l.ni)$summary
mus<-m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
sigmas<-m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]
modsum<-rbind(mus,sigmas)
modsumname<-paste("..//..//analyses/output/measerr/measerrmodsum",i,"csv",sep=".")
write.csv(modsum,modsumname)
modsumname2<-paste("..//..//analyses/output/measerr/measerrmodsumsp",i,"csv",sep=".")
write.csv(m2lni.sum,modsumname2)
modsummn<-cbind(modsummn,modsum[,1])
}

write.csv(modsummn,"..//..//analyses/output/measerr/allmodsums.csv")

#Make figure comparing all mods
load("..//..//analyses/bb_analysis/stan/output/m2lni_spcompexprampfputah_z.Rda") # m2l.ni
fit.z <- summary(m2l.ni)$summary
mu<-fit.z[grep("mu_", rownames(fit.z)),]
sigma<-fit.z[grep("sigma_", rownames(fit.z)),]

quartz()
#par(mfrow=c(4,1))
plot(mu[1,1],4,pch=16,cex=1.5,col="black",xlab="Model estimate",ylab="",yaxt="n",ylim=c(0,5), xlim= c(-10,40)) 
points(mu[2,1],3,pch=16,cex=1.5,col="black")
points(mu[3,1],2,pch=16,cex=1.5,col="black")
points(mu[4,1],1,pch=16,cex=1.5,col="black")

jit<-seq(from =0.01,to =0.4, by = .02 )
points(modsummn[1,],3.9-jit,pch=16,cex=.9,col="gray")
points(modsummn[2,],2.9-jit,pch=16,cex=.9,col="gray")
points(modsummn[3,],1.9-jit,pch=16,cex=.9,col="gray")
points(modsummn[4,],0.9-jit,pch=16,cex=.9,col="gray")
axis(2,at=c(4,3,2,1), labels = c("Intercept", "Forcing","Photoperiod","Chilling"))
#####################################
## Get studies with interactions  ##
## Lizzie took from countinxns. R ##
## Need to deal with species complex! ##
####################################

datesetIDincl <- read.csv("..//limitingcues/output/bbstan_mainmodel_countinxns_datasetIDs.csv")
datesetIDincl$datIDstudy <- paste(datesetIDincl$datasetID, datesetIDincl$study)
bb.stan$datIDstudy <- paste(bb.stan$datasetID, bb.stan$study)
bb.stan.sm <- bb.stan[which(bb.stan$datIDstudy %in% datesetIDincl$datIDstudy),]

datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.z, # should change
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
)

######################################
## Overview of the model run below ##
######################################
# Main model:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
               iter = 3500, warmup=2500)

check_all_diagnostics(m2l.ni)
# launch_shinystan(m2l.ni)

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]

#zmodtable<-read.csv("../../analyses/output/supptables/zmodetable.csv", header=TRUE)

ys<-datalist.bb$y
# posterior predictive checks....
if(FALSE){
y_pred <- extract(m2l.ni, 'y_ppc')

par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}


# Code if you want to save your models (do NOT push output to git)

if(use.flags.for.mainmodel){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfputah_z_wgc.Rda")
}

if (use.flags.for.spcomp.cp){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfpcp_z_wgc.Rda")
}

if (use.flags.for.allspp.utah){
  save(m2l.ni, file="stan/output/m2lni_allsppwcrop_utah_z_wgc.Rda")
}

if (use.flags.for.spcomp.utah.nonz){
  save(m2l.ni, file="stan/output/m2lni_spcompalltypesutah_nonz_wgc.Rda")
}

if (use.flags.for.spcomp.cp.nonz){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfpcp_nonz_wgc.Rda")
}

if (use.flags.for.allspp.utah.nonz){
  save(m2l.ni, file="stan/output/m2lni_allsppwcrop_utah_nonz_wgc.Rda")
}

#Other combinations of flags used at some point (but not in the main bb manuscript)
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE &
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_spcompalltypesutah_z.Rda")
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfputah_nonz.Rda")
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_allsppexprampfputah_z.Rda")
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  save(m2l.ni, file="stan/output/m2lni_allsppexprampfputah_nonz.Rda")
}


if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
save(m2l.ni, file="stan/output/m2lni_spcompalltypescp_z.Rda")
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
save(m2l.ni, file="stan/output/m2lni_spcompwcropsexprampfpcp_z.Rda")
}

