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

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- FALSE
use.flags.for.allsppmodel <- FALSE
use.yourown.flagdesign <- TRUE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
    use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
    & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
use.chillports = FALSE
use.zscore = FALSE
use.allspp =FALSE # for the main model this is false
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
use.chillports = FALSE
use.zscore = FALSE
use.allspp = TRUE
use.multcuespp = FALSE
use.cropspp = TRUE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
use.chillports = FALSE # change to false for using utah instead of chill portions (most models use chill portions z)
use.zscore = TRUE # change to false to use raw predictors

# Default is species complex and no crops
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE

# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE

#Default is all chilling data
use.expchillonly = FALSE # change to true for only experimental chilling 
#note: with only exp chilling, there is only exp photo and force too.
#also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
use.continent = TRUE#if fitting the continent model
use.stage = FALSE#if fitting the material model
use.zohner = FALSE# to see how removing zohner changes estimates
}

source("source/bbstanleadin_newbbmods.R")



#fit stage model with rstanarm
if(use.stage==TRUE){
  bb.stan$stage<-as.factor(bb.stan$stage)
  sdsp<-unique(bb.stan$complex.wname[bb.stan$stage==2])
  bb.stan.bothst<-bb.stan[bb.stan$complex.wname %in% sdsp,]

  m2l.ni.rstanarm <-stan_lmer(resp ~ force.z + photo.z + chill.z + stage+#main effects
                              ((force.z + photo.z+ chill.z)|complex), #random effects
                            data = bb.stan,
                            chains = 2, cores = 2,iter = 4000, warmup=2000,control = list(adapt_delta = 0.999))
  m2l.ni.rstanarm2 <-stan_lmer(resp ~ force.z + photo.z + chill.z + stage+#main effects
                                ((force.z + photo.z+ chill.z)|complex), #random effects
                              data = bb.stan.bothst,
                              chains = 2, cores = 2,iter = 4000, warmup=3000,control = list(adapt_delta = 0.999))
  m2l.ni.rstanarm3 <-stan_lmer(resp ~ force.z + photo.z + chill.z + cont+ force.z:cont + photo.z:cont+chill.z:cont+ #main effects
                                 (1|complex), #random effects
                               data = bb.stan.bothcont,
                               chains = 2, cores = 2,iter = 4000, warmup=3000,control = list(adapt_delta = 0.99))
  summary(m2l.ni.rstanarm)
  summary(m2l.ni.rstanarm2)
  summary(m2l.ni.rstanarm3)
  #10 spp
  summary(m2l.ni.rstanarm2)

  cbind(fixef(m2l.ni.rstanarm),fixef(m2l.ni.rstanarm2))
  stagemod<-summary(m2l.ni.rstanarm,pars=c("(Intercept)","force.z","chill.z","photo.z","stage2","sigma"),probs = c(0.025, 0.25, 0.50,0.75,0.975))
  stagemod<-round(stagemod,digits=2)
  stagemod<-rbind(stagemod,c("37",rep("", times=dim(stagemod)[2]-1)))
  row.names(stagemod)<-c("$\\mu_{\\alpha}$","$\\beta_{forcing}$","$\\beta_{photoperiod}$",   
                         "$\\beta_{chilling}$","$\\beta_{stage}$",
                         "$\\sigma_{y}$","$N_{sp}$") 
  
  write.csv(stagemod,"../output/supptables/stagemodsum.csv")
}
#fit continent model with rstanarm
if(use.continent==TRUE){
  bb.stan$cont<-as.factor(bb.stan$cont)
  nasp<-unique(bb.stan$complex.wname[bb.stan$cont=="north america"])
  bb.stan.bothcont<-bb.stan[bb.stan$complex.wname %in% nasp,]
  
  m2l.ni.rstanarm3 <-stan_lmer(resp ~ force.z + photo.z + chill.z + cont+ force.z:cont + photo.z:cont+chill.z:cont+ #main effects
                              (1|complex), #random effects
                            data = bb.stan.bothcont,
                            chains = 2, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))
  m2l.ni.rstanarm2 <-stan_lmer(resp ~ force.z + photo.z + chill.z + cont+  #main effects
                                ((force.z + photo.z+ chill.z+ cont)|complex), #random effects
                              data = bb.stan.bothcont,
                              chains = 2, cores = 2,iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))
  
  summary(m2l.ni.rstanarm)
  summary(m2l.ni.rstanarm2)
  
  fixef(m2l.ni.rstanarm)
  fixef(m2l.ni.rstanarm2)
  
  contmod<-summary(m2l.ni.rstanarm,pars=c("(Intercept)","force.z","chill.z","photo.z","contnorth america","force.z:contnorth america","chill.z:contnorth america","photo.z:contnorth america","sigma"),probs = c(0.025, 0.25, 0.50,0.75,0.975))
  contmod<-round(contmod,digits=2)
  contmod<-rbind(contmod,c("29",rep("", times=dim(contmod)[2]-1)))
  
  contmod2<-summary(m2l.ni.rstanarm2,pars=c("(Intercept)","force.z","chill.z","photo.z","contnorth america","force.z:contnorth america","chill.z:","photo.z:contnorth america","sigma"),probs = c(0.025, 0.25, 0.50,0.75,0.975))
  contmod2<-round(contmod2,digits=2)
  contmod2<-rbind(contmod2,c("29",rep("", times=dim(contmod2)[2]-1)))
  
  row.names(contmod)<-c("$\\mu_{\\alpha}$","$\\beta_{forcing}$","$\\beta_{photoperiod}$",   
                         "$\\beta_{chilling}$","$\\beta_{stage}$",
                         "$\\sigma_{y}$","$N_{sp}$") 
  
  write.csv(stagemod,"../output/supptables/stagemodsum.csv")
  
}



###If coding model in stan:
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
               iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

check_all_diagnostics(m2l.ni)
# launch_shinystan(m2l.ni.rstanarm)

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]
m2lni.sum[grep("b_stage", rownames(m2lni.sum)),]

ys<-datalist.bb$y
# posterior predictive checks....
if(FALSE){
y_pred <- extract(m2l.ni.rstanarm, 'y_ppc')

par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}



# save zohner
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE & use.zohner==FALSE){
save(m2l.ni, file="stan/output/m2lni_spcompexprampfputah_nozohner_z.Rda")
}



###### SIDE BAR #####
## Getting R2 etc. ##


if(FALSE){
modelhere <- m2l.ni # m2l.nistudy 
observed.here <- bb.stan$resp

mod.sum <- summary(modelhere)$summary
mod.sum[grep("mu_", rownames(mod.sum)),] 

# getting predicted values if needed
preds.mod.sum <- mod.sum[grep("yhat", rownames(mod.sum)),]

# Here's our method to calculate R sq
mod.R2 <- 1- sum((observed.here-preds.mod.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)

# Which seems correct! See  https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
rsq <- function (x, y) cor(x, y) ^ 2
rsq(observed.here, preds.mod.sum[,1])
summary(lm(preds.mod.sum[,1]~observed.here)) # Multiple R-squared

# spcomplex, no crops, group by sp>9: 0.6028132, 0.6086478 for sp>4 ... mult R2 around 0.58
#  0.5689911
}
####### END SIDE BAR #######

