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
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R

use.chillports = FALSE # change to true for using chillportions instead of utah units

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
source("source/bbstanleadin.R")

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
# launch_shinystan(m2l.ni)

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]

ys<-datalist.bb$y
# posterior predictive checks....
if(FALSE){
y_pred <- extract(m2l.ni, 'y_ppc')
par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}


# Code if you want to save your models (do NOT push output to git)
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
save(m2l.ni, file="stan/output/m2lni_alltypes.Rda")
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE){
save(m2l.ni, file="stan/output/m2lni_alltypes.Rda")
}









#########################
## For PEP 725 species ##
#########################
use.pep=FALSE

if(use.pep){
getspp <- subset(bb.stan, select=c("complex", "complex.wname"))
allspp <- getspp[!duplicated(getspp), ]
allspp <- allspp[order(allspp$complex),]
pepspp <- c("Acer_pseudoplatanus", "Aesculus_hippocastanum", "Betula_pendula", "Corylus_avellana",
    "Fagus_sylvatica", "Larix_decidua", "Picea_abies", "Populus_tremula",
    "Prunus_padus","Quercus_robur", "Syringa_vulgaris")
# gymnastics to renumber species
somespp <-  allspp[which(allspp$complex.wname %in% pepspp),]
somespp$complex <- NULL
somespp$complex <- seq(1:nrow(somespp))

bb.stan <- bb.stan[which(bb.stan$complex.wname %in% pepspp),] 
bb.stan$complex <- NULL
dim(bb.stan)
bb.stan <- merge(bb.stan, somespp, by="complex.wname")
dim(bb.stan)

datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

}





##############################
## Code below not updated! ##
### But I think we will neeed it ##

########## SIDE BAR ##########
## Compare R2 today ##
if(FALSE){
observed.here <- bb.stan$resp

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),] 

# getting predicted values if needed
preds.m2lni.sum <- m2lni.sum[grep("yhat", rownames(m2lni.sum)),]
preds.m2lnistudy.sum <- m2lnistudy.sum[grep("yhat", rownames(m2lnistudy.sum)),]

m2lni.R2 <- 1- sum((observed.here-preds.m2lni.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
m2lnistudy.R2 <- 1- sum((observed.here-preds.m2lnistudy.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)

summary(lm(preds.m2lni.sum[,1]~observed.here)) # Multiple R-squared:  0.6051 (Chill Portions)
summary(lm(preds.m2lnistudy.sum[,1]~observed.here)) # Multiple R-squared:  0.7158 (Chill Portions)

# try the w/ interaction
preds.m2l.winsp.sum <- m2l.winsp.sum[grep("yhat", rownames(m2l.winsp.sum)),]
summary(lm(preds.m2l.winsp.sum[,1]~observed.here)) #Multiple R-squared:  0.6122
}
########## END SIDE BAR ##########
