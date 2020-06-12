## Started 8 June 2020 ##
## By Lizzie ##

## Take 1: Stole this code from bb_analysis/models_stan.R

## To do
# (a) subset down to relevant block/transplant treatments for gomory15??
# Impt: not dealing with provenance and material (which mean some treatments show up more than once but the partial pooling should handle this we think)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

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

setwd("..//bb_analysis")
source("source/flags.for.models.in.bbms.R")
source("source/bbstanleadin.R")
setwd("..//ranges")

naspp <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra",
"Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior", "Alnus_rubra",
"Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis", "Acer_saccharum",
"Alnus_incana", "Acer_rubrum", "Cornus_cornuta", "Picea_glauca")

eurspp <- c("Abies_alba", "Acer_pseudoplatanus", "Aesculus_hippocastanum", "Alnus_glutinosa",
"Alnus_incana", "Betula_pendula", "Betula_pubescens", "Carpinus_betulus",
"Cornus_mas", "Corylus_avellana", "Fagus_sylvatica", "Fraxinus_excelsior", "Larix_decidua", "Picea_abies", "Populus_tremula", "Prunus_avium", "Prunus_padus", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Sorbus_aucuparia", "Tilia_cordata")    

allspphere <- c(naspp, eurspp)
allspphere[which(!allspphere %in% unique(bb.stan$complex.wname))]

# To discuss! I swap in some species
## START HERE! Instead of the below, better to actually pull out the original species I think ... 
subspecies <- c("Populus_grandidentata"="Populus_tremuloides",
                "Fagus_grandifolia"="Fagus_complex",
                "Acer_pensylvanicum"="",
                "Alnus_rubra"="",
                "Prunus_pensylvanica"="",
                "Cornus_cornuta"="",
                "Quercus_ilex"="")
allsppwsubs <- unname(subspecies[allspphere])


bb.stan.orig <- bb.stan
bb.stan <- bb.stan[which(bb.stan$complex.wname %in% allspphere),]

# Next ... add column for home continent ... then figure out model... then re-label species names as numeric... 


######################################
## Overview of the model run below ##
######################################
# New model ... THINKING still.... 
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan('stan/nointer_2levelTEST.stan', data = datalist.bb,
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

if(use.flags.for.mainmodel){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfputah_z.Rda")
}

if (use.flags.for.spcomp.cp){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfpcp_z.Rda")
}

if (use.flags.for.allspp.utah){
  save(m2l.ni, file="stan/output/m2lni_allsppwcrop_utah_z.Rda")
}

if (use.flags.for.spcomp.utah.nonz){
  save(m2l.ni, file="stan/output/m2lni_spcompalltypesutah_nonz.Rda")
}

if (use.flags.for.spcomp.cp.nonz){
  save(m2l.ni, file="stan/output/m2lni_spcompexprampfpcp_nonz.Rda")
}

if (use.flags.for.allspp.utah.nonz){
  save(m2l.ni, file="stan/output/m2lni_allsppwcrop_utah_nonz.Rda")
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

