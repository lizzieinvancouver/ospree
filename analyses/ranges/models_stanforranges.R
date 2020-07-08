## Started 8 June 2020 ##
## By Lizzie ##

## Where'd Lizzie go? She disappeared ... #
## but wait! It's 3 July 2020 and she's back. ##

## Take 1: Stole this code from bb_analysis/models_stan.R

## To do
# (a) subset down to relevant block/transplant treatments for gomory15??
# Impt: not dealing with provenance and material (which mean some treatments show up more than once but the partial pooling should handle this we think)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# libraries
library(shinystan)
library(reshape2)

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
use.flags.for.mainmodel <- FALSE
use.flags.for.spcomp.cp <- FALSE
use.flags.for.allspp.utah <- TRUE
use.flags.for.spcomp.utah.nonz <- FALSE
use.flags.for.spcomp.cp.nonz <- FALSE # predictors on natural scale, spcomplex with utah units. Fig 3-4 in main text of budburst ms
use.flags.for.allspp.utah.nonz <- FALSE
use.yourown.flagdesign <- FALSE

setwd("..//bb_analysis")
source("source/flags.for.models.in.bbms.R")
source("source/bbstanleadin.R")
setwd("..//ranges")

bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")
unique(bb.stan$latbi)

naspp <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra",
"Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior", "Alnus_rubra",
"Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis", "Acer_saccharum",
"Alnus_incana", "Acer_rubrum", "Cornus_cornuta", "Picea_glauca") # Will be Corylus_cornuta once data updated

eurspp <- c("Abies_alba", "Acer_pseudoplatanus", "Aesculus_hippocastanum", "Alnus_glutinosa",
"Alnus_incana", "Betula_pendula", "Betula_pubescens", "Carpinus_betulus",
"Cornus_mas", "Corylus_avellana", "Fagus_sylvatica", "Fraxinus_excelsior", "Larix_decidua", "Picea_abies", "Populus_tremula", "Prunus_avium", "Prunus_padus", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Sorbus_aucuparia", "Tilia_cordata")    

allspphere <- c(naspp, eurspp)
allspphere[which(!allspphere %in% unique(bb.stan$latbi))]
# To discuss! How did we pick these species? See issue #379 ... I think we do not want Alnus rubra so okay to let it drop

bb.stan.orig <- bb.stan
bb.stan <- bb.stan[which(bb.stan$latbi %in% allspphere),] # uses about 50% of the bb.stan.orig data

# Check on ambient-only studies ... delete some rows
bb.stanamb <- subset(bb.stan, photo_type=="amb" | force_type=="amb")
unique(bb.stanamb$latbi) # I am not going to check Fagus_sylvatica, but I checked the rest and they all have exp treatments also
# bb.stan <- subset(bb.stan, photo_type!="amb" | force_type!="amb") # deletes the same rows as below ...
bb.stan <- subset(bb.stan, force_type!="amb") # deletes about 100 rows

bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))
   
datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = latbinum,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$latbinum))
                    )
)

######################################
## Overview of the model run below ##
######################################
# It's our basic model with partial pooliing on slopes and intercepts
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan('..//bb_analysis/stan/nointer_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500) 

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

}
####### END SIDE BAR #######


#####part2 extract the posteriors for cue~range paramenter modeling
sample <- rstan::extract(m2l.ni)### extract the posteriors

sample.force <- melt(sample$b_force) ###grab them for each cue
sample.chill <- melt(sample$b_chill)
sample.photo <- melt(sample$b_photo)

names(sample.force) <- c("iter", "latbinum", "b_force") ##rename
names(sample.chill) <- c("iter", "latbinum", "b_chill")
names(sample.photo) <- c("iter", "latbinum", "b_photo")

cue.df<-left_join(sample.force, sample.chill) ##merge them into one data sheet step1
cue.df<-left_join(cue.df,sample.photo) ### "" step 2
cue.df <- subset(cue.df, iter>1500) ## remove warmup iterations from analyses
concordance<-dplyr::select(bb.stan,latbi,latbinum)
concordance<-unique(concordance)

cue.df<-left_join(cue.df,concordance)
write.csv(cue.df,"output/cue_posteriors.csv",row.names = FALSE)
