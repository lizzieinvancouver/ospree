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

# Our flags for ranges, for now ... (see issue #379)
use.chillports = FALSE
use.zscore = TRUE
use.allspp = TRUE
use.multcuespp = FALSE
use.cropspp = TRUE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE
    

setwd("..//bb_analysis")
source("source/bbstanleadin.R")
setwd("..//ranges")

bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")
# unique(bb.stan$latbi)

## Do some population stuff, by latitude
getpopz1 <- subset(bb.stan, select=c("latbi", "provenance.lat")) # "datasetID", "study",
getpopz2 <- getpopz1[!duplicated(getpopz1), ]
getpopz <- aggregate(getpopz2["provenance.lat"], getpopz2["latbi"], FUN=length)
getpopz5 <- subset(getpopz, provenance.lat>4) # 9
getpopz3 <- subset(getpopz, provenance.lat>2) # 29
getpopz2 <- subset(getpopz, provenance.lat>1) # 54

# Species list ...
naspp <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra",
"Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior", "Alnus_rubra",
"Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis", "Acer_saccharum",
"Alnus_incana", "Acer_rubrum", "Cornus_cornuta", "Picea_glauca") # Will be Corylus_cornuta once data updated

eurspp <- c("Abies_alba", "Acer_pseudoplatanus", "Aesculus_hippocastanum", "Alnus_glutinosa",
"Alnus_incana", "Betula_pendula", "Betula_pubescens", "Carpinus_betulus",
"Cornus_mas", "Corylus_avellana", "Fagus_sylvatica", "Fraxinus_excelsior", "Larix_decidua", "Picea_abies", "Populus_tremula", "Prunus_avium", "Prunus_padus", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Sorbus_aucuparia", "Tilia_cordata")    

allspphere <- c(naspp, eurspp)
allspphere[which(!allspphere %in% unique(bb.stan$latbi))]

################################################
## Start sidebar on how we picked these studies

# species in more than two papers
getspp2papers1 <- subset(bb.stan, select=c("latbi", "datasetID")) 
getspp2papers2 <- getspp2papers1[!duplicated(getspp2papers1), ]
getspp2papers3 <- aggregate(getspp2papers2["datasetID"], getspp2papers2["latbi"], FUN=length)
spp2papers <- subset(getspp2papers3, datasetID>1)
spp2papers[order(spp2papers$latbi),]

spp3cues1 <- subset(bb.stan, chill_type!="fldest")
spp3cues2 <- subset(spp3cues1, select=c("latbi", "datasetID", "study", "force", "photo", "chill"))
spp3cuescounts <-
      ddply(spp3cues2, c("latbi", "datasetID", "study"), summarise,
      nforce = length(unique(force)),
      nphoto = length(unique(photo)),
      nchill = length(unique(chill)))

spp3cues <- subset(spp3cuescounts, nforce>1 & nphoto>1 & nchill>1) # this is 172 spp if you exclude field chilling you get worrall67 and flynn18 added

justcues1 <- subset(bb.stan, chill_type!="fldest")
justcues2 <- subset(justcues1, select=c("latbi", "force", "photo", "chill"))
justcuescounts <-
      ddply(justcues2, c("latbi"), summarise,
      nforce = length(unique(force)),
      nphoto = length(unique(photo)),
      nchill = length(unique(chill)))

sppcuecounts <- subset(justcuescounts, nforce>2 & nphoto>2 & nchill>2) # 5 species

unique(spp3cues$latbi)
sort(union(unique(spp3cues$latbi), spp2papers$latbi))
setdiff(allspphere, union(unique(spp3cues$latbi), spp2papers$latbi))

setdiff(union(unique(spp3cues$latbi), spp2papers$latbi), allspphere)

# Okay, will update what we did in issue #379, as best I can guess it now.

## End sidebar on how we picked these studies
################################################


bb.stan.orig <- bb.stan
bb.stan <- bb.stan[which(bb.stan$latbi %in% allspphere),] # uses about 50% of the bb.stan.orig data

# Check on ambient-only studies ... delete some rows
bb.stanamb <- subset(bb.stan, photo_type=="amb" | force_type=="amb")
unique(bb.stanamb$latbi) # I am not going to check Fagus_sylvatica, but I checked the rest and they all have exp treatments also
# bb.stan <- subset(bb.stan, photo_type!="amb" | force_type!="amb") # deletes about 100 rows 

bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))

bb.stan.pop5 <- bb.stan[which(bb.stan$latbi %in% getpopz5$latbi),] # 8 species!
bb.stan.pop3 <- bb.stan[which(bb.stan$latbi %in% getpopz3$latbi),] # 25 species
bb.stan.pop2 <- bb.stan[which(bb.stan$latbi %in% getpopz2$latbi),] # 34 species

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

if(FALSE){
########################################################
# testing 1, 2, 3 ....
# need to make up new data list with unique ID for each pop x sp
########################################################
bb.stan.here <- bb.stan.pop3
getpop <- paste(bb.stan.here$latbinum, bb.stan.here$provenance.lat)
bb.stan.here$pophere <- as.numeric(as.factor(getpop))
bb.stan.here$latbinum <- as.numeric(as.factor(bb.stan.here$latbi))
datalist.bb.pop <- with(bb.stan.here, 
                    list(y = resp,  
                         force = force.z, 
                         sp = latbinum,
                         pop = pophere,
                         N = nrow(bb.stan.here),
                         n_sp = length(unique(bb.stan.here$latbinum)),
                         n_pop = length(unique(bb.stan.here$pophere))
                    )
)
    
m3l.ni = stan('stan/nointer_3levelwpop.stan', data = datalist.bb.pop,
               iter = 4500, warmup=3000, control=list(adapt_delta=0.95))
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
