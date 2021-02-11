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
library(rstan)
library(rstanarm)
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
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE
    

setwd("..//bb_analysis")
source("source/bbstanleadin.R")


# Species complex for ranges, without crops and need species that do not only have field chilling, z-scored
use.rangespp = TRUE
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE & use.rangespp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.alltypes.ranges
  
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

setwd("..//ranges")

bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")
bb.stan$site <-  paste(bb.stan$provenance.lat, bb.stan$provenance.long)

### find only studies with 2 or more latitudes
multisites<-bb.stan %>% group_by(datasetID) %>% dplyr::summarise(unique_sites = n_distinct(site))
multisites<-filter(multisites, unique_sites>=2)
bb.stan.site<-filter(bb.stan,datasetID %in% c(multisites$datasetID)) ###### this is the datasheet for the intra/inter model

## Do some population stuff, by latitude
getpopz1 <- subset(bb.stan.site, select=c("latbi", "site")) # "datasetID", "study",
getpopz2 <- getpopz1[!duplicated(getpopz1), ]
getpopz <- aggregate(getpopz2["site"], getpopz2["latbi"], FUN=length)
getpopz5 <- subset(getpopz, site>4) # 3
getpopz4 <- subset(getpopz, site>3) # 3
getpopz3 <- subset(getpopz, site>2) # 12
getpopz2 <- subset(getpopz, site>1) # 28

if(FALSE){ ## we shouldn't need this anymore with the new species code but save for now
# Species list ...
naspp <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra",
"Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_nigra", #"Alnus_rubra",
"Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis", "Acer_saccharum",
"Alnus_incana", "Acer_rubrum", "Corylus_cornuta", "Picea_glauca","Robinia_pseudoacacia","Populus_tremuloides") # Will be Corylus_cornuta once data updated

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
}
# Okay, will update what we did in issue #379, as best I can guess it now.

## End sidebar on how we picked these studies
################################################


bb.stan.orig <- bb.stan 
# I think we can remove this line because of new species code but double check!
#bb.stan<- bb.stan[which(bb.stan$latbi %in% allspphere),] # uses about 50% of the bb.stan.orig data



# Check on ambient-only studies ... delete some rows
bb.stanamb <- subset(bb.stan, photo_type=="amb" | force_type=="amb")
unique(bb.stanamb$latbi) # I am not going to check Fagus_sylvatica, but I checked the rest and they all have exp treatments also
# bb.stan <- subset(bb.stan, photo_type!="amb" | force_type!="amb") # deletes about 100 rows 

bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))
bb.stan.site$latbinum <- as.numeric(as.factor(bb.stan.site$latbi))

bb.stan.pop5 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz5$latbi),] # 3 species!
bb.stan.pop4 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz4$latbi),] # 12 species
bb.stan.pop3 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz3$latbi),] # 12 species
bb.stan.pop2 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz2$latbi),] # 28 species

############################################################################
########## Quick data checks and looking for collinearity issues ###########
#################### Started by Cat on 9 Sept 2020 #########################
############################################################################
#ggplot(bb.stan.pop5, aes(as.numeric(photo), as.numeric(chill), colour=latbi)) + geom_point() + facet_grid(datasetID~.)

if(FALSE){
library(egg)
quartz()
cf <- ggplot(bb.stan.pop5, aes(chill, force, colour=latbi)) + geom_point() 
fp <- ggplot(bb.stan.pop5, aes(force, photo, colour=latbi)) + geom_point() 
pc <- ggplot(bb.stan.pop5, aes(photo, chill, colour=latbi)) + geom_point()
ggarrange(cf, fp, pc)

cl <- ggplot(bb.stan.pop5, aes(chill, provenance.lat, colour=latbi)) + geom_point()
fl <- ggplot(bb.stan.pop5, aes(force, provenance.lat, colour=latbi)) + geom_point()
pl <- ggplot(bb.stan.pop5, aes(photo, provenance.lat, colour=latbi)) + geom_point()
ggarrange(cl, fl, pl)

quartz()
cf <- ggplot(bb.stan.pop3, aes(chill, force, colour=latbi)) + geom_point() + theme(legend.position = "none") 
fp <- ggplot(bb.stan.pop3, aes(force, photo, colour=latbi)) + geom_point() 
pc <- ggplot(bb.stan.pop3, aes(photo, chill, colour=latbi)) + geom_point() + theme(legend.position = "none") 
ggarrange(cf, fp, pc)

quartz()
cl <- ggplot(bb.stan.pop3, aes(chill, provenance.lat, colour=latbi)) + geom_point() + theme(legend.position = "none") 
fl <- ggplot(bb.stan.pop3, aes(force, provenance.lat, colour=latbi)) + geom_point()
pl <- ggplot(bb.stan.pop3, aes(photo, provenance.lat, colour=latbi)) + geom_point() + theme(legend.position = "none") 
ggarrange(cl, fl, pl)

### Notes: 1) Should we should remove Vitis and Ribes from pop3 and Ribes from pop5?

}

############################################################################
############################################################################
if(FALSE){
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

############################################################################
############################################################################
# Sidebar by Lizzie on 5 November 2020
if(FALSE){
# Trying to see if we could combine the basic OSPREE model with a simmple linear model (see cheapish_model.stan)
# Not working AT ALL now, the Stan code needs to be updated to walk through each observation, not just the b_force vector, see jointtraitphen.stan and follow that method... #
    
# Create a fake variable to test if my model runs ...     
climvar <- rnorm(length(unique(bb.stan$latbinum)), 10, 5)

goober.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = latbinum,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$latbinum)),
                         X = length(climvar),
                         climvar=climvar
                    )
)

goober = stan('stan/cheapish_model.stan', data = goober.bb,
               iter = 4000, warmup=2500)
}


### find the two data sets from each continent with the most species
contsp<-bb.stan %>% dplyr::group_by(datasetID) %>% dplyr::count(complex.wname)
table(contsp$datasetID) 

bb.stan.2dfs<-filter(bb.stan,datasetID %in% c("flynn18","laube14a"))

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
               iter = 4000, warmup=2500) 

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
}


########################################################
# testing 1, 2, 3 ....
# need to make up new data list with unique ID for each pop x sp
########################################################
bb.stan.here <- bb.stan.pop3 ##lets do the 3 pop
getpop <- paste(bb.stan.here$latbinum, bb.stan.here$site)
bb.stan.here$pophere <- as.numeric(as.factor(getpop))
bb.stan.here$latbinum <- as.numeric(as.factor(bb.stan.here$latbi))
bb.stan.here$datasetnum <- as.numeric(as.factor(bb.stan.here$datasetID))
#poppies <- arrange(bb.stan.here, pophere, latbinum)
#popLookupVec <- unique(poppies[c("pophere","latbinum")])[,"latbinum"]
#popLookupVec <- unique(popLookupVec)
#desMat <- model.matrix(object = ~ 1 + force.z + photo.z + latbinum + datasetnum + pophere, data = bb.stan.here)
datalist.bb.pop <- with(bb.stan.here, 
                    list(y = resp,  
                         force = force.z,
                         photo = photo.z,
                         sp = latbinum,
                         study = datasetnum,
                         pop = pophere,
                         N = nrow(bb.stan.here),
                         n_sp = length(unique(bb.stan.here$latbinum)),
                         n_study = length(unique(bb.stan.here$datasetID)),
                         n_pop = length(unique(bb.stan.here$pophere))
                    )
)
    
m3l.ni = stan('stan/nointer_3levelwpop_force&photo_ncp.stan', data = datalist.bb.pop,
               iter = 7000, warmup=5000, chains=4, control=list(adapt_delta=0.999,max_treedepth = 15))



modelhere <- m3l.ni 
mod.sum <- summary(modelhere)$summary
modtosave <- mod.sum[c(1:12),]
mod.sum[grep("mu_b_force_sp", rownames(mod.sum)),]
mod.sum[grep("mu_b_photo_sp", rownames(mod.sum)),]
mod.sum[grep("sigma", rownames(mod.sum)),] 


save(m3l.ni, file="~/Desktop/forcephoto_popmodel.Rdata")

launch_shinystan(m3l.ni)

# Not sure if I am doing this right, but on first blush seem similar to each other!
mod.sum[grep("b_force\\[", rownames(mod.sum)),] 
mean(mod.sum[grep("b_force\\[", rownames(mod.sum)),] [,1])
range(mod.sum[grep("b_force\\[", rownames(mod.sum)),] [,1])
sd(mod.sum[grep("b_force\\[", rownames(mod.sum)),] [,1])

mean(mod.sum[grep("b_force_sppop\\[", rownames(mod.sum)),] [,1])
range(mod.sum[grep("b_force_sppop\\[", rownames(mod.sum)),] [,1])
sd(mod.sum[grep("b_force_sppop\\[", rownames(mod.sum)),] [,1])


if(FALSE){
  
  library(brms)
  checkreal.force <- brm(resp ~ force + (force|latbi/pophere), data=bb.stan.here, warmup = 1500, iter = 2000, 
                         control = list( adapt_delta = 0.99, max_treedepth=15))
  checkreal.chill <- brm(resp ~ chill + (chill|latbi/pophere), data=bb.stan.here, warmup = 1500, iter = 2000, 
                         control = list( adapt_delta = 0.99, max_treedepth=15))
  checkreal.photo <- brm(resp ~ photo + (photo|latbi/pophere), data=bb.stan.here, warmup = 1500, iter = 2000, 
                         control = list( adapt_delta = 0.99, max_treedepth=15))
  
  checkreal.all <- brm(resp ~ force + photo + (force + photo|latbi/pophere), data=bb.stan.here, warmup = 1500, iter = 2000, 
                       control = list( adapt_delta = 0.99, max_treedepth=15))
  
  bb.stan.here$studynum <- as.numeric(as.factor(bb.stan.here$datasetID))
  checkreal.all <- brm(resp ~ datasetID + force.z + photo.z + (force.z + photo.z|latbi/pophere), data=bb.stan.here, warmup = 1500, iter = 2000, 
                       control = list( adapt_delta = 0.99, max_treedepth=15))
  
  checkreal.all <- brm(resp ~ (1|datasetID) + force.z + photo.z + (force.z + photo.z|latbi/pophere), data=bb.stan.here, warmup = 2500, iter = 4000, 
                       control = list( adapt_delta = 0.99, max_treedepth=15))
  
  
  ###(1 | A/B) translates to (1 | A) + (1 | A:B) where A:B simply means creating a new grouping factor with the levels of A and B pasted together. 
  bb.stan.here <- bb.stan.pop3
  getpop <- paste(bb.stan.here$latbinum, bb.stan.here$site)
  bb.stan.here$pophere <- as.numeric(as.factor(getpop))
  bb.stan.here$latbinum <- as.numeric(as.factor(bb.stan.here$latbi))
  
  #modpop3 <- stan_lmer(formula = resp ~ force.z+chill.z+photo.z+(force.z+chill.z+photo.z|latbinum)+(force.z+chill.z+photo.z|latbinum:pophere), 
    #                        data = bb.stan.here,iter=8000,warmup=7000,chains=4, prior = normal(0,20),prior_intercept = normal(35,20) )

  modpop3.force <- stan_lmer(formula = resp ~ force.z+(force.z|latbinum/pophere), 
                       data = bb.stan.here,iter=4500,warmup=2500,chains=4, prior = normal(0,20),prior_intercept = normal(35,20) )
  
  modpop3.photo <- stan_lmer(formula = resp ~ photo.z+(photo.z|latbinum/pophere), 
                       data = bb.stan.here,iter=4500,warmup=2500,chains=4, prior = normal(0,20),prior_intercept = normal(35,20) )
  
  modpop3.chill <- stan_lmer(formula = resp ~ chill.z+(chill.z|latbinum/pophere), 
                       data = bb.stan.here,iter=4500,warmup=2500,chains=4, prior = normal(0,20),prior_intercept = normal(35,20) )
  
  modpop3.forcephoto <- stan_lmer(formula = resp ~ force.z+photo.z+(force.z+photo.z|latbinum/pophere), 
                       data = bb.stan.here,iter=4500,warmup=2500,chains=4, prior = normal(0,20),prior_intercept = normal(35,20) )
  
  modpop3 <- stan_lmer(formula = resp ~ force.z+chill.z+photo.z+(force.z+chill.z+photo.z|latbinum/pophere), 
                     data = bb.stan.here,iter=4500,warmup=2500,chains=4, prior = normal(0,20),prior_intercept = normal(35,20) )
  
  save(modpop3, file="~/Desktop/Misc/Ospree misc/popmodel3_arm.Rdata")

#launch_shinystan(modpop3)

  bb.stan.here <- bb.stan.pop5 ##lets do the 5 pop
  getpop <- paste(bb.stan.here$latbinum, bb.stan.here$provenance.lat)
  bb.stan.here$pophere <- as.numeric(as.factor(getpop))
  bb.stan.here$latbinum <- as.numeric(as.factor(bb.stan.here$latbi))
  
  modpop5 <- stan_lmer(formula = resp ~ force.z+chill.z+photo.z+(force.z+chill.z+photo.z|latbinum)+(force.z+chill.z+photo.z|latbinum:pophere), 
                       data = bb.stan.here,iter=8000,warmup=7000,chains=4, prior = normal(0,20),prior_intercept = normal(35,20) )
  
  
  
  
  PPD1 <- posterior_predict(goo, re.form =  ~ latbinum)
  PPD2 <- posterior_predict(goo, re.form =  ~ latbinum:pophere)
      
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
