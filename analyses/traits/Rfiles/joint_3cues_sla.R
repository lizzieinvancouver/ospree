# Started June 23, 2021 by D. Loughnan

# Finally we have a working model for the test data. Now we can start trying to get the model to run with the actual data!

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/ospree/analyses/traits")
} else{
  setwd("/home/deirdre/ospree")
}

library(rstan)
require(shinystan)
require(bayesplot)
require(truncnorm)
library(ggplot2)


rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

####################################################

# Get the trait data:
# Get the data
dat1 <- read.csv("input/try_bien_nodups_1.csv") 
dat2 <- read.csv("input/try_bien_nodups_2.csv") 

dat <- rbind(dat1, dat2)
names(dat)

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

dat.sub <- dat[dat$speciesname %in% traitors.sp,]

#get the appropriate ospree data: all species, utah
bbstan <- read.csv("input/bbstan_allspp.utah.csv") 
bbstan$speciesname <- paste(bbstan$genus, bbstan$species, sep = "_")

bbstan.spp <- bbstan[bbstan$speciesname %in% traitors.sp,] 

length(unique(bbstan.spp$speciesname)) 

################################################################################
# Starting with a single trait: sla
sla <- subset(dat.sub, traitname == "Specific_leaf_area")

length(unique(sla$speciesname)) #26 species


pheno_data <- list(yTraiti = sla$traitvalue, 
                   N = nrow(sla), 
                   n_spec = length(unique(sla$speciesname)), 
                   species = as.numeric(as.factor(sla$speciesname)), 
                   study = as.numeric(as.factor(sla$datasetid)), 
                   n_study = length(unique(sla$datasetid)), 
                   yPhenoi = bbstan.spp$resp, 
                   Nph = nrow(bbstan.spp), 
                   forcei = bbstan.spp$force.z,
                   photoi = bbstan.spp$photo.z, 
                   chilli = bbstan.spp$chill.z,
                   species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
                   )

mdl.sla <- stan('stan/joint_3cue_newprior.stan',
                                         data = pheno_data, iter = 8000, chains = 4)
 
save(mdl.sla, file = "output.joint.3cue.sla.Rda") 
 
sort(unique(bbstan.spp$species))
sort(unique(sla$species))

load("output/output.joint.3cue.sla.Rda")

ssm <-  as.shinystan(mdl.sla)
launch_shinystan(ssm)

sum.sla <- summary(mdl.sla)$summary
post.sla<- rstan::extract(mdl.sla)


col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat")

# manually to get right order
mu_params <- c("mu_grand","muPhenoSp","muForceSp","muChillSp","muPhotoSp", "sigmaTrait_y","sigmapheno_y","sigma_sp", "sigma_study","sigmaForceSp","sigmaChillSp","sigmaPhotoSp","sigmaPhenoSp", "betaTraitxForce","betaTraitxChill", "betaTraitxPhoto")

mdl.output <- sum.sla[mu_params, col4table]

####################################################
# Next traits: LDMC
####################################################
ldmc <- subset(dat.sub, traitname == "Leaf_dry_matter_content")
length(unique(ldmc$speciesname)) #26 species

length(unique(bbstan.spp$speciesname)) 

pheno_data <- list(yTraiti = ldmc$traitvalue, 
                   N = nrow(ldmc), 
                   n_spec = length(unique(ldmc$speciesname)), 
                   species = as.numeric(as.factor(ldmc$speciesname)), 
                   study = as.numeric(as.factor(ldmc$datasetid)), 
                   n_study = length(unique(ldmc$datasetid)), 
                   yPhenoi = bbstan.spp$resp, 
                   Nph = nrow(bbstan.spp), 
                   forcei = bbstan.spp$force.z,
                   photoi = bbstan.spp$photo.z, 
                   chilli = bbstan.spp$chill.z,
                   species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
)

mdl.ldmc <- stan('stan/joint_3cue_newprior.stan',
                data = pheno_data, iter = 4000, chains = 4)

save(mdl.ldmc, file = "output.joint.3cue.ldmc.Rda") 

####################################################
# Next traits: LNC
####################################################
lnc <- subset(dat.sub, traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")
length(unique(lnc$speciesname)) #26 species

length(unique(bbstan.spp$speciesname)) 

pheno_data <- list(yTraiti = lnc$traitvalue, 
                   N = nrow(lnc), 
                   n_spec = length(unique(lnc$speciesname)), 
                   species = as.numeric(as.factor(lnc$speciesname)), 
                   study = as.numeric(as.factor(lnc$datasetid)), 
                   n_study = length(unique(lnc$datasetid)), 
                   yPhenoi = bbstan.spp$resp, 
                   Nph = nrow(bbstan.spp), 
                   forcei = bbstan.spp$force.z,
                   photoi = bbstan.spp$photo.z, 
                   chilli = bbstan.spp$chill.z,
                   species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
)

mdl.lnc <- stan('stan/joint_3cue_newprior.stan',
                 data = pheno_data, iter = 4000, chains = 4)

save(mdl.lnc, file = "output.joint.3cue.lnc.Rda") 

####################################################
# Next traits: seed mass
####################################################
seed <- subset(dat.sub, traitname == "seed mass")
length(unique(seed$speciesname)) #26 species

length(unique(bbstan.spp$speciesname)) 

pheno_data <- list(yTraiti = seed$traitvalue, 
                   N = nrow(seed), 
                   n_spec = length(unique(seed$speciesname)), 
                   species = as.numeric(as.factor(seed$speciesname)), 
                   study = as.numeric(as.factor(seed$datasetid)), 
                   n_study = length(unique(seed$datasetid)), 
                   yPhenoi = bbstan.spp$resp, 
                   Nph = nrow(bbstan.spp), 
                   forcei = bbstan.spp$force.z,
                   photoi = bbstan.spp$photo.z, 
                   chilli = bbstan.spp$chill.z,
                   species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
)

mdl.seed <- stan('stan/joint_3cue_newprior.stan',
                 data = pheno_data, iter = 4000, chains = 4)

save(mdl.seed, file = "output.joint.3cue.ldmc.Rda") 

####################################################
# Next traits: SSD
####################################################
ssd <- subset(dat.sub, traitname == "Leaf_dry_matter_content")
length(unique(ssd$speciesname)) #26 species

length(unique(bbstan.spp$speciesname)) 

pheno_data <- list(yTraiti = ssd$traitvalue, 
                   N = nrow(ssd), 
                   n_spec = length(unique(ssd$speciesname)), 
                   species = as.numeric(as.factor(ssd$speciesname)), 
                   study = as.numeric(as.factor(ssd$datasetid)), 
                   n_study = length(unique(ssd$datasetid)), 
                   yPhenoi = bbstan.spp$resp, 
                   Nph = nrow(bbstan.spp), 
                   forcei = bbstan.spp$force.z,
                   photoi = bbstan.spp$photo.z, 
                   chilli = bbstan.spp$chill.z,
                   species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
)

mdl.ssd <- stan('stan/joint_3cue_newprior.stan',
                 data = pheno_data, iter = 4000, chains = 4)

save(mdl.ssd, file = "output.joint.3cue.ssd.Rda") 

####################################################
# Next traits: height 
####################################################
ht <- subset(dat.sub, traitname == "Leaf_dry_matter_content")
length(unique(ht$speciesname)) #26 species

length(unique(bbstan.spp$speciesname)) 

pheno_data <- list(yTraiti = ht$traitvalue, 
                   N = nrow(ht), 
                   n_spec = length(unique(ht$speciesname)), 
                   species = as.numeric(as.factor(ht$speciesname)), 
                   study = as.numeric(as.factor(ht$datasetid)), 
                   n_study = length(unique(ht$datasetid)), 
                   yPhenoi = bbstan.spp$resp, 
                   Nph = nrow(bbstan.spp), 
                   forcei = bbstan.spp$force.z,
                   photoi = bbstan.spp$photo.z, 
                   chilli = bbstan.spp$chill.z,
                   species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
)

mdl.ht <- stan('stan/joint_3cue_newprior.stan',
                 data = pheno_data, iter = 4000, chains = 4)

save(mdl.ht, file = "output.joint.3cue.ht.Rda") 

