# Started June 23, 2021 by D. Loughnan

# Finally we have a working model for the test data. Now we can start trying to get the model to run with the actual data!

# if(length(grep("deirdreloughnan", getwd()) > 0)) {
#   setwd("~/Documents/github/ospree/analyses/traits")
# } else{
#   setwd("~/home/deirdre/ospree")
# }

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
# Starting with a single trait: height
sla <- subset(dat.sub, traitname == "Specific_leaf_area")

missing.sp <- c("Betula_populifolia","Juglans_regia","Quercus_coccifera")
sla <- sla[!sla$speciesname %in% missing.sp,]
length(unique(sla$speciesname)) #26 species

####################################################

bbstan <- read.csv("input/bbstan_mainmodel.csv") 
bbstan$speciesname <- paste(bbstan$genus, bbstan$species, sep = "_")

bbstan.sla <- bbstan[bbstan$speciesname %in% traitors.sp,]

length(unique(bbstan.sla$speciesname)) # for some reason there are only 23?

pheno_data <- list(yTraiti = sla$traitvalue, 
                   N = nrow(sla), 
                   n_spec = length(unique(sla$speciesname)), 
                   species = as.numeric(as.factor(sla$speciesname)), 
                   study = as.numeric(as.factor(sla$datasetid)), 
                   n_study = length(unique(sla$datasetid)), 
                   yPhenoi = bbstan.sla$resp, 
                   Nph = nrow(bbstan.sla), 
                   forcei = bbstan.sla$force.z,
                   photoi = bbstan.sla$photo.z, 
                   chilli = bbstan.sla$chill.z,
                   species2 = as.numeric(as.factor(bbstan.sla$speciesname)) 
                   )

mdl.sla <- stan('stan/joint_3cue_newprior.stan',
                                         data = pheno_data, iter = 4000, chains = 4)
 
save(mdl.sla, file = "output.joint.3cue.sla.Rda") 
 
sort(unique(bbstan.sla$species))
sort(unique(sla$species))

load("output/output.joint.3cue.sla.Rda")

ssm <-  as.shinystan(mdl.sla)
launch_shinystan(ssm)

sum.sla <- summary(mdl.sla)$summary
post.sla<- rstan::extract(mdl.sla)
