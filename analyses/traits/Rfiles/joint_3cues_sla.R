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

mdl.sla <- stan('stan/joint_3cue_sla_stan.stan',
                                         data = pheno_data, iter = 8000, chains = 4)

save(mdl.sla, file = "output.joint.3cue.sla.2.Rda")

sort(unique(bbstan.spp$species))
sort(unique(sla$species))

load("output/output.joint.3cue.sla.Rda")

# ssm <-  as.shinystan(mdl.sla)
# launch_shinystan(ssm)

sum.sla <- summary(mdl.sla)$summary
post.sla<- rstan::extract(mdl.sla)

range(sum.sla[, "n_eff"])

# plot the priors for the traits model
plot(density(post.sla$sigmaTrait_y), xlim = c(0, 20)) ; lines(density(rnorm(1000, 5,1)), col = "red")
h1 <- hist(rnorm(1000, 5,1))
h2 <- hist(post.sla$sigmaTrait_y)

plot(h1, col=rgb(1,0,1,1/4), xlim = c(0,8))
plot(h2, col=rgb(0,0,1,1/4), add = T)

plot(density(post.sla$sigma_sp)); lines(density(rnorm(1000, 10, 0.5)), col = "red")
h1 <- hist(rnorm(1000, 10, 0.5))
h2 <- hist(post.sla$sigma_sp)

plot(h2, col=rgb(0,0,1,1/4), xlim = c(6, 15))
plot(h1, col=rgb(1,0,1,1/4), add = T)

plot(density(post.sla$sigma_study)); lines(density(rnorm(1000, 5, 0.5)), col = "red")

h1 <- hist(rnorm(1000, 5, 0.5))
h2 <- hist(post.sla$sigma_study)

plot(h2, col=rgb(0,0,1,1/4), xlim = c(0, 15))
plot(h1, col=rgb(1,0,1,1/4), add = T)

plot(density(post.sla$mu_grand), xlim = c(0, 20)) ; lines(density(rnorm(1000, 10, 0.1)), col = "red")
plot(density(post.sla$muSp)); lines(density(rnorm(1000, 0, 30)), col = "red")
h1 <- hist(rnorm(1000, 0, 10))
h2 <- hist(post.sla$muSp)

plot(h2, col=rgb(0,0,1,1/4), xlim = c(0, 15))
plot(h1, col=rgb(1,0,1,1/4), add = T)

plot(density(post.sla$muStudy)); lines(density(rnorm(1000, 0, 5)), col = "red")

# check the priors for the phenology model
plot(density(post.sla$sigmapheno_y), xlim = c(0, 20)) ; lines(density(rnorm(1000, 15,3)), col = "red")
plot(density(post.sla$sigmaForceSp)); lines(density(rnorm(1000, 5, 0.1)), col = "red")
plot(density(post.sla$muForceSp)); lines(density(rnorm(1000, -2, 0.5)), col = "red")
plot(density(post.sla$sigmaChillSp), xlim = c(0, 20)) ; lines(density(rnorm(1000, 5, 0.5)), col = "red")
plot(density(post.sla$muChillSp)); lines(density(rnorm(1000, -3,0.5)), col = "red")
plot(density(post.sla$sigmaPhotoSp), xlim = c(0, 20)) ; lines(density(rnorm(1000, 5, 0.5)), col = "red")
plot(density(post.sla$muPhotoSp)); lines(density(rnorm(1000, -2,0.5)), col = "red")
plot(density(post.sla$sigmaPhenoSp), xlim = c(0, 20)) ; lines(density(rnorm(1000, 10, 0.5)), col = "red")
plot(density(post.sla$muPhenoSp)); lines(density(rnorm(1000, 30, 10)), col = "red")

plot(density(post.sla$betaTraitxChill), xlim = c(-10,5)); lines(density(rnorm(1000, -2, 1)), col = "red")
h1 <- hist(rnorm(1000, 0, 1))
h2 <- hist(post.sla$betaTraitxChill)

plot(h2, col=rgb(0,0,1,1/4), xlim = c(-2, 6))
plot(h1, col=rgb(1,0,1,1/4), add = T)

plot(density(post.sla$betaTraitxPhoto), xlim = c(-2,2)); lines(density(rnorm(1000, -2, 1)), col = "red")
plot(density(post.sla$betaTraitxForce), xlim = c(-2,2)); lines(density(rnorm(1000, -2, 1)), col = "red")

col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat","n_eff")

# manually to get right order
mu_params <- c("mu_grand","muPhenoSp","muForceSp","muChillSp","muPhotoSp", "sigmaTrait_y","sigmapheno_y","sigma_sp", "sigma_study","sigmaForceSp","sigmaChillSp","sigmaPhotoSp","sigmaPhenoSp", "betaTraitxForce","betaTraitxChill", "betaTraitxPhoto")

mdl.output <- sum.sla[mu_params, col4table]
mdl.output

# go back to the test data and test why it only worked when -ve, don't really know what the prior should be
# try making the priors very large! ie wide, look into what the postivie slopes do with other numbers
# model is very sensitive to test
# if it is very sensitive then you just want to know the limitations

# start my increasing the no. samples, see if mdl drifts bc it is difficult, increase sp and reps

# if worried it is sensitive, make a set of test data that is small and very similar to the actual data

#plot the data and plot the data and then plot the slopes on top

#double check that the cues are z-scored both if 1 and if in the bb lead in

# Email Thackeray and check in with them
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

mdl.ldmc <- stan('stan/joint_3cue_ldmcmdl.stan',
                data = pheno_data, iter = 4000, chains = 4)

save(mdl.ldmc, file = "output.joint.3cue.ldmc.Rda") 

# 8000 exceed tree depth large Rhat
####################################################
# Next traits: LNC
####################################################
 lnc <- subset(dat.sub, traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")
# length(unique(lnc$speciesname)) #26 species
# 
# length(unique(bbstan.spp$speciesname)) 
# 
# pheno_data <- list(yTraiti = lnc$traitvalue, 
#                    N = nrow(lnc), 
#                    n_spec = length(unique(lnc$speciesname)), 
#                    species = as.numeric(as.factor(lnc$speciesname)), 
#                    study = as.numeric(as.factor(lnc$datasetid)), 
#                    n_study = length(unique(lnc$datasetid)), 
#                    yPhenoi = bbstan.spp$resp, 
#                    Nph = nrow(bbstan.spp), 
#                    forcei = bbstan.spp$force.z,
#                    photoi = bbstan.spp$photo.z, 
#                    chilli = bbstan.spp$chill.z,
#                    species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
# )
# 
# mdl.lnc <- stan('stan/joint_3cue_newprior.stan',
#                  data = pheno_data, iter = 4000, chains = 4)
# 
# save(mdl.lnc, file = "output.joint.3cue.lnc.Rda") 

load("output/output.joint.3cue.lnc.Rda")

ssm <-  as.shinystan(mdl.sla)
launch_shinystan(ssm)

sum.lnc <- summary(mdl.lnc)$summary
post.lnc<- rstan::extract(mdl.lnc)


plot(density(post.lnc$sigmaTrait_y), xlim = c(0, 20)) ; lines(density(rnorm(1000, 15,1)), col = "red")
plot(density(post.lnc$sigma_sp)); lines(density(rnorm(1000, 10, 0.5)), col = "red")
plot(density(post.lnc$sigma_study)); lines(density(rnorm(1000, 5, 0.5)), col = "red")
plot(density(post.lnc$mu_grand), xlim = c(0, 20)) ; lines(density(rnorm(1000, 10, 0.1)), col = "red")
plot(density(post.lnc$muSp)); lines(density(rnorm(1000, 0, 10)), col = "red")
plot(density(post.lnc$muStudy)); lines(density(rnorm(1000, 0, 5)), col = "red")

# check the priors for the phenology model
plot(density(post.lnc$sigmapheno_y), xlim = c(0, 20)) ; lines(density(rnorm(1000, 5,3)), col = "red")
plot(density(post.lnc$sigmaForceSp)); lines(density(rnorm(1000, 5, 0.1)), col = "red")
plot(density(post.lnc$muForceSp)); lines(density(rnorm(1000, -1, 0.5)), col = "red")
plot(density(post.lnc$sigmaChillSp), xlim = c(0, 20)) ; lines(density(rnorm(1000, 5, 0.5)), col = "red")
plot(density(post.lnc$muChillSp)); lines(density(rnorm(1000, -2,0.5)), col = "red")
plot(density(post.lnc$sigmaPhotoSp), xlim = c(0, 20)) ; lines(density(rnorm(1000, 5, 0.5)), col = "red")
plot(density(post.lnc$muPhotoSp)); lines(density(rnorm(1000, -2,0.5)), col = "red")
plot(density(post.lnc$sigmaPhenoSp), xlim = c(0, 20)) ; lines(density(rnorm(1000, 10, 0.5)), col = "red")
plot(density(post.lnc$muPhenoSp)); lines(density(rnorm(1000, 30, 10)), col = "red")

plot(density(post.lnc$betaTraitxChill)); lines(density(rnorm(1000, 2, 1)), col = "red")
plot(density(post.lnc$betaTraitxPhoto)); lines(density(rnorm(1000, 2, 1)), col = "red")
plot(density(post.lnc$betaTraitxForce)); lines(density(rnorm(1000, 2, 1)), col = "red")

####################################################
# Next traits: seed mass
####################################################
# seed <- subset(dat.sub, traitname == "seed mass")
# length(unique(seed$speciesname)) #26 species
# 
# length(unique(bbstan.spp$speciesname)) 
# 
# pheno_data <- list(yTraiti = seed$traitvalue, 
#                    N = nrow(seed), 
#                    n_spec = length(unique(seed$speciesname)), 
#                    species = as.numeric(as.factor(seed$speciesname)), 
#                    study = as.numeric(as.factor(seed$datasetid)), 
#                    n_study = length(unique(seed$datasetid)), 
#                    yPhenoi = bbstan.spp$resp, 
#                    Nph = nrow(bbstan.spp), 
#                    forcei = bbstan.spp$force.z,
#                    photoi = bbstan.spp$photo.z, 
#                    chilli = bbstan.spp$chill.z,
#                    species2 = as.numeric(as.factor(bbstan.spp$speciesname)) 
# )
# 
# mdl.seed <- stan('stan/joint_3cue_newprior.stan',
#                  data = pheno_data, iter = 4000, chains = 4)
# 
# save(mdl.seed, file = "output.joint.3cue.ldmc.Rda") 

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
                 data = pheno_data, iter = 8000, chains = 4)

save(mdl.ssd, file = "output.joint.3cue.ssd.Rda") 

#8000 that exceed max tree depth
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
                data = pheno_data, iter = 6000, chains = 4, 
                control = list(max_treedepth = 11))

save(mdl.ht, file = "output.joint.3cue.ht.Rda") 
#8000 iterations 