## Started 4 June 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##
## Converted by Cat for the cluster

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#--------------------------------------
# Now simulate the phenology side
# pheno ~ a_pheno[sp] + bphoto[sp]*P + sigma_y_pheno
# bphoto[sp] ~ aphoto[sp] + betaLatxPheno*mua_sp

# Parameters for pheno
sigma_yphoto <- 1
sigma_yforce <- 2
sigma_ychill <- 2
sigma_y <- 2

### Okay do we actually want to know what the influence of "more northern than normal" 
## lats is rather than the actual latitude?
# So maybe we should center the data?
# Parameters
a_min <- 0
a_max <- 0

sigma_bphotomin <- 2
sigma_bphotomax <- 2
sigma_bforcemin <- 0.5
sigma_bforcemax <- 0.5
sigma_bchillmin <- 1
sigma_bchillmax <- 1

beta_photomin <- 1
beta_photomax <- 2

beta_forcemin <- -1
beta_forcemax <- -2

beta_chillmin <- -0.5
beta_chillmax <- 3

n <- 10 # number of replicates per sp x study (may eventually want to draw this from a distribution to make data more realistic)
nsp <- 30 # number of species

### This will be a fixed effects model but I think we need some mua_sp to create some variation around our species estimates
## And now let's add a greater sigma since our data is centered
sigma_asp <- 2
mua_sp <- rnorm(nsp, 0, sigma_asp)

# Set up the data ...
simlat <- data.frame(sp=rep(1:nsp, each=10), mua_sp=rep(mua_sp, each=10))

simlat$minlat <- a_min + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$maxlat <- a_max + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)


nsp # Same as above (you could vary it but would be a little trickier) 
mua_sp # this is the effect of species trait differences from the trait model (above)

sigma_aphoto <- 1
sigma_aforce <- 1.5
sigma_achill <- 2

a_photo <- rnorm(nsp, -2, sigma_aphoto)
a_force <- rnorm(nsp, -4, sigma_aforce)
a_chill <- rnorm(nsp, -7, sigma_achill)

mua_photomin <- -2
mua_photomax <- -1

mua_forcemin <- -2
mua_forcemax <- 0.5

mua_chillmin <- -1
mua_chillmax <- 2

sigma_aphotomin <- 0.5
sigma_aphotomax <- 0.5
sigma_aforcemin <- 0.5
sigma_aforcemax <- 0.5
sigma_achillmin <- 0.5
sigma_achillmax <- 0.5

Pmean <- 6
Psigma <- 2

Fmean <- 10
Fsigma <- 3

Cmean <- 5
Csigma <- 3

simpheno <- data.frame(sp=numeric(), a_photo=numeric(), a_force=numeric(), a_chill=numeric(), P=numeric(), F=numeric(), C=numeric())

nph <- 50 # number of observations per species/phenological combination 
Nph <- nsp * nph # obervations per species for phenological event and photoperiod

for (sp in 1:nsp){
  Phere <- rnorm(nph, Pmean, Psigma)
  Fhere <- rnorm(nph, Fmean, Fsigma)
  Chere <- rnorm(nph, Cmean, Csigma)
  simphenoadd <- data.frame(sp=rep(sp, nph), a_photo=rep(a_photo[sp], nph),
                            a_force=rep(a_force[sp], nph),
                            a_chill=rep(a_chill[sp], nph), P=Phere, F=Fhere, C=Chere)
  simpheno <- rbind(simpheno, simphenoadd)
}


# Nothing left to do but to try Stan, I think
simlat <- data.frame(sp=rep(1:nsp, each=10), mua_sp=rep(mua_sp, each=10))

simlat$minlat <- a_min + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$maxlat <- a_max + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)

#bphoto_min <- mua_photomin + beta_photomin*mua_sp 
bphoto_min <- mua_photomin * simlat$minlat + rnorm(nrow(simpheno), 0, sigma_aphotomin)
bphoto_max <- mua_photomax * simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_aphotomax)

bforce_min <- mua_forcemin * simlat$minlat + rnorm(nrow(simpheno), 0, sigma_aforcemin)
bforce_max <- mua_forcemax * simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_aforcemax)

bchill_min <- mua_chillmin * simlat$minlat + rnorm(nrow(simpheno), 0, sigma_achillmin)
bchill_max <- mua_chillmax * simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_achillmax)

simpheno$photodat <- simpheno$a_photo + simpheno$P*bphoto_min + simpheno$P*bphoto_max + rnorm(nrow(simpheno), 0, sigma_yphoto)
simpheno$forcedat <- simpheno$a_force + simpheno$F*bforce_min + simpheno$F*bforce_max + rnorm(nrow(simpheno), 0, sigma_yforce)
simpheno$chilldat <- simpheno$a_chill + simpheno$C*bchill_min + simpheno$C*bchill_max + rnorm(nrow(simpheno), 0, sigma_ychill)

#agrand <- 40

#simpheno$bbresp <- agrand + simpheno$photo + simpheno$force + simpheno$chill + rnorm(nrow(simpheno), 0, sigma_y) 

table(simpheno$sp)

N <- length(simlat$minlat)

Npheno <- length(simpheno$photodat)
latstanpheno <- list(mindat = simlat$minlat, maxdat = simlat$maxlat,
                     photodat = simpheno$photodat, forcedat = simpheno$forcedat,
                     chilldat = simpheno$chilldat,
                     N = N, nsp = nsp, species = simlat$sp, 
                     Npheno = Npheno, nsppheno = nsp,
                     speciespheno = simpheno$sp, photoperiod = simpheno$P, forcing = simpheno$F, chilling = simpheno$C)

jointfit <- stan(file = "/n/wolkovich_lab/Lab/Cat/joint_latextent_cuesresp.stan", data = latstanpheno, warmup = 2000, iter = 3000,
                 chains = 4, cores = 4,  control=list(max_treedepth = 15)) 

save(jointfit, file="/n/wolkovich_lab/Lab/Cat/jointlatallcue.Rda")

