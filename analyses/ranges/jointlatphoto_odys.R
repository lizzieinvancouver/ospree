## Started 4 June 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##
## Converted by Cat for the cluster

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


--------------------------------
# Set up the trait model
# trait ~ agrand + a[sp] + a[study] + sigma_y 
# a[sp] and a[study] are your standard hierarhical thingys, given hierarchical effect

  # Parameters
  agrand <- 40
sigma_asp <- 10
sigma_astudy <- 5
sigma_y <- 0.5

n <- 10 # number of replicates per sp x study (may eventually want to draw this from a distribution to make data more realistic)
nsp <- 30 # number of species
nstudy <- 30 # number of studies
studymin <- nstudy # min number of studies a species appears in
studymax <- nstudy # max number of studies as species appears in
howmanystudiespersp <- round(runif(nsp, studymin, studymax)) # get list of studies per species

# Sp and study-level parameters (hyperparameters?)
mua_sp <- rnorm(nsp, 0, sigma_asp)
mua_study <- rnorm(nstudy, 0, sigma_astudy)

# Set up the data ...
simlat <- data.frame(sp=numeric(), study=numeric(), mua_sp=numeric(), mua_study=numeric())
for (sp in 1:nsp){
  whichstudies <- sample(c(1:nstudy), howmanystudiespersp[sp])
  simlatadd <- data.frame(sp=rep(sp, length(whichstudies)*n), study=rep(whichstudies, each=n),
                          mua_sp=rep(mua_sp[sp], length(whichstudies)*n), mua_study=rep(mua_study[whichstudies], each=n))
  simlat <- rbind(simlat, simlatadd)
}
simlat$lat <- agrand+simlat$mua_sp+simlat$mua_study+rnorm(nrow(simlat), 0, sigma_y)
table(simlat$sp, simlat$study)


# Close enough to validate trying Stan, I think
N <- length(simlat$lat)
latstan <- list(latdat = simlat$lat, N = N, nsp = nsp, species = simlat$sp, 
                study = simlat$study, nstudy = nstudy)

latfit.ncpvector <- stan(file = "stan/jointlat_latmodel_ncp.stan", data = latstan, warmup = 2000, iter = 3000,
               chains = 4, cores = 4,  control=list(max_treedepth = 15)) 

save(latfit.ncpvector, file="/n/wolkovich_lab/Lab/Cat/latfit_ncpvector.Rda")

latfit.ncp <- stan(file = "/n/wolkovich_lab/Lab/Cat/jointlat_latmodel_ncp.stan", data = latstan, warmup = 2000, iter = 3000,
                           chains = 4, cores = 4) #,  control=list(max_treedepth = 15)) 

save(latfit.ncp, file="/n/wolkovich_lab/Lab/Cat/latfit_ncp.Rda")




#--------------------------------------
# Now simulate the phenology side
# pheno ~ a_pheno[sp] + bphoto[sp]*P + sigma_y_pheno
# bphoto[sp] ~ aphoto[sp] + betaLatxPheno*mua_sp

# Parameters for pheno
sigma_apheno <- 3
sigma_ypheno <- 0.5
sigma_aphoto <- 2
betaLatxPheno <- 1.1

nsp # Same as above (you could vary it but would be a little trickier) 
mua_sp # this is the effect of species trait differences from the trait model (above)
mua_pheno <- rnorm(nsp, 0, sigma_apheno)
mua_photo <- rnorm(nsp, 0, sigma_aphoto)

nph <- 100 # number of observations per species/phenological combination 
Nph <- nsp * nph # obervations per species for phenological event and forcing
Fmean <- 3
Fsigma <- 10

# Set up the data ...
bphoto <- data.frame(sp=1:nsp, mua_photo=mua_photo, mua_sp=mua_sp)
bphoto$bphoto <- bforce$mua_force + betaTraitxPheno*bphoto$mua_sp
simpheno <- data.frame(sp=numeric(), mua_pheno=numeric(), bphoto=numeric(), F=numeric())
for (sp in 1:nsp){
  Phere <- rnorm(nph, Pmean, Psigma)
  simphenoadd <- data.frame(sp=rep(sp, nph), mua_pheno=rep(mua_pheno[sp], nph),
                            bphoto=rep(bphoto$bphoto[sp], nph), P=Phere)
  simpheno <- rbind(simpheno, simphenoadd)
}
simpheno$pheno <- simpheno$mua_pheno+simpheno$bphoto*simpheno$P+rnorm(nrow(simpheno), 0, sigma_ypheno)
table(simpheno$sp)


# Nothing left to do but to try Stan, I think
Npheno <- length(simpheno$pheno)
latstanpheno <- list(latdat = simlat$lat, N = N, nsp = nsp, species = simlat$sp, 
                     study = simlat$study, nstudy = nstudy, phendat = simpheno$pheno, Npheno = Npheno, nsppheno = nsp,
                     speciespheno = simpheno$sp, photoperiod = simpheno$P)



jointfit <- stan(file = "/n/wolkovich_lab/Lab/Cat/jointlatphen.stan", data = latstanpheno, warmup = 2000, iter = 3000,
                 chains = 4, cores = 4,  control=list(max_treedepth = 15)) # 3 hrs on Lizzie's machine!

save(jointfit, file="/n/wolkovich_lab/Lab/Cat/jointlatphoto.Rda")

