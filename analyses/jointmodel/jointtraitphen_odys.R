## Started 4 June 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##
## Converted by Cat for the cluster

require(shinystan)
require(rstan)

options(stringsAsFactors = FALSE)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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
simtrait <- data.frame(sp=numeric(), study=numeric(), mua_sp=numeric(), mua_study=numeric())
for (sp in 1:nsp){
  whichstudies <- sample(c(1:nstudy), howmanystudiespersp[sp])
  simtraitadd <- data.frame(sp=rep(sp, length(whichstudies)*n), study=rep(whichstudies, each=n),
                            mua_sp=rep(mua_sp[sp], length(whichstudies)*n), mua_study=rep(mua_study[whichstudies], each=n))
  simtrait <- rbind(simtrait, simtraitadd)
}
simtrait$trait <- agrand+simtrait$mua_sp+simtrait$mua_study+rnorm(nrow(simtrait), 0, sigma_y)
table(simtrait$sp, simtrait$study)


# Close enough to validate trying Stan, I think
N <- length(simtrait$trait)
traitstan <- list(traitdat = simtrait$trait, N = N, nsp = nsp, species = simtrait$sp, 
                  study = simtrait$study, nstudy = nstudy)


traitfit.ncp <- stan(file = "/n/wolkovich_lab/Lab/Cat/jointtrait_traitmodel_ncp.stan", data = traitstan, warmup = 2000, iter = 3000,
                           chains = 4, cores = 2,  control=list(max_treedepth = 15)) 

save(traitfit.ncp, file="/n/wolkovich_lab/Lab/Cat/traitfit_ncp.Rda")




#--------------------------------------
# Now simulate the phenology side
# pheno ~ a_pheno[sp] + bforce[sp]*F + sigma_y_pheno
# bforce[sp] ~ aforce[sp] + betaTraitxPheno*mua_sp

# Parameters for pheno
sigma_apheno <- 3
sigma_ypheno <- 0.5
sigma_aforce <- 2
betaTraitxPheno <- 1.1

nsp # Same as above (you could vary it but would be a little trickier) 
mua_sp # this is the effect of species trait differences from the trait model (above)
mua_pheno <- rnorm(nsp, 0, sigma_apheno)
mua_force <- rnorm(nsp, 0, sigma_aforce)

nph <- 100 # number of observations per species/phenological combination 
Nph <- nsp * nph # obervations per species for phenological event and forcing
Fmean <- 3
Fsigma <- 10

# Set up the data ...
bforce <- data.frame(sp=1:nsp, mua_force=mua_force, mua_sp=mua_sp)
bforce$bforce <- bforce$mua_force + betaTraitxPheno*bforce$mua_sp
simpheno <- data.frame(sp=numeric(), mua_pheno=numeric(), bforce=numeric(), F=numeric())
for (sp in 1:nsp){
  Fhere <- rnorm(nph, Fmean, Fsigma)
  simphenoadd <- data.frame(sp=rep(sp, nph), mua_pheno=rep(mua_pheno[sp], nph),
                            bforce=rep(bforce$bforce[sp], nph), F=Fhere)
  simpheno <- rbind(simpheno, simphenoadd)
}
simpheno$pheno <- simpheno$mua_pheno+simpheno$bforce*simpheno$F+rnorm(nrow(simpheno), 0, sigma_ypheno)
table(simpheno$sp)


# Nothing left to do but to try Stan, I think
N <- length(simtrait$trait)
Npheno <- length(simpheno$pheno)
traitstanpheno <- list(traitdat = simtrait$trait, N = N, nsp = nsp, species = simtrait$sp, 
                       study = simtrait$study, nstudy = nstudy, phendat = simpheno$pheno, Npheno = Npheno, nsppheno = nsp,
                       speciespheno = simpheno$sp, forcing = simpheno$F)



bigfit.ncp <- stan(file = "/n/wolkovich_lab/Lab/Cat/jointtraitphen_ncp.stan", data = traitstanpheno, warmup = 2000, iter = 3000,
               chains = 4, cores = 2,  control=list(max_treedepth = 15)) # 3 hrs on Lizzie's machine!

save(bigfit.ncp, file="/n/wolkovich_lab/Lab/Cat/bigtry_ncp.Rda")

