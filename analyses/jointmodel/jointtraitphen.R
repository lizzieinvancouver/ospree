## Started 23 March 2020 ##
## By Lizzie mainly pulling from JointModelSim_fj.R ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)

# flags to run Stan
runtraitmodel <- FALSE
runfullmodel <- TRUE

# setwd
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/joint_model")

#--------------------------------------
# Set up the trait model
# trait ~ agrand + a[sp] + a[study] + sigma_y 
# a[sp] and a[study] are your standard hierarhical thingys, given hierarchical effect

# Parameters
agrand <- 40
sigma_asp <- 7
sigma_astudy <- 4
sigma_y <- 0.5

n <- 10 # number of replicates per sp x study (may eventually want to draw this from a distribution to make data more realistic)
nsp <- 20 # number of species
nstudy <- 30 # number of studies
studymin <- 10 # min number of studies a species appears in
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

library(lme4)
# Fixed effects should be very close to coefficients used in simulating data
summary(lme1 <- lmer(trait ~ (1|sp) + (1|study), data = simtrait)) 
ranef(lme1)
fixef(lme1)

plot(ranef(lme1)$study[,], mua_study)
plot(ranef(lme1)$sp[,], mua_sp)

# Close enough to validate trying Stan, I think
N <- length(simtrait$trait)
traitstan <- list(traitdat = simtrait$trait, N = N, nsp = nsp, species = simtrait$sp, 
	study = simtrait$study, nstudy = nstudy)
if(runtraitmodel){
# Try to run the Stan model
traitfit <- stan(file = "jointtrait_traitmodel.stan", data = traitstan, warmup = 3000, iter = 4000,
    chains = 4, cores = 4,  control=list(max_treedepth = 15))

fitsum <- summary(traitfit)$summary

pairs(traitfit, pars=c("sigma_sp", "sigma_study", "sigma_y", "lp__"))
pairs(traitfit, pars=c("agrand", "muaSp", "muaStudy", "lp__"))
pairs(traitfit, pars=c("muaSp", "muaStudy", "lp__"))

# Checking against sim data
sigma_y
mua_sp
mua_study
fitsum[grep("sigma", rownames(fitsum)), "mean"]

# Checking against sim data more, these are okay matches (sp plots suggest we need more species for good estimates?)
agrand
fitsum[grep("agrand", rownames(fitsum)),"mean"]

mua_sp
fitsum[grep("muaSp\\[", rownames(fitsum)),"mean"]
plot(fitsum[grep("muaSp\\[", rownames(fitsum)),"mean"]~mua_sp)

mua_study
fitsum[grep("muaStudy\\[", rownames(fitsum)),"mean"] 
plot(fitsum[grep("muaStudy\\[", rownames(fitsum)),"mean"]~mua_study)
}

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
Npheno <- length(simpheno$pheno)
traitstanpheno <- list(traitdat = simtrait$trait, N = N, nsp = nsp, species = simtrait$sp, 
	study = simtrait$study, nstudy = nstudy, phendat = simpheno$pheno, Npheno = Npheno, nsppheno = nsp,
        speciespheno = simpheno$sp, forcing = simpheno$F)

if(runfullmodel){
# Try to run the Stan model (takes an hour with 4 cores, [gulp])
bigfit <- stan(file = "jointtraitphen.stan", data = traitstanpheno, warmup = 3000, iter = 4000,
    chains = 4, cores = 4,  control=list(max_treedepth = 15))

save(bigfit, file="output/stan/bigtry.Rda")
}


if(!runfullmodel){
load("output/stan/bigtry.Rda")
}

# Checking against sim data
bigfitpost <- extract(bigfit)
bigfitsum <- summary(bigfit)$summary

sigma_apheno
mean(bigfitpost[["sigma_apheno"]])
sigma_ypheno
mean(bigfitpost[["sigma_ypheno"]])
sigma_aforce 
mean(bigfitpost[["sigma_bforce"]])
betaTraitxPheno 
mean(bigfitpost[["bforce_trait"]])
mean(bforce$bforce)
mean(bigfitpost[["b_force_final"]])

# Trait model
sigma_y 
mean(bigfitpost[["sigma_y"]])
agrand 
mean(bigfitpost[["agrand"]])
sigma_asp 
mean(bigfitpost[["sigma_sp"]])
sigma_astudy 
mean(bigfitpost[["sigma_study"]])

# Hyperparameters
mua_sp
bigfitsum[grep("muaSp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("muaSp\\[", rownames(bigfitsum)),"mean"]~mua_sp)

mua_study
bigfitsum[grep("muaStudy\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("muaStudy\\[", rownames(bigfitsum)),"mean"]~mua_study)

muaSpheno
bigfitsum[grep("muaSpheno\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("muaSpheno\\[", rownames(bigfitsum)),"mean"]~mua_pheno)

muaforce
bigfitsum[grep("muaforce\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("muaforce\\[", rownames(bigfitsum)),"mean"]~mua_force)
