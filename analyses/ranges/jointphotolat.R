## Started 23 March 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##

#Ypheno,i=alphapheno,sp[i]+betaphoto,sp[i]*P+sigmapheno,I 

#Betaphotosp=alpha_photo_sp+beta_LIXphoto*alphaLI,sp 


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)
set.seed(12221)


# setwd
# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) { setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if
(length(grep("Ignacio", getwd()))>0) { setwd("~/GitHub/ospree/analyses/ranges") 
} else 
  setwd("~/Documents/git/ospree/analyses/ranges")

#--------------------------------------
# Set up the lat model - we can change to whatever range parameter we choose to use in the future
# lat ~ agrand + a[sp] + a[study] + sigma_y 
# a[sp] and a[study] are your standard hierarhical thingys, given hierarchical effect

# Parameters
agrand_lat <- 40
agrand_extent <- 10
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
                          mua_sp=rep(mua_sp[sp], length(whichstudies)*n), mua_study=rep(mua_study[whichstudies], each=n),
                          lat=rnorm(length((whichstudies)*n), 45, 10), extent=rnorm((length(whichstudies)*n), 10, 10))
  simlat <- rbind(simlat, simlatadd)
}
simlat$lat<- agrand_lat + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$extent<- agrand_extent + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$range <- agrand_lat + agrand_extent + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
table(simlat$sp, simlat$study)

library(lme4)
# Fixed effects should be very close to coefficients used in simulating data
# These look good, they get better with higher nsp and nstudy
summary(lme1 <- lmer(lat ~ (1|sp) + (1|study), data = simlat)) 
ranef(lme1)
fixef(lme1)

summary(lme2 <- lmer(extent ~ (1|sp) + (1|study), data = simlat)) 
ranef(lme2)
fixef(lme2)

plot(ranef(lme1)$sp[,], mua_sp)
plot(ranef(lme2)$sp[,], mua_sp)

# Close enough to validate trying Stan, I think
N <- length(simlat$lat)
latstan <- list(latdat = simlat$lat, extentdat = simlat$extent, rangedat = simlat$range, N = N, nsp = nsp, species = simlat$sp, 
	study = simlat$study, nstudy = nstudy)

# Try to run the Stan model
latfit <- stan(file = "stan/jointlat_latmodel.stan", data = latstan, warmup = 500, iter = 1000,
    chains = 1, cores = 1,  control=list(max_treedepth = 15)) 

# pairs(traitfit, pars=c("sigma_sp", "sigma_study", "sigma_y", "lp__"))
# pairs(traitfit, pars=c("mua_sp", "mua_study", "lp__")) # very big!

# Checking against sim data
sigma_y
sigma_asp
sigma_astudy
fitsum[grep("sigma", rownames(fitsum)), "mean"] # 3.5, 4, 4.5 currently
    
# Checking against sim data more, these are okay matches (sp plots suggest we need more species for good estimates?)
agrand
fitsum[grep("agrand", rownames(fitsum)),"mean"] # 38.5 

mua_sp
fitsum[grep("mua_sp\\[", rownames(fitsum)),"mean"]
plot(fitsum[grep("mua_sp\\[", rownames(fitsum)),"mean"]~mua_sp) # pretty good

mua_study
fitsum[grep("mua_study\\[", rownames(fitsum)),"mean"] 
plot(fitsum[grep("mua_study\\[", rownames(fitsum)),"mean"]~mua_study) # pretty good



#--------------------------------------
# Now simulate the phenology side
# pheno ~ a_pheno[sp] + bphoto[sp]*P + sigma_y_pheno
# bphoto[sp] ~ aphoto[sp] + betaLatxPheno*mua_sp

# Parameters for pheno
sigma_apheno <- 3
sigma_ypheno <- 0.5
sigma_aphoto <- 2
betaLatxPheno <- 1.1
betaExtentxPheno <- -0.5

nsp # Same as above (you could vary it but would be a little trickier) 
mua_sp # this is the effect of species trait differences from the trait model (above)
mua_pheno <- rnorm(nsp, 0, sigma_apheno)
mua_photo <- rnorm(nsp, 0, sigma_aphoto)

nph <- 100 # number of observations per species/phenological combination 
Nph <- nsp * nph # obervations per species for phenological event and photoperiod
Pmean <- 6
Psigma <- 10

# Set up the data ...
bphoto <- data.frame(sp=1:nsp, mua_photo=mua_photo, mua_sp=mua_sp)
bphoto$bphoto <- bphoto$mua_photo + betaLatxPheno*bphoto$mua_sp + betaExtentxPheno*bphoto$mua_sp
simpheno <- data.frame(sp=numeric(), mua_pheno=numeric(), bphoto=numeric(), P=numeric())
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


# Try to run the Stan model (takes an hour with 4 cores, [gulp])
jointfit <- stan(file = "joint_latextent_photo.stan", data = traitstanpheno, warmup = 2000, iter = 3000,
    chains = 4, cores = 4,  control=list(max_treedepth = 15)) # 3 hrs on Lizzie's machine!

save(jointfit, file="output/stan/jointlatphoto.Rda")



if(!runfullmodel){
load("output/stan/jointlatphoto.Rda")
}

# Checking against sim data
bigfitpost <- extract(jointfit)
bigfitsum <- summary(jointfit)$summary

sigma_apheno
mean(bigfitpost[["sigma_apheno"]])
sigma_ypheno
mean(bigfitpost[["sigma_ypheno"]])
sigma_aphoto 
mean(bigfitpost[["sigma_bphoto"]])
betaLatxPheno 
mean(bigfitpost[["bphoto_lat"]])
mean(bphoto$bphoto)
mean(bigfitpost[["b_photo_final"]])

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
bigfitsum[grep("mua_sp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mua_sp\\[", rownames(bigfitsum)),"mean"]~mua_sp)

mua_study
bigfitsum[grep("mua_study\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mua_study\\[", rownames(bigfitsum)),"mean"]~mua_study)

mua_pheno
bigfitsum[grep("mua_spheno\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mua_spheno\\[", rownames(bigfitsum)),"mean"]~mua_pheno)

muaphoto
bigfitsum[grep("muaphoto\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("muaphoto\\[", rownames(bigfitsum)),"mean"]~mua_photo)
