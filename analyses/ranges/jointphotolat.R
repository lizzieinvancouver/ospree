## Started 23 March 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##

#Ypheno,i=alphapheno,sp[i]+betaphoto,sp[i]*P+sigmapheno,I 

#Betaphotosp=alpha_photo_sp+beta_LIXphoto*alphaLI,sp 

library(rstan)

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
a_min <- 30
a_max <- 60
a_mean <- 45
sigma_y <- 5

n <- 10 # number of replicates per sp x study (may eventually want to draw this from a distribution to make data more realistic)
nsp <- 30 # number of species

### This will be a fixed effects model but I think we need some mua_sp to create some variation around our species estimates
sigma_asp <- 0.5
mua_sp <- rnorm(nsp, 0, sigma_asp)

# Set up the data ...
simlat <- data.frame(sp=rep(1:nsp, each=10), mua_sp=rep(mua_sp, each=10))

simlat$minlat <- a_min + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$maxlat <- a_max + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$meanlat <- a_mean + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)

library(lme4)
# Fixed effects should be very close to coefficients used in simulating data
summary(lme1 <- lmer(minlat ~ (1|sp), data = simlat)) 
ranef(lme1)
fixef(lme1)

summary(lme2 <- lmer(maxlat ~ (1|sp), data = simlat)) 
ranef(lme2)
fixef(lme2)

summary(lme3 <- lmer(meanlat ~ (1|sp), data = simlat)) 
ranef(lme3)
fixef(lme3)

plot(ranef(lme1)$sp[,], mua_sp)
plot(ranef(lme2)$sp[,], mua_sp)
plot(ranef(lme3)$sp[,], mua_sp)

# Close enough to validate trying Stan, I think
N <- length(simlat$minlat)
latstan <- list(mindat = simlat$minlat, maxdat = simlat$maxlat, meandat = simlat$meanlat, N = N, 
                nsp = nsp, species = simlat$sp)

# Try to run the Stan model
latfit <- stan(file = "stan/jointlat_latmodel.stan", data = latstan, warmup = 1000, iter = 2000,
    chains = 4,  control=list(max_treedepth = 15)) 
fitsum <- summary(latfit)$summary

# pairs(traitfit, pars=c("sigma_sp", "sigma_study", "sigma_y", "lp__"))
# pairs(traitfit, pars=c("mua_sp", "mua_study", "lp__")) # very big!

# Checking against sim data
sigma_y
a_min
a_max
a_mean
fitsum[grep("sigma", rownames(fitsum)), "mean"] # 5.09
fitsum[grep("a_", rownames(fitsum)), "mean"]
    
# Checking against sim data more, these are okay matches (sp plots suggest we need more species for good estimates?)
a_min
mean(fitsum[grep("a_mins_sp", rownames(fitsum)),"mean"]) # 30.19
a_max
mean(fitsum[grep("a_maxs_sp", rownames(fitsum)),"mean"]) # 60.08
a_mean
mean(fitsum[grep("a_means_sp", rownames(fitsum)),"mean"]) # 44.7

fitsum[grep("a_mins_sp\\[", rownames(fitsum)),"mean"]
plot(fitsum[grep("a_mins_sp\\[", rownames(fitsum)),"mean"]~unique(ave(simlat$minlat, simlat$sp))) # pretty good

fitsum[grep("a_maxs_sp\\[", rownames(fitsum)),"mean"]
plot(fitsum[grep("a_maxs_sp\\[", rownames(fitsum)),"mean"]~unique(ave(simlat$maxlat, simlat$sp))) # pretty good

fitsum[grep("a_means_sp\\[", rownames(fitsum)),"mean"]
plot(fitsum[grep("a_means_sp\\[", rownames(fitsum)),"mean"]~unique(ave(simlat$meanlat, simlat$sp))) # pretty good


##### Great! Now let's work with real data...
latdat <- read.csv("output/latitudeextents_allspp.csv")

N <- length(latdat$lat)
latstan <- list(mindat = latdat$lat[latdat$type=="min"], maxdat = latdat$lat[latdat$type=="max"], 
                meandat = latdat$lat[latdat$type=="mean"], N = N, 
                nsp = nsp, species = latdat$species)

latmodel <- stan(file = "stan/jointlat_latmodel.stan", data = latstan, warmup = 1000, iter = 2000,
               chains = 4,  control=list(max_treedepth = 15)) 


#--------------------------------------
# Now simulate the phenology side
# pheno ~ a_pheno[sp] + bphoto[sp]*P + sigma_y_pheno
# bphoto[sp] ~ aphoto[sp] + betaLatxPheno*mua_sp

# Parameters for pheno
sigma_apheno <- 3
sigma_ypheno <- 0.5
sigma_aphoto <- 2
betaMinLatxPheno <- -1
betaMaxLatxPheno <- -0.5
betaMeanLatxPheno <- -2

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
bphoto$bphotomin <- bphoto$mua_photo + betaMinLatxPheno*bphoto$mua_sp
bphoto$bphotomax <- bphoto$mua_photo + betaMaxLatxPheno*bphoto$mua_sp
bphoto$bphotomean <- bphoto$mua_photo + betaMeanLatxPheno*bphoto$mua_sp

simpheno <- data.frame(sp=numeric(), mua_pheno=numeric(), 
                       bphotomin=numeric(), bphotomax=numeric(), bphotomean=numeric(), 
                       P=numeric())
for (sp in 1:nsp){
  Phere <- rnorm(nph, Pmean, Psigma)
  simphenoadd <- data.frame(sp=rep(sp, nph), mua_pheno=rep(mua_pheno[sp], nph),
                            bphotomin=rep(bphoto$bphotomin[sp], nph),
                            bphotomax=rep(bphoto$bphotomax[sp], nph),
                            bphotomean=rep(bphoto$bphotomean[sp], nph), P=Phere)
  simpheno <- rbind(simpheno, simphenoadd)
}
simpheno$pheno <- simpheno$mua_pheno + simpheno$bphotomin*simpheno$P +
  simpheno$bphotomax*simpheno$P + simpheno$bphotomean*simpheno$P +
  rnorm(nrow(simpheno), 0, sigma_ypheno)
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
