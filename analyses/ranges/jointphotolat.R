## Started 23 March 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##

#Ypheno,i=alphapheno,sp[i]+betaphoto,sp[i]*P+sigmapheno,I 

#Betaphotosp=alpha_photo_sp+beta_LIXphoto*alphaLI,sp 

library(rstan)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

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
#a_mean <- 45
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
#simlat$meanlat <- a_mean + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)

library(lme4)
# Fixed effects should be very close to coefficients used in simulating data
summary(lme1 <- lmer(minlat ~ (1|sp), data = simlat)) 
ranef(lme1)
fixef(lme1)

summary(lme2 <- lmer(maxlat ~ (1|sp), data = simlat)) 
ranef(lme2)
fixef(lme2)

#summary(lme3 <- lmer(meanlat ~ (1|sp), data = simlat)) 
#ranef(lme3)
#fixef(lme3)

plot(ranef(lme1)$sp[,], mua_sp)
plot(ranef(lme2)$sp[,], mua_sp)
#plot(ranef(lme3)$sp[,], mua_sp)

# Close enough to validate trying Stan, I think
N <- length(simlat$minlat)
latstan <- list(mindat = simlat$minlat, maxdat = simlat$maxlat, meandat = simlat$meanlat, N = N, 
                nsp = nsp, species = simlat$sp)

# Try to run the Stan model
if(FALSE){
latfit <- stan(file = "stan/jointlat_latmodel.stan", data = latstan, warmup = 500, iter = 1000,
    chains = 1,  control=list(max_treedepth = 15)) 
fitsum <- summary(latfit)$summary
}
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


if(FALSE){
##### Great! Now let's work with real data...
latdat <- read.csv("output/minmax_rangeextent.csv")

N <- length(latdat$minlat)
latstan <- list(mindat = latdat$minlat, maxdat = latdat$maxlat, 
                N = N, 
                species = as.numeric(as.factor(latdat$species)), nsp = length(unique(latdat$species)))

latmodel <- stan(file = "stan/jointlat_latmodel.stan", data = latstan, warmup = 1000, iter = 2000,
               chains = 4,  control=list(max_treedepth = 15)) 

fitsum <- summary(latmodel)$summary

ymins <- latdat$minlat
ymaxs <- latdat$maxlat

fitsum[grep("sigma", rownames(fitsum)), "mean"] # 5.09
fitsum[grep("a_", rownames(fitsum)), "mean"]

# Checking against sim data more, these are okay matches (sp plots suggest we need more species for good estimates?)
mean(latdat$minlat) ## 40.1
mean(fitsum[grep("a_mins_sp", rownames(fitsum)),"mean"]) # 38.2
mean(latdat$maxlat) ## 53.96
mean(fitsum[grep("a_maxs_sp", rownames(fitsum)),"mean"]) # 51.9
#a_mean
#mean(fitsum[grep("a_means_sp", rownames(fitsum)),"mean"]) # 44.7

fitsum[grep("a_mins_sp\\[", rownames(fitsum)),"mean"]
plot(fitsum[grep("a_mins_sp\\[", rownames(fitsum)),"mean"]~unique(ave(latdat$minlat, latdat$species))) # okay

fitsum[grep("a_maxs_sp\\[", rownames(fitsum)),"mean"]
plot(fitsum[grep("a_maxs_sp\\[", rownames(fitsum)),"mean"]~unique(ave(latdat$maxlat, latdat$species))) # not great

#fitsum[grep("a_means_sp\\[", rownames(fitsum)),"mean"]
#plot(fitsum[grep("a_means_sp\\[", rownames(fitsum)),"mean"]~unique(ave(simlat$meanlat, simlat$sp))) 


shinystan::launch_shinystan(latmodel)
}

#--------------------------------------
# Now simulate the phenology side
# pheno ~ a_pheno[sp] + bphoto[sp]*P + sigma_y_pheno
# bphoto[sp] ~ aphoto[sp] + betaLatxPheno*mua_sp

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
beta_photomax <- 0.5

beta_forcemin <- -0.5
beta_forcemax <- -1

beta_chillmin <- -0.5
beta_chillmax <- 1.5

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

mua_photomin <- -1
mua_photomax <- -0.5

mua_forcemin <- -1
mua_forcemax <- 0.5

mua_chillmin <- -0.5
mua_chillmax <- 1

sigma_aphotomin <- 0.5
sigma_aphotomax <- 0.5
sigma_aforcemin <- 0.5
sigma_aforcemax <- 0.5
sigma_achillmin <- 0.5
sigma_achillmax <- 0.5

Pmean <- 6
Psigma <- 2

Fmean <- 5
Fsigma <- 2

Cmean <- 2
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
bphoto_min <- mua_photomin + beta_photomin * simlat$minlat + rnorm(nrow(simpheno), 0, sigma_aphotomin)
bphoto_max <- mua_photomax + beta_photomax * simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_aphotomax)

bforce_min <- mua_forcemin + beta_forcemin * simlat$minlat + rnorm(nrow(simpheno), 0, sigma_aforcemin)
bforce_max <- mua_forcemax + beta_forcemax * simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_aforcemax)

bchill_min <- mua_chillmin + beta_chillmin * simlat$minlat + rnorm(nrow(simpheno), 0, sigma_achillmin)
bchill_max <- mua_chillmax + beta_chillmax * simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_achillmax)

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
                     speciespheno = simpheno$sp, photoperiod = simpheno$P, 
                     forcing = simpheno$F, chilling = simpheno$C, latmins = simlat$a_min, latmaxs = simlat$a_max)



# Try to run the Stan model 
jointfit <- stan(file = "stan/joint_latextent_cuesresp.stan", data = latstanpheno, warmup = 500, iter = 1000,
    chains = 1, cores = 2,  control=list(max_treedepth = 12)) 

#save(jointfit, file="output/stan/jointlatphoto.Rda")



if(!runfullmodel){
load("~/Desktop/ranges_jointmod.Rda")
}

# Checking against sim data
bigfitpost <- rstan::extract(jointfit)
bigfitsum <- rstan::summary(jointfit)$summary

sd(simpheno$photo) ## 6.74
mean(bigfitsum[grep("sigma_yphoto", rownames(bigfitsum)),"mean"]) ### 6.96
sd(simpheno$force) ## 12.42
mean(bigfitsum[grep("sigma_yforce", rownames(bigfitsum)),"mean"]) ### 13.71
sd(simpheno$chill) ## 19.64
mean(bigfitsum[grep("sigma_ychill", rownames(bigfitsum)),"mean"]) ### 20.95


mean(simpheno$a_photo) ## -1.89
mean(bigfitsum[grep("a_photo", rownames(bigfitsum)),"mean"]) ## 0.32
mean(simpheno$a_force) ## -4.1
mean(bigfitsum[grep("a_force", rownames(bigfitsum)),"mean"]) ## 0.5
mean(simpheno$a_chill) ## -6.4
mean(bigfitsum[grep("a_chill", rownames(bigfitsum)),"mean"]) ## 0.92

mean(simlat$minlat) ## 0.16
mean(bigfitsum[grep("a_mins_sp", rownames(bigfitsum)),"mean"]) # 0.09
mean(simlat$maxlat) ## 0.07
mean(bigfitsum[grep("a_maxs_sp", rownames(bigfitsum)),"mean"]) # 0.05


bigfitsum[grep("a_mins_sp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_mins_sp\\[", rownames(bigfitsum)),"mean"]~unique(ave(simlat$minlat, simlat$sp))) # great

bigfitsum[grep("a_maxs_sp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_maxs_sp\\[", rownames(bigfitsum)),"mean"]~unique(ave(simlat$maxlat, simlat$sp))) # great

# Trait model
sigma_y  ## 2
mean(bigfitpost[["sigma_y"]]) ## 2.01
a_min ## 0
mean(bigfitpost[["a_mins_sp"]]) ## 0.09
a_max ## 0
mean(bigfitpost[["a_maxs_sp"]]) ## 0.05

# Hyperparameters
a_photo
bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]~a_photo) ### this okay...

bphoto_min
bigfitsum[grep("mu_bphotomin\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mu_bphotomin\\[", rownames(bigfitsum)),"mean"]~bphoto_min) ### bad!!!!
bphoto_max
bigfitsum[grep("mu_bphotomax\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mu_bphotomax\\[", rownames(bigfitsum)),"mean"]~bphoto_max) ### bad

#######
a_force
bigfitsum[grep("a_force\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_force\\[", rownames(bigfitsum)),"mean"]~a_force) ### this is good

bforce_min
bigfitsum[grep("mu_bforcemin\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mu_bforcemin\\[", rownames(bigfitsum)),"mean"]~bforce_min) ### this okay...
bforce_max
bigfitsum[grep("mu_bforcemax\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mu_bforcemax\\[", rownames(bigfitsum)),"mean"]~bforce_max) ### this okay...

#######
a_chill
bigfitsum[grep("a_chill\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_chill\\[", rownames(bigfitsum)),"mean"]~a_chill) ### this looks good!

bchill_min
bigfitsum[grep("mu_bchillmin\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mu_bchillmin\\[", rownames(bigfitsum)),"mean"]~bchill_min) ### this okay...
bchill_max
bigfitsum[grep("mu_bchillmax\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("mu_bchillmax\\[", rownames(bigfitsum)),"mean"]~bchill_max) ### this okay...


#######
yphotos <- simpheno$photodat
yforces <- simpheno$forcedat
ychills <- simpheno$chilldat

y_ppphoto <- bigfitpost$y_ppphoto[1:1500]
y_ppforce <- bigfitpost$y_ppforce[1:1500]
y_ppchill <- bigfitpost$y_ppchill[1:1500]

plot(yphotos~y_ppphoto)
plot(yforces~y_ppforce)
plot(ychills~y_ppchill)

shinystan::launch_shinystan(jointfit)

# Set up colors
library(viridis)
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(viridis_pal(option="viridis")(12), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

figpath <- "figures"

modelhere <- jointfit

source("source/photocue_muplot.R")
figpathmore <- "photocue"
muplotfx(modelhere, "", 7, 8, c(0,3), c(-10, 10) , 12, 3)

source("source/forcecue_muplot.R")
figpathmore <- "forcecue"
muplotfx(modelhere, "", 7, 8, c(0,3), c(-10, 10) , 12, 3)

source("source/chillcue_muplot.R")
figpathmore <- "chillcue"
muplotfx(modelhere, "", 7, 8, c(0,3), c(-15, 10) , 12, 3)


