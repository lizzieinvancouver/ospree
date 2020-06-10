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

# Parameters for pheno
sigma_yphoto <- 1
sigma_yforce <- 2
sigma_ychill <- 3
sigma_y <- 2

### Okay do we actually want to know what the influence of "more northern than normal" 
## lats is rather than the actual latitude?
# So maybe we should center the data?
# Parameters
a_min <- 0
a_max <- 0

n <- 10 # number of replicates per sp x study (may eventually want to draw this from a distribution to make data more realistic)
nsp <- 30 # number of species

### This will be a fixed effects model but I think we need some mua_sp to create some variation around our species estimates
## And now let's add a greater sigma since our data is centered
sigma_asp <- 0.5
mua_sp <- rnorm(nsp, 0, sigma_asp)

# Set up the data ...
simlat <- data.frame(sp=rep(1:nsp, each=10), mua_sp=rep(mua_sp, each=10))

simlat$minlat <- a_min + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$maxlat <- a_max + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)


nsp # Same as above (you could vary it but would be a little trickier) 
mua_sp # this is the effect of species trait differences from the trait model (above)
mua_photo <- rnorm(nsp, -2, sigma_yphoto)
mua_force <- rnorm(nsp, -4, sigma_yforce)
mua_chill <- rnorm(nsp, -7, sigma_ychill)

nph <- 50 # number of observations per species/phenological combination 
Nph <- nsp * nph # obervations per species for phenological event and photoperiod


simpheno <- data.frame(sp=numeric(), mua_photo=numeric(), mua_force=numeric(), mua_chill=numeric())

for (sp in 1:nsp){
  simphenoadd <- data.frame(sp=rep(sp, nph), mua_photo=rep(mua_photo[sp], nph),
                            mua_force=rep(mua_force[sp], nph),
                            mua_chill=rep(mua_chill[sp], nph))
  simpheno <- rbind(simpheno, simphenoadd)
}

simpheno$photo <- simpheno$mua_photo + simlat$minlat + simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_yphoto)
simpheno$force <- simpheno$mua_force + simlat$minlat + simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_yforce)
simpheno$chill <- simpheno$mua_chill + simlat$minlat + simlat$maxlat + rnorm(nrow(simpheno), 0, sigma_ychill)

#agrand <- 40

#simpheno$bbresp <- agrand + simpheno$photo + simpheno$force + simpheno$chill + rnorm(nrow(simpheno), 0, sigma_y) 

table(simpheno$sp)


# Nothing left to do but to try Stan, I think
Npheno <- length(simpheno$photo)
latstanpheno <- list(mindat = simlat$minlat, maxdat = simlat$maxlat,
                     photodat = simpheno$photo, forcedat = simpheno$force,
                     chilldat = simpheno$chill,
                     N = N, nsp = nsp, species = simlat$sp, 
                     Npheno = Npheno, nsppheno = nsp,
                     speciespheno = simpheno$sp)


# Try to run the Stan model 
jointfit <- stan(file = "stan/joint_latextent_cuesresp.stan", data = latstanpheno, warmup = 500, iter = 1000,
    chains = 1, cores = 1,  control=list(max_treedepth = 15)) 

#save(jointfit, file="output/stan/jointlatphoto.Rda")



if(!runfullmodel){
load("output/stan/jointlatphoto.Rda")
}

# Checking against sim data
bigfitpost <- extract(jointfit)
bigfitsum <- summary(jointfit)$summary

sigma_yphoto ## 1
mean(bigfitsum[grep("sigma_yphoto", rownames(bigfitsum)),"mean"]) ### 3.3
sigma_yforce ## 2
mean(bigfitsum[grep("sigma_yforce", rownames(bigfitsum)),"mean"]) ### 3.9
sigma_ychill ## 3
mean(bigfitsum[grep("sigma_ychill", rownames(bigfitsum)),"mean"]) ### 4.4


mean(simpheno$mua_photo)
mean(bigfitsum[grep("a_photo", rownames(bigfitsum)),"mean"]) ### these look good!! 
mean(simpheno$mua_force)
mean(bigfitsum[grep("a_force", rownames(bigfitsum)),"mean"])
mean(simpheno$mua_chill)
mean(bigfitsum[grep("a_chill", rownames(bigfitsum)),"mean"])

mean(simlat$minlat) ## 0.025
mean(bigfitsum[grep("a_mins_sp", rownames(bigfitsum)),"mean"]) # 0.015
mean(simlat$maxlat) ## -0.14
mean(bigfitsum[grep("a_maxs_sp", rownames(bigfitsum)),"mean"]) # -0.15


bigfitsum[grep("a_mins_sp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_mins_sp\\[", rownames(bigfitsum)),"mean"]~unique(ave(simlat$minlat, simlat$sp))) # great

bigfitsum[grep("a_maxs_sp\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_maxs_sp\\[", rownames(bigfitsum)),"mean"]~unique(ave(simlat$maxlat, simlat$sp))) # great

# Trait model
sigma_y  ## 2
mean(bigfitpost[["sigma_y"]]) ## 2.1
a_min ## 0
mean(bigfitpost[["a_mins_sp"]]) ## -0.02
a_max ## 0
mean(bigfitpost[["a_maxs_sp"]]) ## -0.4

# Hyperparameters
mua_photo
bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]~mua_photo) ### this okay...

mua_force
bigfitsum[grep("a_force\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_force\\[", rownames(bigfitsum)),"mean"]~mua_force) ### this is good

mua_chill
bigfitsum[grep("a_chill\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_chill\\[", rownames(bigfitsum)),"mean"]~mua_chill) ### this looks good!


yphotos <- simpheno$photo
yforces <- simpheno$force
ychills <- simpheno$chill

y_ppphoto <- bigfitpost$y_ppphoto[1:1500]
y_ppforce <- bigfitpost$y_ppforce[1:1500]
y_ppchill <- bigfitpost$y_ppchill[1:1500]

plot(yphotos~y_ppphoto)
plot(yforces~y_ppforce)
plot(ychills~y_ppchill)


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


