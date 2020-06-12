## Started 23 March 2020 ##
## By Lizzie, pulling some from JointModelSim_fj.R ##

#Ypheno,i=alphapheno,sp[i]+betaphoto,sp[i]*P+sigmapheno,I 

#Betaphotosp=alpha_photo_sp+beta_LIXphoto*alphaLI,sp 

library(rstan)

options(stringsAsFactors = FALSE)

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
sigma_asp <- 0.5
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

mua_photo <- rnorm(nsp, -2, sigma_aphoto)
mua_force <- rnorm(nsp, -4, sigma_aforce)
mua_chill <- rnorm(nsp, -7, sigma_achill)

bphoto_min <- mua_photo + beta_photomin*mua_sp
bphoto_max <- mua_photo + beta_photomax*mua_sp

bforce_min <- mua_force + beta_forcemin*mua_sp
bforce_max <- mua_force + beta_forcemax*mua_sp

bchill_min <- mua_chill + beta_chillmin*mua_sp
bchill_max <- mua_chill + beta_chillmax*mua_sp

nph <- 50 # number of observations per species/phenological combination 
Nph <- nsp * nph # obervations per species for phenological event and photoperiod


simpheno <- data.frame(sp=numeric(), mua_photo=numeric(), mua_force=numeric(), mua_chill=numeric())

for (sp in 1:nsp){
  simphenoadd <- data.frame(sp=rep(sp, nph), mua_photo=rep(mua_photo[sp], nph),
                            mua_force=rep(mua_force[sp], nph),
                            mua_chill=rep(mua_chill[sp], nph))
  simpheno <- rbind(simpheno, simphenoadd)
}

simpheno$photo <- simpheno$mua_photo + simlat$minlat*bphoto_min + simlat$maxlat*bphoto_max + rnorm(nrow(simpheno), 0, sigma_yphoto)
simpheno$force <- simpheno$mua_force + simlat$minlat*bforce_min + simlat$maxlat*bforce_max + rnorm(nrow(simpheno), 0, sigma_yforce)
simpheno$chill <- simpheno$mua_chill + simlat$minlat*bchill_min + simlat$maxlat*bchill_max + rnorm(nrow(simpheno), 0, sigma_ychill)

#agrand <- 40

#simpheno$bbresp <- agrand + simpheno$photo + simpheno$force + simpheno$chill + rnorm(nrow(simpheno), 0, sigma_y) 

table(simpheno$sp)


# Nothing left to do but to try Stan, I think
simlat <- data.frame(sp=rep(1:nsp, each=10), mua_sp=rep(mua_sp, each=10))

simlat$minlat <- a_min + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)
simlat$maxlat <- a_max + simlat$mua_sp + rnorm(nrow(simlat), 0, sigma_y)

N <- length(simlat$minlat)

Npheno <- length(simpheno$photo)
latstanpheno <- list(mindat = simlat$minlat, maxdat = simlat$maxlat,
                     photodat = simpheno$photo, forcedat = simpheno$force,
                     chilldat = simpheno$chill,
                     N = N, nsp = nsp, species = simlat$sp, 
                     Npheno = Npheno, nsppheno = nsp,
                     speciespheno = simpheno$sp)


# Try to run the Stan model 
jointfit <- stan(file = "/n/wolkovich_lab/Lab/Cat/joint_latextent_cuesresp.stan", data = latstanpheno, warmup = 500, iter = 1000,
    chains = 1, cores = 1,  control=list(max_treedepth = 12)) 

save(jointfit, file="/n/wolkovich_lab/Lab/Cat/ranges_jointmod.Rda")


if(FALSE){
if(!runfullmodel){
load("output/stan/jointlatphoto.Rda")
}

# Checking against sim data
bigfitpost <- extract(jointfit)
bigfitsum <- summary(jointfit)$summary

sd(simpheno$photo) ## 7.5
mean(bigfitsum[grep("sigma_yphoto", rownames(bigfitsum)),"mean"]) ### 7.2
sd(simpheno$force) ## 5.4
mean(bigfitsum[grep("sigma_yforce", rownames(bigfitsum)),"mean"]) ### 5.1
sd(simpheno$chill) ## 7.2
mean(bigfitsum[grep("sigma_ychill", rownames(bigfitsum)),"mean"]) ### 6.9


mean(simpheno$mua_photo) ## -1.9
mean(bigfitsum[grep("a_photo", rownames(bigfitsum)),"mean"]) ## -1.7
mean(simpheno$mua_force) ## -4.1
mean(bigfitsum[grep("a_force", rownames(bigfitsum)),"mean"]) ## -4.3
mean(simpheno$mua_chill) ## -6.4
mean(bigfitsum[grep("a_chill", rownames(bigfitsum)),"mean"]) ## -5.5

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
mua_photo
bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_photo\\[", rownames(bigfitsum)),"mean"]~mua_photo) ### this okay...

bphoto_min
bigfitsum[grep("b_photomin\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("b_photomin\\[", rownames(bigfitsum)),"mean"]~bphoto_min) ### bad!!!!
bphoto_max
bigfitsum[grep("b_photomax\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("b_photomax\\[", rownames(bigfitsum)),"mean"]~bphoto_max) ### bad

#######
mua_force
bigfitsum[grep("a_force\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_force\\[", rownames(bigfitsum)),"mean"]~mua_force) ### this is good

bforce_min
bigfitsum[grep("b_forcemin\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("b_forcemin\\[", rownames(bigfitsum)),"mean"]~bforce_min) ### this okay...
bforce_max
bigfitsum[grep("b_forcemax\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("b_forcemax\\[", rownames(bigfitsum)),"mean"]~bforce_max) ### this okay...

#######
mua_chill
bigfitsum[grep("a_chill\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("a_chill\\[", rownames(bigfitsum)),"mean"]~mua_chill) ### this looks good!

bchill_min
bigfitsum[grep("b_chillmin\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("b_chillmin\\[", rownames(bigfitsum)),"mean"]~bchill_min) ### this okay...
bchill_max
bigfitsum[grep("b_chillmax\\[", rownames(bigfitsum)),"mean"]
plot(bigfitsum[grep("b_chillmax\\[", rownames(bigfitsum)),"mean"]~bchill_max) ### this okay...


#######
yphotos <- simpheno$photo
yforces <- simpheno$force
ychills <- simpheno$chill

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

}
