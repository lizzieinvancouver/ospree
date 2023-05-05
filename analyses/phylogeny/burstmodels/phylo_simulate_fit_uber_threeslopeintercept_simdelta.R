## Started 5 May 2023 ##
## By Lizzie ##
## Copied from PMM repo
## pmm/analyses/burstdeltamodels/simulate_fit_uber_threeslopeintercept_simdelta.R ##

## Generate test data for phylogeny Stan model ##

# Load packages
require(ape)
require(geiger)
require(phytools)
require(rstan)

setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny/burstmodels")
#setwd("~/GitHub/ospree/analyses/phylogeny/burstmodels/")

# Set number of cores available
## options(mc.cores = parallel::detectCores())
options(mc.cores = 4)

useOSPREEtree <- FALSE # can use our phylogeny if you prefer (instead of PB simulated tree)
usefastslowdelta <- TRUE # Currently ... usefastslowdelta=TRUE, sets forcing delta=2 and photo delta=0.5 and usefastslowdelta=TRUE, sets all to 1 (same as lambda=1)

# Set seed
set.seed(2022)

# Simulate species tree with a pure birth model
if(useOSPREEtree){
spetree <- read.tree("ospreephylo.tre")
dropspp <- sample(spetree$tip.label, 80)
spetree <- drop.tip(spetree, dropspp)
nspecies <- length(spetree$tip.label)
}
if(!useOSPREEtree){
nspecies = 200
spetree <- pbtree(n=nspecies, nsim=1, b=1, complete=FALSE,scale=1)
}

spetree$tip.label <- paste("s", 1:nspecies, sep="")
nind = 10

# Now set up the trait parameters
if(usefastslowdelta){ # Make photoperiod (delta=0.5) as early-burst and forcing as 'late-burst' (delta=2) ... and make the other lambda values 1
param <- list(a_z = 30, # root value intercept
              lam_interceptsa = 1, # lambda intercept
              sigma_interceptsa = 5, # rate of evolution intercept
              b_zf = 5, # root value trait1
              delta_interceptsbf = 2, # DELTA trait1
              sigma_interceptsbf = 5, # rate of evolution trait1
              b_zc = 5, # root value trait2
              lam_interceptsbc = 1, # lambda trait2
              sigma_interceptsbc = 5, # rate of evolution trait2
              b_zp = 5, # root value trait3
              delta_interceptsbp = 0.5, # DELTA trait3
              sigma_interceptsbp = 5, # rate of evolution trait3
              sigma_y = 0.5 # overall sigma
              )
}

if(!usefastslowdelta){ # Make everything lambda of 1 (which is also delta=1) 
param <- list(a_z = 30, # root value intercept
              lam_interceptsa = 1, # lambda intercept
              sigma_interceptsa = 5, # rate of evolution intercept
              b_zf = 5, # root value trait1
              delta_interceptsbf = 1, # DELTA trait1
              sigma_interceptsbf = 5, # rate of evolution trait1
              b_zc = 5, # root value trait2
              lam_interceptsbc = 1, # lambda trait2
              sigma_interceptsbc = 5, # rate of evolution trait2
              b_zp = 5, # root value trait3
              delta_interceptsbp = 1, # DELTA trait3
              sigma_interceptsbp = 5, # rate of evolution trait3
              sigma_y = 0.5 # overall sigma
              )
    }

# Set priors
phypriors <- list(
    a_z_prior_mu = 30, 
    a_z_prior_sigma = 5,
    lam_interceptsa_prior_alpha = 6, 
    lam_interceptsa_prior_beta = 6,  
    sigma_interceptsa_prior_mu = 10, 
    sigma_interceptsa_prior_sigma = 10,
    b_zf_prior_mu = 0, 
    b_zf_prior_sigma = 20, 
    lam_interceptsbf_prior_alpha = 6, 
    lam_interceptsbf_prior_beta = 6,  
    sigma_interceptsbf_prior_mu = 10, 
    sigma_interceptsbf_prior_sigma = 10,
    b_zc_prior_mu = 0, 
    b_zc_prior_sigma = 20,
    lam_interceptsbc_prior_alpha = 6, 
    lam_interceptsbc_prior_beta = 6,  
    sigma_interceptsbc_prior_mu = 10,
    sigma_interceptsbc_prior_sigma = 10,
    b_zp_prior_mu = 0,
    b_zp_prior_sigma = 20,
    lam_interceptsbp_prior_alpha = 6, 
    lam_interceptsbp_prior_beta = 6,  
    sigma_interceptsbp_prior_mu = 10, 
    sigma_interceptsbp_prior_sigma = 10,
    sigma_y_mu_prior = 5, 
    sigma_y_mu_sigma = 10
)


# Generate intercept
scaledtree_intercept <- rescale(spetree, model = "lambda", param[["lam_interceptsa"]])         
intercepts <- fastBM(scaledtree_intercept, a = param[["a_z"]], mu = 0, sig2 = param[["sigma_interceptsa"]] ^ 2)
# Generate bf slope
scaledtree_bf <- rescale(spetree, model = "delta", param[["delta_interceptsbf"]])         
slopes_bf <- fastBM(scaledtree_bf, a = param[["b_zf"]], mu = 0, sig2 = param[["sigma_interceptsbf"]] ^ 2)
# Generate bc slope
scaledtree_bc <- rescale(spetree, model = "lambda", param[["lam_interceptsbc"]])         
slopes_bc <- fastBM(scaledtree_bc, a = param[["b_zc"]], mu = 0, sig2 = param[["sigma_interceptsbc"]] ^ 2)
# Generate bp slope
scaledtree_bp <- rescale(spetree, model = "delta", param[["delta_interceptsbp"]])         
slopes_bp <- fastBM(scaledtree_bp, a = param[["b_zp"]], mu = 0, sig2 = param[["sigma_interceptsbp"]] ^ 2)

dfhere <- data.frame(sp = c(), intercept = c(), x1=numeric(),
                     trait1=numeric(), x2 = numeric(), trait2 = numeric())
for (i in 1:nspecies){
    temp <- data.frame(sp = rep(i, nind),
                       intercept = rep(intercepts[i], nind),
                       x1 = rnorm(n = nind, mean = 0, sd = 5),
                       trait1 = rep(slopes_bf[i], nind),
                       x2 = rnorm(n = nind, mean = 0, sd = 5),
                       trait2 = rep(slopes_bc[i], nind),
                       x3 = rnorm(n = nind, mean = 0, sd = 5),
                       trait3 = rep(slopes_bp[i], nind))
    dfhere <- rbind(dfhere, temp)
}
dfhere$mu <- dfhere$intercept + dfhere$x1 * dfhere$trait1 + dfhere$x2 * dfhere$trait2 + dfhere$x3 * dfhere$trait3
dfhere$y <- rnorm(n = nrow(dfhere), mean = dfhere$mu, sd = param[["sigma_y"]])

# Deleting the simulated units; needed to do this to get it to run...

# Fit model
testme <- stan("uber_threeslopeintercept.stan",
               data = append(list(N=nrow(dfhere),
                                  n_sp=nspecies,
                                  sp=dfhere$sp,
                                  x1=dfhere$x1,
                                  x2=dfhere$x2,
                                  x3=dfhere$x3,
                                  y=dfhere$y,
                                  Vphy=vcv(spetree, corr = TRUE)),
                             phypriors),
               iter = 2000,
               warmup = 1000,
               chains = 4,
               seed = 62921,
               refresh = 20
               )

# Summarize fit
# list of actual parameters in Stan code
stan.param <- c("a_z", "lam_interceptsa", "sigma_interceptsa",
                "b_zf", "lam_interceptsbf" , "sigma_interceptsbf",
                "b_zc", "lam_interceptsbc", "sigma_interceptsbc",
                "b_zp", "lam_interceptsbp", "sigma_interceptsbp",
                "sigma_y")

# Compare to true values
summary(testme)$summary[stan.param, c("mean", "25%", "50%", "75%")]
t(param)


# Compare to GEIGER-fitted model
fitContinuous(spetree, intercepts, model="lambda")
fitContinuous(spetree, slopes_bf, model="lambda")
fitContinuous(spetree, slopes_bc, model="lambda")
fitContinuous(spetree, slopes_bp, model="lambda")

if(useOSPREEtree){
fitContinuous(multi2di(spetree), intercepts, model="lambda")
fitContinuous(multi2di(spetree), slopes_bf, model="lambda")
fitContinuous(multi2di(spetree), slopes_bc, model="lambda")
fitContinuous(multi2di(spetree), slopes_bp, model="lambda")
}

# save output ...
if(usefastslowdelta){
    saveRDS(testme, "output/testsigmavarydelta.RDS")
}

if(!usefastslowdelta){
    saveRDS(testme, "output/testsigmacontsantdelta.RDS")
}

# Plot posteriors of sigmas ...
# saved as sigmadeltasim_constantdelta and sigmadeltasim_varydelta
pdf("..//figures/burstmodelfigquick.pdf", width=6, height=4)
testmepost <- extract(testme)
xlim <- c(2, 8)
ylim <- c(0, 2.1)
plot(density(testmepost$sigma_interceptsbf), col="red", xlim=xlim, ylim=ylim,
   main="", xlab="Sigmas") 
lines(density(testmepost$sigma_interceptsbc), col="blue", xlim=xlim, ylim=ylim) 
lines(density(testmepost$sigma_interceptsbp), col="orange", xlim=xlim, ylim=ylim) 
dev.off()
