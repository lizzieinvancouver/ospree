
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Master flags! Here you pick if you want the flags for the main model (figure 2 in main text) versus other versions (all spp model, chill portions, uncentered predictors, as in supp table and figures 3-4)
use.flags.for.mainmodel <- TRUE
use.flags.for.spcomp.cp <- FALSE
use.flags.for.allspp.utah <- FALSE
use.flags.for.spcomp.utah.nonz <- FALSE
use.flags.for.spcomp.cp.nonz <- FALSE # predictors on natural scale, spcomplex with utah units. Fig 3-4 in main text of budburst ms
use.flags.for.allspp.utah.nonz <- FALSE
use.yourown.flagdesign <- FALSE

# Source main model files
setwd("../../bb_analysis")
source("source/flags.for.models.in.bbms.R")
source("source/bbstanleadin.R")

######################################
## Overview of the model run below ##
######################################
# Main model:
# m2l.ni: a(sp) + f(sp) + p(sp) + c(sp)

########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni = stan("stan/nointer_2level.stan", data = datalist.bb,
               iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))
# Return to local working directory
setwd("../traits/Rfiles")

# Diagnostics
## check_all_diagnostics(m2l.ni)
## launch_shinystan(m2l.ni)

# Extract random slopes and mu's / sigmas
## m2lni.all <- summary(m2l.ni, pars = list("mu_b_force_sp", "sigma_b_force_sp", "b_force",
##                                          "mu_b_photo_sp", "sigma_b_photo_sp", "b_photo",
##                                          "mu_b_chill_sp", "sigma_b_chill_sp", "b_chill"))$summary

# Extract random slopes and save
m2lni.slopes <- summary(m2l.ni, pars = list("b_force", "b_photo", "b_chill"))$summary

# Make table containing species names and numbers
## USING COMPLEXES 
sp.names.num <- data.frame(Species = bb.stan$complex.wname,
                           Number = bb.stan$complex)
## Remove duplicates
sp.names.num <- sp.names.num[!duplicated(sp.names.num$Number), ]
## Order
sp.names.num <- sp.names.num[order(sp.names.num$Number), ]
## Merge summary table and species names
m2lni.slopes <- cbind(cbind(data.frame(Coefficient = c(rep("b_force", nrow(sp.names.num)),
                                                       rep("b_photo", nrow(sp.names.num)),
                                                       rep("b_chill", nrow(sp.names.num)))),
                            sp.names.num),
                      m2lni.slopes)
## NOT USING COMPLEXES 
## sp.names.num <- data.frame(Species = c(paste(bb.stan$genus, bb.stan$species, sep = "_")),
##                            Number = bb.stan$complex)
## ## Remove duplicates
## sp.names.num <- sp.names.num[!duplicated(sp.names.num$Number), ]
## ## Order
## sp.names.num <- sp.names.num[order(sp.names.num$Number), ]
## ## Merge summary table and species names
## m2lni.slopes <- cbind(cbind(data.frame(Coefficient = c(rep("b_force", nrow(sp.names.num)),
##                                                        rep("b_photo", nrow(sp.names.num)),
##                                                        rep("b_chill", nrow(sp.names.num)))),
##                             sp.names.num),
##                       m2lni.slopes)

# Save estimates (COMPLEXES)
write.table(x = m2lni.slopes, file = "traitors_bb_results.csv", row.names = FALSE, sep = ",")

# Save estimates (NO COMPLEXES)
## write.table(x = m2lni.slopes, file = "traitors_bb_results_nocomplex.csv", row.names = FALSE, sep = ",")

