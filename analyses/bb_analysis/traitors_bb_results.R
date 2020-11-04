
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
use.rangespp  <- FALSE # line added by Geoff to prevent error message from bbstanleadin "use.rangespp not found"

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
m2l.ni = stan('stan/nointer_2level.stan', data = datalist.bb,
               iter = 2500, warmup=1500,control = list(adapt_delta = 0.99))

check_all_diagnostics(m2l.ni)
# launch_shinystan(m2l.ni)

# Extract random slopes and save
m2lni.slopes <- summary(m2l.ni, pars = list("mu_b_force_sp", "sigma_b_force_sp", "b_force",
                                            "mu_b_photo_sp", "sigma_b_photo_sp", "b_photo",
                                            "mu_b_chill_sp", "sigma_b_chill_sp", "b_chill"))$summary

write.table(x = m2lni.slopes, file = "bb_results_geoff.csv", row.names = TRUE, sep = ",")
