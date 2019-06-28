## Started 16 December 2018 ##
## By Lizzie ##

## Built off models_stan.R but for models less commonly used ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- TRUE
use.flags.for.allsppmodel <- FALSE
use.yourown.flagdesign <- FALSE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
   use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
   & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp =FALSE # for the main model this is false
  use.multcuespp = FALSE
  use.cropspp = FALSE
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  
  # Default is species complex and no crops
  use.allspp = FALSE
  use.multcuespp = FALSE
  use.cropspp = FALSE
  
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  
  #Default is all chilling data
  use.expchillonly = FALSE # change to true for only experimental chilling 
  #note: with only exp chilling, there is only exp photo and force too.
  #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}


source("source/bbstanleadin.R")

######################################
## Overview of the models run below ##
######################################
# m2l.nib (b for basic): a(sp) + f + p + c
# m2l.winsp: a(sp) + f(sp) + p(sp) + c(sp) + cf + cp + fp
# m2l.nistudy: a(sp) + a(datasetID) + f(sp) + p(sp) + c(sp)



########################################################
# real data on 2 level model (sp on intercept only) with no interactions 
########################################################
m2l.nib = stan('stan/archive/nointer_2level_interceptonly.stan', data = datalist.bb,
               iter = 2500, warmup=1500) 
  
m2l.nibsum <- summary(m2l.nib)$summary
m2l.nibsum[grep("mu_", rownames(m2l.nibsum)),] 
m2l.nibsum[grep("b_", rownames(m2l.nibsum)),]

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE){
    save(m2l.nib, file="stan/output/m2lnib_spcompexprampfp_z.Rda")
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE){
    save(m2l.nib, file="stan/output/m2lnib_spcompexprampfp_nonz.Rda")
}



########################################################
# real data on 2 level model (sp) with 2 two-way interactions but no partial pooling on interactions
########################################################
m2l.winsp = stan('stan/archive/winternosp_2level.stan', data = datalist.bb,
               iter = 4000, warmup=2500) 
 

m2l.winsp.sum <- summary(m2l.winsp)$summary 
m2l.winsp.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
    "b_cf","b_cp","b_fp"),]


if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE){
    save(m2l.winsp, file="stan/output/m2l.winsp_spcompexprampfp_z.Rda")
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE){
    save(m2l.winsp, file="stan/output/m2l.winsp_spcompexprampfp.Rda")
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE){
    save(m2l.winsp, file="stan/output/m2l.winsp_allsppexprampfp_z.Rda")
}



#################################################################################
# real data on 2 level model (sp) with no interactions and study ID on intercept
#################################################################################
datalist.bb.study <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         study = as.numeric(as.factor(bb.stan$datasetID)),
                         n_study = length(unique(bb.stan$datasetID)), 
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

datalist.bb.study.z <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         study = as.numeric(as.factor(bb.stan$datasetID)),
                         n_study = length(unique(bb.stan$datasetID)), 
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

# m2l.nistudy = stan('stan/nointer_2level_studyint.stan', data = datalist.bb.study,
  #             iter = 10000, warmup=6000)

m2l.nistudy = stan('stan/nointer_2level_studyint.stan', data = datalist.bb.study.z,
               iter = 5000, warmup=3000)


if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE){
    save(m2l.nistudy, file="stan/output/m2l.nistudy_spcompexprampfp_z.Rda")
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE){
    save(m2l.nistudy, file="stan/output/m2l.nistudy_allsppexprampfp_z.Rda")
}


betas.m2l.nistudy <- as.matrix(m2l.nistudy, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","b_force",
    "b_photo", "b_chill", "alpha"))
m2lnistudy.sum <- summary(m2l.nistudy)$summary
m2lnistudy.sum[grep("mu_", rownames(m2lnistudy.sum)),]
head(m2lnistudy.sum)
m2lnistudy.sum[grep("alpha", rownames(m2lnistudy.sum)),]


########################################################
# real data on 2 level model (sp) with no interactions, with sigmoid
########################################################
# Note: We are in progress on this model ... 
# So for now, it is block-commented out
if(FALSE){
m2l.lincomp= stan('stan/nointer_2level_interceptonly.stan', data = datalist.bb,
                 iter = 4000, warmup=3500, control=list(adapt_delta=0.95)) 
  
m2l.nisig = stan('stan/nointer_2level_interceptonly_sigmoid.stan', data = datalist.bb,
               iter = 4000, warmup=3500, control=list(adapt_delta=0.95)) 

summary(m2l.nisig)$summary[c("b_force", "b_photo","a_chill", "b_chill"),]
betas.m2l.nisig  <- as.matrix(m2l.nisig, pars = c("b_force", "b_photo","a_chill", "b_chill"))

summary(m2l.lincomp)$summary[c("b_force", "b_photo", "b_chill"),]
  
#mcmc_intervals(betas.m2l.nisig[,1:5])

###rsq
observed.here <- bb.stan$resp
nonlin.sum<-summary(m2l.nisig)$summary 

preds.nonlin.sum <- nonlin.sum[grep("yhat", rownames(nonlin.sum)),]
nonlin.sum.R2 <- 1- sum((observed.here-preds.nonlin.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
nonlin.mod.R2 <- 1- sum((observed.here-preds.nonlin.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
summary(lm(preds.nonlin.sum[,1]~observed.here))  #0.33

lin.sum<-summary(m2l.lincomp)$summary 
preds.lin.sum <- lin.sum[grep("yhat", rownames(lin.sum)),]
lin.sum.R2 <- 1- sum((observed.here-preds.lin.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
lin.mod.R2 <- 1- sum((observed.here-preds.lin.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
summary(lm(preds.lin.sum[,1]~observed.here))


##check chilling but currently broke (DB June)
#fakephoto<-12
#fakeforce<-20
#b_photo<-summary(m2l.nisig)$summary[c("b_photo"),1]
#b_force<-summary(m2l.nisig)$summary[c("b_force"),1]

fakechill <- seq(from=0, to=1000, by=0.1)

a_chill<-summary(m2l.nisig)$summary[c("a_chill"),1]
b_chill<-summary(m2l.nisig)$summary[c("b_chill"),1]



dfnonlin<-data.frame(ychill=numeric(),fakechill=numeric())  ##generate fake data
for(i in c(1:length(fakechill))){
ychill<-(1 /( 1 + exp(a_chill*(fakechill[i]-b_chill)) ) )
dfhere <- data.frame(ychill=ychill,fakechill=fakechill[i])

dfnonlin <- rbind(dfnonlin, dfhere) 

}



jpeg("figures/nonlinearplotbad")
plot(dfnonlin$fakechill,dfnonlin$ychill)
dev.off()

