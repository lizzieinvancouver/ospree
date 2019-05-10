## StartedMay 2019 
## By Ailene, modified from models_stan_ploting to match names in models_stan
## Plotting results from Stan models ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd()))>0) { 
  setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

figpath <- "figures"

## set up the flags
use.chillports = FALSE
use.zscore = FALSE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

source("source/bbstanleadin.R")

## name your figures paths (based on flags above) ... this needs work
##Ailene updated to match models_stan file
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==TRUE){
  figpathmore <- "spcompexprampfpcp_nonz"
  load("stan/output/m2lni_spcompexprampfpcp_nonz.Rda")
}


# Code if you want to save your models (do NOT push output to git)
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
  figpathmore <- "spcompalltypescp_z"
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
  figpathmore <- "spcompexprampfpcp_z"
  load("stan/output/m2lni_spcompexprampfpcp_z.Rda")
  
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==TRUE){
  figpathmore <- "m2lni_spcompexprampfpcp_nonz"
  load("stan/output/m2lni_spcompexprampfpcp_nonz.Rda")
  
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
  figpathmore <- "spcompwcropsexprampfpcp_z"
  load("stan/stan/output/m2lni_spcompwcropsexprampfpcp_z.Rda")
}

# utah ...
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE){
  figpathmore <- "spcompexprampfputah_z"
  load("stan/output/m2lni_spcompexprampfputah_z.Rda")
  
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==FALSE &
    use.chillports==FALSE){
  figpathmore <- "spcompalltypesutah_nonz"
  load("stan/output/m2lni_spcompalltypesutah_nonz.Rda")
  
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE &
    use.chillports==FALSE){
  figpathmore <- "spcompalltypesutah_z"
  load("stan/output/m2lni_spcompalltypesutah_z.Rda")
  
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  figpathmore <- "spcompexprampfputah_nonz"
  load("stan/output/m2lni_spcompexprampfputah_nonz.Rda")
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE){
  figpathmore <- "allsppexprampfputah_z"
  load("stan/output/m2lni_allsppexprampfputah_z.Rda")
  
}

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  figpathmore <- "allsppexprampfputah_nonz"
  load("stan/output/m2lni_allsppexprampfputah_nonz.Rda")
  
}

##
source("source/bb_muplot.R")
source("source/plotletfx.R")

sumer.ni <- summary(m2l.ni)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]
modelhere <- m2l.ni

# Set up colors
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

muplotfx(modelhere, "model", 7, 8, c(0,3), c(-3, 1) , 12, 3)

