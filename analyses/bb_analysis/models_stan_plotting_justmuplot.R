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
use.chillports =FALSE
use.zscore = TRUE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE


## name your figures paths (based on flags above) ... this needs work
##Ailene updated to match models_stan file
# chill ports centered, with only expramptypes, with crops, not all sp
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
  figpathmore <- "spcompwcropsexprampfpcp_z"
  load("stan/stan/output/m2lni_spcompwcropsexprampfpcp_z.Rda")
  xlim=c(-32,10)
}
# chill ports centered, with all chill/force types, with crops, not all sp
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
  figpathmore <- "spcompalltypescp_z"
  xlim=c(-32,10)
  
}
# chill ports centered, with only expramptypes, with crops, not all sp
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==TRUE){
  figpathmore <- "spcompexprampfpcp_z"
  load("stan/output/m2lni_spcompexprampfpcp_z.Rda")
  xlim=c(-32,10)
}

#chill ports, not centered, with only expramptypes, not all species not crops
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==TRUE){
  figpathmore <- "spcompexprampfpcp_nonz"
  load("stan/output/m2lni_spcompexprampfpcp_nonz.Rda")
  xlim=c(-6,2)
}


#chill ports, not centered, with all chill/force types, not all species not crops
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==TRUE){
  figpathmore <- "m2lni_spcompexprampfpcp_nonz"
  load("stan/output/m2lni_spcompexprampfpcp_nonz.Rda")
  xlim=c(-6,2)
}

# utah centered, with only expramptypes, no crops, not all sp
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE){
  figpathmore <- "spcompexprampfputah_z"
  load("stan/output/m2lni_spcompexprampfputah_z.Rda")
  xlim=c(-32,10)
}

# utah centered, with all typesof forcing and chilling, no crops, not all sp
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==TRUE &
    use.chillports==FALSE){
  figpathmore <- "spcompalltypesutah_z"
  load("stan/output/m2lni_spcompalltypesutah_z.Rda")
  xlim=c(-32,10)
}

# utah centered, with only expramptypes and all species (not crops)
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==TRUE & 
    use.chillports==FALSE){
  figpathmore <- "allsppexprampfputah_z"
  load("stan/output/m2lni_allsppexprampfputah_z.Rda")
  xlim=c(-32,10)
}
#utah, not centered, only expramptypes
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  figpathmore <- "spcompexprampfputah_nonz"
  load("stan/output/m2lni_spcompexprampfputah_nonz.Rda")
  xlim=c(-6,2)
}
#utah, not centered, all types of forcing and chilling
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.zscore==FALSE &
    use.chillports==FALSE){
  figpathmore <- "spcompalltypesutah_nonz"
  load("stan/output/m2lni_spcompalltypesutah_nonz.Rda")
  xlim=c(-6,2)
}

#utah, not centered, only expramptypes and all species
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.zscore==FALSE & 
    use.chillports==FALSE){
  figpathmore <- "allsppexprampfputah_nonz"
  load("stan/output/m2lni_allsppexprampfputah_nonz.Rda")
  xlim=c(-6,2)
}

## functions for plotting 
source("source/bbstanleadin.R")
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

muplotfx(modelhere, "", 7, 8, c(0,3), xlim , 12, 3)

