## Comparing Lizzie's ubermini_2 test data (in pmm repo) to OSPREE data
## Trouble shooting gaussian process model,
## which works on test data but not ospree data ##


#### remove objects  ####
rm(list=ls())
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")


#We want to Compare test data to ospree data

#to do this, first get the ospree data in (code taken from Phylo_ospree_reanalyses_inprogress.R)

# Loading packages
library(shinystan)
library(caper)
library(pez)
library(rstan)
library(phytools)
library(plyr)
library(dplyr)
require(ape)
require(geiger)
require(phytools)
require(rstan)

options(mc.cores = parallel::detectCores())


#'######################################
#### get osdata through bbstanleadin ####
#'######################################

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- FALSE
use.flags.for.allsppmodel <- TRUE
use.yourown.flagdesign <- FALSE
nocrops <- TRUE
agiosponly <- TRUE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
   use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
   & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = TRUE
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
  use.zscore = TRUE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = F # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  
  # Default is species complex and no crops
  use.allspp = F
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

source("../bb_analysis/source/bbstanleadin.R")
#note: i get an error "use.rangespp" not found- has this been added to bbstanleadin.R>

namesdat <- unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
bb.stan$spps <- paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$phylo <- paste(bb.stan$genus,bb.stan$species,sep="_")


##Now, Lizzie's code to prepare the test data

## Uber simple to start ...
## skip an intercept and just estimate
# y ~ b_force[phylo]*x1 + error


nspecies = 90
nind = 10

# Simulate species tree with a pure birth model
spetree <- pbtree(n=nspecies, nsim=1, b=1, complete=FALSE,scale=1)
spetree$tip.label <- paste("s", 1:nspecies, sep="")

# now set up the trait
m <- 0.6
lam <- 0.7
sigy <- 0.01
sig2 <- 0.1

scaledtree <- rescale(spetree, model="lambda", lam)
slopez <- fastBM(scaledtree, a=m, mu=0, sig2=sig2)
phylosig(x=slopez, tree=spetree, method="lambda")

# for testing ...
nulltree <- rescale(spetree, model="lambda", 0)

dfhere <- data.frame(x=numeric(), y=numeric())

for (i in 1:length(slopez)){
    slopehere <- slopez[i]
    xhere <- rnorm(nind, 10, 3) # these are experiments, no phylo structure in x 
    yhere <- xhere*slopehere
    dfadd <- data.frame(x=xhere, y=yhere)
    dfhere <- rbind(dfhere, dfadd)
}

dfhere$sp <- rep(spetree$tip.label, each=nind)
dfhere$spnum <- as.numeric(gsub('s', '', dfhere$sp))
    
dfhere$yerr <- dfhere$y + rnorm(nrow(dfhere), 0, sigy)

testme <- stan("phylogeny/stan/ubermini_2_biggerpriors.stan", # Note: changed to a new model
                data=list(N=nrow(dfhere), n_sp=nspecies, sp=dfhere$spnum,
                x=dfhere$x, y=dfhere$yerr,
                Vphy=vcv(spetree)), # Note: dropped the corr=TRUE
                iter=4000, chains=4)


#Compare test data to ospree data
#test data are in red, ospree in blue- not meant to be political! just whats on my brain today...
pdf("../phylogeny/figures/phylotestospcomp.pdf", height = 6, width = 10)

par(mfrow=c(2,2))
#first, compare sinmply the number of species
nspecies.osp = length(unique(bb.stan$spps))
hist(nspecies.osp,main = "Num sp", col = "light blue", xlim = c(0,nspecies.osp))
hist(nspecies,col = "salmon", add = TRUE)
nind.osp = unique(table(bb.stan$spps))

#Now look at evenness- number of rows or individuals per species
hist(table(bb.stan$spps), main = "Num rows/ind per sp", col = "light blue")
abline(v = nind, col = "salmon", lwd = 3)

#Now look at resp
hist(bb.stan$resp, col = "light blue", xlim = c(0,max(bb.stan$resp)), main = "Y (Resp)")
hist(dfhere$y, col = "salmon", add = TRUE)#compare to resp from ospree

#Now look at x (force.z)
hist(bb.stan$force.z, col = "light blue", xlim = range(c(bb.stan$force.z, dfhere$x)), main = "X (Force.z)")
hist(dfhere$x, col = "salmon", add = TRUE)
dev.off()

#Other things to look at
range(bb.stan$force.z)
range(bb.stan$force)
#Question: simulated slopes are almost all positive but ospree forcing slopes usally negative- does this matter?
hist(slopez)
