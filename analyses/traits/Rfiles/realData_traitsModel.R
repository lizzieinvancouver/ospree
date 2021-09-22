#Running our real traits data though our Stan model to see if it fits ok
#Started September 9th 2021 by Faith during an Ospree retereit, and designed to run on Midge 


library(rstan)
#require(rstanarm)
require(shinystan)
require(bayesplot)
require(truncnorm)
library(ggplot2)

rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

#specify if this code should be run on Midge or on your own computer.

MidgeFlag <- FALSE

if (MidgeFlag == TRUE){
	traitsData1 <- read.csv("../../data/Ospree_traits/try_bien_nodups_1.csv")
	traitsData2 <- read.csv("../../data/Ospree_traits/try_bien_nodups_2.csv")
} else if(MidgeFlag == FALSE) {

    if(length(grep("Lizzie", getwd())>0)) { 
        setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits/")
        traitsData1 <- read.csv("input/try_bien_nodups_1.csv")
	traitsData2 <- read.csv("input/try_bien_nodups_2.csv")}
else
        setwd("/home/faith/Documents/github/ospree/analyses/traits/")
	traitsData1 <- read.csv("input/try_bien_nodups_1.csv")
	traitsData2 <- read.csv("input/try_bien_nodups_2.csv")
}

#The code to test on Faith's computer

traitsData <- rbind(traitsData1,traitsData2)

#Select a trait to test data with
#First try SLA
unique(traitsData$traitname)
SLAData <- traitsData[traitsData$traitname == "Specific_leaf_area",]

#Select some priors based on tha data for now - we can chose better priors later
meanSLA <- mean(SLAData$traitvalue)
sdSLA <- sd(SLAData$traitvalue)
hist(SLAData$traitvalue)

str(SLAData)

Ntrt <- nrow(SLAData)#overall number of trait data rows 
Nspp <- length(unique(SLAData$speciesname)) # count species names
Nstudy <- length(unique(SLAData$datasetid)) # count study names

SLAData$species <- as.integer(as.factor(SLAData$speciesname))
SLAData$study <- as.integer(as.factor(SLAData$datasetid))


## Trait only stan model ###########################################################
trait_data <- list(yTraiti = SLAData$traitvalue, 
                   N = Ntrt, 
                   n_spec = Nspp, 
                   species = SLAData$species, 
                   study = SLAData$study, 
                   n_study = Nstudy,
                   prior_mu_grand = 15,
                   prior_sigma_grand = 5,
                   prior_mu_sp = 0,
                   prior_sigma_sp_mu = 10,
                   prior_sigma_sp_sigma = 10,
                   prior_mu_study = 0,
                   prior_sigma_study_mu = 10,
                   prior_sigma_study_sigma = 10,
                   prior_sigma_traity_mu = 5,
                   prior_sigma_traity_sigma = 1) 

mdl.traitonly <- stan('stan/joint_traitonly.stan',
                      data = trait_data, iter = 3000, warmup = 1000,
                      include = FALSE, pars = c("ymu"))

postSLA <- extract(mdl.traitonly)
launch_shinystan(mdl.traitonly)
