#Running teh phenology model with a potential effect of trait, but feeding in mean trait valus rather than using another model to estimate trait for each species
#We are doing this so we can have a backup if teh joint models dont work, and so we can sanity check the joint model results

#Started by Faith Jones Sep 23 2021

#Thsi code is designed to run either on midge or on a hoem comuter. If it runs on a hoem computer then it sources data from ..//bb_analysis/source/bbstanleadin.R
#If it runs on midge it sources ospree/traits data combined that has been saved from a previous running of this script. 

rm(list = ls()) 
options(stringsAsFactors = FALSE)

library(tidyr)
library(plyr)
library(dplyr)
library(reshape2)
library(rstan)
library(bayesplot)# nice posterior check plots 
library(shinystan)
library(viridis)

# Set Midge Flag
Midge <- FALSE

color_scheme_set("viridis")

#Source ospree trators data if thsi runs on Faith's section of Midge
if(Midge == TRUE){
  setwd("~/traits")
  load("traitOspreeData.Rdata")

} 



#Code taken from Phylo_ospree_reanalyses.R to feed in the Ospree data
#----------------------------------------------------------------------

if(Midge == FALSE){
    # Anyone else working with this code should add their info/path here
    if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
    } else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
    } else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
    } 

    dat1 <- read.csv("input/try_bien_nodups_1.csv") 
    dat2 <- read.csv("input/try_bien_nodups_2.csv") 
    dat <- rbind(dat1, dat2)
    names(dat)
    setwd("..//bb_analysis") 

    # Flags to choose for bbstanleadin.R #

    # Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
    use.flags.for.mainmodel <- F
    use.flags.for.allsppmodel <- T
    use.yourown.flagdesign <- F
    nocrops <- T
    agiosponly <- T

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
      
      #Default is all chilling dataN
      use.expchillonly = FALSE # change to true for only experimental chilling 
      #note: with only exp chilling, there is only exp photo and force too.
      #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
    }
    source("..//bb_analysis/source/bbstanleadin.R")
    namesdat<-unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
    bb.stan$spps<-paste(bb.stan$genus,bb.stan$species,sep="_")
    bb.stan$phylo<-paste(bb.stan$genus,bb.stan$species,sep="_")


    traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", 
    "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", 
    "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", 
    "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

    ospree_traitors <- bb.stan[bb.stan$spps %in% traitors.sp,]

    #Get Trait data ready for merging with ospree 
    #Rename some trait names to be more weildy 
    dat$traitname[which(dat$traitname == "seed mass")] <- "Seed_mass"
    dat$traitname[which(dat$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")] <- "LNC"
    dat$traitname[which(dat$traitname == "Specific_leaf_area")] <- "SLA"
    dat$traitname[which(dat$traitname == "Stem_specific_density")] <- "SSD"

    #Exploring the data 
    unique(dat$traitname)
    table(dat$speciesname, dat$traitname)

    densityData <- dat[dat$traitname == "SSD",]
    table(densityData$speciesname)
    unique(densityData$speciesname)
    unique(densityData$datasetid)

    #select traits we are interested in
    triatSelect <- c("Seed_mass", "SLA", "SSD", "LNC", "Plant_height_vegetative")
    selectData <- dat[dat$traitname %in% triatSelect,]

    #Calculate mean values for each species
    meanTrait <- aggregate(selectData$traitvalue, by = list(selectData$traitname, selectData$speciesname), FUN = mean)
    names(meanTrait) <- c("traitname", "speciesname", "traitvalue")

    meanSSD <- meanTrait[meanTrait$traitname == "SSD",]
    nrow(meanSSD)
    meanSLA <- meanTrait[meanTrait$traitname == "SLA",]
    nrow(meanSLA)
    meanLNC <- meanTrait[meanTrait$traitname == "LNC",]
    nrow(meanLNC)
    meanSeed <- meanTrait[meanTrait$traitname == "Seed_mass",]
    nrow(meanSeed)
    meanHeight <- meanTrait[meanTrait$traitname == "Plant_height_vegetative",]
    nrow(meanHeight)

    #Merge ospree and traits data 
    meanTraitWide <- dcast(meanTrait, speciesname ~ traitname)

    #select species
    traitSelect <- meanTraitWide[meanTraitWide$species %in% traitors.sp,]


    #combine data 
    traitOspree <- merge(ospree_traitors,traitSelect, by.y = "speciesname", by.x = "spps")



    setwd("..//traits") 
    save(traitOspree, file = "Rfiles/traitOspreeData.Rdata")
}



#Run stan model for SLA
#---------------------------------------------------------------------

#Remove rows without SLA data
SLAData <- traitOspree[!is.na(traitOspree$SLA),]

#Nnumber of species 
Nspp <- length(unique(SLAData$spps))

#Number of observations
N <- nrow(SLAData)

#species column as an integer 
species <- as.integer(as.factor(SLAData$spps))

#Cues
force.i <- SLAData$force.z
photo.i <- SLAData$photo.z 
chill.i <- SLAData$chill.z 

#budburst date
yPhenoi <- SLAData$resp

## pehnology and mean trait stan model ###########################################################
pheno_data <- list(alphaTraitSp = SLAData$SLA - mean( SLAData$SLA), #mean species trait value 
                   Nph = N, 
                   n_spec = Nspp, 
                   species = species, 
                   yPhenoi = yPhenoi, 
                   forcei = force.i,
                   photoi = photo.i, 
                   chilli = chill.i,
                #Priors
                   prior_sigmaphenoy_mu =20,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                   prior_sigmaphenoy_sigma = 5, # variance of the prior distribution of the general error sigma)y around the mean predicted value
                   
                   prior_muForceSp_mu = 0, # mean of the prior distribution of the mean effect of forcing 
                   prior_muForceSp_sigma = 5, # vareince of the prior distributionof the mean effect of forcing 
                   prior_sigmaForceSp_mu = 5, # mean of the prior distribution of the varience around the mean effect of forcing 
                   prior_sigmaForceSp_sigma = 5,# variance of the prior distribution of the varience around the mean effect of forcing ,

                   prior_muChillSp_mu = 0,# mean of the prior distribution of the mean effect of chilling 
                   prior_muChillSp_sigma = 5,# varience of the prior distribution of the mean effect of chilling 
                   prior_sigmaChillSp_mu = 5,# mean of the prior distribution of the varience around the mean effect of chilling 
                   prior_sigmaChillSp_sigma= 5, #variance of the prior distribution of the varience around the mean effect of chilling

                   prior_muPhotoSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of photoperiod 
                   prior_muPhotoSp_sigma = 5,# varience of the prior distribution of the varience around the mean effect of photoperiod 
                   prior_sigmaPhotoSp_mu=5,# mean of the prior distribution of the varience around the mean effect of photoperiod
                   prior_sigmaPhotoSp_sigma=5,#variance of the prior distribution of the varience around the mean effect of photoperiod

                   prior_muPhenoSp_mu = 150, # mean of prior distribution of the mean (grand alpha) value of the phenology model
                   prior_muPhenoSp_sigma = 10, # variance of prior distribution of the mean (grand alpha) value of the phenology model
                   prior_sigmaPhenoSp_mu = 0,#the mean of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                   prior_sigmaPhenoSp_sigma = 10,  #the varience of the prior of the spread of species phenology values around teh grand mean muPhenoSp 


                   #prior_sigma_sp_sigma = 10,  # Faith doesn't knwo what these might
                   #prior_mu_study = 0,
                   #prior_sigma_study_mu = 10,
                   #prior_sigma_study_sigma = 10,



                   prior_betaTraitxForce_mu=0, # the mean of the prior distribution of the effect of trait on the slope of forcing 
                   prior_betaTraitxForce_sigma=1, # the varience of the prior distribution of the effect of trait on the slope of forcing 
                   prior_betaTraitxChill_mu=0,# the mean of the prior distribution of the effect of trait on the slope of chilling 
                   prior_betaTraitxChill_sigma=1,# the varience of the prior distribution of the effect of trait on the slope of chilling 
                   prior_betaTraitxPhoto_mu=0,# the mean of the prior distribution of the effect of trait on the slope of photo period 
                   prior_betaTraitxPhoto_sigma=1# the varience of the prior distribution of the effect of trait on the slope of photo period 
                ) 

#Run model
mdl.phen <- stan('stan/joint_3cue_phenoonly.stan',
                     data = pheno_data, warmup=3000, iter = 4000, cores = 4)
postMeanSLA <- extract(mdl.phen)

if(Midge== TRUE){ # only save data if on Midge
  save(mdl.phen, file = "phenologyMeanTrait_SLA.RData")
}

if(Midge == FALSE){ #if running on my computer I can load output from Midge
  load("Rfiles/phenologyMeanTrait_SLA.RData")
}



#Assessing output(not on Midge)
#-----------------------------------------------



if(Midge == FALSE){


  postMeanSLA<- extract(mdl.phen)
  str(mdl.phen)

  mean(postMeanSLA$betaTraitxChill)

  hist(postMeanSLA$betaTraitxChill)
  hist(postMeanSLA$betaTraitxForce)
  hist(postMeanSLA$betaTraitxPhoto)

  plot(mdl.phen)

  #launch_shinystan(mdl.phen)

  #plot main effects of cues
    postMeanSLAdf <- data.frame(postMeanSLA)

  cueEffects <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% c("muPhenoSp", "muForceSp", "muChillSp", "muPhotoSp", "sigmapheno_y")]

  cueEffectPlot <- mcmc_intervals(cueEffects) + 
     theme_classic() + 
      labs(title = "main intercept, cue slopes and general error")

  #Different species slopes for forcing, without the effect of trait

  postMeanSLA_alpaForceSp <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% grep( "alphaForceSp", colnames(postMeanSLAdf), value = TRUE)]
     colnames(postMeanSLA_alpaForceSp) <- levels(as.factor(SLAData$spps))

  alphaForcePlot <- mcmc_intervals(postMeanSLA_alpaForceSp) + 
     geom_vline(xintercept = mean(postMeanSLAdf$muForceSp), linetype="dotted", color = "grey")  +
     theme_classic() + 
     labs(subtitle = paste0("Mean muForceSp was ", round(mean(postMeanSLAdf$muForceSp),3)),
      title = "muForceSp - species forcing slopes no trait")

  #Different species slopes for forcing, with the effect of trait
  postMeanSLA_betaForceSp <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% grep( "betaForceSp", colnames(postMeanSLAdf), value = TRUE)]
  colnames(postMeanSLA_betaForceSp) <- levels(as.factor(SLAData$spps))

  betaForcePlot <- mcmc_intervals(postMeanSLA_betaForceSp) + 
      theme_classic() + 
      labs(title = "betaForceSp - Species forcing slopes with trait value")

  #Different species slopes for chilling, without the effect of trait
  postMeanSLA_alphaChillSp <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% grep( "alphaChillSp", colnames(postMeanSLAdf), value = TRUE)]
  colnames(postMeanSLA_alphaChillSp) <- levels(as.factor(SLAData$spps))

  alphaChillPlot <- mcmc_intervals(postMeanSLA_alphaChillSp) + 
      geom_vline(xintercept = mean(postMeanSLAdf$muChillSp), linetype="dotted", color = "grey")  +
      theme_classic() + 
      labs(subtitle = paste0("Mean muChillSp was ", round(mean(postMeanSLAdf$muChillSp),3)),
        title = "alphaChillSp - Species chill slopes no trait")

  #Different species slopes for forcing, with the effect of trait
  postMeanSLA_betaChillSp <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% grep( "betaChillSp", colnames(postMeanSLAdf), value = TRUE)]
  colnames(postMeanSLA_betaChillSp) <- levels(as.factor(SLAData$spps))

  betaChillPlot <- mcmc_intervals(postMeanSLA_betaChillSp) + 
      theme_classic() + 
      labs(title = "betaChillSp - Species chilling slopes with trait value")

  #Different species slopes for photoperiod, without the effect of trait
  postMeanSLA_alphaPhotoSp <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% grep( "alphaPhotoSp", colnames(postMeanSLAdf), value = TRUE)]
  colnames(postMeanSLA_alphaPhotoSp) <- levels(as.factor(SLAData$spps))

  alphaPhotoPlot <- mcmc_intervals(postMeanSLA_alphaPhotoSp) + 
      geom_vline(xintercept = mean(postMeanSLAdf$muPhotoSp), linetype="dotted", color = "grey")  +
      theme_classic() + 
      labs(subtitle = paste0("Mean muPhotoSp was ", round(mean(postMeanSLAdf$muPhotoSp),3)),
        title = "muPhotoSp - Species photo period slopes no trait")

  #Different species slopes for forcing, with the effect of trait
  postMeanSLA_betaPhotoSp <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% grep( "betaPhotoSp", colnames(postMeanSLAdf), value = TRUE)]
  colnames(postMeanSLA_betaPhotoSp) <- levels(as.factor(SLAData$spps))

  betaPhotoPlot <- mcmc_intervals(postMeanSLA_betaPhotoSp) + 
      theme_classic() + 
      labs(title = "betaPhotoSp - Species photoperiod slopes with trait value")


  #Different species slopes for forcing only the effect of trait
  postMeanSLA_betaTraitx <- postMeanSLAdf[,colnames(postMeanSLAdf) %in% grep( "betaTraitx", colnames(postMeanSLAdf), value = TRUE)]

  betaTraitxPlot <- mcmc_intervals(postMeanSLA_betaTraitx) + 
    theme_classic() + 
    labs(title = "effect's of traits on cue slopes")

  pdf("figures/MeanSLAphenologyPlots.pdf")
  cueEffectPlot
  betaTraitxPlot
  alphaForcePlot
  betaForcePlot
  alphaChillPlot
  betaChillPlot
  alphaPhotoPlot
  betaPhotoPlot
  dev.off()
}