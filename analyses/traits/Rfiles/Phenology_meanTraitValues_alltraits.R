#Running teh phenology model with a potential effect of trait, but feeding in mean trait valus rather than using another model to estimate trait for each species
#We are doing this so we can have a backup if teh joint models dont work, and so we can sanity check the joint model results

#Started by Faith Jones Sep 23 2021
# Adapted by Deirdre to use flags

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

# Set Midge Flag
Midge <- FALSE

color_scheme_set("viridis")

#Source ospree trators data if thsi runs on Faith's section of Midge
if(Midge == TRUE){
  setwd("~/ospree")
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

SLA <- FALSE
LNC <- FALSE
SSD<- FALSE
Seed <- TRUE

source("Rfiles/trait_flags.R")

#Run model
mdl.phen <- stan('stan/joint_3cue_phenoonly.stan',
                     data = pheno_data, warmup=3000, iter = 4000, cores = 4)
postMeanSSD <- extract(mdl.phen)

if(Midge== TRUE){ # only save data if on Midge
  save(mdl.phen, file = "phenologyMeanTrait_Seed.RData")
}

if(Midge == FALSE){ #if running on my computer I can load output from Midge
  load("output/phenologyMeanTrait_Seed.RData")
}



#Assessing output(not on Midge)
#-----------------------------------------------



if(Midge == FALSE){

  sum <- summary(mdl.phen)$summary
  postMeanTrt<- extract(mdl.phen)
  str(mdl.phen)

  mean(postMeanTrt$betaTraitxChill)

  plot(mdl.phen)
  pdf("figures/mdl_pheno_seed_pairs.pdf")
  pairs(mdl.phen, pars = c("alphaForceSp", "alphaPhotoSp", "alphaChillSp", "alphaPhenoSp", "lp__")) 
  dev.off()

  #launch_shinystan(mdl.phen)

  #plot main effects of cues
  postMeanTrtdf <- data.frame(postMeanTrt)
  cueEffects <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% c("muPhenoSp", "muForceSp", "muChillSp", "muPhotoSp", "sigmapheno_y")]

  cueEffectPlot <- mcmc_intervals(cueEffects) + 
     theme_classic() + 
      labs(title = "main intercept, cue slopes and general error")

  #Different species slopes for forcing, without the effect of trait
  #postMeanSSDdf <- data.frame(postMeanSSD)
  postMeanTrt_alpaForceSp <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "alphaForceSp", colnames(postMeanTrtdf), value = TRUE)]
     colnames(postMeanTrt_alpaForceSp) <- levels(as.factor(seedData$spps))

  alphaForcePlot <- mcmc_intervals(postMeanTrt_alpaForceSp) + 
     geom_vline(xintercept = mean(postMeanTrtdf$muForceSp), linetype="dotted", color = "grey")  +
     theme_classic() + 
     labs(subtitle = paste0("Mean muForceSp was ", round(mean(postMeanTrtdf$muForceSp),3)),
      title = "muForceSp - species forcing slopes no trait")

  #Different species slopes for forcing, with the effect of trait
  postMeanTrt_betaForceSp <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "betaForceSp", colnames(postMeanTrtdf), value = TRUE)]
  colnames(postMeanTrt_betaForceSp) <- levels(as.factor(seedData$spps))

  betaForcePlot <- mcmc_intervals(postMeanTrt_betaForceSp) + 
      theme_classic() + 
      labs(title = "betaForceSp - Species forcing slopes with trait value")

  #Different species slopes for chilling, without the effect of trait
  postMeanTrt_alphaChillSp <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "alphaChillSp", colnames(postMeanSSDdf), value = TRUE)]
  colnames(postMeanTrt_alphaChillSp) <- levels(as.factor(seedData$spps))

  alphaChillPlot <- mcmc_intervals(postMeanTrt_alphaChillSp) + 
      geom_vline(xintercept = mean(postMeanTrtdf$muChillSp), linetype="dotted", color = "grey")  +
      theme_classic() + 
      labs(subtitle = paste0("Mean muChillSp was ", round(mean(postMeanTrtdf$muChillSp),3)),
        title = "alphaChillSp - Species chill slopes no trait")

  #Different species slopes for forcing, with the effect of trait
  postMeanTrt_betaChillSp <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "betaChillSp", colnames(postMeanTrtdf), value = TRUE)]
  colnames(postMeanTrt_betaChillSp) <- levels(as.factor(seedData$spps))

  betaChillPlot <- mcmc_intervals(postMeanTrt_betaChillSp) + 
      theme_classic() + 
      labs(title = "betaChillSp - Species chilling slopes with trait value")

  #Different species slopes for photoperiod, without the effect of trait
  postMeanTrt_alphaPhotoSp <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "alphaPhotoSp", colnames(postMeanTrtdf), value = TRUE)]
  colnames(postMeanTrt_alphaPhotoSp) <- levels(as.factor(seedData$spps))

  alphaPhotoPlot <- mcmc_intervals(postMeanTrt_alphaPhotoSp) + 
      geom_vline(xintercept = mean(postMeanTrtdf$muPhotoSp), linetype="dotted", color = "grey")  +
      theme_classic() + 
      labs(subtitle = paste0("Mean muPhotoSp was ", round(mean(postMeanTrtdf$muPhotoSp),3)),
        title = "muPhotoSp - Species photo period slopes no trait")

  #Different species slopes for forcing, with the effect of trait
  postMeanTrt_betaPhotoSp <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "betaPhotoSp", colnames(postMeanTrtdf), value = TRUE)]
  colnames(postMeanTrt_betaPhotoSp) <- levels(as.factor(seedData$spps))

  betaPhotoPlot <- mcmc_intervals(postMeanTrt_betaPhotoSp) + 
      theme_classic() + 
      labs(title = "betaPhotoSp - Species photoperiod slopes with trait value")


  #Different species slopes for forcing only the effect of trait
  postMeanTrt_betaTraitx <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "betaTraitx", colnames(postMeanTrtdf), value = TRUE)]

  betaTraitxPlot <- mcmc_intervals(postMeanTrt_betaTraitx) + 
    theme_classic() + 
    labs(title = "effect's of traits on cue slopes")

  pdf("figures/MeanSeedphenologyPlots.pdf")
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