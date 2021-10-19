# Started Oct 19, 2021 by Deirdre Loughnan

# This aim of this script is to summarize the model output of the mean trait + pheno model for seed mass specifially, to look at the priors, and to do posterior predictive checks
rm(list=ls())
options(stringsAsFactors = FALSE)

library(tidyr)
library(plyr)
library(dplyr)
library(reshape2)
library(rstan)
library(bayesplot)# nice posterior check plots 
library(shinystan)

if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if (length(grep("faith", getwd())>0)) { setwd("/home/faith/Documents/github/ospree/analyses/traits")
} else if (length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
} 

dat1 <- read.csv("input/try_bien_nodups_1.csv") 
dat2 <- read.csv("input/try_bien_nodups_2.csv") 
dat <- rbind(dat1, dat2)
names(dat)

dat$traitname[which(dat$traitname == "seed mass")] <- "Seed_mass"
dat$traitname[which(dat$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")] <- "LNC"
dat$traitname[which(dat$traitname == "Specific_leaf_area")] <- "SLA"
dat$traitname[which(dat$traitname == "Stem_specific_density")] <- "SSD"
#(dat$speciesname, dat$traitname)

#select traits we are interested in
traitSelect <- c("Seed_mass", "SLA", "SSD", "LNC", "Plant_height_vegetative")
selectData <- dat[dat$traitname %in% traitSelect,]

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

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana",
                 "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia",
                 "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur",
                 "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

#select species
traitSelect <- meanTraitWide[meanTraitWide$species %in% traitors.sp,]


#combine data 
#traitOspree <- merge(ospree_traitors,traitSelect, by.y = "speciesname", by.x = "spps")
traitOspree <- traitSelect
seedData <- traitOspree[!is.na(traitOspree$Seed_mass),]
seedData$spps <- seedData$speciesname
# # select the species we are interested in 
# traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", 
#                  "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", 
#                  "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", 
#                  "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")
# 
# selectData <- selectData[selectData$speciesname %in% traitors.sp,]
# 
# SLAData <- traitOspree[!is.na(traitOspree$SLA),]

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


load("output/phenologyMeanTrait_Seed.RData")

sum <- summary(mdl.phen)$summary
postMeanTrt<- extract(mdl.phen)
#str(mdl.phen)

# ssm <-  as.shinystan(mdl.phen)
# launch_shinystan(ssm)

mean(postMeanTrt$betaTraitxChill) # -0.2147

## look at priors:
#muForceSp
h1 <- hist(rnorm(1000, 0, 5))
h2 <- hist(postMeanTrt$muForceSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#muChillSp
h1 <- hist(rnorm(1000, 0, 5))
h2 <- hist(postMeanTrt$muChillSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#muPhotoSp
h1 <- hist(rnorm(1000, 0, 5))
h2 <- hist(postMeanTrt$muPhotoSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigmaForceSp
h1 <- hist(rnorm(1000, 5, 5))
h2 <- hist(postMeanTrt$sigmaForceSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigmaChillSp
h1 <- hist(rnorm(1000, 5, 5))
h2 <- hist(postMeanTrt$sigmaChillSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigmaPhotoSp
h1 <- hist(rnorm(1000, 5, 5))
h2 <- hist(postMeanTrt$sigmaPhotoSp)
plot(h2, col=rgb(0,0,1,1/4), xlim =c(-30,30))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#muPhenoSp
h1 <- hist(rnorm(1000, 150, 50))
h2 <- hist(postMeanTrt$muPhenoSp)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(0, 200))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#sigmaPhenoSp
h1 <- hist(rnorm(1000, 150, 50))
h2 <- hist(postMeanTrt$sigmaPhenoSp)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(0, 200))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#betaTraitxForce
h1 <- hist(rnorm(1000, 0,1))
h2 <- hist(postMeanTrt$betaTraitxForce)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-5, 5))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#betaTraitxChill
h1 <- hist(rnorm(1000, 0,1))
h2 <- hist(postMeanTrt$betaTraitxChill)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-5, 5))
plot(h1, col=rgb(1,0,1,1/4), add = T)

#betaTraitxPhoto
h1 <- hist(rnorm(1000, 0,1))
h2 <- hist(postMeanTrt$betaTraitxPhoto)
plot(h2, col=rgb(0,0,1,1/4), xlim = c(-5, 5))
plot(h1, col=rgb(1,0,1,1/4), add = T)


# plot(mdl.phen)
# pdf("figures/mdl_pheno_seed_pairs.pdf")
# pairs(mdl.phen, pars = c("alphaForceSp", "alphaPhotoSp", "alphaChillSp", "alphaPhenoSp", "lp__")) 
# dev.off()

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
postMeanTrt_alphaChillSp <- postMeanTrtdf[,colnames(postMeanTrtdf) %in% grep( "alphaChillSp", colnames(postMeanTrtdf), value = TRUE)]
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

####################################################################
# Generate the summary table

mu_forcesp <- sum[grep("muForceSp", rownames(sum))]
mu_chillsp <- sum[grep("muChillSp", rownames(sum))]
mu_photosp <- sum[grep("muPhotoSp", rownames(sum))]
mu_phenosp <- sum[grep("muPhenoSp", rownames(sum))]

sigma_forcesp <- sum[grep("sigmaForceSp", rownames(sum))]
sigma_chillsp <- sum[grep("sigmaChillSp", rownames(sum))]
sigma_photosp <- sum[grep("sigmaPhotoSp", rownames(sum))]
sigma_phenosp <- sum[grep("sigmaPhenoSp", rownames(sum))]
sigma_phenoy <- sum[grep("sigmapheno_y", rownames(sum))]

beta_tc <- sum[grep("betaTraitxChill", rownames(sum))]
beta_tp <- sum[grep("betaTraitxPhoto", rownames(sum))]
beta_tf <- sum[grep("betaTraitxForce", rownames(sum))]

# ------------------------------------------------------------# 
mu_forcesp2.5 <- sum[grep("muForceSp", rownames(sum)), "2.5%"]
mu_chillsp2.5 <- sum[grep("muChillSp", rownames(sum)), "2.5%"]
mu_photosp2.5 <- sum[grep("muPhotoSp", rownames(sum)), "2.5%"]
mu_phenosp2.5 <- sum[grep("muPhenoSp", rownames(sum)), "2.5%"]

sigma_chillsp2.5 <- sum[grep("sigmaChillSp", rownames(sum)), "2.5%"]
sigma_photosp2.5 <- sum[grep("sigmaPhotoSp", rownames(sum)), "2.5%"]
sigma_forcesp2.5 <- sum[grep("sigmaForceSp", rownames(sum)), "2.5%"]
sigma_phenosp2.5 <- sum[grep("sigmaPhenoSp", rownames(sum)), "2.5%"]
sigma_phenoy2.5 <- sum[grep("sigmapheno_y", rownames(sum)), "2.5%"]

beta_tc2.5 <- sum[grep("betaTraitxChill", rownames(sum)), "2.5%"]
beta_tp2.5 <- sum[grep("betaTraitxPhoto", rownames(sum)), "2.5%"]
beta_tf2.5 <- sum[grep("betaTraitxForce", rownames(sum)), "2.5%"]

# ------------------------------------------------------------# 
mu_forcesp97.5 <- sum[grep("muForceSp", rownames(sum)), "97.5%"]
mu_chillsp97.5 <- sum[grep("muChillSp", rownames(sum)), "97.5%"]
mu_photosp97.5 <- sum[grep("muPhotoSp", rownames(sum)), "97.5%"]
mu_phenosp97.5 <- sum[grep("muPhenoSp", rownames(sum)), "97.5%"]

sigma_forcesp97.5 <- sum[grep("sigmaForceSp", rownames(sum)), "97.5%"]
sigma_chillsp97.5 <- sum[grep("sigmaChillSp", rownames(sum)), "97.5%"]
sigma_photosp97.5 <- sum[grep("sigmaPhotoSp", rownames(sum)), "97.5%"]
sigma_phenosp97.5 <- sum[grep("sigmaPhenoSp", rownames(sum)), "97.5%"]
sigma_phenoy97.5 <- sum[grep("sigmapheno_y", rownames(sum)), "97.5%"]

beta_tc97.5 <- sum[grep("betaTraitxChill", rownames(sum)), "97.5%"]
beta_tp97.5 <- sum[grep("betaTraitxPhoto", rownames(sum)), "97.5%"]
beta_tf97.5 <- sum[grep("betaTraitxForce", rownames(sum)), "97.5%"]


mdl.out <- data.frame( "Parameter" = c("mu_forcesp","mu_chillsp","mu_photosp","mu_phenosp","sigma_forcesp","sigma_chillsp","sigma_photosp", "sigma_phenosp","sigma_phenoy", "beta_tf", "beta_tc","beta_tp"),
                       "Estiamte" = c(mu_forcesp, mu_chillsp, mu_photosp, mu_phenosp, sigma_forcesp, sigma_chillsp, sigma_photosp, sigma_phenosp, sigma_phenoy, beta_tf, beta_tc,  beta_tp),
                       "2.5" = c(mu_forcesp2.5, mu_chillsp2.5, mu_photosp2.5, mu_phenosp2.5, sigma_forcesp2.5, sigma_chillsp2.5, sigma_photosp2.5, sigma_phenosp2.5, sigma_phenoy2.5, beta_tf2.5, beta_tc2.5,  beta_tp2.5),
                       "97.5" = c(mu_forcesp97.5, mu_chillsp97.5, mu_photosp97.5, mu_phenosp97.5, sigma_forcesp97.5, sigma_chillsp97.5, sigma_photosp97.5, sigma_phenosp97.5, sigma_phenoy97.5, beta_tf97.5, beta_tc97.5,  beta_tp97.5)
                       
)

mdl.out


pdf("figures/MeanSeedphenologyPlots.pdf")
cueEffectPlot
betaTraitxPlot
alphaForcePlot
betaForcePlot
alphaChillPlot
betaChillPlot
alphaPhotoPlot
betaPhotoPlot
write.table(mdl.out,"seed_summary.txt")
dev.off()

