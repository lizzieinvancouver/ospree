## Started 19 March 2021 ##
## By Lizzie ##

## Run the Ospree data for the traitors species ##
## With Stan! ##

## This code is based heavily off models_stan.R 

## To do

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(ggplot2)
library(gridExtra)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("deirdreloughnan", getwd()))>0) { 
  setwd("~/Documents/github/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

######################################
# Flags to choose for bbstanleadin.R #
######################################
runzscoremodel <- TRUE # as of Nov 2021, you can run the z-scored model or natural units model

if(!runzscoremodel){
# Master flags! Here you pick if you want the flags for the main model (figure 2 in main text) versus other versions (all spp model, chill portions, uncentered predictors, as in supp table and figures 3-4)
use.flags.for.mainmodel <- FALSE
use.flags.for.spcomp.cp <- FALSE
use.flags.for.allspp.utah <- FALSE # March 2021 -- picking this as includes the MOST data
use.flags.for.spcomp.utah.nonz <- FALSE
use.flags.for.spcomp.cp.nonz <- FALSE # predictors on natural scale, spcomplex with utah units. Fig 3-4 in main text of budburst ms
use.flags.for.allspp.utah.nonz <- TRUE
use.yourown.flagdesign <- FALSE
}

if(runzscoremodel){
use.flags.for.mainmodel <- FALSE
use.flags.for.spcomp.cp <- FALSE
use.flags.for.allspp.utah <- TRUE # March 2021 -- picking this as includes the MOST data
use.flags.for.spcomp.utah.nonz <- FALSE
use.flags.for.spcomp.cp.nonz <- FALSE # predictors on natural scale, spcomplex with utah units. Fig 3-4 in main text of budburst ms
use.flags.for.allspp.utah.nonz <- FALSE
use.yourown.flagdesign <- FALSE    
}

source("source/flags.for.models.in.bbms.R")

# this below file does a bunch of further cleaning and then makes little list of data to feed to stan
# it could probably be adjusted to include traitors, but for now I just manipulate the output from it below
# in a hacky, inelegant way ... but hey -- it's a start ...
source("source/bbstanleadin.R")

# Set this to run the 26 traitor species, otherwise it runs the model on all species... 
runtraitorssp <- TRUE

######################################
# Some hacky code to get the species #
######################################

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")
traitors.sp[which(!traitors.sp %in% unique(bb.stan$latbi))] # promising


if(runzscoremodel){
if(runtraitorssp){
bb.traitors <- bb.stan[which(bb.stan$latbi %in% traitors.sp),]
bb.traitors$complex <- as.numeric(as.factor(bb.traitors$latbi)) # re-number the species

datalist.bb <- with(bb.traitors, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.traitors),
                         n_sp = length(unique(bb.traitors$complex))
                    )
)
}
}

if(!runzscoremodel){
if(runtraitorssp){
bb.traitors <- bb.stan[which(bb.stan$latbi %in% traitors.sp),]
bb.traitors$complex <- as.numeric(as.factor(bb.traitors$latbi)) # re-number the species

datalist.bb <- with(bb.traitors, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.traitors),
                         n_sp = length(unique(bb.traitors$complex))
                    )
)
}
}

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
               iter = 3000, warmup=1500)

check_all_diagnostics(m2l.ni)
# launch_shinystan(m2l.ni)

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]

ys<-datalist.bb$y
# posterior predictive checks....
if(FALSE){
y_pred <- extract(m2l.ni, 'y_ppc')

par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="No intxn model")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="")
}


# Code if you want to save your models (do NOT push output to git)
#save(m2l.ni, file="m2lni_traitors_utah_z.Rda")

sumer <- summary(m2l.ni)$summary
sumer[grep("mu_", rownames(sumer)),]

b_chill <- sumer[grep("^b_chill", rownames(sumer)),]
b_force <- sumer[grep("^b_force", rownames(sumer)),]
b_photo <- sumer[grep("^b_photo", rownames(sumer)),]

## Side bar to look at cues versus leafout day
if(!runzscoremodel){ # this makes the most sense using a natural units model (where the intercept is cues=0)
a_sp <- sumer[grep("^a_sp", rownames(sumer)),]

# set up some values of cues
hist(bb.stan$chill)
chillvalue <- 15
forcevalue <- 10
photovalue <- 14

leafoutdays <- a_sp[,"mean"] + b_chill[,"mean"]*chillvalue + b_force[,"mean"]*forcevalue + b_photo[,"mean"]*photovalue
plot(leafoutdays~b_chill[,"mean"], ylab="Leafout day estimated from model", xlab="chilling cue", pch=16)
text(x=b_chill[,"mean"], y=leafoutdays, labels=unique(bb.traitors$latbi))
# Sort of what I expected, known late-budburst species (non-ilex and cocc. Quercus, Fagus show up as the earliest leafout since they have higher photo and chill cues and we use linear regression

par(mfrow=c(1,4))
plot(leafoutdays ~ a_sp[,"mean"], ylab="leafoutdays", xlab="intercept")
plot(leafoutdays ~ b_chill[,"mean"], ylab="leafoutdays", xlab="chilling cue")
plot(leafoutdays ~ b_force[,"mean"], ylab="leafoutdays", xlab="forcing cue")
plot(leafoutdays ~ b_photo[,"mean"], ylab="leafoutdays", xlab="photoperiod cue")
par(mfrow=c(1,3))
plot(a_sp[,"mean"] ~ b_chill[,"mean"], xlab="chilling cue", ylab="intercept")
abline(lm((a_sp[,"mean"] ~ b_chill[,"mean"])))
plot(a_sp[,"mean"] ~ b_force[,"mean"], xlab="forcing cue", ylab="intercept")
abline(lm((a_sp[,"mean"] ~ b_force[,"mean"])))
plot(a_sp[,"mean"] ~ b_photo[,"mean"], xlab="photoperiod cue", ylab="intercept")
abline(lm((a_sp[,"mean"] ~ b_photo[,"mean"])))

# Lizzie's take-homes from this:
# (a) We can't use the model to establish lafe versus early, if we model everything as thresholds and not linear regression we could perhaps, but we don't
# (b) There is a correlation between chill and intercept, suggestive that higher chill cues are later-active species

}
## End side bar looking at leafout days

## Side bar to compare to cues estimated from joint model (ooh! Exciting)
lnccues <- read.csv("..//traits/output/lnccues.csv", header=TRUE)
row.names(lnccues) <- lnccues$X
lncphoto <- lnccues[grep("betaPhoto", rownames(lnccues)),]
lncforce <- lnccues[grep("betaForce", rownames(lnccues)),]
lncchill <- lnccues[grep("betaChill", rownames(lnccues)),]

pdf("..//traits/figures/comparecuesestimates_phenovslncjoint.pdf", width=10, height=4)
par(mfrow=c(1,3))
plot(b_chill[,"mean"]~lncchill$mean, ylab="pheno-only model cue estimate", xlab="LNC-model cue estimate", main="chill")
abline(0,1)
plot(b_force[,"mean"]~lncforce$mean, ylab="pheno-only model cue estimate", xlab="LNC-model cue estimate", main="force")
abline(0,1)
plot(b_photo[,"mean"]~lncphoto$mean, ylab="pheno-only model cue estimate", xlab="LNC-model cue estimate", main="photo")
abline(0,1)
dev.off()
## End side bar compare cues between models


dat1 <- read.csv("..//traits/input/try_bien_nodups_1.csv") 
dat2 <- read.csv("..//traits/input/try_bien_nodups_2.csv") 
dat <- rbind(dat1, dat2)

dat_sp <-dat[dat$species %in% traitors.sp,]

dat_sp$traitname[which(dat_sp$traitname == "seed mass")] <- "Seed_mass"
dat_sp$traitname[which(dat_sp$traitname == "Leaf_nitrogen_.N._content_per_leaf_dry_mass")] <- "LNC"
dat_sp$traitname[which(dat_sp$traitname == "Specific_leaf_area")] <- "SLA"
dat_sp$traitname[which(dat_sp$traitname == "Stem_specific_density")] <- "SSD"

triatSelect <- c("Seed_mass", "SLA", "SSD", "LNC", "Plant_height_vegetative")
selectData <- dat_sp[dat_sp$traitname %in% triatSelect,]

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



bchill <- as.data.frame(b_chill)
chill <- cbind(meanSSD, bchill); colnames(chill)[colnames(chill) == "traitvalue"] <- "meanSSD"
chill <- merge(chill[,2:13], meanSLA, by = "speciesname"); colnames(chill)[colnames(chill) == "traitvalue"] <- "meanSLA"
chill <- merge(chill, meanLNC, by = "speciesname"); colnames(chill)[colnames(chill) == "traitvalue"] <- "meanLNC"
chill <- merge(chill, meanSeed, by = "speciesname"); colnames(chill)[colnames(chill) == "traitvalue"] <- "meanSeed"
chill <- merge(chill, meanHeight, by = "speciesname"); colnames(chill)[colnames(chill) == "traitvalue"] <- "meanHeight"
chill <- chill[,c("speciesname","meanSSD","meanSLA","meanLNC","meanSeed","meanHeight","mean","se_mean","sd","2.5%","25%","50%","75%","97.5%")]

cSSD <- ggplot(chill, aes(x= meanSSD, y = mean)) +
  geom_point()+ labs(y="chilling cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
cSLA <- ggplot(chill, aes(x= meanSLA, y = mean)) +
  geom_point()+ labs(y="chilling cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

cSeed <- ggplot(chill, aes(x= log10(meanSeed), y = mean)) +
  geom_point()+ labs(y="chilling cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

cLNC <- ggplot(chill, aes(x= meanLNC, y = mean)) +
  geom_point()+ labs(y="chilling cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

cHeight <- ggplot(chill, aes(x= meanHeight, y = mean)) +
  geom_point()+ labs(y="chilling cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pdf("..//traits/figures/chillvstraits.pdf", width = 15, height = 3)
grid.arrange(cSSD, cSLA,cLNC,cHeight,cSeed, nrow=1)
dev.off()



bforce <- as.data.frame(b_force)
force <- cbind(meanSSD, bforce); colnames(force)[colnames(force) == "traitvalue"] <- "meanSSD"
force <- merge(force[,2:13], meanSLA, by = "speciesname"); colnames(force)[colnames(force) == "traitvalue"] <- "meanSLA"
force <- merge(force, meanLNC, by = "speciesname"); colnames(force)[colnames(force) == "traitvalue"] <- "meanLNC"
force <- merge(force, meanSeed, by = "speciesname"); colnames(force)[colnames(force) == "traitvalue"] <- "meanSeed"
force <- merge(force, meanHeight, by = "speciesname"); colnames(force)[colnames(force) == "traitvalue"] <- "meanHeight"
force <- force[,c("speciesname","meanSSD","meanSLA","meanLNC","meanSeed","meanHeight","mean","se_mean","sd","2.5%","25%","50%","75%","97.5%")]

fSSD <- ggplot(force, aes(x= meanSSD, y = mean)) +
  geom_point()+ labs(y="forceing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

fSLA <- ggplot(force, aes(x= meanSLA, y = mean)) +
  geom_point()+ labs(y="forceing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

fSeed <- ggplot(force, aes(x= log10(meanSeed), y = mean)) +
  geom_point()+ labs(y="forceing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

fLNC <- ggplot(force, aes(x= meanLNC, y = mean)) +
  geom_point()+ labs(y="forceing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

fHeight <- ggplot(force, aes(x= meanHeight, y = mean)) +
  geom_point()+ labs(y="forceing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pdf("..//traits/figures/forcevstraits.pdf", width = 15, height = 3)
grid.arrange(fSSD, fSLA,fLNC,fHeight,fSeed, nrow=1)
dev.off()

bphoto <- as.data.frame(b_photo)
photo <- cbind(meanSSD, bphoto); colnames(photo)[colnames(photo) == "traitvalue"] <- "meanSSD"
photo <- merge(photo[,2:13], meanSLA, by = "speciesname"); colnames(photo)[colnames(photo) == "traitvalue"] <- "meanSLA"
photo <- merge(photo, meanLNC, by = "speciesname"); colnames(photo)[colnames(photo) == "traitvalue"] <- "meanLNC"
photo <- merge(photo, meanSeed, by = "speciesname"); colnames(photo)[colnames(photo) == "traitvalue"] <- "meanSeed"
photo <- merge(photo, meanHeight, by = "speciesname"); colnames(photo)[colnames(photo) == "traitvalue"] <- "meanHeight"
photo <- photo[,c("speciesname","meanSSD","meanSLA","meanLNC","meanSeed","meanHeight","mean","se_mean","sd","2.5%","25%","50%","75%","97.5%")]

pSSD <- ggplot(photo, aes(x= meanSSD, y = mean)) +
  geom_point()+ labs(y="photoing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pSLA <- ggplot(photo, aes(x= meanSLA, y = mean)) +
  geom_point()+ labs(y="photoing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pSeed <- ggplot(photo, aes(x= log10(meanSeed), y = mean)) +
  geom_point()+ labs(y="photoing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pLNC <- ggplot(photo, aes(x= meanLNC, y = mean)) +
  geom_point()+ labs(y="photoing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pHeight <- ggplot(photo, aes(x= meanHeight, y = mean)) +
  geom_point()+ labs(y="photoing cue") +
  geom_errorbar(aes(ymin = mean - se_mean, ymax = mean + se_mean))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pdf("..//traits/figures/photovstraits.pdf", width = 15, height = 3)
grid.arrange(pSSD, pSLA,pLNC,pHeight,pSeed, nrow=1)
dev.off()
