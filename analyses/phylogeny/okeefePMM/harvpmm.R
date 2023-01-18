## Started 8-9 January 2023 ##
## Some code by JD ##

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Load libraries
library(ape)
library(rstan)
options(mc.cores = parallel::detectCores())

# Set working directory 
setwd("/Users/lizzie/Desktop/HF")

## By Lizzie
d <- read.csv("phenology/hf003-06-spring-mean-spp.csv")
length(unique(d$species))
sort(unique(d$species))

spp <- read.csv("phenology/hf003-01-plant.csv")
length(unique(spp$latin)) # ugh ...  "CORNUS ALTERNIFLORA" is poor form
sort(unique(spp$latin))

dsm <-  subset(d, is.na(l75.doy)==FALSE) # pretty sure l75 has the most data...
table(dsm$species) ## hmm, the species ones are not much more rare ... 

# clean up names for JD
spp <- subset(spp, latin!="CORNUS ALTERNIFLORA")
dspall <- subset(spp, select=c("latin", "common"))
dsp <- dspall[!duplicated(dspall), ]

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
dsp$genus <- firstup(tolower(unlist(lapply(strsplit(dsp$latin, split=" "), "[", 1))))
dsp$species <- tolower(unlist(lapply(strsplit(dsp$latin, split=" "), "[", 2)))

write.csv(dsp, "output/speciesnames.csv", row.names=FALSE)

# Guess Aronia based on what Dan Flynn and I used?
# Aronia melanocarpa
# Guess Amelanchier based on https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=hf003
# Amelanchier laevis and canadensis both mentioned

## Get phylogeny ...
# Below does not run because of how DAMN annoying R is (cannot install devtools)

if(FALSE){
# devtools::install_github("jinyizju/V.PhyloMaker")
library(V.PhyloMaker)
library(taxize)

spp<-read.csv("speciesnames.csv")
specieslist <- paste(spp$genus, spp$species)
genus<-spp$genus

#Fiddly bit to get taxonomic Family
tax.fam<-classification(genus, db = 'ncbi')
fam<-NULL
for (n in 1:length(genus)){
fam[n]<-tax.fam[[n]][,1][tax.fam[[n]][,2]=="family"]
}

input<-data.frame(species = specieslist, genus = genus, family = fam)
HF.tree <- phylo.maker(input, scenarios=c("S3"))$scenario.3
write.tree(HF.tree, file = "output/HF.tree.tre")
}

hftree <- read.tree("output/HF.tree.tre")

# Now get the data for the model ... 
head(d)
hftree$tip.label
dsp$lookup <- paste(substr(toupper(dsp$genus), 1, 2),
                    substr(toupper(dsp$species), 1, 2), sep="")
dsp$phylonam <- paste(dsp$genus, dsp$species, sep="_")
dsp$species <- NULL

datwithnames <- merge(d, dsp, by.x="species", by.y="lookup")

clim <- read.csv("climate/hf300-03-monthly-m.csv")
clim$year <- as.numeric(unlist(lapply(strsplit(clim$date, split="-"), "[", 1)))
clim$month <- as.numeric(unlist(lapply(strsplit(clim$date, split="-"), "[", 2)))
climsm <- subset(clim, year>1989)
climspr <- subset(climsm, month>2 & month<6)

climuse <- aggregate(climspr["airt"], climspr["year"], FUN=mean)

datuse <- merge(datwithnames, climuse, by="year")
datusesm <- subset(datuse, is.na(l75.doy)==FALSE)
datusesm <- subset(datusesm, select=c("year", "l75.doy", "phylonam", "airt"))

phymatch <- data.frame(tip=hftree$tip.label, sppnum=c(1:length(hftree$tip.label)))

dwithphy <- merge(datusesm, phymatch, by.x="phylonam", by.y="tip")
dwithphy <- dwithphy[order(dwithphy$sppnum),]

meanz <- aggregate(dwithphy["l75.doy"], dwithphy["sppnum"], FUN=mean)

## Now, onto Stan!

## First, fit the phylo model ...
# It needed a NCP, which thanks to Deirdre we added. Now runs great. 

fit <- stan("stan/uber_oneslope_cholesky_ncp.stan",
               data=list(N=nrow(dwithphy),
                         n_sp=length(unique(dwithphy$sppnum)),
                         sp=dwithphy$sppnum,
                         x1=dwithphy$airt,
                         y=dwithphy$l75.doy,
                         Vphy=vcv(hftree, corr = TRUE)),
               iter = 4000,
               warmup = 2000, # half the iter as warmp is default, but leaving in case we want to change
               chains = 4,
               seed = 117 
)

library(shinystan)
launch_shinystan(fit)

## Save fitted posterior
# saveRDS(fit, "output/fit_goober.rds")

## Next, fit the non-phylo model
# I never got this to run, the lambda=0 model has lots of divergent transitions
# I then wrote a straight-up partial pooling model, which clearly needed an NCP on the slope
# But adding it did not help (I don't think that I fit it wrong) ... the model just really wanted to drive sigma_b to zero ...
# Eventually I gave up and ran it in rstanarm

fitlamb0 <- stan("stan/uber_threeslopeintercept_modified_cholesky_updatedpriors_lamb0.stan",
               data=list(N=nrow(dwithphy),
                         n_sp=length(unique(dwithphy$sppnum)),
                         sp=dwithphy$sppnum,
                         x1=dwithphy$airt,
                         y=dwithphy$l75.doy,
                         Vphy=vcv(hftree, corr = TRUE)),
               iter = 4000,
               warmup = 2000, 
               chains = 4,
               seed = 117)

fit0 <- stan("stan/regularmodel.stan", # also possible: regularmodel_ncpb.stan and regularmodel_ncpba.stan
               data=list(N=nrow(dwithphy),
                         n_sp=length(unique(dwithphy$sppnum)),
                         sp=dwithphy$sppnum,
                         x1=dwithphy$airt,
                         y=dwithphy$l75.doy),
               iter = 5000,
               warmup = 4000,
               chains = 4,
               seed = 117 
)

launch_shinystan(fit0)

library(rstanarm)
fitarm <- stan_lmer(l75.doy~(airt|sppnum), data=dwithphy)

# Try to compare estimates ... 
fitarmcoef <- posterior_interval(fitarm, prob = 0.5)
fitarmsppb <- fitarmcoef[grep("b\\[airt", rownames(fitarmcoef))]
fitarmsppa <- fitarmcoef[grep("b\\[\\(Intercept", rownames(fitarmcoef))] + fitarmcoef[1]

sumfit <- summary(fit)$summary
fitsppb <- sumfit[grep("b\\[", rownames(sumfit)),]
fitsppa <- sumfit[grep("a\\[", rownames(sumfit)),]
fitsppb[,1]

# Compare slopes from the two models ...
# Hmm, not clear how to know the 'true' slope
plot(x=fitsppb[,1], y=fitarmsppb, xlab="PMM -- slope", ylab="ARM -- slope")
abline(0,1)
abline(v=mean(coef(fitarm)$sppnum[["airt"]]), col="blue")

# compare intercepts ... 
plot(x=fitsppa[,1], y=fitarmsppa, xlab="PMM -- intercept", ylab="ARM -- intercept")
abline(0,1)

# Try to compare with mean estimates from raw data.. meh
dwithphyagg <- aggregate(dwithphy["l75.doy"], dwithphy["sppnum"], FUN=mean)
plot(y=fitsppa[,1], x=dwithphyagg$l75.doy)
