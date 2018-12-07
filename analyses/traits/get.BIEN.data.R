### Started Dec 5 2018 ###

## What species should we get trait data for? How much can we get for the good species in OSPREE? ##

#The aim of this code is to get a list of the speices for which there are more than one study, excluding the species that are in the species complex's
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/github/ospree/analyses/trait_analysis")

library(rstan)
library(ggplot2)
library(shinystan)
# library(bayesplot)
library(dplyr)
# library(rstanarm)

#source('..//stan/savestan.R')
source("source/trait.species.R")
source("source/bbdataplease.R")
source("source/commoncols.R")
source("source/othertreats.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
#Stealing code from the bbstanleadin.R file#
#Aim is to get a list of species that we want to use 
################################################################# 

## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!

## (1) Get the data and slim down to correct response and no NAs ...
source("source/bbdataplease.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA
## (3) Get fewer columns for sanity
source("source/commoncols.R")
bb <- subset(d, select=c(columnstokeep, columnscentered, columnschillunits))

# remove the two values above 600
bb <- subset(bb, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb$chill <- bb$chill/240
length(unique(bb$datasetID))
length(unique(bb$genus))
head(bb.expphoto)

# deal with photo
#
unique(bb$photo_type)
bb.expphoto <- subset(bb, photo_type=="exp"); head(bb.expphoto)
bb.rampphoto <- subset(bb, photo_type=="ramped"); head(bb.rampphoto)
bb.exprampphoto <- subset(bb, photo_type=="exp" | photo_type=="ramped"); head(bb.exprampphoto)
#
bb.ambphoto <- subset(bb, photo_type=="amb" | photo_type=="none"); head(bb.ambphoto)
#sort(unique(bb.expphoto$datasetID))
#sort(unique(bb.rampphoto$datasetID))
#sort(unique(bb.ambphoto$datasetID))

# add in forcing
bb.exprampphotoforce <- subset(bb.exprampphoto, force_type=="exp"|force_type=="ramped")
bb.expphotoforce <- subset(bb.expphoto, force_type=="exp")
head(bb.expphotoforce)

temp<-spptraitfx(bb.expphotoforce)
temp

spp.list<-unique(temp$name); spp.list
length(spp.list)
#write.table(spp.list, file="Species.list.OSPREE.csv", row.names = FALSE, col.names=FALSE)

require(BIEN)

result<-vector()
for(i in 1:length(spp.list)){
  traittemp<-BIEN_trait_traitbyspecies(species=spp.list[1], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))
  result<-traittemp
}

#### This should work but doesn't 
traitadd<-BIEN_trait_traitbyspecies(species=spp.list[1], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

for (i in c(2:length(spp.list))){
  
  traittemp<-BIEN_trait_traitbyspecies(species=spp.list[i], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))
# df<-data.frame(traitadd)
 traitadd<-rbind(traitadd, traittemp)
}


i
unique(traitadd$scrubbed_species_binomial)
###########################
final=list()
for( i in 1:length(spp.list)){
  traitsub<-subset(spp.list, identifyer)
  traittemp<-BIEN_trait_traitbyspecies(species=spp.list[i], trait=c("flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))
final[paste(i, specieslist[i])]<-list()
}


  final<-traittemp[[i]]
  
}

head(traits)
unique(traits$scrubbed_species_binomial)

######################################################################################################
spp1<-BIEN_trait_traitbyspecies(species=spp.list[1], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp2<-BIEN_trait_traitbyspecies(species=spp.list[2], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp3<-BIEN_trait_traitbyspecies(species=spp.list[3], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp4<-BIEN_trait_traitbyspecies(species=spp.list[4], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp5<-BIEN_trait_traitbyspecies(species=spp.list[5], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp6<-BIEN_trait_traitbyspecies(species=spp.list[6], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp7<-BIEN_trait_traitbyspecies(species=spp.list[7], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp8<-BIEN_trait_traitbyspecies(species=spp.list[8], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp9<-BIEN_trait_traitbyspecies(species=spp.list[9], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp10<-BIEN_trait_traitbyspecies(species=spp.list[10], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp11<-BIEN_trait_traitbyspecies(species=spp.list[11], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp12<-BIEN_trait_traitbyspecies(species=spp.list[12], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp14<-BIEN_trait_traitbyspecies(species=spp.list[14], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

spp15<-BIEN_trait_traitbyspecies(species=spp.list[15], trait=c("diameter at breast height (1.3 m)","flower pollination syndrome","leaf area","leaf life span","leaf area per leaf dry mass","leaf carbon content per leaf nitrogen content","leaf dry mass","leaf dry mass per leaf fresh mass","leaf fresh mass","leaf life span","leaf relative growth rate","maximum whole plant height","root dry mass","seed mass","stem dry mass","stem relative growth rate","stem wood density","whole plant growth form","whole plant height","whole plant woodiness"))

require(plyr)

data<-rbind(spp1,spp2,spp3,spp4,spp5,spp6,spp7,spp8,spp9,spp10,spp11,spp12,spp14,spp15)
head(data)

nodbh<-subset(data, trait_name!="diameter at breast height (1.3 m)")

require(tidyr)
#DBH swamps all other traits and I have no reason to believe it is a significant trait
#BIEN_traitdata<-spread(data, key="trait_name", value="trait_value")

BIEN_traitdata<-spread(test, key=c("trait_name"), value="trait_value")
bien<-BIEN_traitdata
#There are some issues with the names in this dataset, there are a lot of spaces now in the column names

#write.csv(BIEN_traitdata, file="BIEN_traitdata.csv")


