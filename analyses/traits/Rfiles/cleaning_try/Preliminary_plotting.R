# Code started Nov 5, 2020 at the OSPREE retreat

# This code is to help us visualize the data and look for outliers
rm(list = ls())
## No one likes factors
options(stringsAsFactors = FALSE)


if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

require(ggplot2)
trydat<-read.csv("input/try_subsptraits.csv")
head(trydat)

tryosp<-read.csv("input/try_ospree.csv")

unique(trydat$UnitName)
unique(trydat$TraitName)

#sla
sla<-subset(trydat, TraitName=="Specific_leaf_area")
hist(sla$StdValue)
range(sla$StdValue)
temp<-sla[order(sla$StdValue),]

# should just subset the 0 value
#####################################################################

#ldmc
ldmc<-subset(trydat, TraitName=="Leaf_dry_matter_content")
hist(ldmc$StdValue)

#lnc
lnc<-subset(trydat, TraitName=="Leaf_nitrogen_.N._content_per_leaf_dry_mass")
hist(lnc$StdValue)

range(lnc$StdValue)
temp<-lnc[order(lnc$StdValue),]


#phr
phr<-subset(trydat, TraitName=="Leaf_photosynthesis_rate_per_leaf_area")
hist(phr$StdValue)

#ssd
ssd<-subset(trydat, TraitName=="Stem_specific_density")
hist(ssd$StdValue)

#diam
diam<-subset(trydat, TraitName=="Stem_diameter")
hist(diam$StdValue)

range(diam$StdValue)
temp<-diam[order(diam$StdValue),]
# again we have some werid zero values and I am a little suspicious of the Betula that have stem diameters of less than one cm -- could these be juveniles or seedlings? I think it would be fair to subset them out

#veg ht
ht<-subset(trydat, TraitName=="Plant_height_vegetative")
hist(ht$StdValue)
range(ht$StdValue)
#subset out the zero values

head(tryosp)
chill<-subset(tryosp, Coefficient=="b_chill")
force<-subset(tryosp, Coefficient=="b_force")
photo<-subset(tryosp, Coefficient=="b_photo")

ggplot(data=chill,aes(mean,trait.mean)) +
  geom_point()
