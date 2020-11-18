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

trybien<-read.csv("input/try_bien_Nov2020.csv")

all<-read.csv("input/try_bien_ospree_Nov2020.csv")

bien<-read.csv("input/bien_cleaned_Nov2020.csv")
try<-read.csv("input/try_subsptraits.csv")
unique(trybien$UnitName)
unique(trybien$TraitName)

#sla
sla<-subset(trybien, TraitName=="Specific_leaf_area")
hist(sla$TraitValue)
range(sla$TraitValue)
temp<-sla[order(sla$TraitValue),]

ggplot(sla, aes(TraitValue,database)) +
  geom_point()
# should just subset the 0 value

slatry<-subset(try, TraitName=="Specific_leaf_area"); unique(slatry$UnitName)
slabien<-subset(bien, trait_name=="leaf area per leaf dry mass"); unique(slabien$unit)
head(bien)

#####################################################################

#ldmc
ldmc<-subset(trybien, TraitName=="Leaf_dry_matter_content")
hist(ldmc$TraitValue)

ggplot(ldmc, aes(TraitValue,database)) +
  geom_point()

ldmctry<-subset(try, TraitName=="Leaf_dry_matter_content"); unique(ldmctry$UnitName)
ldmcbien<-subset(bien, trait_name=="leaf dry mass per leaf fresh mass"); unique(ldmcbien$unit)
head(bien)

#lnc
lnc<-subset(trybien, TraitName=="Leaf_nitrogen_.N._content_per_leaf_dry_mass")
hist(lnc$TraitValue)

ggplot(lnc, aes(TraitValue,database)) +
  geom_point()
#one exceedingly large value over 800

range(lnc$TraitValue)
temp<-lnc[order(lnc$TraitValue),]

#lnc
cn<-subset(trybien, TraitName=="leaf carbon content per leaf nitrogen content")
hist(cn$TraitValue)

ggplot(cn, aes(TraitValue,database)) +
  geom_point()
#one exceedingly large value over 30
temp<-cn[order(cn$TraitValue),]


#phr
phr<-subset(trybien, TraitName=="Leaf_photosynthesis_rate_per_leaf_area")
hist(phr$TraitValue)

ggplot(phr, aes(TraitValue,database)) +
  geom_point()

#ssd
ssd<-subset(trybien, TraitName=="Stem_specific_density")
hist(ssd$TraitValue)

ggplot(ssd, aes(TraitValue,database)) +
  geom_point()

#diam
diam<-subset(trybien, TraitName=="Stem_diameter")
hist(diam$TraitValue)

ggplot(diam, aes(TraitValue,database)) +
  geom_point()

range(diam$TraitValue)
temp<-diam[order(diam$TraitValue),]
# again we have some werid zero values and I am a little suspicious of the Betula that have stem diameters of less than one cm -- could these be juveniles or seedlings? I think it would be fair to subset them out

#veg ht
ht<-subset(trybien, TraitName=="Plant_height_vegetative")
hist(ht$TraitValue)
range(ht$TraitValue)

ggplot(ht, aes(TraitValue,database)) +
  geom_point()
#subset out the zero values

head(tryosp)
chill<-subset(all, Coefficient=="b_chill")
force<-subset(all, Coefficient=="b_force")
photo<-subset(all, Coefficient=="b_photo")


