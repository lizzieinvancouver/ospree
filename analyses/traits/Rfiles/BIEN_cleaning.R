### Started January 8 2019 ###

## DL getting to know the BIEN Data ##

#The aim of this code is to better understand the scope and completeness of the BIEN data downloaded in December 2018##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

biendat<-read.csv("input/newspp_BIEN_traitdata_Nov11.csv", header=TRUE)

#biendattemp<-read.csv("input/Phylospp_BIEN_traitdata.csv", header=TRUE)
biendat<-biendat[,3:15] #remove the first col that is just numbers
names(biendat)[1]<-"SpeciesName"
head(biendat)

unique(biendat$trait_name) # 13 traits, but might not want leaf dry mass, leaf area, whole plant growth form, whole plant woodiness, or leaf lifespan, or flowr pollination syndrome 
 
# What traits are worth keeping?
#Delete:
pollsyn<-subset(biendat, trait_name=="flower pollination syndrome") #only six rows of data for six species all "biotic"

plntwood<-subset(biendat, trait_name=="whole plant woodiness") #...this is just the word woody

seed<-subset(biendat, trait_name=="seed mass") #...this is just the word woody

# Keeping
life<-subset(biendat, trait_name=="leaf life span") #247 rows
unique(life$SpeciesName) # wow 27 species!

plntgrwth<-subset(biendat, trait_name=="whole plant growth form") # I like this as a covariate, but not as a "trait"
unique(plntgrwth$SpeciesName) #we ahve it for 90 species
unique(plntgrwth$trait_value)
herby<-subset(plntgrwth, trait_value=="Herb") # this is wrong, its a betula and fagus not a herb
woody<-subset(plntgrwth, trait_value=="woody") # this is wrong, its a betula and fagus not a herb

###################################################
# Excluding the traits that are not useful
biendat.subtrait<-subset(biendat, trait_name == "leaf area per leaf dry mass" | trait_name == "leaf carbon content per leaf nitrogen content" | trait_name == "leaf dry mass per leaf fresh mass" | trait_name == "maximum whole plant height" | trait_name == "stem wood density" | trait_name == "whole plant height" | trait_name == "leaf life span"| trait_name == "seed mass") 

unique(biendat.subtrait$trait_name)

# the trait values are not numberic because of the woodiness
biendat.subtrait$trait_value<-as.numeric(biendat.subtrait$trait_value)

##################################################
# Checking species names for spelling mistakes
sort(unique(biendat.subtrait$SpeciesName)) # does not appear to be any typos

# break up name column into genus, species, extra stuff 
breakname <- strsplit(as.character(biendat.subtrait$SpeciesName), " ", fixed=TRUE)
biendat.subtrait$genus <- unlist(lapply(breakname, function(x) x[1]))
biendat.subtrait$species <- unlist(lapply(breakname, function(x) x[2]))
head(biendat.subtrait)

# this dataset has pear! 


##################################################
# Standardize the units and thinking about 

# start with leaf area per leaf dry mass (ie SLA)
#despite the units be so different, I think they are equivalent 1m^2/kg = 1000000 mm^2/ 1000000 mg
sla<-subset(biendat.subtrait, trait_name == "leaf area per leaf dry mass" )
unique(sla$unit)
range(sla$trait_value)
hist(sla$trait_value)

temp<-sla[order(sla$trait_value),]

# ldmc
ldmc<-subset(biendat.subtrait, trait_name == "leaf dry mass per leaf fresh mass")
unique(ldmc$unit)

# seed mass
seed<-subset(biendat.subtrait, trait_name == "seed mass")
hist(seed$trait_value)

# height
maxht<-subset(biendat.subtrait, trait_name == "maximum whole plant height")
hist(maxht$trait_value)

wlht<-subset(biendat.subtrait, trait_name == "whole plant height")
hist(wlht$trait_value)
temp<-wlht[order(wlht$trait_value),]


# Darwin and I agree that the simplest way to convert it might just to make it wide again and then long again
library(tidyr)
biendat.subtrait$trait_value<-as.numeric(biendat.subtrait$trait_value)
bien_wide<-spread(biendat.subtrait, key=c("trait_name"), value="trait_value")
head(bien_wide)

require(reshape2)
bien_w<-dcast(biendat.subtrait, SpeciesName + url_source + id  ~ trait_name, value.var= "trait_value", fun.aggregate = NULL)
?dcast

#this is a crude way of seeing what units there are for a trait
library(dplyr)
table<- biendat.subtrait %>%
  group_by(trait_name,unit) %>%
  summarise(mtrait = mean(trait_value))
table

try.tbl<- trydat %>%
  group_by(Traits,UnitName) %>%
  summarise(mtrait = mean(TraitValue_std))
try.tbl

#### Merge bien with the ospree model output #######
ospree<-read.csv("input/traitors_bb_results.csv")
head(ospree)

breakname <- strsplit(as.character(ospree$Species), "_", fixed=TRUE)
ospree$genus <- unlist(lapply(breakname, function(x) x[1]))
ospree$species <- unlist(lapply(breakname, function(x) x[2]))
ospree$ospree.sp<-paste(ospree$genus, ospree$species, sep=" ")
head(ospree)


# Removing the gymnosperm from the ospree model data 
gymno<-c("Abies","Pinus","Picea","Pseudotsuga")

ospree.angio <- ospree[!ospree$genus %in% gymno,]
unique(ospree.angio$Species)

# I think for now, we could remove the complexes
cmplx<-"complex"
ospree.angio.nocmplx <- ospree.angio[!ospree$species %in% cmplx,]
unique(ospree.angio$Species)

## Create table of known pairwise interactions
ids.list <- unique(ospree.angio$ospree.sp)
stor <- vector()
for(i in 1:length(ids.list)){
  temp <- subset(ospree.angio, ospree.sp == ids.list[i]) #creates new subset for every pair
}

## Weird datasets to be aware of ##################
 
# 1. Greg Reams height data:  3019683 ie 96% of the Bien data 
reams<-subset(dat, project_pi == "Greg Reams")
unique(reams$SpeciesName) # 35 species
unique(reams$trait_name) # just whole plant height 

length(unique(dat$url_source))