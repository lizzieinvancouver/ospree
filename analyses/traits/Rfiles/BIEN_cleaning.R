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
gymno<-c("Abies","Pinus","Picea","Pseudotsuga")

biendat.subtrait.deci<- biendat.subtrait[!biendat.subtrait$genus %in% gymno,] # only want the deciduous species
biendat.subtrait.deci$new.SpeciesName<-paste(biendat.subtrait.deci$genus,biendat.subtrait.deci$species,sep="_")
unique(biendat.subtrait.deci$SpeciesName)
##################################################
# Standardize the units and thinking about 

# start with leaf area per leaf dry mass (ie SLA)
#despite the units be so different, I think they are equivalent 1m^2/kg = 1000000 mm^2/ 1000000 mg
#changing sla set
sla<-subset(biendat.subtrait, trait_name == "leaf area per leaf dry mass" )
sla$trait_value <- (sla$trait_value * 1000000 )
sla$unit <- "m2.mg-1"
unique(sla$unit)
range(sla$trait_value)
hist(sla$trait_value)

temp<-sla[order(sla$trait_value),]

# ldmc
ldmc<-subset(biendat.subtrait, trait_name == "leaf dry mass per leaf fresh mass")
unique(ldmc$unit) #correct unit based on Pérez-Harguindeguy

# seed mass
seed<-subset(biendat.subtrait, trait_name == "seed mass")
unique(seed$unit) #correct unit based on Pérez-Harguindeguy
hist(seed$trait_value)

# height
maxht<-subset(biendat.subtrait, trait_name == "maximum whole plant height")
unique(maxht$unit) #correct unit based on Pérez-Harguindeguy
hist(maxht$trait_value)

wlht<-subset(biendat.subtrait, trait_name == "whole plant height")
unique(wlht$unit) #correct unit based on Pérez-Harguindeguy
hist(wlht$trait_value)
temp<-wlht[order(wlht$trait_value),]

######
##### Merging datasets together
#creates dataset with all relevant data not including SLA
test <- subset(biendat.subtrait, trait_name != "leaf area per leaf dry mass" )

#rowbinds the new SLA values with the rest of the data we are interested in
biendat2.0<- rbind(test,sla)

write.csv(biendat2.0, "bien_cleaned_Nov2020.csv")

## Weird datasets to be aware of ##################
 
# 1. Greg Reams height data:  3019683 ie 96% of the Bien data 
reams<-subset(dat, project_pi == "Greg Reams")
unique(reams$SpeciesName) # 35 species
unique(reams$trait_name) # just whole plant height 

length(unique(dat$url_source))