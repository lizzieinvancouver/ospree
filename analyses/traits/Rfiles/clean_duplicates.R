# Looking for duplicated data in the traits data

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(stringr)
library(plyr)
library(dplyr)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/ospree_trait_analysis/")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

## reading in data
#setwd("~/GitHub/ospree/analyses/output/")

d <- read.csv("input/try_bien.csv") 

refs <- aggregate(d["SpeciesName"], d[c("Reference", "Reference...source","database")], FUN=length) 
#There are abou 38 datasets that might be duplicated

head(d)
sort(unique(d$Reference...source))

# Geoff pointed out that using 4 characters is too few, since it causes issues with for example "Camp" becomes "Campb" and "Campe","Chen" becomes "Chen " and "Cheng", so we will try 5   
# there are some irregular references that also need fixing:
# Feng and Feng_
d$Reference...source[d$Reference...source== "Feng_etal_Halle"] <- "Feng et al Halle"
#Guilia
d$Reference...source[d$Reference...source== "Gul\xedas et al 2002"] <- "Gulias et al 2002"
d$Reference...source[d$Reference...source== "Gul\xedas et al 2003"] <- "Gulias et al 2003"
#Kudo
d$Reference...source[d$Reference...source== "KudoCorn"] <- "Kudo_Cornelissen"
d$Reference...source[d$Reference...source== "Kudo9"] <- "Kudo_9"
d$Reference...source[d$Reference...source== "\xd6sterdahl Sofia(2003): V\xe4xtsamh\xe4llen p\xe5 fj\xe4llr\xe4vslyor iett subarktiskt/alpint landskap / (Plant comm"] <- "Kleyer"
d$Reference...source[d$Reference...source== "Ant\xfanez Isabel(2001): Relative growth rate in phylogenetically related deciduous and evergreen ..."] <- "Kleyer"
d$Reference...source[d$Reference...source== "Kr\xf6ber et al, 2012"] <- "Kleyer"

#######################################################################################################################
######### Subset with 5 characters ####################################################################################
d$refabr5<-strtrim(d$Reference...source,5);head(d); #since the references are often not written in the same format, I am creating a new variable of just the first four letters
sort(unique(d$refabr5))
## select target variables for which we will search for duplicates:
tar.var5<-c("SpeciesName","TraitName","UnitName","refabr5", "Latitude","Longitude","Reference","project_pi")
resp.var<-c("TraitValue")

## subset data to look for duplicates (resp.var are included or most of the subset is duplicated)
trt.sub5<-d[,c(tar.var5,resp.var)]
head(trt.sub5)
## remove duplicated rows in a simple way
trt.sub.no.dup5<-d[!duplicated(trt.sub5),]

dim(trt.sub5)
dim(trt.sub.no.dup5) 
# with 5 characters, and without including project_pi and lat/long we are left with only 28578 rows of data, deleting 1233622, only 2% of the data left
# with 5 characters, and with including reference and lat/long we are left with only 823610 rows of data,  ~65% of the data kept
# with 5 characters, and with including project_pi,reference and lat/long we are left with only 823754 rows of data,  ~65% of the data kept

sort(unique(trt.sub.no.dup5$refabr5))
length((unique(trt.sub.no.dup5$refabr5))) #197

refs <- aggregate(d["SpeciesName"], d[c("Reference", "Reference...source", "refabr5","database")], FUN=length) 
refs.nd <- aggregate(trt.sub.no.dup5["SpeciesName"], trt.sub.no.dup5[c("Reference", "Reference...source", "refabr5","database")], FUN=length) 

# Think it worked, but it would be useful to get someone else to do some double checks.
#write.csv(trt.sub.no.dup5, "try_bien_nodups.csv", row.names=FALSE)

#Let's take a closer look at the duplicated data

unique(trt.dup5$database) # removing data from both bien and try
dupbien<-subset(trt.dup5, database == "bien") # this is 33.9% of the full bien data that we started with
#How many studies have data lost -- 20 studies, 99.7% of the removed data is from the height dataset by greg reams
unique(dupbien$project_pi)
greams<-subset(dupbien, project_pi == "Greg Reams")


duptry <-subset(trt.dup5, database == "try") # this is 48.6% of the full try data that we started with
#How many studies have data lost -- 19 studies
length(unique(duptry$DatasetID)) 
length(unique(duptry$Reference...source)) 

#########################################################################################################################
# What are we gaining when we include lat long ref?
tar.var<-c("SpeciesName","TraitName","UnitName","refabr5")
resp.var<-c("TraitValue")

## subset data to look for duplicates (resp.var are included or most of the subset is duplicated)
trt.sub<-d[,c(tar.var,resp.var)]
head(trt.sub)
## remove duplicated rows in a simple way
trt.sub.no.dup<-d[!duplicated(trt.sub),]

dim(trt.sub5)
dim(trt.sub.no.dup5) 
dim(trt.sub.no.dup) 

length(unique(trt.sub.no.dup$Latitude))
length(unique(trt.sub.no.dup5$Latitude))

length(unique(trt.sub.no.dup$Longitude))
length(unique(trt.sub.no.dup5$Longitude))

length(unique(trt.sub.no.dup$refabr5))
length(unique(trt.sub.no.dup5$refabr5))
#########################################################################################################################
# Code that was used to verify that neither 4 (too short) or 6 (too long) were appropraite numbers of characters to be using
######### Subset with 4 characters ####################################################################################
d$refabr4<-strtrim(d$Reference...source,4);head(d); #since the references are often not written in the same format, I am creating a new variable of just the first four letters

## select target variables for which we will search for duplicates:
tar.var4<-c("SpeciesName","TraitName","UnitName","refabr4")
resp.var<-c("TraitValue")

## subset data to look for duplicates (resp.var are included or most of the subset is duplicated)
trt.sub4<-d[,c(tar.var4,resp.var)]
head(trt.sub4)
## remove duplicated rows in a simple way
trt.sub.no.dup4<-d[!duplicated(trt.sub4),]

dim(trt.sub4)
dim(trt.sub.no.dup4) 
# with 4 characters we are left with only 28578 rows of data, deleting 1233622

sort(unique(trt.sub.no.dup4$refabr4))
length((unique(trt.sub.no.dup4$refabr4))) #190


######### Subset with 6 characters ####################################################################################
d$refabr6<-strtrim(d$Reference...source,6);head(d); #since the references are often not written in the same format, I am creating a new variable of just the first four letters

## select target variables for which we will search for duplicates:
tar.var6<-c("SpeciesName","TraitName","UnitName","refabr6")
resp.var<-c("TraitValue")

## subset data to look for duplicates (resp.var are included or most of the subset is duplicated)
trt.sub6<-d[,c(tar.var6,resp.var)]
head(trt.sub6)
## remove duplicated rows in a simple way
trt.sub.no.dup6<-d[!duplicated(trt.sub6),]

dim(trt.sub6)
dim(trt.sub.no.dup6) 
# with 6 characters we are left with only 28610 rows of data, deleting 1233590

sort(unique(trt.sub.no.dup6$refabr6))
length((unique(trt.sub.no.dup6$refabr6)))
#######################################################################################################################
# There are also several references that due to their irregular formatting might be an issue:

# Feng & Feng_
#Kudo_ Kudo9 KudoC
kudo <- subset(trt.sub.no.dup6, refabr4 == "Kudo") # Kudo_ and KudoC are duplicated data, but have different Project_pi
#Niine, NiinE, NiinT
niin <- subset(trt.sub.no.dup6, refabr4 == "Niin") # no duplicated data, all different
#chen e & Chen Z
chen <- subset(trt.sub.no.dup6, refabr4 == "Chen") # no duplicated data, all different
#Reich & Reiche
reich <- subset(trt.sub.no.dup6, refabr4 == "Reic") #this data is duplicated 2-3 times, Reich is the right unique identifier
#Koba I, T ,J
koba <- subset(trt.sub.no.dup6, refabr4 == "Koba") # no duplicated data, all different
#unpublished
unpub <- subset(trt.sub.no.dup6, refabr4 == "unpu") # no obviously duplicated data, appears to be all different



# Don't seem to have lost anything unique
length(unique(d$SpeciesName)); length(unique(trt.sub.no.dup5$SpeciesName))
length(unique(d$TraitName)); length(unique(trt.sub.no.dup5$TraitName))
length(unique(d$TraitValue)); length(unique(trt.sub.no.dup5$TraitValue))

unique(trt.sub.no.dup$TraitName)

ssd<-subset(trt.sub.no.dup, TraitName == "Stem_specific_density"); hist (ssd$TraitValue)
ht<-subset(trt.sub.no.dup, TraitName == "Plant_height_vegetative"); hist (ht$TraitValue)
sla<-subset(trt.sub.no.dup, TraitName == "Specific_leaf_area"); hist (sla$TraitValue)
sd<-subset(trt.sub.no.dup, TraitName == "Stem_diameter"); hist (sd$TraitValue)
seed<-subset(trt.sub.no.dup, TraitName == "seed mass"); hist (seed$TraitValue)
lnc<-subset(trt.sub.no.dup, TraitName == "Leaf_nitrogen_.N._content_per_leaf_dry_mass" & TraitValue<800); hist (lnc$TraitValue)
ldmc<-subset(trt.sub.no.dup, TraitName == "Leaf_dry_matter_content"); hist (ldmc$TraitValue)
lcc<-subset(trt.sub.no.dup, TraitName == "Leaf_carbon_.C._content_per_leaf_dry_mass"); hist (lcc$TraitValue)
photo<-subset(trt.sub.no.dup, TraitName == "Leaf_photosynthesis_rate_per_leaf_area"); hist (photo$TraitValue)
life<-subset(trt.sub.no.dup, TraitName == "leaf life span"); hist (life$TraitValue)

