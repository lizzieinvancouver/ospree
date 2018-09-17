## Try and organize so useful for all models running
## Started by Dan B - 26 July 2017
## Edits by Cat C - 17 August 2017
### reworked by Dan 19 sept 2018
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

d<-read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

library(tidyverse)

d$name<-paste(d$genus,d$species,sep="_") ###make  a column for genus species

xx<-d
### make a list of which studies manipulate what.
xx <- within(xx, { prov.lat <- ave(provenance.lat, name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ave(fieldsample.date, name, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, name, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, name, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chill <- ave(chilltemp, name, species, FUN=function(x) length(unique(x)))}) # mult expchill
xx <- within(xx, { spp <- ave(species, name, FUN=function(x) length(unique(x)))}) # mult species
xx <- within(xx, { prov.long <- ave(provenance.long,name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
xx <- within(xx, { datasets <- ave(datasetID, name, species, FUN=function(x) length(unique(x)))}) 

xx<-dplyr::select(xx,name,genus, datasets, force, photo, chill,field.sample,prov.lat)
xx<-xx[!duplicated(xx),]

#write.csv(xx, file="~/Documents/git/ospree/analyses/output/species_manipulation_levels.csv", row.names = FALSE)



###make object with all acceptable (<1 data set species) This make a data sheet with all the complex that can be indivudal species
accept<-dplyr::filter(xx,datasets>1)
species4taxon<-c(accept$name) ## make a list of species with more than 1 study study
accept$complex<-accept$name
accept$use<-"Y"

###accept is a list of species that are good to go

##integrates with rest of data
taxon<-dplyr::filter(d, name %in% species4taxon)
taxon$complex<-taxon$name
taxon$use<-"Y"

###making complexes#######################
comp<-dplyr::filter(xx,datasets==1) ## this are the singleton species
complex4taxon<-c(comp$name) ### make a liust of them


intersect(species4taxon,complex4taxon) #checks to make sure there are no species that over lap between this and above

###building complexes
taxon2<- dplyr::filter(d, name %in% complex4taxon) ###This filters main data sheet for rows that have species with only 1 dataset ID


taxon2<- within(taxon2, {datasets<- ave(datasetID, genus, FUN=function(x) length(unique(x)))})
taxon2<-dplyr::arrange(taxon2, genus)

taxon2$complex<-paste(taxon2$genus, "complex", sep="_")
taxon2 <- within(taxon2, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
taxon2 <- within(taxon2, { field.sample <- ave(fieldsample.date, complex, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
taxon2 <- within(taxon2, { force <- ave(forcetemp, complex, FUN=function(x) length(unique(x)))}) # mult forcetemp
taxon2 <- within(taxon2, { photo <- ave(photoperiod_day, complex, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
taxon2 <- within(taxon2, { chill <- ave(chilltemp, complex, FUN=function(x) length(unique(x)))}) # mult expchill
taxon2<- within(taxon2, { prov.long <- ave(provenance.long,complex, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
taxon2<-dplyr::select(taxon2,name,genus, datasets, force, photo, chill,field.sample,prov.lat, complex,datasetID)
taxon2<-taxon2[!duplicated(taxon2),]

#taxon2<- within(taxon2, {datasets.complex<- ave(complex, FUN=function(x) length(unique(x)))}) ###this function is wrong!
guppy<-as.data.frame<-count(taxon2,complex) #how many complexes have multiple species? ###THis is the part thats sorta broken, Cat will make a sweet loop maybe.
taxon2<-left_join(taxon2,guppy, by="complex") ### add this summary column to main data

accept.complex<-taxon2
accept.complex$use<-ifelse(accept.complex$n>1,"Y","N") ###this is your list of complexes

###makes a data sheet
complex<-dplyr::filter(d, name %in% complex4taxon)
complex$complex<-NA
complex$complex <- paste(complex$genus, "complex", sep="_")
complex$use<-"Y"


###if you want a data sheet to merge later in the work flow with working data sheet do this
accept$n<-accept$datasets
complexlist<-rbind(accept,accept.complex)
##Currently you have 2 datasets complex and taxon, that have a #complex data set.
setdiff(unique(d$name),unique(complexlist$name))

unique(complexlist$complex)
uselist<-filter(complexlist,n>1)
unique(uselist$complex)

###ignore what is beolow this

#### Check it to make sure.... ###This is a list from summer 2017. IF you do this now there should be fewer complexed because we've added more studies thoughcleaning.
#unique(accept.comp$complex)
###  [1] "Acer_complex"         "Betula_complex"       "Fraxinus_complex"     "Juglans_complex"      "Picea_complex"        "Pinus_complex"        "Populus_complex"      "Prunus_complex"      
#[9] "Pyrus_complex"        "Quercus_complex"      "Rhododendron_complex"      "Salix_complex"        "Sorbus_complex"       "Tilia_complex"        "Ulmus_complex"       
#[16] "Vaccinium_complex"   

##same note as above. Summer 2017
#sort(unique(goob$complex))
## [1] "Acer_complex"         "Betula_complex"       "Fraxinus_complex"     "Juglans_complex"      "Picea_complex"        "Pinus_complex"        "Populus_complex"      "Prunus_complex"      
#[9] "Pyrus_complex"        "Quercus_complex"      "Rhododendron_complex"         "Salix_complex"        "Sorbus_complex"       "Tilia_complex"        "Ulmus_complex"       
#[17] "Vaccinium_complex"   

#d$complex<-NA
#can.use<-c(goo$name, goob$name)
#d<-dplyr::filter(d, name %in% can.use)

#complexes<-ifelse(goob$name%in%goo$name, NA, goob$name)
#complexes<-na.omit(complexes)
#d$complex<-NA
#d$complex<-ifelse(d$name%in%complexes, paste(d$genus, "complex", sep="_"), d$name)
#d$use<-"Y"

#library(tidyverse)
### Check again....
#check.this<-d %>% dplyr::select(datasetID, genus, species, name, complex)
#check.this<-check.this[!duplicated(check.this),]

#sort(c(unique(goo$complex), unique(goob$complex)))
#  [1] "Acer_pseudoplatanus"     "Betula_pendula"          "Aesculus_hippocastanum"  "Syringa_vulgaris"        "Corylus_avellana"        "Fraxinus_excelsior"      "Fagus_sylvatica"        
#[8] "Picea_abies"             "Larix_decidua"           "Prunus_avium"            "Tilia_cordata"           "Sorbus_aucuparia"        "Abies_alba"              "Quercus_petraea"        
#[15] "Actinidia_deliciosa"     "Vitis_vinifera"          "Betula_pubescens"        "Quercus_rubra"           "Acer_saccharum"          "Betula_alleghaniensis"   "Pseudotsuga_menziesii"  
#[22] "Prunus_persica"          "Malus_domestica"         "Quercus_robur"           "Liquidambar_styraciflua" "Alnus_glutinosa"         "Sorbus_commixta"         "Ribes_nigrum"           
#[29] "Prunus_padus"            "Populus_tremula"         "Alnus_incana"            "Rubus_idaeus"            "Carpinus_betulus"        "Cornus_alba"             "Cornus_mas"             
#[36] "Robinia_pseudoacacia"    "Symphoricarpos_albus"    "Picea_glauca"            "Quercus_ilex"            "Pyrus_pyrifolia"         "Betula_nana"             "Quercus_faginea"        
#[43] "Tilia_complex"           "Acer_complex"            "Betula_complex"          "Fraxinus_complex"        "Picea_complex"           "Prunus_complex"          "Quercus_complex"        
#[50] "Sorbus_complex"          "Pinus_complex"           "Salix_complex"           "Juglans_complex"         "Rosa_complex"            "Ulmus_complex"           "Populus_complex"        
#[57] "Pyrus_complex"           "Rhododendron_complex"    "Vaccinium_complex"      


#sort(unique(d$complex))
#[1] "Tilia_complex"           "Acer_pseudoplatanus"     "Betula_pendula"          "Aesculus_hippocastanum"  "Syringa_vulgaris"        "Corylus_avellana"        "Fraxinus_excelsior"     
#[8] "Fagus_sylvatica"         "Picea_abies"             "Larix_decidua"           "Prunus_avium"            "Quercus_complex"         "Tilia_cordata"           "Sorbus_aucuparia"       
#[15] "Abies_alba"              "Quercus_petraea"         "Actinidia_deliciosa"     "Vitis_vinifera"          "Pinus_complex"           "Betula_pubescens"        "Salix_complex"          
#[22] "Quercus_rubra"           "Acer_saccharum"          "Betula_alleghaniensis"   "Pseudotsuga_menziesii"   "Picea_complex"           "Juglans_complex"         "Prunus_persica"         
#[29] "Malus_domestica"         "Rosa_complex"            "Quercus_robur"           "Betula_complex"          "Ulmus_complex"           "Liquidambar_styraciflua" "Populus_complex"        
#[36] "Alnus_glutinosa"         "Pyrus_complex"           "Prunus_complex"          "Sorbus_complex"          "Sorbus_commixta"         "Ribes_nigrum"            "Prunus_padus"           
#[43] "Populus_tremula"         "Alnus_incana"            "Rubus_idaeus"            "Carpinus_betulus"        "Acer_complex"            "Cornus_alba"             "Cornus_mas"             
#[50] "Fraxinus_complex"        "Robinia_pseudoacacia"    "Symphoricarpos_albus"    "Picea_glauca"            "Quercus_ilex"            "Pyrus_pyrifolia"         "Rhododendron_complex"   
#[57] "Betula_nana"             "Quercus_faginea"         "Vaccinium_complex" 

#write.csv(d, file="~/Documents/git/ospree/analyses/output/speciescomplex.list.csv", row.names=FALSE)
