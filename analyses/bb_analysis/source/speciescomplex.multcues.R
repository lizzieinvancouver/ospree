## Try and organize so useful for all models running
## Started by Dan B - 26 July 2017
## Edits by Cat C - 17 August 2017
### reworked by Dan 19 sept 2018 
## and again by Cat 4 October 2019 with new species list and new parameters

##### New requirments for species and complexes
## 1. all three cues were manipulated in one study (i.e., two levels) - applies to species
## 2. all three cues were manipulated somehow across >1 study (i.e., each study used must have two levels of at least one cue) - applies to species and complexes


######## USE THIS SECTION TO CHECK CODE! ##########
## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!
## (1) Get the data and slim down to correct response and no NAs ...
#source("source/bbdataplease.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
#source("source/othertreats.R")
#dim(bb.noNA)
#bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 28 March 2018 should delete about 359 rows
#dim(bb.noNA)
## (3) Deal with species
#d <- bb.noNA

sppcomplexfx.multcue <- function(d){

#d <- read.csv("~/Documents/git/ospree/analyses/output/ospree_clean_withchill_BB.csv")
d$name<-paste(d$genus,d$species,sep="_") ###make  a column for genus_species

xx<-d
### make a list of which studies manipulate what.
xx <- within(xx, { prov.lat <- ave(provenance.lat, datasetID, name, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ave(fieldsample.date, datasetID, name, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, datasetID, name, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, datasetID, name, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chilltemp <- ave(chilltemp, datasetID, name, FUN=function(x) length(unique(x)))}) # mult expchill
xx <- within(xx, { chilltime <- ifelse(chilldays!=0, ave(chilldays, datasetID, name, FUN=function(x) length(unique(x))), 0)}) # mult studychill
#xx <- within(xx, { prov.long <- ave(provenance.long,name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
#xx <- within(xx, { datasets <- ave(datasetID, name, FUN=function(x) length(unique(x)))}) 

xx<-dplyr::select(xx, name, genus, species, force, photo, chilltemp, chilltime, field.sample, datasetID)
xx<-xx[!duplicated(xx),]


###make object with all acceptable (>1 levels of each cue manipulated for each species)
### This make a data sheet with all the complexes that can be indivudal species
xx$force<-ifelse(xx$force<=1, 0, 1) ### 1 means there was one level of forcing in the study, we need multiple levels for each cue
xx$photo<-ifelse(xx$photo<=1, 0, 1)
xx$chilltemp<-ifelse(xx$chilltemp<=1, 0, 1) 
xx$chilltime <- ifelse(is.na(xx$chilltime), 0, xx$chilltime)
xx$chilltime<-ifelse(xx$chilltime<=1, 0, 1) ## Different methods of manipulating chilling
xx$field.sample<-ifelse(xx$field.sample<=1, 0, 1) ## Different methods of manipulating chilling
xx$chill<-ifelse(xx$chilltemp==1 | xx$chilltime==1 | xx$field.sample==1,1, 0)
xx$numcues<-xx$force + xx$photo + xx$chill ## Determine how many cues were manipulated in each study

#check<-subset(xx, select=c(name, datasetID, datasets, numcues))

xx$numcues <- ifelse(is.na(xx$numcues), 0, xx$numcues)

### 1. all three cues were manipulated in one study (i.e., two levels) - from issue #308 https://github.com/lizzieinvancouver/ospree/issues/308
accept<-xx[(xx$numcues==3),] ## if species were in at least one dataset that manipulated all three cues
species4taxon<-c(accept$name) ## make a list of species using above requirements
accept$complex<-accept$name
accept$use<-"Y"

accept <- subset(accept, select=c("name", "genus", "species", "force", "photo", "chill", "numcues", "complex", "use"))

###accept is a list of species that are good to go

##integrates with rest of data
taxon<-dplyr::filter(d, name %in% species4taxon)
taxon$complex<-taxon$name
taxon$use<-"Y"

### List of species to accept:***** TO BE DELETED ONCE CODE IS CHECKED!!! *******
#[1] "Acer_pensylvanicum"     "Acer_pseudoplatanus"    "Acer_rubrum"            "Acer_saccharum"        
#[5] "Alnus_glutinosa"        "Alnus_incana"           "Alnus_rubra"            "Betula_alleghaniensis" 
#[9] "Betula_papyrifera"      "Betula_pendula"         "Betula_pubescens"       "Cornus_cornuta"        
#[13] "Fagus_grandifolia"      "Fagus_sylvatica"        "Fraxinus_nigra"         "Ilex_mucronata"        
#[17] "Kalmia_angustifolia"    "Larix_decidua"          "Lonicera_canadensis"    "Malus_domestica"       
#[21] "Picea_abies"            "Pieris_japonica"        "Populus_grandidentata"  "Populus_tremula"       
#[25] "Prunus_padus"           "Prunus_pensylvanica"    "Quercus_petraea"        "Quercus_rubra"         
#[29] "Ribes_nigrum"           "Salix_smithiana"        "Spirea_alba"            "Tilia_cordata"         
#[33] "Ulmus_minor"            "Ulmus_parvifolia"       "Ulmus_pumila"           "Ulmus_villosa"         
#[37] "Vaccinium_myrtilloides" "Viburnum_cassinoides"   "Viburnum_lantanoides"   "Vitis_vinifera"


#### 2. all three cues were manipulated somehow across >1 study (i.e., each study used must have two levels of at least one cue) - for species and complexes
#### Let's start with species that fulfill this requirement... 

sppmultstudies <- xx[!(xx$name%in%species4taxon),]
sppmultstudies <- subset(sppmultstudies, select=c("name", "genus", "species", "force", "photo", "chill"))

xxsppmult <- within(sppmultstudies, { forcecue <- ave(force, name, FUN=function(x) sum(unique(x)))}) # where forcing was manipulated across more than one level for each species
xxsppmult <- within(xxsppmult, { photocue <- ave(photo, name, FUN=function(x) sum(unique(x)))}) # where photo was manipulated across more than one level for each species
xxsppmult <- within(xxsppmult, { chillcue <- ave(chill, name, FUN=function(x) sum(unique(x)))})# where chill was manipulated across more than one level for each species

## If at least one study had multiple levels for each cue across species:
xxsppmult$force<-ifelse(xxsppmult$forcecue<1, 0, 1) 
xxsppmult$photo<-ifelse(xxsppmult$photocue<1, 0, 1)
xxsppmult$chill<-ifelse(xxsppmult$chillcue<1, 0, 1)


## To see if all cues were covered across all the possible datasets:
xxsppmult$numcues <- xxsppmult$force + xxsppmult$photo + xxsppmult$chill

xxsppmult$numcues <- ifelse(is.na(xxsppmult$numcues), 0, xxsppmult$numcues)

acceptsppmult<-xxsppmult[(xxsppmult$numcues==3),] ## if species had all three cues manipulated across multiple studies
speciesmult4taxon<-c(acceptsppmult$name) ## make a list of species using above requirements
acceptsppmult$complex<-acceptsppmult$name
acceptsppmult$use<-"Y"

acceptsppmult <- subset(acceptsppmult, select=c("name", "genus", "species", "force", "photo", "chill", "numcues", "complex", "use"))

### List of additional species that manipulate all three cues across multiple studies
### ***** TO BE DELETED ONCE CODE IS CHECKED!!! *******
#[1] "Abies_alba"             "Aesculus_hippocastanum" "Aronia_melanocarpa"     "Betula_lenta"          
#[5] "Corylus_avellana"       "Fraxinus_excelsior"     "Picea_glauca"           "Prunus_avium"          
#[9] "Prunus_persica"         "Pseudotsuga_menziesii"  "Pyrus_pyrifolia"        "Quercus_robur"         
#[13] "Sorbus_aucuparia"       "Sorbus_commixta"        "Syringa_vulgaris"

intersect(species4taxon,speciesmult4taxon) #checks to make sure there are no species that over lap between this and above

###accept is a list of species that are good to go

##integrates with rest of data
taxonmult<-dplyr::filter(d, name %in% speciesmult4taxon)
taxonmult$complex<-taxonmult$name
taxonmult$use<-"Y"



#### Okay nearly there now...
## Again: 2. all three cues were manipulated somehow across >1 study (i.e., each study used must have two levels of at least one cue) - for species and complexes
#### Just need to do the complexes now...

allspp <- c(species4taxon, speciesmult4taxon)

comp <- xx[!(xx$name%in%allspp),]
#comp$species <- gsub(".*_", "", comp$name)
comp$name <- paste(comp$genus, "complex", sep="_")

comp <- subset(comp, select=c("name", "genus", "species", "force", "photo", "chill"))

xxcomp <- within(comp, { forcecue <- ave(force, name, FUN=function(x) sum(unique(x)))}) # mult forcetemp across mult datasets for each genus, excluding species already found to use
xxcomp <- within(xxcomp, { photocue <- ave(photo, name, FUN=function(x) sum(unique(x)))})
xxcomp <- within(xxcomp, { chillcue <- ave(chill, name, FUN=function(x) sum(unique(x)))})

xxcomp$force<-ifelse(xxcomp$forcecue<1, 0, 1)
xxcomp$photo<-ifelse(xxcomp$photocue<1, 0, 1)
xxcomp$chill<-ifelse(xxcomp$chillcue<1, 0, 1)

xxcomp$numcues <- xxcomp$force + xxcomp$photo + xxcomp$chill

xxcomp$numcues <- ifelse(is.na(xxcomp$numcues), 0, xxcomp$numcues)

acceptcomp<-xxcomp[(xxcomp$numcues==3),] ## if genus had all three cues manipulated across multiple studies
complex4taxon<-c(acceptcomp$name) ## make a list of species using above requirements
acceptcomp$complex<-acceptcomp$name
acceptcomp$use<-"Y"

acceptcomp <- subset(acceptcomp, select=c("name", "genus", "force", "species", "photo", "chill", "numcues", "complex", "use"))

### Complexes being added in ***** TO BE DELETED ONCE CODE IS CHECKED!!! *******
#[1] "Betula_complex"       "Hamamelis_complex"    "Juglans_complex"      "Pinus_complex"       
#[5] "Prunus_complex"       "Pyrus_complex"        "Quercus_complex"      "Rhamnus_complex"     
#[9] "Rhododendron_complex" "Sorbus_complex" 


intersect(allspp,complex4taxon) #checks to make sure there are no species that over lap between this and above

### complete list of species and complexes
complexlist<-rbind(accept, acceptsppmult, acceptcomp)

#unique(complexlist$complex)
uselist<-filter(complexlist,use=="Y")
#unique(uselist$complex)

## Brings all the accepted species and accepted complexes together
accepties<-rbind(accept, acceptsppmult, acceptcomp)
#accepties$species<-gsub(".*_", "", accepties$name)
accepties<-subset(accepties, select=c(genus, species, complex, use))
accepties<-accepties[!duplicated(accepties),]

dim(d)
bb.wtaxa<-left_join(d, accepties) 
dim(bb.wtaxa) # gaining rows here, which is bad (9 rows I think)
bb.wtaxa<-dplyr::select(bb.wtaxa, -name)
bb.wtaxa$use<-ifelse(is.na(bb.wtaxa$use), "N", bb.wtaxa$use)

sort(unique(bb.wtaxa$complex[bb.wtaxa$use=="Y"]))

bb.all.wtaxa <- bb.wtaxa[(bb.wtaxa$use=="Y"),]
bb.all.wtaxa$use <- NULL
bb.noNA.wtaxa <- bb.all.wtaxa
bb.noNA.wtaxa$complex.wname <- bb.noNA.wtaxa$complex
bb.noNA.wtaxa$complex <- as.numeric(as.factor(bb.noNA.wtaxa$complex))


return(bb.noNA.wtaxa)
#write.csv(bb.wtaxa, file="..//output/ospree_clean_withchill_BB_taxon.csv", row.names = FALSE)

#write.csv(uselist, file="~/Documents/git/ospree/analyses/output/speciescomplex.list.csv", row.names=FALSE)

}


