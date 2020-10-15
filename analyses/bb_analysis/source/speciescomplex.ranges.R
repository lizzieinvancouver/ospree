## Try and organize so useful for all models running
## Started by Dan B - 26 July 2017
## Edits by Cat C - 17 August 2017
### reworked by Dan 19 sept 2018 
## and again by Cat 4 October 2019 with new species list and new parameters

##### New requirments for species and complexes
## 1. at least two cues were manipulated in one study (i.e., two levels) - applies to species
## 2. Study does not use ONLY experimental chilling


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

sppcomplexfx.ranges <- function(d){

#d <- read.csv("~/Documents/git/ospree/analyses/output/ospree_clean_withchill_BB.csv")
  #d <- bb.all
d$name<-paste(d$genus,d$species,sep="_") ###make  a column for genus_species
cropspp <- c("Actinidia_deliciosa", "Malus_domestica", "Malus_pumila", "Vitis_vinifera", "Ribes_nigrum", 
             "Vaccinium_ashei", "Vaccinium_corymbosum", "Prunus_persica", "Syringa_vulgaris", "Olea_europaea",
             "Cornus_alba", "Rubus_idaeus", "Pyrus_communis", "Pieris_japonica", "Pyrus_pyrifolia" )
d<-d[!(d$name%in%cropspp),]

xx<-d
### make a list of which studies manipulate what.
xx <- within(xx, { prov.lat <- ave(provenance.lat, name, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
xx <- within(xx, { field.sample <- ave(fieldsample.date, name, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
xx <- within(xx, { force <- ave(forcetemp, name, FUN=function(x) length(unique(x)))}) # mult forcetemp
xx <- within(xx, { photo <- ave(photoperiod_day, name, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
xx <- within(xx, { chilltemp <- ave(chilltemp, name, FUN=function(x) length(unique(x)))}) # mult expchill
xx <- within(xx, { chilltime <- ifelse(chilldays!=0, ave(chilldays, name, FUN=function(x) length(unique(x))), 0)}) # mult studychill
xx <- within(xx, { datasets <- ave(datasetID, name, FUN=function(x) length(unique(x)))}) # mult expchill

xx<-dplyr::select(xx, name, genus, species, datasetID, force, photo, chilltemp, chilltime, field.sample, datasets)
xx<-xx[!duplicated(xx),]


###make object with all acceptable (>1 levels of each cue manipulated for each species)
### This make a data sheet with all the complexes that can be indivudal species
xx$force<-ifelse(xx$force<=1, 0, 1) ### 1 means there was one level of forcing in the study, we need multiple levels for each cue
xx$photo<-ifelse(xx$photo<=1, 0, 1)
xx$chilltemp<-ifelse(xx$chilltemp<=1, 0, 1) 
xx$chilltime <- ifelse(is.na(xx$chilltime), 0, xx$chilltime)
xx$chilltime<-ifelse(xx$chilltime<=1, 0, 1) ## Different methods of manipulating chilling
xx$field.sample<-ifelse(xx$field.sample<=1, 0, 1) ## Different methods of manipulating chilling
xx$chill<-ifelse(xx$chilltemp==1 | xx$chilltime==1 | xx$field.sample==1, 1, 0) ### Do NOT include studies that ONLY use field chilling (xx$chilltemp==1 & xx$field.sample==1) |(xx$chilltime==1 & 
xx$numcues<-xx$force + xx$photo + xx$chill ## Determine how many cues were manipulated in each study

#check<-subset(xx, select=c(name, datasetID, datasets, numcues, force, chill, photo))
#check <- check[!duplicated(check),]
#foo <- check[(check$name%in%allspphere),]

xx$numcues <- ifelse(is.na(xx$numcues), 0, xx$numcues)

### 1. at least two cues were manipulated in one study (i.e., two levels) OR
      ### all three cues are manipulated across multiple studies- from issue #379 https://github.com/lizzieinvancouver/ospree/issues/379
accept<-xx[((xx$numcues==3)),]## if species were in at least one dataset that manipulated at least two cues | (xx$numcues==2 & xx$force==1 & xx$photo==1 & xx$chill==0 & xx$field.sample==1)
species4taxon<-c(accept$name) ## make a list of species using above requirements
accept$complex<-accept$name
accept$use<-"Y"

accept <- subset(accept, select=c("name", "genus", "species", "force", "photo", "chill", "numcues", "complex", "use"))

###accept is a list of species that are good to go

##integrates with rest of data
taxon<-dplyr::filter(d, name %in% species4taxon)
taxon$complex<-taxon$name
taxon$use<-"Y"

## Remove below species because no species maps available at this time
nomaps <- c("Ilex_mucronata", "Kalmia_angustifolia", "Lonicera_canadensis", "Spirea_alba",
            "Vaccinium_myrtilloides", "Viburnum_cassinoides", "Viburnum_lantanoides", "Alnus_rubra",
            "Aronia_melanocarpa", "Symphoricarpos_albus", "Quercus_faginea", "Rhamnus_cathartica",
            "Salix_smithiana", "Sorbus_commixta", "Ulmus_parvifolia", "Ulmus_pumila", "Ulmus_villosa")

taxon <- taxon[!(taxon$complex%in%nomaps),]

sort(unique(taxon$complex))


####### THE list of range species we have...
#[1] "Abies_alba"             "Acer_pensylvanicum"     "Acer_pseudoplatanus"    "Acer_rubrum"           
#[5] "Acer_saccharum"         "Aesculus_hippocastanum" "Alnus_glutinosa"        "Alnus_incana"          
#[9] "Betula_alleghaniensis"  "Betula_lenta"           "Betula_papyrifera"      "Betula_pendula"        
#[13] "Betula_pubescens"       "Carpinus_betulus"       "Cornus_mas"             "Corylus_avellana"      
#[17] "Corylus_cornuta"        "Fagus_grandifolia"      "Fagus_sylvatica"        "Fraxinus_excelsior"    
#[21] "Fraxinus_nigra"         "Larix_decidua"          "Picea_abies"            "Picea_glauca"  
#[25] "Populus_grandidentata"  "Populus_tremula"        "Populus_tremuloides"    "Prunus_avium"          
#[29] "Prunus_padus"           "Prunus_pensylvanica"    "Pseudotsuga_menziesii"  "Quercus_ilex"          
#[33] "Quercus_petraea"        "Quercus_robur"          "Quercus_rubra"          "Robinia_pseudoacacia"  
#[37] "Sorbus_aucuparia"       "Tilia_cordata"          "Ulmus_minor"           "Picea_mariana"



#### No complexes included!!!
## Brings all the accepted species together
accepties<-taxon
accepties<-subset(accepties, select=c(genus, species, complex, use))
accepties<-accepties[!duplicated(accepties),]

dim(d)
bb.wtaxa<-left_join(d, accepties) 
dim(bb.wtaxa) 
bb.wtaxa<-dplyr::select(bb.wtaxa, -name)
bb.wtaxa$use<-ifelse(is.na(bb.wtaxa$use), "N", bb.wtaxa$use)

sort(unique(bb.wtaxa$complex[bb.wtaxa$use=="Y"]))

bb.all.wtaxa <- bb.wtaxa[(bb.wtaxa$use=="Y"),]
bb.all.wtaxa$use <- NULL
bb.noNA.wtaxa <- bb.all.wtaxa
bb.noNA.wtaxa$complex.wname <- bb.noNA.wtaxa$complex
bb.noNA.wtaxa$complex <- as.numeric(as.factor(bb.noNA.wtaxa$complex))

return(bb.noNA.wtaxa)

}


