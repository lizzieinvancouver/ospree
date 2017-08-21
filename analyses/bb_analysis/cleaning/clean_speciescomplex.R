## Try and organize so useful for all models running
## Started by Dan B - 26 July 2017
# Edits by Cat C - 17 August 2017


d$name<-paste(d$genus,d$species,sep="_")

xx<-d

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

#############  Source this section once you subset down! ###############

###make object with all acceptable (<1 data set species)
accept<-filter(xx,datasets>1)
species4taxon<-c(accept$name)
goo<-filter(d, name %in% species4taxon)
goo$complex<-goo$name
goo$use<-"Y"

###making complexes

comp<-filter(xx,datasets==1)
complex4taxon<-c(comp$name)
###building complexes
goober<- filter(d, name %in% complex4taxon)
#goober<-dplyr::select(goober,name,genus, datasetID)
#goober<-goober[!duplicated(goober),]

goober<- within(goober, {datasets<- ave(datasetID, genus, FUN=function(x) length(unique(x)))})
goober<-arrange(goober, genus)
goober$complex<-paste(goober$genus, "complex", sep="_")
goober <- within(goober, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
goober <- within(goober, { field.sample <- ave(fieldsample.date, complex, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
goober <- within(goober, { force <- ave(forcetemp, complex, FUN=function(x) length(unique(x)))}) # mult forcetemp
goober <- within(goober, { photo <- ave(photoperiod_day, complex, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
goober <- within(goober, { chill <- ave(chilltemp, complex, FUN=function(x) length(unique(x)))}) # mult expchill
goober <- within(goober, { prov.long <- ave(provenance.long,complex, FUN=function(x) length(unique(x)))}) # multiple provenance.longs

goober<-dplyr::select(goober,name,genus, datasets, force, photo, chill,field.sample,prov.lat, complex)
goober<-goober[!duplicated(goober),]

accept.comp<-filter(goober,datasets>1)
genus4taxon<-c(accept.comp$genus)
goob<-filter(d, genus %in% genus4taxon)
goob$complex<-NA
goob$complex <- paste(goob$genus, "complex", sep="_")
goob$use<-"Y"


#### Check it to make sure....
unique(accept.comp$complex)
###  [1] "Acer_complex"         "Betula_complex"       "Fraxinus_complex"     "Juglans_complex"      "Picea_complex"        "Pinus_complex"        "Populus_complex"      "Prunus_complex"      
#[9] "Pyrus_complex"        "Quercus_complex"      "Rhododendron_complex" "Rosa_complex"         "Salix_complex"        "Sorbus_complex"       "Tilia_complex"        "Ulmus_complex"       
#[17] "Vaccinium_complex"   

sort(unique(goob$complex))
## [1] "Acer_complex"         "Betula_complex"       "Fraxinus_complex"     "Juglans_complex"      "Picea_complex"        "Pinus_complex"        "Populus_complex"      "Prunus_complex"      
#[9] "Pyrus_complex"        "Quercus_complex"      "Rhododendron_complex" "Rosa_complex"         "Salix_complex"        "Sorbus_complex"       "Tilia_complex"        "Ulmus_complex"       
#[17] "Vaccinium_complex"   

d$complex<-NA
can.use<-c(goo$name, goob$name)
d<-filter(d, name %in% can.use)

complexes<-ifelse(goob$name%in%goo$name, NA, goob$name)
complexes<-na.omit(complexes)
d$complex<-NA
d$complex<-ifelse(d$name%in%complexes, paste(d$genus, "complex", sep="_"), d$name)
d$use<-"Y"


### Check again....
check.this<-d%>%dplyr::select(datasetID, genus, species, name, complex)
check.this<-check.this[!duplicated(check.this),]

sort(c(unique(goo$complex), unique(goob$complex)))
#  [1] "Acer_pseudoplatanus"     "Betula_pendula"          "Aesculus_hippocastanum"  "Syringa_vulgaris"        "Corylus_avellana"        "Fraxinus_excelsior"      "Fagus_sylvatica"        
#[8] "Picea_abies"             "Larix_decidua"           "Prunus_avium"            "Tilia_cordata"           "Sorbus_aucuparia"        "Abies_alba"              "Quercus_petraea"        
#[15] "Actinidia_deliciosa"     "Vitis_vinifera"          "Betula_pubescens"        "Quercus_rubra"           "Acer_saccharum"          "Betula_alleghaniensis"   "Pseudotsuga_menziesii"  
#[22] "Prunus_persica"          "Malus_domestica"         "Quercus_robur"           "Liquidambar_styraciflua" "Alnus_glutinosa"         "Sorbus_commixta"         "Ribes_nigrum"           
#[29] "Prunus_padus"            "Populus_tremula"         "Alnus_incana"            "Rubus_idaeus"            "Carpinus_betulus"        "Cornus_alba"             "Cornus_mas"             
#[36] "Robinia_pseudoacacia"    "Symphoricarpos_albus"    "Picea_glauca"            "Quercus_ilex"            "Pyrus_pyrifolia"         "Betula_nana"             "Quercus_faginea"        
#[43] "Tilia_complex"           "Acer_complex"            "Betula_complex"          "Fraxinus_complex"        "Picea_complex"           "Prunus_complex"          "Quercus_complex"        
#[50] "Sorbus_complex"          "Pinus_complex"           "Salix_complex"           "Juglans_complex"         "Rosa_complex"            "Ulmus_complex"           "Populus_complex"        
#[57] "Pyrus_complex"           "Rhododendron_complex"    "Vaccinium_complex"      


sort(unique(d$complex))
#[1] "Tilia_complex"           "Acer_pseudoplatanus"     "Betula_pendula"          "Aesculus_hippocastanum"  "Syringa_vulgaris"        "Corylus_avellana"        "Fraxinus_excelsior"     
#[8] "Fagus_sylvatica"         "Picea_abies"             "Larix_decidua"           "Prunus_avium"            "Quercus_complex"         "Tilia_cordata"           "Sorbus_aucuparia"       
#[15] "Abies_alba"              "Quercus_petraea"         "Actinidia_deliciosa"     "Vitis_vinifera"          "Pinus_complex"           "Betula_pubescens"        "Salix_complex"          
#[22] "Quercus_rubra"           "Acer_saccharum"          "Betula_alleghaniensis"   "Pseudotsuga_menziesii"   "Picea_complex"           "Juglans_complex"         "Prunus_persica"         
#[29] "Malus_domestica"         "Rosa_complex"            "Quercus_robur"           "Betula_complex"          "Ulmus_complex"           "Liquidambar_styraciflua" "Populus_complex"        
#[36] "Alnus_glutinosa"         "Pyrus_complex"           "Prunus_complex"          "Sorbus_complex"          "Sorbus_commixta"         "Ribes_nigrum"            "Prunus_padus"           
#[43] "Populus_tremula"         "Alnus_incana"            "Rubus_idaeus"            "Carpinus_betulus"        "Acer_complex"            "Cornus_alba"             "Cornus_mas"             
#[50] "Fraxinus_complex"        "Robinia_pseudoacacia"    "Symphoricarpos_albus"    "Picea_glauca"            "Quercus_ilex"            "Pyrus_pyrifolia"         "Rhododendron_complex"   
#[57] "Betula_nana"             "Quercus_faginea"         "Vaccinium_complex" 
