#December 5, 2018

#This code has been taken from the speciescomplex.R used for the bb analysis, but just to get species names that have more than one study associated with them.

spptraitfx <- function(d){
  
  
  d$name<-paste(d$genus,d$species,sep=" ") ###make  a column for genus species
  
  xx<-d
  ### make a list of which studies manipulate what.
  xx <- within(xx, { prov.lat <- ave(provenance.lat, name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
  xx <- within(xx, { field.sample <- ave(fieldsample.date, name, species, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
  xx <- within(xx, { force <- ave(forcetemp, name, species, FUN=function(x) length(unique(x)))}) # mult forcetemp
  xx <- within(xx, { photo <- ave(photoperiod_day, name, species, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
  xx <- within(xx, { chill <- ave(chill, name, species, FUN=function(x) length(unique(x)))}) # mult expchill
  xx <- within(xx, { chilltime <- ifelse(chilldays!=0, ave(chilldays, datasetID, species, FUN=function(x) length(unique(x))), 0)}) # mult studychill
  #xx <- within(xx, { spp <- ave(species, name, FUN=function(x) length(unique(x)))}) # mult species
  #xx <- within(xx, { prov.long <- ave(provenance.long,name, species, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
  xx <- within(xx, { datasets <- ave(datasetID, name, species, FUN=function(x) length(unique(x)))}) 
  
  xx<-dplyr::select(xx,name,genus, datasets, force, photo, chill,chilltime,field.sample,#prov.lat, 
                    datasetID)
  xx<-xx[!duplicated(xx),]
  
  
  ###make object with all acceptable (>1 data set species and manipulated more than one cue) 
  ### This make a data sheet with all the complex that can be indivudal species
  xx$force<-ifelse(xx$force<=1, 0, 1)
  xx$photo<-ifelse(xx$photo<=1, 0, 1)
  xx$chill<-ifelse(xx$chill<=1, 0, 1)
  xx$chilltime<-ifelse(xx$chilltime<=1, 0, 1) ## Different methods of manipulating chilling
  xx$field.sample<-ifelse(xx$field.sample<=1, 0, 1) ## Different methods of manipulating chilling
  xx$chill<-ifelse(xx$chill==1 | xx$chilltime==1 | xx$field.sample==1,1, 0)
  xx$numcues<-xx$force + xx$photo + xx$chill ## Determine how many cues were manipulated
  
  #check<-subset(xx, select=c(name, datasetID, datasets, numcues))
  
  accept<-xx[(xx$numcues>=1 & xx$datasets>1),] ## if species were in more than one dataset and manipulated more than one cue that kept
  species4taxon<-c(accept$name) ## make a list of species with more than 1 study
  # accept$complex<-accept$name
  # accept$use<-"Y"
  # 
  # ###accept is a list of species that are good to go
  # 
  # ##integrates with rest of data
  # taxon<-dplyr::filter(d, name %in% species4taxon)
  # taxon$complex<-taxon$name
  # taxon$use<-"Y"
  # 
  return (accept)
}
