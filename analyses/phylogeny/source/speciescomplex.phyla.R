## New Species complex f(x) that includes more studies
# criteria is just needs to be in one study now

##### Species were selected if they were in one or more studies and if studies manipulated BOTH photoperiod and forcing

sppcomplexfx.phyla <- function(d){
  
  
  d$name<-paste(d$genus,d$species,sep="_") ###make  a column for genus species
  
  
  xx<-d
  ### make a list of which studies manipulate what.
  xx <- within(xx, { force <- ave(forcetemp, name, FUN=function(x) length(unique(x)))}) # mult forcetemp
  xx <- within(xx, { photo <- ave(photoperiod_day, name, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
  xx <- within(xx, { datasets <- ave(datasetID, name, FUN=function(x) length(unique(x)))}) 
  
  xx<-dplyr::select(xx, name, datasets, force, photo, datasetID)
  xx<-xx[!duplicated(xx),]
  
  
  ###make object with all acceptable (>1 data set species and manipulated more than one cue) 
  ### This make a data sheet with all the complex that can be indivudal species
  xx$force<-ifelse(xx$force<=1, 0, 1)
  xx$photo<-ifelse(xx$photo<=1, 0, 1)
  xx$numcues<-xx$force + xx$photo # Determine how many cues were manipulated, minus chill
  
  accept<-xx[(xx$numcues==2 & xx$datasets>=1),] ## if species were in more than one dataset and manipulated more than one cue that kept
  species4taxon<-c(accept$name) ## make a list of species with more than 1 study
  accept$complex<-accept$name
  accept$use<-"Y"
  
  ###accept is a list of species that are good to go
  
  ##integrates with rest of data
  bb.noNA.wtaxa<-dplyr::filter(d, name %in% species4taxon)
  bb.noNA.wtaxa$complex<-bb.noNA.wtaxa$name
  bb.noNA.wtaxa$use<-"Y"
  
  bb.noNA.wtaxa$complex.wname <- bb.noNA.wtaxa$complex
  bb.noNA.wtaxa$complex <- as.numeric(as.factor(bb.noNA.wtaxa$complex))
  
  
  return(bb.noNA.wtaxa)

}


