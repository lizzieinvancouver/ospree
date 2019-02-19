## Try and organize so useful for all models running
## Started by Dan B - 26 July 2017
## Edits by Cat C - 17 August 2017
### reworked by Dan 19 sept 2018

##### Species were selected if they were in more than one study and if both studies manipulated more than one cue
### Complexes were selected if (1) they were not already species (2) the genus was used in more than one study and (3) those studies used more than one cue


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


d$name<-paste(d$genus,d$species,sep="_") ###make  a column for genus species

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

accept<-xx[(xx$numcues>1 & xx$datasets>1),] ## if species were in more than one dataset and manipulated more than one cue that kept
species4taxon<-c(accept$name) ## make a list of species with more than 1 study
accept$complex<-accept$name
accept$use<-"Y"

###accept is a list of species that are good to go

##integrates with rest of data
taxon<-dplyr::filter(d, name %in% species4taxon)
taxon$complex<-taxon$name
taxon$use<-"Y"

###making complexes#######################
### This is when the same genus has multiple datasets and manipulates more than one cue

comp<-xx[(xx$numcues>1 & xx$datasets==1),] ## this are the singleton species
complex4taxon<-c(comp$name) ### make a list of them


intersect(species4taxon,complex4taxon) #checks to make sure there are no species that over lap between this and above

###building complexes
taxon2<- dplyr::filter(d, name %in% complex4taxon) ###This filters main data sheet for rows that have species with only 1 dataset ID


### Same as above - see what complexes manipulated
taxon2<- within(taxon2, {datasets<- ave(datasetID, genus, FUN=function(x) length(unique(x)))})
taxon2<-dplyr::arrange(taxon2, genus)

taxon2$complex<-paste(taxon2$genus, "complex", sep="_")
taxon2 <- within(taxon2, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
taxon2 <- within(taxon2, { field.sample <- ave(fieldsample.date, complex, FUN=function(x) length(unique(x)))}) # mult fieldsample.date
taxon2 <- within(taxon2, { force <- ave(forcetemp, complex, FUN=function(x) length(unique(x)))}) # mult forcetemp
taxon2 <- within(taxon2, { photo <- ave(photoperiod_day, complex, FUN=function(x) length(unique(x)))}) # mult photoperiod_day
taxon2 <- within(taxon2, { chill <- ave(chill, complex, FUN=function(x) length(unique(x)))}) # mult expchill
taxon2 <- within(taxon2, { chilltime <- ifelse(chilldays!=0, ave(chilldays, datasetID, species, FUN=function(x) length(unique(x))), 0)}) # mult studychill
#taxon2<- within(taxon2, { prov.long <- ave(provenance.long,complex, FUN=function(x) length(unique(x)))}) # multiple provenance.longs
taxon2<-dplyr::select(taxon2,name,genus, datasets, force, photo, chill, chilltime, field.sample,#prov.lat, 
                      complex,datasetID)
taxon2<-taxon2[!duplicated(taxon2),]


### Accepts complexes that are in more than one study and manipulated more than one cue
taxon2$force<-ifelse(taxon2$force<=1, 0, 1)
taxon2$photo<-ifelse(taxon2$photo<=1, 0, 1)
taxon2$chill<-ifelse(taxon2$chill<=1, 0, 1)
taxon2$chilltime<-ifelse(taxon2$chilltime<=1, 0, 1) ## Different methods of manipulating chilling
taxon2$field.sample<-ifelse(taxon2$field.sample<=1, 0, 1) ## Different methods of manipulating chilling
taxon2$chill<-ifelse(taxon2$chill==1 | taxon2$chilltime==1 | taxon2$field.sample==1, 1, 0)
taxon2$numcues<-taxon2$force + taxon2$photo + taxon2$chill ## total number of cues manipulated for the complexes

## This loop makes sure the complex is represented in multiple datasets and not just in one dataset (e.g. Zohner et al 2016)
comps<-unique(taxon2$complex)
dats<-vector()
for(i in comps){
  dats[i]<-length(unique(taxon2$datasetID[taxon2$complex==i]))
  numsets<-data.frame(numstudies=dats)
  numsets <- cbind(complex = rownames(numsets), numsets)
}

taxon2<-left_join(taxon2, numsets, by="complex")

accept.complex<-taxon2
accept.complex$use<-ifelse((accept.complex$numstudies>1 & accept.complex$numcues>1),"Y","N") 


###if you want a data sheet to merge later in the work flow with working data sheet do this
accept$numstudies<-accept$datasets
complexlist<-rbind(accept,accept.complex)

#unique(complexlist$complex)
uselist<-filter(complexlist,use=="Y")
#unique(uselist$complex)

## Brings all the accepted species and accepted complexes together
accepties<-rbind(accept, accept.complex)
accepties$species<-gsub(".*_", "", accepties$name)
accepties<-subset(accepties, select=c(genus, species, complex, use))
accepties<-accepties[!duplicated(accepties),]


bb.wtaxa<-full_join(d, accepties)
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


