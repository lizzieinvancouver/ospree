## Try and organize so useful for all models running
## Started by Dan B - 26 July 2017
## Edits by Cat C - 17 August 2017
### reworked by Dan 19 sept 2018

d<-read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

library(dplyr)

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

xx<-dplyr::select(xx,name,genus, datasets, force, photo, chill,field.sample,prov.lat, datasetID)
xx<-xx[!duplicated(xx),]


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

comps<-unique(taxon2$complex)
dats<-vector()
for(i in comps){
  dats[i]<-length(unique(taxon2$datasetID[taxon2$complex==i]))
  numsets<-data.frame(numstudies=dats)
  numsets <- cbind(complex = rownames(numsets), numsets)
}

taxon2<-left_join(taxon2, numsets, by="complex")

accept.complex<-taxon2
accept.complex$use<-ifelse(accept.complex$numstudies>1,"Y","N") 

###if you want a data sheet to merge later in the work flow with working data sheet do this
accept$numstudies<-accept$datasets
complexlist<-rbind(accept,accept.complex)

unique(complexlist$complex)
uselist<-filter(complexlist,use=="Y")
unique(uselist$complex)

accepties<-rbind(accept, accept.complex)
accepties$species<-gsub(".*_", "", accepties$name)
accepties<-subset(accepties, select=c(genus, species, complex, use))
accepties<-accepties[!duplicated(accepties),]


bb.wtaxa<-full_join(d, accepties)
bb.wtaxa<-dplyr::select(bb.wtaxa, -name)

write.csv(bb.wtaxa, file="..//output/ospree_clean_withchill_BB_taxon.csv", row.names = FALSE)

#write.csv(uselist, file="~/Documents/git/ospree/analyses/output/speciescomplex.list.csv", row.names=FALSE)

