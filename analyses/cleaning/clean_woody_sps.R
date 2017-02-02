###Script.checking.woody.sps.R
##'
##'1/22/2017
##'Ignacio Morales-Castilla
##'
##' Updated on Jan 31st to clean from dataframe d, that should already be in the workspace
##' Updated on Feb 2nd
##' 
## to start
#rm(list=ls())
#options(stringsAsFactors = FALSE)

## read in file
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/output")
#setwd("C:/Users/Ignacio/Documents/GitHub/ospree/analyses/zarchive")


#setwd("~ospree/analyses/input/")
#d<-read.csv("ospree_clean_withchill.csv")
if(is.data.frame(d)){


## set output folder
#out.folder<-"~ospree/analyses/output/"

## fill blanks with NAs
#dat[dat==""]<-NA

## look at the structure of variables within
#str(dat)

## summarize unique species
sps<-sort(unique(paste(d$genus,d$species,sep="_")))

## summarize unique genus
genus<-sort(unique(d$genus))

## genus is non-woody (in ospree)
is.non.woody<-c("Colchicum","Dahlia","Fragaria","Opuntia","Pharbitis"    
  ,"Schlumbergera","Sedum","Silene","Sinapis","Sorghum")  

## remove non-woody species
d.woody<-d[-which(d$genus%in%is.non.woody),]

## correct species misspellings
unique.spp<-sort(unique(d.woody$species))

d.woody[which(d.woody$species==""),"species"]<-"simsii"
d.woody[which(d.woody$species==" coccifera"),"species"]<-"coccifera"
d.woody[which(d.woody$species==" faginea"),"species"]<-"faginea"
d.woody[which(d.woody$species==" japonica"),"species"]<-"japonica"
d.woody[which(d.woody$species=="jezoensis hondoensis"),"species"]<-"jezoensis"
d.woody[which(d.woody$species=="psuedoplatanus"),"species"]<-"pseudoplatanus"
d.woody[which(d.woody$species=="pumila Mill."),"species"]<-"pumila"
d.woody[which(d.woody$species=="sylvatica L"),"species"]<-"sylvatica"

## save
#write.csv(d.woody,paste(out.folder,"ospree_clean_woody.csv",sep=""))
d<-d.woody

## alternatively, correct the "woody" column within ospree
#dat$woody[which(dat$genus%in%is.non.woody)]<-"no"
#write.csv(dat,paste(out.folder,"ospree.woody.corrected.csv",sep=""))

}


