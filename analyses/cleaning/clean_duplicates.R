# Fixing merge issues
## Originally by Dan Flynn, updates by Cat, Nacho, Lizzie in January-May 2017 ##
## Nacho is taking over in 26th Jan 2017 -- deal with identify/remove duplicates

# This file works to check for any studies that may have been entered twice #
# A few more notes on this in _cleaningREADME.txt and issue 79 on github #

## housekeeping
#rm(list=ls()) 
options(stringsAsFactors = FALSE)


## reading in data
#setwd("~/GitHub/ospree/analyses/output/")

#d <- read.csv("ospree_clean.csv") 
if(is.data.frame(d)){
  

## select target variables for which we will search for duplicates:
tar.var<-c("datasetID","study","genus","species","varetc","year","population",
           "provenance.long","provenance.lat", "population.altitude.m",
           "growing.lat", "growing.long", "irradiance", "irradiance.units",
           "fieldsample.date","dormancy_induction_days",
           "dormancy_induction_photoperiod_day", "dormancy_induction_photoperiod_night",
           "dormancy_induction_temp_day", "dormancy_induction_temp_night", 
           "forcetemp", "photoperiod_day","freeze.treatment.temp_day",
           "freeze.treatment.temp_night", "field.chill.units",
           "chilltemp","chilldays", "other.treatment", "material")
resp.var<-c("respvar", "response","response.time")

# Notes from Lizzie (not comprehensive, but a few quick ones): 
# falusi03 varies by material (apical, median, small or basal bud), we should probably delete these elsewhere for some analyses
# falusi 97 also varies by material
# heide05 appears to have been entered twice, the delete code is working, yaaayyy!
# myking95 also appears to be duplicated data (across figure numers), success again!

## subset data to look for duplicates (resp.var are included or most of the subset is duplicated)
ospree.sub<-d[,c(tar.var,resp.var)]

## remove duplicated rows in a simple way
ospree.sub.no.dup<-d[!duplicated(ospree.sub),]
#dim(ospree.sub)
#dim(ospree.sub.no.dup) # as of 13 June 2017 we delete 104 rows here


## Additionally: looking at which duplicates among target variables have very similar response variables
## and optionally remove these lines too.

# generate a vector with names of predictors that may be duplicated
ospree.sub.no.dup$vector.duplicates<-apply(ospree.sub.no.dup[,c(tar.var,
                              #"respvar","response","response.time"
                              "n","response..pre.treatment.",
                              "error.type","resp_error", "number.longdays"
                              )], 1,function(x) paste(x, collapse = "_"))

# filter the vector to include only those lines that appear more than once in the dataset
duplicate.blocks<-names(which(table(ospree.sub.no.dup$vector.duplicates)>1))

# add a new column that will be used to identify which lines to remove
ospree.sub.no.dup$to.remove<-rep(0,nrow(ospree.sub.no.dup))

# run loop for each unique block identifying which lines are too similar in their response variable
for(i in 1:length(duplicate.blocks)){ #i=1020;j=1;h=2
  
  block.i<-duplicate.blocks[i]
  d.subset<-subset(ospree.sub.no.dup,vector.duplicates==block.i)
  index.subset<-which(ospree.sub.no.dup$vector.duplicates==block.i)
  resp.duplicated<-d.subset$respvar[duplicated(d.subset$respvar)]
  
  if(length(unique(d.subset$respvar))<nrow(d.subset) & sum(is.na(unique(d.subset$respvar)))==0){
    for(j in 1:length(unique(resp.duplicated))){
      d.subset.j<-subset(d.subset,respvar==unique(resp.duplicated)[j])
      dists<-as.matrix(dist(d.subset.j$response,upper=FALSE))
      dists[dists==0]<-NA
      
      storing<-list()
      for(h in 1:nrow(d.subset.j)){
        if(sum(dists[h,]<as.numeric(d.subset.j$response[h])*0.0025,na.rm=T)>0){
          index.dists<-which(dists[h,]<as.numeric(d.subset.j$response[h])*0.0025)
          
          for(k in 1:length(index.dists)){
          if(dist(c(d.subset.j$response.time[h],as.numeric(d.subset.j$response.time[index.dists[k]])))<as.numeric(d.subset.j$response.time[h])*0.005){
            print(paste(i,j,h,k))
            to.rem<-index.dists[k]
            storing<-list()
            
            if(length(index.dists)>0 & 
               as.numeric(row.names(d.subset.j[index.dists[k],]))>as.numeric(row.names(d.subset.j[h,]))){
              
          ospree.sub.no.dup$to.remove[which(row.names(ospree.sub.no.dup)==row.names(d.subset.j[index.dists[k],]))]<-1
          
          }
          
          }
      
          }
        }
        }
  }
  

  }
  
}
# as of 13 June 2017 we delete 14 rows here
    
# checking what is going to be deleted
#todelete.subset<-subset(ospree.sub.no.dup,to.remove==1)
#dim(todelete.subset)


# subsetting
ospree.sub.no.dup<-subset(ospree.sub.no.dup,to.remove!=1)

## move on without duplicated values
    
d<-ospree.sub.no.dup


} else {
  print("Error: input file is not a data.frame")
}

stop("Not an error, duplicates have been removed;
     No need to worry about the warnings below, they inform that the dataset has empty
     elements that are assigned NA values by default")

