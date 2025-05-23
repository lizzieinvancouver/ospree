## Started on 24 May 2017 ##
## By Lizzie ## 

## Based on Nacho's clean_duplicates.R ##

# This file is similar but it uses a smaller (more restrictive) list of tar.var #
# This means for some studies we delete replicates we're not interested in #
# For example: falusi03 varies by material (apical, median, small or basal bud) as does 
# falusi 97 also varies by material and we do not include material in our list ##

## Here's what we HAD in clean_duplicates tar.var that we deleted here:
# "irradiance", "irradiance.units",
# "dormancy_induction_days", "dormancy_induction_photoperiod_day",
# "dormancy_induction_photoperiod_night", "dormancy_induction_temp_day",
# "dormancy_induction_temp_night",
# "freeze.treatment.temp_day", "freeze.treatment.temp_night",
# "material" , "other.treatment"

#d <- read.csv("ospree_clean.csv")#Note from Ailene: this should be updated to "output/"ospree_clean_withchill.csv" 
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

## subset data to look for duplicates (resp.var are included or most of the subset is duplicated)
bbdat.sub<-d[,c(tar.var,resp.var)]

## remove duplicated rows in a simple way
bbdat.sub.no.dup<-d[!duplicated(bbdat.sub),]
#dim(bbdat.sub)
#dim(bbdat.sub.no.dup)


## Additionally: looking at which duplicates among target variables have very similar response variables
## and optionally remove these lines too.

# generate a vector with names of predictors that may be duplicated
bbdat.sub.no.dup$vector.duplicates<-apply(bbdat.sub.no.dup[,c(tar.var,
                              #"respvar","response","response.time"
                              "n","response..pre.treatment.",
                              "error.type","resp_error", "number.longdays"
                              )], 1,function(x) paste(x, collapse = "_"))

# filter the vector to include only those lines that appear more than once in the dataset
duplicate.blocks<-names(which(table(bbdat.sub.no.dup$vector.duplicates)>1))

# add a new column that will be used to identify which lines to remove
bbdat.sub.no.dup$to.remove<-rep(0,nrow(bbdat.sub.no.dup))

# run loop for each unique block identifying which lines are too similar in their response variable
for(i in 1:length(duplicate.blocks)){
  
  block.i<-duplicate.blocks[i]
  d.subset<-subset(bbdat.sub.no.dup,vector.duplicates==block.i)
  index.subset<-which(bbdat.sub.no.dup$vector.duplicates==block.i)
  resp.duplicated<-d.subset$respvar[duplicated(d.subset$respvar)]
  
  if(length(unique(d.subset$respvar))<nrow(d.subset) & sum(is.na(unique(d.subset$respvar)))==0){
    for(j in 1:length(unique(resp.duplicated))){
      d.subset.j<-subset(d.subset,respvar==unique(resp.duplicated)[j])
      dists<-as.matrix(dist(d.subset.j$response,upper=FALSE))
      dists[dists==0]<-NA
      
  
      for(h in 1:nrow(d.subset.j)){
        if(sum(dists[h,]<as.numeric(d.subset.j$response[h])*0.005,na.rm=T)>0){
          index.dists<-which(dists[h,]<as.numeric(d.subset.j$response[h])*0.005)
          
          for(k in 1:length(index.dists)){
            if(dist(c(d.subset.j$response.time[h],as.numeric(d.subset.j$response.time[index.dists[k]])))<as.numeric(d.subset.j$response.time[h])*0.005){
              print(paste(i,j,h,k))
              to.rem<-index.dists[k]
              storing<-list()
              
              if(length(index.dists)>0 & 
                 as.numeric(row.names(d.subset.j[index.dists[k],]))>as.numeric(row.names(d.subset.j[h,]))){
                
                bbdat.sub.no.dup$to.remove[which(row.names(bbdat.sub.no.dup)==row.names(d.subset.j[index.dists[k],]))]<-1
                
              }
              
            }
            
          }
        }
      }
    }
    
    
  }
  
}

# checking data entries to be removed
#toberemoved<-subset(bbdat.sub.no.dup,to.remove==1)
#dim(toberemoved)

# subsetting
bbdat.sub.no.dup<-subset(bbdat.sub.no.dup,to.remove!=1)


## save file without duplicated values 
d<-bbdat.sub.no.dup


} else {
  print("Error: input file is not a data.frame")
}

stop("Not an error, more duplicates are all cleaned;
     No need to worry about the warnings below, they inform that the dataset has empty
     elements that are assigned NA values by default")
