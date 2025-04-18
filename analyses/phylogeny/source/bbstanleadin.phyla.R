# libraries needed for the leadin code
library(rstan)
library(dplyr)

#source('..//..//bb_analysis/stan/savestan.R') # Dan Flynn code
# incl. f(x)s to deal with biases in the data related to species, study, and design....
source("source/speciescomplex.phyla.R") # this makes sure all species/complexes present in 2 or more studies
source("source/speciescomplex.nocrops.R") # similar to speciescomplex.R but removes 4 crop species
source("..//bb_analysis/source/stan_utility.R") # From Mike Betancourt

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
# Running the models with fake data? See bb_testdata_analysis.R #
################################################################# 

## First steps to cleaning: Get the data, subset down to exact data columns etc. that we want 
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!

## (1) Get the data and slim down to correct response and no NAs ...
source("..//bb_analysis/source/bbdataplease.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("..//bb_analysis/source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA
## (3) Get fewer columns for sanity
source("..//bb_analysis/source/commoncols.R")
bb <- subset(d, select=c(columnstokeep, columnschillunits))

# remove the values above 600 (which means remove the right-censored data, coded as 999)
bb <- subset(bb, resp<600)

## get Dan Flynn's data
source("source/format_flynnsdata.R")


# deal with photo
bb.expphoto <- subset(bb, photo_type=="exp")
bb.rampphoto <- subset(bb, photo_type=="ramped")
bb.exprampphoto <- subset(bb, photo_type=="exp" | photo_type=="ramped")
bb.ambphoto <- subset(bb, photo_type=="amb" | photo_type=="none")

#sort(unique(bb.expphoto$datasetID))
#sort(unique(bb.rampphoto$datasetID))
#sort(unique(bb.ambphoto$datasetID))

# add in forcing
bb.exprampphotoforce <- subset(bb.exprampphoto, force_type=="exp"|force_type=="ramped")
bb.expphotoforce <- subset(bb.expphoto, force_type=="exp")

##################################################
# Set the data you want to use deal with species #
##################################################

# set up data for when using all types of designs (exp, ramped, amb)
bb.all <- bb
if (use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
    bb.stan.alltypes <- sppcomplexfx.phyla(bb.all) 
    bb.stan.alltypes.nocrops <- sppcomplexfx.nocrops(bb.all)
}

# set up data for when using exp AND ramped for photo + forceexp for photo + force
if (use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE){
    bb.stan.expramptypes <- sppcomplexfx.phyla(bb.exprampphotoforce) 
    bb.stan.expramptypes.nocrops <- sppcomplexfx.nocrops(bb.exprampphotoforce)
}


# set up data for when using exp for photo + force
if (use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE){
    bb.stan.exptypes <- sppcomplexfx.phyla(bb.expphotoforce)  
    bb.stan.exptypes.nocrops <- sppcomplexfx.nocrops(bb.expphotoforce)
}


##################################
## Prep the data for Stan model ##
##################################

# making some list out of the processed data. It will be input for the model ...
# This is the default .... (uses basic species complex)
if (use.allspp==FALSE & use.nocropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
    bb.stan <- bb.stan.alltypes
    source("..//bb_analysis/source/bb_zscorepreds.R")
    
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# making some list out of the processed data. It will be input for the model ...
# This is the default .... (uses basic species complex)
if (use.allspp==FALSE & use.nocropspp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
  bb.stan <- bb.stan.alltypes.nocrops
  source("..//bb_analysis/source/bb_zscorepreds.R")
  
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}


# This is ALL species (no species complex used)
if (use.allspp==TRUE & use.nocropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
    # TO DO: check the complex code below!
    bb.all$latbi <- paste(bb.all$genus, bb.all$species, sep="_")
    bb.all$complex -> as.numeric(bb.all$latbi)
    bb.stan <- bb.all
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# Species complex with only exp & ramped photoforce
if (use.allspp==FALSE & use.nocropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE){
    bb.stan <- bb.stan.expramptypes
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# Species complex with only exp photoforce
if (use.allspp==FALSE & use.nocropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE){
    bb.stan <- bb.stan.expphotoforce
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}




## AT THE END ...
str(datalist.bb)
print("Unique forcing types are ...")
print(unique(bb.stan$force_type))
print("Unique photo types are ...")
print(unique(bb.stan$photo_type))
print("Unique chilling types are ...")
print(unique(bb.stan$chill_type))


