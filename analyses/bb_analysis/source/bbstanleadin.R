# libraries needed for the leadin code
library(rstan)
library(dplyr)

source('..//stan/savestan.R') # Dan Flynn code
# incl. f(x)s to deal with biases in the data related to species, study, and design....
source("source/speciescomplex.R") # this makes sure all species/complexes present in 2 or more studies
source("source/speciescomplex.multcues.R") # as above, but requires all species/complexes to have more than one cue manipulated
source("source/speciescomplex.nocrops.R") # similar to speciescomplex.R but removes 4 crop species
source("source/stan_utility.R") # From Mike Betancourt

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
# Running the models with fake data? See bb_testdata_analysis.R #
################################################################# 

## First steps to cleaning: Get the data, subset down to exact data columns etc. that we want 
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!

## (1) Get the data and slim down to correct response and no NAs ...
source("source/bbdataplease.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA
## (3) Get fewer columns for sanity
source("source/commoncols.R")
bb <- subset(d, select=c(columnstokeep, columnscentered, columnschillunits))

# remove the values above 600 (which means remove the right-censored data, coded as 999)
bb <- subset(bb, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb$chill <- bb$chill/240
length(unique(bb$datasetID))
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
    bb.stan.alltypes <- sppcomplexfx(bb.all) 
    bb.stan.alltypes.multcue <- sppcomplexfx.multcue(bb.all) 
    bb.stan.alltypes.nocrops <- sppcomplexfx.nocrops(bb.all)
}

# set up data for when using exp AND ramped for photo + forceexp for photo + force
if (use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE){
    bb.stan.expramptypes <- sppcomplexfx(bb.exprampphotoforce) 
    bb.stan.expramptypes.multcue <- sppcomplexfx.multcue(bb.exprampphotoforce) 
    bb.stan.expramptypes.nocrops <- sppcomplexfx.nocrops(bb.exprampphotoforce)
}


# set up data for when using exp for photo + force
if (use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE){
    bb.stan.exptypes <- sppcomplexfx(bb.expphotoforce) 
    bb.stan.exptypes.multcue <- sppcomplexfx.multcue(bb.expphotoforce) 
    bb.stan.exptypes.nocrops <- sppcomplexfx.nocrops(bb.expphotoforce)
}


# set up data for when using different types of chilling


##################################
## Prep the data for Stan model ##
##################################

# making some list out of the processed data. It will be input for the model ...
# This is the default .... (uses basic species complex)
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
    bb.stan <- bb.stan.alltypes
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
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
    # TO DO: check the complex code below!
    bb.all$latbi <- paste(bb.all$genus, bb.all$species, sep="_")
    bb.all$complex -> as.numeric(bb.all$latbi)
    bb.stan <- bb.all
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
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE){
    bb.stan <- bb.stan.exprampphotoforce
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
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE){
    bb.stan <- bb.stan.expphotoforce
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
# This is ALL species (no species complex used)
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE){
    # TO DO: check the complex code below!
    bb.all$latbi <- paste(bb.all$genus, bb.all$species, sep="_")
    bb.all$complex -> as.numeric(bb.all$latbi)
    bb.stan <- bb.all
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
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE){
    bb.stan <- bb.stan.exprampphotoforce
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
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE){
    bb.stan <- bb.stan.expphotoforce
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




## Ailene works on below 
if(FALSE){
if(use.chillunits){
# datalist for utah
datalist.bb.utah <- with(bb.stan, 
                           list(y = resp, 
                                chill = chill, 
                                force = force, 
                                photo = photo,
                                sp = complex,
                                N = nrow(bb.stan),
                                n_sp = length(unique(bb.stan$complex)),
                                study = as.numeric(as.factor(bb.stan$datasetID)),
                                n_study = length(unique(bb.stan$datasetID)) 
                           )
  )
# datalist for utah: z-scored
datalist.bb.utah.z <- with(bb.stan, 
                           list(y = resp, 
                                chill = chill.z, 
                                force = force.z, 
                                photo = photo.z,
                                sp = complex,
                                N = nrow(bb.stan),
                                n_sp = length(unique(bb.stan$complex)),
                                study = as.numeric(as.factor(bb.stan$datasetID)),
                                n_study = length(unique(bb.stan$datasetID)) 
                           )
)

# datalist for chillhrs
datalist.bb.chrs <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.hrs, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex)),
                         study = as.numeric(as.factor(bb.stan$datasetID)),
                         n_study = length(unique(bb.stan$datasetID)) 
                    )
)

# datalist for chillhrs: z-scored
datalist.bb.chrs.z <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.hrs.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex)),
                           study = as.numeric(as.factor(bb.stan$datasetID)),
                           n_study = length(unique(bb.stan$datasetID)) 
                      )
)
# datalist for chill portions
datalist.bb.cports <- with(bb.stan, 
                         list(y = resp, 
                              chill = chill.ports, 
                              force = force, 
                              photo = photo,
                              sp = complex,
                              N = nrow(bb.stan),
                              n_sp = length(unique(bb.stan$complex)),
                              study = as.numeric(as.factor(bb.stan$datasetID)),
                              n_study = length(unique(bb.stan$datasetID)) 
                         )
)

# datalist for chill portions: z-scored
datalist.bb.cports.z <- with(bb.stan, 
                           list(y = resp, 
                                chill = chill.ports.z, 
                                force = force.z, 
                                photo = photo.z,
                                sp = complex,
                                N = nrow(bb.stan),
                                n_sp = length(unique(bb.stan$complex)),
                                study = as.numeric(as.factor(bb.stan$datasetID)),
                                n_study = length(unique(bb.stan$datasetID))
                           )
)
}
}
