# libraries needed for the leadin code
library(plyr)
library(dplyr)
library(rstan)

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
bb<-subset(d,select=c(columnstokeepnvar, columnschillunits))
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

# add in chilling (exp)
bb.exprampphotoforceexpch <- subset(bb.exprampphotoforce, chill_type=="exp")
bb.allexpch <- subset(bb, chill_type=="exp")

##################################################
# Set the data you want to use deal with species #
##################################################

bb.all <- bb

# set up data for when using all types of designs (exp, ramped, amb)
if (use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == FALSE){
    bb.stan.alltypes <- sppcomplexfx(bb.all) 
    bb.stan.alltypes.multcue <- sppcomplexfx.multcue(bb.all) 
    bb.stan.alltypes.nocrops <- sppcomplexfx.nocrops(bb.all)
}


# set up data for when using exp AND ramped for photo + forceexp for photo + force
if (use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE){
    bb.stan.expramptypes <- sppcomplexfx(bb.exprampphotoforce) 
    bb.stan.expramptypes.multcue <- sppcomplexfx.multcue(bb.exprampphotoforce) 
    bb.stan.expramptypes.nocrops <- sppcomplexfx.nocrops(bb.exprampphotoforce)
}


# set up data for when using exp for photo + force
if (use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE & use.expchillonly == FALSE){
    bb.stan.exptypes <- sppcomplexfx(bb.expphotoforce) 
    bb.stan.exptypes.multcue <- sppcomplexfx.multcue(bb.expphotoforce) 
    bb.stan.exptypes.nocrops <- sppcomplexfx.nocrops(bb.expphotoforce)
}

# set up data for when using only experimental chilling
if (use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == TRUE){
  bb.stan.exprampphotoforceexpch<- sppcomplexfx(bb.exprampphotoforceexpch) 
  bb.stan.exprampphotoforceexpch.multcue <- sppcomplexfx.multcue(bb.exprampphotoforceexpch) 
  bb.stan.exprampphotoforceexpch.nocrops <- sppcomplexfx.nocrops(bb.exprampphotoforceexpch)
}
if (use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == TRUE){
  bb.stan.alltypesexpch<- sppcomplexfx(bb.allexpch) 
  bb.stan.alltypesexpch <- sppcomplexfx.multcue(bb.allexpch) 
  bb.stan.alltypesexpch <- sppcomplexfx.nocrops(bb.allexpch)
}
##################################
## Prep the data for Stan model ##
##################################

# making some list out of the processed data. It will be input for the model ...

##
## ALL TYPES (amb, ramped, exp etc.)
## 

# This is ALL species (no species complex used) and all types & z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & 
    use.expchillonly == FALSE & use.chillports == FALSE & use.zscore == TRUE){
    bb.stan <- addcomplex.allspp(bb.all)
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

# This is ALL species (no species complex used) and all types & NOT z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & 
    use.expchillonly == FALSE & use.chillports == FALSE & use.zscore == FALSE){
  bb.stan <- addcomplex.allspp(bb.all)
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}



# Species complex and all types & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.alltypes.nocrops
    source("source/bb_zscorepreds.R")
    
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

##
## EXP and RAMPED TYPES for photo and force
## 

# This is ALL species (no species complex used) with only exp & ramped photoforce & z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- addcomplex.allspp(bb.exprampphotoforce)
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}


# This is ALL species (no species complex used) with only exp & ramped photoforce & NOT z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == FALSE){
    bb.stan <- addcomplex.allspp(bb.exprampphotoforce)
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}



# Species complex with only exp & ramped photoforce & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.expramptypes.nocrops
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}


# Species complex with only exp & ramped photoforce WITH crops & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.expramptypes
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# Species complex with only exp & ramped photoforce & NOT z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes.nocrops
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.ports, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}


# Species complex with only exp & ramped photoforce WITH crops & NOT z-scored 
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.ports, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

##
## EXP only as TYPE for photo and force
## 

# Species complex with only exp photoforce & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.exptypes.nocrops
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# ALL species with only exp photoforce & z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE & use.expchillonly == FALSE  &
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- addcomplex.allspp(bb.expphotoforce)
    source("source/bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}


##
## Chilling
## 

# Species complex, default photo and forcing and ONLY exp chill & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
  use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == TRUE &
  use.chillports == TRUE & use.zscore == TRUE){
  bb.stan <- bb.stan.exprampphotoforceexpch.nocrops
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill.ports.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}
# Species complex, ALL photo and forcing and ONLY exp chill & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == TRUE &
    use.chillports == TRUE & use.zscore == TRUE){
  bb.stan <- bb.stan.alltypesexpch.nocrops
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill.ports.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}
# Species complex, exo and photo and forcing and using utah & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.expramptypes.nocrops
  source("source/bb_zscorepreds.R")#37 species, 1729 rows of data
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

#checking: 
# Species complex, exp and photo and forcing and using utah WITH CROPS & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.expramptypes
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

# Species complex with only exp & ramped photoforce & NOT z-scored and utah
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes.nocrops 
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}
# Species complex, exp and photo and forcing and using utah WITH CROPS & NOT z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.expramptypes
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

# Species complex, exp and photo and forcing and using utah WITH CROPS & NOT z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes
  
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
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
print("Range of chilling is ...")
print(range(datalist.bb$chil))



