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
if(use.zohner == FALSE){
  bb.noNA<-bb.noNA[bb.noNA$datasetID!="zohner16",]
}
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA

if(use.stage== TRUE){#if using stage, add adult vs. juvenile
  d$stage = 1#1 = adult
  d$stage[grep("seedling",d$material)]<-2#2 = juv
  d$stage[grep("sapling",d$material)]<-2
}
source("source/commoncols_newbbmods.R")
if(use.continent == TRUE){columnstokeep <- c("datasetID","study", "genus", "species", "varetc", "woody", "forcetemp", "forcetemp_night",
                                             "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", "chilltemp",
                                             "chilldays", "fieldsample.date",
                                             "provenance.lat", "force", "photo", "chill", "resp", "force_type",  "photo_type", "chill_type","continent")}

## (3) Get fewer columns for sanity
bb <- subset(d, select=c(columnstokeep, columnschillunits))

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

# Species complex, exo and photo and forcing, z-scored, continent (so on chilling)
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE & use.continent == TRUE){
  bb.stan <- bb.stan.expramptypes.nocrops
  source("source/bb_zscorepreds.R")#37 species, 1729 rows of data
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           cont = continent,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex)),
                           n_cont = length(unique(bb.stan$continent))
                      )
  )
}

# Species complex, exo and photo and forcing and using utah & z-scored, life stage
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE & use.stage == TRUE){
  bb.stan <- bb.stan.expramptypes.nocrops
  source("source/bb_zscorepreds.R")#37 species, 1729 rows of data
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           stage = stage,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex)),
                           n_stage = length(unique(bb.stan$stage))
                      )
  )
}

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE & use.continent == FALSE){
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

## AT THE END ...
str(datalist.bb)
print("Unique forcing types are ...")
print(unique(bb.stan$force_type))
print("Unique photo types are ...")
print(unique(bb.stan$photo_type))
if(use.stage == TRUE){
  print("Unique chilling types are ...")
  print(unique(bb.stan$chill_type))
  print("Range of chilling is ...")
  print(range(datalist.bb$chil))
  print("Unique life stages are ...")
  print(unique(bb.stan$stage))}
if(use.continent == TRUE){
  print("Unique continents are ...")
  print(unique(bb.stan$continent))}



