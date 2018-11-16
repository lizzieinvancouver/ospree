
library(rstan)
library(ggplot2)
library(shinystan)
# library(bayesplot)
library(dplyr)
# library(rstanarm)

source('..//stan/savestan.R')
source("source/speciescomplex.R") # incl. f(x) to remove species with too few or biased experiments
source("source/speciescomplex_onecue.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################################################
# Running the models with fake data? See bb_testdata_analysis.R #
################################################################# 

## 3 steps to major cleaning: Get the data, merge in taxa info, subset down to what we want for:
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!
## (1) Get the data and slim down to correct response and no NAs ...
source("source/bbdataplease.R")
## (2) Remove rows that had freezing or dormancy treatments set to anything other than 'ambient'
source("source/othertreats.R")
dim(bb.noNA)
bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 18 March October should delete about 273 rows
dim(bb.noNA)
d <- bb.noNA

# (3) Get fewer columns for sanity
source("source/commoncols.R")
bb <- subset(d, select=c(columnstokeep, columnscentered, columnschillunits))

# remove the two values above 600
bb <- subset(bb, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb$chill <- bb$chill/240
length(unique(bb$datasetID))

# deal with photo
bb.expphoto <- subset(bb, photo_type=="exp")
bb.rampphoto <- subset(bb, photo_type=="ramped")
bb.ambphoto <- subset(bb, photo_type=="amb" | photo_type=="none")

#sort(unique(bb.expphoto$datasetID))
#sort(unique(bb.rampphoto$datasetID))
#sort(unique(bb.ambphoto$datasetID))

# add in forcing
bb.expphotoforce <- subset(bb.expphoto, force_type=="exp")

#################################################################
# Set the data you want to use as bb.stan and deal with species #
#################################################################

# currently we use bb.stan.expphoto ...
bb.stan.allphoto.allspp <- bb
bb.stan.allspp <- bb.expphotoforce

bb.stan.allphoto<-sppcomplexfx(bb.stan.allphoto.allspp) # 40 species/complexes
#bb.stan.allphoto<-sppcomplexfx.onecue(bb.stan.allphoto.allspp) ## 43 species/complexes


if(use.allspp){
    bb.stan <- bb.stan.allspp
    bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")
    allspp <- data.frame(latbi=unique(bb.stan$latbi), complex=seq(1:length(unique(bb.stan$latbi))))
    bb.stan <- merge(bb.stan, allspp, by="latbi")
}
    
if(!use.allspp){
    bb.stan <- sppcomplexfx(bb.stan.allspp) # 21 species/complexes
    #bb.stan<-sppcomplexfx.onecue(bb.stan.allspp) ## 29 species/complexes
}

sort(unique(bb.stan.allphoto$complex.wname)) # 40
sort(unique(bb.stan$complex.wname)) # 21

## subsetting data, preparing genus variable, removing NAs (err, again
# remove crops?
# bb <- subset(bb, type!="crop")


##################################
## Prep the data for Stan model ##
##################################
# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling.

# making a list out of the processed data. It will be input for the model
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



## real data with only experimental chilling (no field chilling)
#osp.td3 = stan('stan/nointer_2level.stan', data = datalist.td,
 #              iter = 2000,warmup=1500,control=list(adapt_delta=0.95))

datalist.bb.cen <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.cen, 
                         force = force.cen, 
                         photo = photo.cen,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)


# all photoperiod types (not currently using) 
datalist.bb.allphoto <- with(bb.stan.allphoto, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

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
