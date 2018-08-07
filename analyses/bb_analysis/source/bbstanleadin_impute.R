## same as bbstanleadin, but instead of deleting NAs, imputing data based on Grand means

library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
# library(rstanarm)

source('..//stan/savestan.R')

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
#dim(bb.noNA)
#bb.noNA <- bb.noNA[-c(othertreats.delete),] # as of 28 March 2018 should delete about 359 rows
dim(bb.resp)
bb.resp <- bb.resp[-c(othertreats.delete),] # as of 28 March 2018 should delete about 359 rows

## (3) Deal with species
d <- bb.resp

source("source/speciescomplex.R")
bb.noNA.wtaxa <- d
dim(bb.noNA.wtaxa)
unique(bb.noNA.wtaxa$complex)

# (4) Get fewer columns for sanity
source("source/commoncols.R")
bb <- subset(bb.noNA.wtaxa, select=c(columnstokeep, "chill.cen", "photo.cen", "force.cen","force.z","chill.z", "photo.z"))

## subsetting data, preparing genus variable, removing NAs (err, again
# remove crops?
# bb <- subset(bb, type!="crop")
bb.stan <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "complex", "type",
                               "force.cen","chill.cen", "photo.cen", "force.z","chill.z", "photo.z"))
bb.stan$complex.wname <- bb.stan$complex
bb.stan$complex <- as.numeric(as.factor(bb.stan$complex))

# remove the two values above 600
bb.stan <- subset(bb.stan, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb.stan$chill <- bb.stan$chill/240
length(unique(bb.stan$datasetID))


# assign means values to NAs
bb.stan$resp[which(is.na(bb.stan$resp))]<-mean(bb.stan$resp,na.rm=T)
bb.stan$chill[which(is.na(bb.stan$chill))]<-mean(bb.stan$chill,na.rm=T)
bb.stan$force[which(is.na(bb.stan$force))]<-mean(bb.stan$force,na.rm=T)
bb.stan$photo[which(is.na(bb.stan$photo))]<-mean(bb.stan$photo,na.rm=T)

bb.stan$chill.cen[which(is.na(bb.stan$chill.cen))]<-mean(bb.stan$chill.cen,na.rm=T)
bb.stan$force.cen[which(is.na(bb.stan$force.cen))]<-mean(bb.stan$force.cen,na.rm=T)
bb.stan$photo.cen[which(is.na(bb.stan$photo.cen))]<-mean(bb.stan$photo.cen,na.rm=T)


# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling. 

## Prep the data for Stan model
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


