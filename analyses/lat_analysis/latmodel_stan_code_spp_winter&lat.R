## Started 6 July 2016 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

## Take 2: February 2017! ##
## Take 3: July 2017! ## Nacho's clean code to run stan models on Ospree
############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

setwd("~/Documents/git/ospree/analyses")
source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

taxon <- read.csv("output/bb_analysis/taxon/complex_levels.csv", header=TRUE)


## Old code to remove Olea, probably the new subset of species is getting rid of it 
#bb[bb$genus=="Olea",]
#bb<-subset(bb,genus!="Olea")

## subsetting for experimental chilling
#bb<-subset(bb,!is.na(as.numeric(chilltemp)))

# merge in labgroup (we could do this elsewhere someday)
bb.wlab <- merge(bb, taxon, by=c("genus","species"), all.x=TRUE)
myspp<-c("Betula_pendula", "Betula_pubscens", "Fagus_sylvatica", "Picea_abies",
         "Ribes_nigrum", "Corylus_avellana", "Quercus_robur", "Larix_decidua")
bb.wlab<-dplyr::filter(bb.wlab, complex%in%myspp)

columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours",
                   "complex", "provenance.lat")

bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)


## make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$chill <- as.numeric(bb.wlab.sm$Total_Chilling_Hours)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)
bb.wlab.sm$lat<-as.numeric(bb.wlab.sm$provenance.lat)


## In case we decide to center data, not doing it for now
#bb.wlab.sm$photo.cen <- scale(bb.wlab.sm$photo, center=TRUE, scale=TRUE)
#bb.wlab.sm$force.cen <- scale(bb.wlab.sm$force, center=TRUE, scale=TRUE)
#bb.wlab.sm$chill.cen <- scale(bb.wlab.sm$chill, center=TRUE, scale=TRUE)


## subsetting data, preparing genus variable, removing NAs
ospr.prepdata <- subset(bb.wlab.sm, select=c("resp", "chill", "photo", "force", "complex", "lat"))
dim(subset(bb.wlab.sm, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE 
           & is.na(lat)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]
ospr.stan$complex <- as.numeric(as.factor(ospr.stan$complex))


# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
#photoperiod, and where we have a response in days and total chilling. There are NA in each 
#of these columns, including labgroup!

## remove NAs individually .... (not needed currently)
ospr.stan$resp<-ospr.stan$resp[which(is.na(ospr.stan$resp)==FALSE)]
ospr.stan$chill<-ospr.stan$chill[which(is.na(ospr.stan$chill)==FALSE)]
ospr.stan$force<-ospr.stan$force[which(is.na(ospr.stan$force)==FALSE)]
ospr.stan$photo<-ospr.stan$photo[which(is.na(ospr.stan$photo)==FALSE)]
ospr.stan$lat<-ospr.stan$lat[which(is.na(ospr.stan$lat)==FALSE)]
ospr.stan$complex<-ospr.stan$complex[which(is.na(ospr.stan$complex)==FALSE)]


y = ospr.stan$resp
chill = ospr.stan$chill
force = ospr.stan$force
photo = ospr.stan$photo
lat = ospr.stan$lat
sp = ospr.stan$complex
N = length(y)
n_sp = length(unique(sp))



## remove outliers: some values of y > 600 - maybe driving huge estimated values for alpha
chill = chill[which(y<300)]
force = force[which(y<300)]
photo = photo[which(y<300)]
sp = sp[which(y<300)]
lat = lat[which(y<300)]
y = y[which(y<300)]
N = length(y)
n_sp = length(unique(sp))


# making a list out of the processed data. It will be input for the model
datalist.td <- list(y=y,chill=chill, force=force,photo=photo,sp=sp,N=N,n_sp=n_sp, lat=lat)

# we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
datalist.td$chill<-datalist.td$chill/240


## real data with only experimental chilling (no field chilling)
#osp.td3 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td,
 #              iter = 2000,warmup=1500,control=list(adapt_delta=0.95)) 


##############################
###### real data all chilling
osp.td4 = stan('stan/lat/LAT_daysBBwinter_2level.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.99)) 

betas <- as.matrix(osp.td4, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","mu_b_lat_sp",
"b_force", "b_photo", "b_chill", "b_lat"))
mcmc_intervals(betas[,1:4])

launch_shinystan(osp.td4)
load("/Users/CatherineChamberlain/Downloads/shinystan-multiparam-gg.RData")
shinystan_multiparam_gg


#td4 <- summary(osp.td4)$summary
#preds.4<-td4[grep("yhat", rownames(td4)),]



######################################
###### real data all chilling sigmoid
osp.td5 = stan('stan/bb/M1_daysBBnointer_2level_interceptonly_sigmoid.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.95)) 

betas.td5 <- as.matrix(osp.td5, pars = c("b_force", "b_photo","a_chill", "b_chill"))
mcmc_intervals(betas[,1:5])

#td5 <- summary(osp.td5)$summary
#preds.5<-td5[grep("yhat", rownames(td5)),]



########### Running the models with fake data
#setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
source("bb_analysis/bb_testdata_generate.R")

# lme version
summary(lme1 <- lmer(bb ~ chill+force+photo + (1|sp), data = testdat)) 
ranef(lme1)
fixef(lme1)
#head(testdat)
#head(list.coeffs)

##
# try the model
datalist.td <- with(testdat, 
                    list(y = bb, 
                         chill = as.numeric(chill), 
                         force = as.numeric(force), 
                         photo = as.numeric(photo),
                         sp = as.numeric(sp),
                         N = nrow(testdat),
                         n_sp = length(unique(sp))
                    )
)



## running model with fake data
osp.td2 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td, 
             iter = 2000,warmup=1500,control=list(adapt_delta=0.90)) 

