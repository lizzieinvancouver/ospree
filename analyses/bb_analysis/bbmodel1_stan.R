## Started 6 July 2016 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

## Take 2: February 2017! ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
  } else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
labgroups <- read.csv("output/labgroups.csv", header=TRUE)

# merge in labgroup (we could do this elsewhere someday
bb.wlab <- merge(bb, labgroups, by="datasetID", all.x=TRUE)

columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
    "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", "cat")

bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)

# make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$chill <- as.numeric(bb.wlab.sm$Total_Chilling_Hours)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)

bb.wlab.sm$photo.cen <- scale(bb.wlab.sm$photo, center=TRUE, scale=TRUE)
bb.wlab.sm$force.cen <- scale(bb.wlab.sm$force, center=TRUE, scale=TRUE)
bb.wlab.sm$chill.cen <- scale(bb.wlab.sm$chill, center=TRUE, scale=TRUE)

# deal with NAs for now ...
ospr.prepdata <- subset(bb.wlab.sm, select=c("resp", "chill.cen", "photo.cen", "force.cen", "genus"))
dim(subset(bb.wlab.sm, is.na(chill.cen)==FALSE & is.na(photo.cen)==FALSE & is.na(force.cen)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]
ospr.stan$genus <- as.numeric(as.factor(ospr.stan$genus))

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, photoperiod, and where we have a response in days and total chilling. There are NA in each of these columns, including labgroup!

## remove NAs individually .... (not needed currently)
y = ospr.stan$resp[which(is.na(ospr.stan$resp)==FALSE)]
chill = ospr.stan$chill[which(is.na(ospr.stan$chill)==FALSE)]
force = ospr.stan$force[which(is.na(ospr.stan$force)==FALSE)]
photo = ospr.stan$photo[which(is.na(ospr.stan$photo)==FALSE)]
sp = ospr.stan$genus[which(is.na(ospr.stan$genus)==FALSE)]
N = length(y)
n_sp = length(unique(sp))

m1.bb <- stan('stan/M1_daysBB_2level.stan', data = c("y", "chill", "force",
    "photo", "sp", "N", "n_sp"), iter = 2000, chains=4) 

###
## older code
###
ospr.stan <- ospree.bb.lab
ospr.stan.noNA <- ospr.stan[complete.cases(ospr.stan),] # 1234 rows ....

dim(ospr.stan.noNA)

datalist.real <- with(ospr.stan.noNA, 
    list(y = responsedays, 
         chill = as.numeric(totalchill),  
         force = as.numeric(forcetemp), 
         photo = as.numeric(photoperiod_day), 
         lat = as.numeric(provenance.lat), 
         sp = as.numeric(as.factor(spp)),
        #  lab = as.numeric(as.factor(labgroup)),
         N = nrow(ospr.stan.noNA),
         n_sp = length(unique(spp)) # CHECK: that this agrees with sp just above
       #  n_lab = length(unique(labgroup))
         )
)

if(dostan){
  osp.r <- stan('stan/M1_daysBB_simple_winter.stan', data = datalist.real, 
                 iter = 3123
                  ) 
  sf <- summary(osp.r)$summary
  
  ssm.r <- as.shinystan(osp.r)
  # launch_shinystan(ssm.r)
  savestan("real")
}


## try the model without latitude
# ... though I am not sure that's the problem
# and now that I have run the model I don't think it is ... 

# deal with NAs for now ...
ospr.stan <- ospree.bb.lab

ospr.stan$totalchill <- as.numeric(ospr.stan$Total_Chilling_Hours)
ospr.stan$forcetemp <- as.numeric(ospr.stan$forcetemp)
ospr.stan$photoperiod_day <- as.numeric(ospr.stan$photoperiod_day)
ospr.stan$spp <- as.numeric(as.factor(ospr.stan$spp))
ospr.stan$responsedays <- as.numeric(ospr.stan$responsedays)

ospr.stan <- subset(ospr.stan, select=c("responsedays", "totalchill", "forcetemp", 
    "photoperiod_day", "spp", "labgroup"))

ospr.stan.noNA <- ospr.stan[complete.cases(ospr.stan),]

dim(ospr.stan.noNA)

datalist.real <- with(ospr.stan.noNA, 
    list(y = responsedays, 
         chill = as.numeric(totalchill),  
         force = as.numeric(forcetemp), 
         photo = as.numeric(photoperiod_day), 
         sp = as.numeric(as.factor(spp)),
         lab = as.numeric(as.factor(labgroup)),
         N = nrow(ospr.stan.noNA),
         n_sp = length(unique(spp)),
         n_lab = length(unique(labgroup))
         )
)

if(dostan){
  osp.r.nolat <- stan('stan/ospreeM1_nolat.stan', data = datalist.real, 
                 iter = 3003
                  ) 
  sf <- summary(osp.r.nolat)$summary
  
  ssm.r <- as.shinystan(osp.r.nolat)
  # launch_shinystan(osp.r.nolat)
  savestan("real.nolat")
}
