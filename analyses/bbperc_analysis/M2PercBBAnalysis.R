## Started 7 July 2016 ##
## By Ailene, after modifying Dan and Lizzie's code ##
##updated 24 January 2017 by Ailene, to include new version of database
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)

setwd("~/git/ospree")
source('analyses/stan/savestan.R')
# get latest .Rdata file

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

ospree <- read.csv("analyses/output/ospree_clean_withchill.csv", header=TRUE)

percbbvars <- c("percentbudburst", "percentstage01", "percentstage02","percentstage03","percentstage04")

ospree.percbb <- ospree[which(ospree$respvar %in% percbbvars),]

dim(ospree)
dim(ospree.percbb) # 4003 rows

ospree.percbb$spp <- paste(ospree.percbb$genus, ospree.percbb$species, sep=".")

ospree.percbb <- subset(ospree.percbb, is.na(spp)==FALSE)

# deal with response vs. responsetime (quick fix for now)
resp1 <- subset(ospree.percbb, response==1) # most of are data are like this
resp1.timeNA <- subset(resp1, is.na(response.time)==TRUE) # about 20 rows have this

ospree.percbb$responsedays <- ospree.percbb$response.time
ospree.percbb$responsedays[which(ospree.percbb$response>1 & ospree.percbb$response.time=="")] <- ospree.percbb$response

# deal with NAs for now ...
ospr.stan <- ospree.percbb

ospr.stan$totalchill <- as.numeric(ospr.stan$Total_Chilling_Hours)
ospr.stan$forcetemp <- as.numeric(ospr.stan$forcetemp)
ospr.stan$photoperiod_day <- as.numeric(ospr.stan$photoperiod_day)
ospr.stan$spp <- as.numeric(as.factor(ospr.stan$spp))
ospr.stan$response <- as.numeric(ospr.stan$response)#this is percbudburst
ospr.stan$datasetID <- as.numeric(as.factor(ospr.stan$datasetID))
ospr.stan$responsedays <- as.numeric(ospr.stan$responsedays)

ospr.stan <- subset(ospr.stan, select=c("response","responsedays", "totalchill", "forcetemp", 
    "photoperiod_day", "spp", "datasetID"))

ospr.stan.noNA <- ospr.stan[complete.cases(ospr.stan),]

dim(ospr.stan.noNA)#1736 rows of data

datalist.real <- with(ospr.stan.noNA, 
    list(y = response, 
         days = as.numeric(responsedays), 
         chill = as.numeric(totalchill),  
         force = as.numeric(forcetemp), 
         photo = as.numeric(photoperiod_day), 
         sp = as.numeric(as.factor(spp)),
         study = as.numeric(as.factor(datasetID)),
         N = nrow(ospr.stan.noNA),
         n_sp = length(unique(spp)),
         n_study = length(unique(datasetID))
         )
)

if(dostan){
  osp.r <- stan('stan/ospree_percbb.stan', data = datalist.real, 
                 iter = 1666
                  ) 
  sf <- summary(osp.r)$summary
  
  ssm.r <- as.shinystan(osp.r)
  # launch_shinystan(ssm.f)
  savestan("real")
}
