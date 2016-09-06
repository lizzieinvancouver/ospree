## Started 6 July 2016 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
  } else setwd("~/Documents/git/projects/treegarden/budreview/budreview/")

source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

ospree <- read.csv("input/ospree_clean_withchill.csv", header=TRUE)

bbvars <- c("daystobudburst", "daysto50percentbudburst", "daysto20%budburst",
    "daysto10percentbudburst","daysto50%budburst", "daystoleafout")

ospree.bb <- ospree[which(ospree$respvar %in% bbvars),]

dim(ospree)
dim(ospree.bb) # not so much data, 2392 rows if just do days to budburst

kmeans <- read.csv("refs/kmeans6.csv", header=TRUE, skip=1,
    col.names=c("datID", "labgroup"))

ospree.bb.lab <- merge(ospree.bb, kmeans, by.x="datasetID",
   by.y="datID", all.x=TRUE)

ospree.bb.lab$spp <- paste(ospree.bb.lab$genus, ospree.bb.lab$species, sep="")

ospree.bb.lab <- subset(ospree.bb.lab, is.na(spp)==FALSE)

# deal with response vs. responsetime (quick fix for now)
resp1 <- subset(ospree.bb.lab, response==1) # most of are data is like this
resp1.timeNA <- subset(resp1, is.na(response.time)==TRUE) # about 20 rows have this

ospree.bb.lab$responsedays <- ospree.bb.lab$response.time
ospree.bb.lab$responsedays[which(ospree.bb.lab$response>1 & ospree.bb.lab$response.time=="")] <- ospree.bb.lab$response

# just to think on ...
ospree.bb.lab[which(ospree.bb.lab$response>1),28:31]

# deal with NAs for now ...
ospr.stan <- ospree.bb.lab

ospr.stan$totalchill <- as.numeric(ospr.stan$Total_Chilling_Hours) 
ospr.stan$forcetemp <- as.numeric(ospr.stan$forcetemp)
ospr.stan$photoperiod_day <- as.numeric(ospr.stan$photoperiod_day)
ospr.stan$provenance.lat <- as.numeric(ospr.stan$provenance.lat)
ospr.stan$spp <- as.numeric(as.factor(ospr.stan$spp))
ospr.stan$responsedays <- as.numeric(ospr.stan$responsedays)

ospr.stan <- subset(ospr.stan, select=c("responsedays", "totalchill", "forcetemp", 
    "photoperiod_day", "provenance.lat", "spp"))

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, photoperiod, and where we have a response in days and total chilling. There are NA in each of these columns, including labgroup!

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
         n_sp = length(unique(spp))
       #  n_lab = length(unique(labgroup))
         )
)

if(dostan){
  osp.r <- stan('stan/ospreeM4.stan', data = datalist.real, 
                 iter = 3333
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
