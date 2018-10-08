## Started 6 July 2017 ##
## By Cat and Dan and others ##
# Updated a tiny bit by Dan 19 June 2018
# Updated 8 Oct 2018
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/lat_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/lat_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/lat_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/lat_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/lat_analysis")

library(rstanarm)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source/bbstanleadin.R")

bb.wlab <- bb
bb.wlab <- within(bb.wlab, { prov.lat <- ave(lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
bb.wlab <- subset(bb.wlab, bb.wlab$prov.lat>1) 
#bb.wlab.photo<- within(bb.wlab, { photo <- ave(photoperiod_day, complex, FUN=function(x) length(unique(x)))}) # multiple photoperiods
#bb.wlab.photo <- subset(bb.wlab.photo, bb.wlab.photo$photo>1) 
tt <- table(bb.wlab$complex)### testing 
#bb.wlab<-bb.wlab.photo

myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies", "Malus_domestica", "Ribes_nigrum", "Ulmus_complex")
bb.wlab<-subset(bb.wlab, complex%in%myspp)

lat.stan <- subset(bb.wlab, select=c(columnstokeep, "chill.cen", "photo.cen", "force.cen", "lat.cen",
                                     "force.z","chill.z", "photo.z", "lat.z"))

#write.csv(lat.stan, "lat_output/lat_arm.csv", row.names = FALSE)


lat.mod_arm <- stan_glmer(resp ~ (force + photo + chill +#main effects
                                force:photo + force:chill + photo:chill + force:lat + 
                                  photo:lat + chill:lat)+ #interactions
                        ((force + photo + chill +#main effects
                            force:photo + force:chill + photo:chill + force:lat + 
                            photo:lat + chill:lat)|complex), data = lat.stan,
                      chains = 4, cores = 4,control = list(max_treedepth = 12,adapt_delta = 0.99))

