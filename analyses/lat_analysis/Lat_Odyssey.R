### Looking at Lat Model in Odyssey

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(brms)

ospr.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/lat_wRibesandUlmus.csv", header=TRUE)

lat.inter_brm<-brm(resp~ force + photo + sm.chill + lat + sm.chill:lat + photo:lat + force:lat +
                     (force + photo + sm.chill + lat|sp), 
                   data=ospr.stan, warmup=2500,iter=4000, chains = 2, cores = 4,
                   control = list(max_treedepth = 12,adapt_delta = 0.99))

save(lat.inter_brm, file="/n/wolkovich_lab/Lab/Cat/lat_justlat.Rdata")

