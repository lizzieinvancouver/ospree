### Looking at Lat Model in Odyssey

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstanarm)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

lat.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/lat_arm.csv", header=TRUE)

lat.mod_arm <- stan_glmer(resp ~ (force + photo + chill +
                                    force:photo + force:chill + photo:chill + force:lat + 
                                    photo:lat + chill:lat)+ 
                            ((force + photo + chill +
                                force:photo + force:chill + photo:chill + force:lat + 
                                photo:lat + chill:lat)|complex), data = lat.stan,
                          chains = 4, cores = 4,control = list(max_treedepth = 12,adapt_delta = 0.99))

save(lat.mod_arm, file="/n/wolkovich_lab/Lab/Cat/lat_all_nocen.Rdata")

