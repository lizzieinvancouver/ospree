### Looking at Lat Model in Odyssey
library(rstanarm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

lat.stan<-read.csv("/n/wolkovich_lab/Lab/Cat/lat_arm.csv", header=TRUE)

lat.stan<-subset(lat.stan, lat.stan$resp<600)

lat.cen <- stan_glmer(resp ~ (force.z + photo.z + chill.z + lat.z +
                             force.z:photo.z + force.z:chill.z + photo.z:chill.z + force.z:lat.z + 
                             photo.z:lat.z + chill.z:lat.z)+ 
                     ((force.z + photo.z + chill.z + lat.z +
                         force.z:photo.z + force.z:chill.z + photo.z:chill.z + force.z:lat.z + 
                         photo.z:lat.z + chill.z:lat.z)|complex), data = lat.stan,
                   chains = 4, cores = 4,control = list(max_treedepth = 12,adapt_delta = 0.99))

save(lat.cen, file="/n/wolkovich_lab/Lab/Cat/lat_cen.Rdata")

lat.nocen <- stan_glmer(resp ~ (force + photo + chill + lat +
                         force:photo + force:chill + photo:chill + force:lat + 
                         photo:lat + chill:lat)+ 
                 ((force + photo + chill + lat +
                     force:photo + force:chill + photo:chill + force:lat + 
                     photo:lat + chill:lat)|complex), data = lat.stan,
               chains = 4, cores = 4,control = list(max_treedepth = 12,adapt_delta = 0.99))

save(lat.nocen, file="/n/wolkovich_lab/Lab/Cat/lat_nocen.Rdata")


lat.nocen.nochill <- stan_glmer(resp ~ (force + photo + chill + lat +
                           force:photo + force:chill + photo:chill + force:lat + 
                           photo:lat)+ ((force + photo + chill + lat +
                       force:photo + force:chill + photo:chill + force:lat + 
                       photo:lat)|complex), data = lat.stan,
                 chains = 4, cores = 4,control = list(max_treedepth = 12,adapt_delta = 0.99))

save(lat.nocen.nochill, file="/n/wolkovich_lab/Lab/Cat/lat_nocen_nochill.Rdata")


