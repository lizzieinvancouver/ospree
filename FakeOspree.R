# Fake data for Ospree stan work

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: same as in the meta-analysis. 100 species, ~ 20 labgroups as random effects. Expect labgroup to have minimal effect. Forcing, photoperiod, chilling, and latitude as drivers. Add the six 2-way interactions first, then do the 3-ways with latitude. 

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsp = 100
ngroup = 20
  
nforce = 2 # Within forcing treatments, continuous values for temperature
nphoto = 2
nchill = 2 
nlat = 1 # will be continuous random variable

(ntot = nforce*nphoto*nlat*nchill) # 8 row. Will be looping over species below

rep = 1 # replicates for how many times a species occurs

# Build up the data frame. use gl to generate levels, and subtract 1 to make them start at 0 (default is 1) if there are multiple levels

force = as.numeric(gl(nforce, rep, length = ntot))-1
photo = as.numeric(gl(nphoto, rep*nforce, length = ntot))-1
chill = as.numeric(gl(nchill, rep*nforce*nphoto, length = ntot))-1
lat = as.numeric(gl(nlat, rep, length = ntot))

treatcombo = paste(chill, force, photo, lat, sep = "_")

(d <- data.frame(chill, force, photo, lat, treatcombo))

###### Set up differences for each level
chilldiff = -20 # days earlier from 0 to 1
forcediff = -10
photodiff = -5
latcoef = 1 

######## SD for each treatment
chilldiff.sd = 1
forcediff.sd = 1 
photodiff.sd = 1
latcoeff.sd = 1

mm <- model.matrix(~(chill*force*photo*lat)^2, data.frame(chill, force, photo, lat))

#  with species

baseinter = 50 # baseline intercept across all species 
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

fake <- vector()

for(i in 1:nsp){ # loop over species

  coeff <- c(spint[i], 
             rnorm(1, chilldiff, chilldiff.sd),
             rnorm(1, forcediff, forcediff.sd),
             rnorm(1, photodiff, photodiff.sd),
  )
  
  bb <- rnorm(n = length(force), mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(bb, sp = i, ind = paste(i, j, sep="_"),
                      force, photo)
  
  fake <- rbind(fake, fakex)  
  }
  }

summary(lm(bb ~ (force+photo)^2, data = fake)) # sanity check. No interaction

save(list=c("fake"), file = "Fake_Budburst_ind2.RData")




