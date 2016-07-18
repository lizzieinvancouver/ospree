# Fake data for Ospree stan work

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: same as in the meta-analysis. 100 species, ~ 20 labgroups as random effects. Expect labgroup to have minimal effect. Forcing, photoperiod, chilling, and latitude as drivers. Add the six 2-way interactions first, then do the 3-ways with latitude. 

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
library(lme4)

nsp = 100
nlab = 20

ntot = 50 # Values per species. 

#  with species

baseinter = 80 # baseline intercept across all species. Days to budburst (assume Julian days)
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

fake <- vector()

for(i in 1:nsp){ # loop over species. i = 1

# Build up the data frame. If we had factors (as in, an experimental design with definied treatment levels), would use gl to generate levels, and subtract 1 to make them start at 0 (default is 1) if there are multiple levels. For continuous predictors here, use rnorm. Easiest way to see if the input coefficients are working is to use mean 0, sd 1 in the predictors (as in, scaled predictors).
  
  force = rnorm(ntot, 0, 1) # continuous predictor. Uncorrelated with each other
  photo = rnorm(ntot, 0, 1)
  chill = rnorm(ntot, 0, 1)
  lat = rnorm(ntot, 0, 1)
  
  ###### Set up differences for each level
  chillcoef = -3 # steep slope for earlier day with higher chilling
  forcecoef = -2 # less steep for forcing
  photocoef = -1
  latcoef = 1 # perhaps delayed if populations from higher latitude
  chillforce = 0.5 # exchangeability between chilling and forcing
  chillphoto = 0.3
  chilllat = -0.3 # chilling effect strengthened at higher latitudes perhaps
  forcephoto = 0.1
  forcelat = 0.1 
  photolat = 0.1
  
  ######## SD for each treatment, can change these as necessary. 
  chillcoef.sd = 1
  forcecoef.sd = 0.5 
  photocoef.sd = 0.1
  latcoeff.sd = 1
  chillforce.sd = 0.5
  chillphoto.sd = 0.5
  chilllat.sd = 0.5
  forcephoto.sd = 0.5
  forcelat.sd = 0.5
  photolat.sd = 0.5
  
  # Make model matrix. Limit to two-way interactions with + between predictors, and ( )^2 surrounding the term
  
  mm <- model.matrix(~(chill+force+photo+lat)^2, data.frame(chill, force, photo, lat))
  
  
  # Coefficients need to match the order of the colums in the model matrix, mm. First one is the intercept
  coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd),
             rnorm(1, latcoef, latcoeff.sd), 
             rnorm(1, chillforce, chillforce.sd), 
             rnorm(1, chillphoto, chillphoto.sd), 
             rnorm(1, chilllat, chilllat.sd), 
             rnorm(1, forcephoto, forcephoto.sd), 
             rnorm(1, forcelat, forcelat.sd), 
             rnorm(1, photolat, photolat.sd)
             )
  
  bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(bb, sp = i, 
                      chill, force, photo, lat)
  
  fake <- rbind(fake, fakex)  
}

# now modify the intercepts with the lab group values (should be a small effect)
labint <- c(1:nlab)-mean(1:nlab) # different intercepts by lab group

# Make groups of papers at random
fake$lab = sample(cut(1:nrow(fake), nlab, labels = 1:nlab))

# now modify the y values by the lab group intercept
for(i in 1:nlab){
  fake[fake$lab == i,"bb"] <- fake[fake$lab == i,"bb"]+labint[i]
  }

ggplot(fake, aes(chill, bb, color = as.factor(sp))) + geom_point() + geom_smooth(method = 'lm', se=F)

ggplot(fake, aes(force, bb, color = as.factor(lab))) + geom_point() + geom_smooth(method = 'lm', se=F)
ggplot(fake, aes(force, bb, color = as.factor(sp))) + geom_point() + geom_smooth(method = 'lm', se=F)

# Not quite right, because missing species and lab group
summary(lm1 <- lm(bb ~ (chill+force+photo+lat)^2, data = fake)) # sanity check. 

# Fixed effects should be very close to coefficients used in simulating data
summary(lme1 <- lmer(bb ~ (chill+force+photo+lat)^2 + (1|sp) + (1|lab), data = fake)) 
ranef(lme1)
fixef(lme1)

summary(lme2 <- lmer(bb ~ (chill+force+photo+lat)^2 + (1|sp), data = fake)) 
ranef(lme2)


AIC(lm1, lme1, lme2) # full mixed effect model much better, good!


save(list=c("fake"), file = "FakeOspree.RData")


