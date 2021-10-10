if(SLA){
  SLAData <- traitOspree[!is.na(traitOspree$SLA),]
  Nspp <- length(unique(SLAData$spps))
  N <- nrow(SLAData)
  species <- as.integer(as.factor(SLAData$spps))
  
  #Cues
  force.i <- SLAData$force.z
  photo.i <- SLAData$photo.z 
  chill.i <- SLAData$chill.z 
  
  #budburst date
  yPhenoi <- SLAData$resp
  
  ## pehnology and mean trait stan model ###########################################################
  pheno_data <- list(alphaTraitSp = SLAData$SLA, #mean species trait value 
                     Nph = N, 
                     n_spec = Nspp, 
                     species = species, 
                     yPhenoi = yPhenoi, 
                     forcei = force.i,
                     photoi = photo.i, 
                     chilli = chill.i,
                     #Priors
                     prior_sigmaphenoy_mu =20,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                     prior_sigmaphenoy_sigma = 5, # variance of the prior distribution of the general error sigma)y around the mean predicted value
                     
                     prior_muForceSp_mu = 0, # mean of the prior distribution of the mean effect of forcing 
                     prior_muForceSp_sigma = 5, # vareince of the prior distributionof the mean effect of forcing 
                     prior_sigmaForceSp_mu = 5, # mean of the prior distribution of the varience around the mean effect of forcing 
                     prior_sigmaForceSp_sigma = 5,# variance of the prior distribution of the varience around the mean effect of forcing ,
                     
                     prior_muChillSp_mu = 0,# mean of the prior distribution of the mean effect of chilling 
                     prior_muChillSp_sigma = 5,# varience of the prior distribution of the mean effect of chilling 
                     prior_sigmaChillSp_mu = 5,# mean of the prior distribution of the varience around the mean effect of chilling 
                     prior_sigmaChillSp_sigma= 5, #variance of the prior distribution of the varience around the mean effect of chilling
                     
                     prior_muPhotoSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_muPhotoSp_sigma = 5,# varience of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_sigmaPhotoSp_mu=5,# mean of the prior distribution of the varience around the mean effect of photoperiod
                     prior_sigmaPhotoSp_sigma=5,#variance of the prior distribution of the varience around the mean effect of photoperiod
                     
                     prior_muPhenoSp_mu = 150, # mean of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_muPhenoSp_sigma = 10, # variance of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_sigmaPhenoSp_mu = 0,#the mean of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     prior_sigmaPhenoSp_sigma = 10,  #the varience of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     #prior_sigma_sp_sigma = 10,  # Faith doesn't knwo what these might
                     #prior_mu_study = 0,
                     #prior_sigma_study_mu = 10,
                     #prior_sigma_study_sigma = 10,
                     
                     prior_betaTraitxForce_mu=0, # the mean of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxForce_sigma=1, # the varience of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxChill_mu=0,# the mean of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxChill_sigma=1,# the varience of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxPhoto_mu=0,# the mean of the prior distribution of the effect of trait on the slope of photo period 
                     prior_betaTraitxPhoto_sigma=1# the varience of the prior distribution of the effect of trait on the slope of photo period 
  ) 
} 

######################################################################################
if(Seed){
  seedData <- traitOspree[!is.na(traitOspree$Seed_mass),]
  Nspp <- length(unique(seedData$spps))
  N <- nrow(seedData)
  species <- as.integer(as.factor(seedData$spps))
  
  #Cues
  force.i <- seedData$force.z
  photo.i <- seedData$photo.z 
  chill.i <- seedData$chill.z 
  
  #budburst date
  yPhenoi <- seedData$resp
  
  ## pehnology and mean trait stan model ######
  pheno_data <- list(alphaTraitSp = seedData$Seed_mass, #mean species trait value 
                     Nph = N, 
                     n_spec = Nspp, 
                     species = species, 
                     yPhenoi = yPhenoi, 
                     forcei = force.i,
                     photoi = photo.i, 
                     chilli = chill.i,
                     #Priors
                     prior_sigmaphenoy_mu = 2000,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                     prior_sigmaphenoy_sigma = 500, # variance of the prior distribution of the general error sigma)y around the mean predicted value
                     
                     prior_muForceSp_mu = 0, # mean of the prior distribution of the mean effect of forcing 
                     prior_muForceSp_sigma = 5, # vareince of the prior distributionof the mean effect of forcing 
                     prior_sigmaForceSp_mu = 5, # mean of the prior distribution of the varience around the mean effect of forcing 
                     prior_sigmaForceSp_sigma = 5,# variance of the prior distribution of the varience around the mean effect of forcing ,
                     
                     prior_muChillSp_mu = 0,# mean of the prior distribution of the mean effect of chilling 
                     prior_muChillSp_sigma = 5,# varience of the prior distribution of the mean effect of chilling 
                     prior_sigmaChillSp_mu = 5,# mean of the prior distribution of the varience around the mean effect of chilling 
                     prior_sigmaChillSp_sigma= 5, #variance of the prior distribution of the varience around the mean effect of chilling
                     
                     prior_muPhotoSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_muPhotoSp_sigma = 5,# varience of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_sigmaPhotoSp_mu=5,# mean of the prior distribution of the varience around the mean effect of photoperiod
                     prior_sigmaPhotoSp_sigma=5,#variance of the prior distribution of the varience around the mean effect of photoperiod
                     
                     prior_muPhenoSp_mu = 150, # mean of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_muPhenoSp_sigma = 10, # variance of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_sigmaPhenoSp_mu = 0,#the mean of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     prior_sigmaPhenoSp_sigma = 10,  #the varience of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     #prior_sigma_sp_sigma = 10,  # Faith doesn't knwo what these might
                     #prior_mu_study = 0,
                     #prior_sigma_study_mu = 10,
                     #prior_sigma_study_sigma = 10,
                     
                     prior_betaTraitxForce_mu=0, # the mean of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxForce_sigma=1, # the varience of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxChill_mu=0,# the mean of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxChill_sigma=1,# the varience of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxPhoto_mu=0,# the mean of the prior distribution of the effect of trait on the slope of photo period 
                     prior_betaTraitxPhoto_sigma=1# the varience of the prior distribution of the effect of trait on the slope of photo period 
  ) 
} 

######################################################################################
if(LNC){
  LNCData <- traitOspree[!is.na(traitOspree$LNC),]
  Nspp <- length(unique(LNCData$spps))
  N <- nrow(LNCData)
  species <- as.integer(as.factor(LNCData$spps))
  
  #Cues
  force.i <- LNCData$force.z
  photo.i <- LNCData$photo.z 
  chill.i <- LNCData$chill.z 
  
  #budburst date
  yPhenoi <- LNCData$resp
  
  ## pehnology and mean trait stan model ######
  pheno_data <- list(alphaTraitSp = LNCData$LNC, #mean species trait value 
                     Nph = N, 
                     n_spec = Nspp, 
                     species = species, 
                     yPhenoi = yPhenoi, 
                     forcei = force.i,
                     photoi = photo.i, 
                     chilli = chill.i,
                     #Priors
                     prior_sigmaphenoy_mu =20,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                     prior_sigmaphenoy_sigma = 5, # variance of the prior distribution of the general error sigma)y around the mean predicted value
                     
                     prior_muForceSp_mu = 0, # mean of the prior distribution of the mean effect of forcing 
                     prior_muForceSp_sigma = 5, # vareince of the prior distributionof the mean effect of forcing 
                     prior_sigmaForceSp_mu = 5, # mean of the prior distribution of the varience around the mean effect of forcing 
                     prior_sigmaForceSp_sigma = 5,# variance of the prior distribution of the varience around the mean effect of forcing ,
                     
                     prior_muChillSp_mu = 0,# mean of the prior distribution of the mean effect of chilling 
                     prior_muChillSp_sigma = 5,# varience of the prior distribution of the mean effect of chilling 
                     prior_sigmaChillSp_mu = 5,# mean of the prior distribution of the varience around the mean effect of chilling 
                     prior_sigmaChillSp_sigma= 5, #variance of the prior distribution of the varience around the mean effect of chilling
                     
                     prior_muPhotoSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_muPhotoSp_sigma = 5,# varience of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_sigmaPhotoSp_mu=5,# mean of the prior distribution of the varience around the mean effect of photoperiod
                     prior_sigmaPhotoSp_sigma=5,#variance of the prior distribution of the varience around the mean effect of photoperiod
                     
                     prior_muPhenoSp_mu = 150, # mean of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_muPhenoSp_sigma = 10, # variance of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_sigmaPhenoSp_mu = 0,#the mean of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     prior_sigmaPhenoSp_sigma = 10,  #the varience of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     #prior_sigma_sp_sigma = 10,  # Faith doesn't knwo what these might
                     #prior_mu_study = 0,
                     #prior_sigma_study_mu = 10,
                     #prior_sigma_study_sigma = 10,
                     
                     prior_betaTraitxForce_mu=0, # the mean of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxForce_sigma=1, # the varience of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxChill_mu=0,# the mean of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxChill_sigma=1,# the varience of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxPhoto_mu=0,# the mean of the prior distribution of the effect of trait on the slope of photo period 
                     prior_betaTraitxPhoto_sigma=1# the varience of the prior distribution of the effect of trait on the slope of photo period 
  ) 
} 

######################################################################################
if(SSD){
  SSDData <- traitOspree[!is.na(traitOspree$SSD),]
  Nspp <- length(unique(SSDData$spps))
  N <- nrow(SSDData)
  species <- as.integer(as.factor(SSDData$spps))
  
  #Cues
  force.i <- SSDData$force.z
  photo.i <- SSDData$photo.z 
  chill.i <- SSDData$chill.z 
  
  #budburst date
  yPhenoi <- SSDData$resp
  
  ## pehnology and mean trait stan model ######
  pheno_data <- list(alphaTraitSp = SSDData$SSD, #mean species trait value 
                     Nph = N, 
                     n_spec = Nspp, 
                     species = species, 
                     yPhenoi = yPhenoi, 
                     forcei = force.i,
                     photoi = photo.i, 
                     chilli = chill.i,
                     #Priors
                     prior_sigmaphenoy_mu =0.2,  #mean of prior distribution of the general error (sigma_y) around the mean predicted value
                     prior_sigmaphenoy_sigma = 0.5, # variance of the prior distribution of the general error sigma)y around the mean predicted value
                     
                     prior_muForceSp_mu = 0, # mean of the prior distribution of the mean effect of forcing 
                     prior_muForceSp_sigma = 5, # vareince of the prior distributionof the mean effect of forcing 
                     prior_sigmaForceSp_mu = 5, # mean of the prior distribution of the varience around the mean effect of forcing 
                     prior_sigmaForceSp_sigma = 5,# variance of the prior distribution of the varience around the mean effect of forcing ,
                     
                     prior_muChillSp_mu = 0,# mean of the prior distribution of the mean effect of chilling 
                     prior_muChillSp_sigma = 5,# varience of the prior distribution of the mean effect of chilling 
                     prior_sigmaChillSp_mu = 5,# mean of the prior distribution of the varience around the mean effect of chilling 
                     prior_sigmaChillSp_sigma= 5, #variance of the prior distribution of the varience around the mean effect of chilling
                     
                     prior_muPhotoSp_mu = 0,# mean of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_muPhotoSp_sigma = 5,# varience of the prior distribution of the varience around the mean effect of photoperiod 
                     prior_sigmaPhotoSp_mu=5,# mean of the prior distribution of the varience around the mean effect of photoperiod
                     prior_sigmaPhotoSp_sigma=5,#variance of the prior distribution of the varience around the mean effect of photoperiod
                     
                     prior_muPhenoSp_mu = 150, # mean of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_muPhenoSp_sigma = 10, # variance of prior distribution of the mean (grand alpha) value of the phenology model
                     prior_sigmaPhenoSp_mu = 0,#the mean of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     prior_sigmaPhenoSp_sigma = 10,  #the varience of the prior of the spread of species phenology values around teh grand mean muPhenoSp 
                     #prior_sigma_sp_sigma = 10,  # Faith doesn't knwo what these might
                     #prior_mu_study = 0,
                     #prior_sigma_study_mu = 10,
                     #prior_sigma_study_sigma = 10,
                     
                     prior_betaTraitxForce_mu=0, # the mean of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxForce_sigma=1, # the varience of the prior distribution of the effect of trait on the slope of forcing 
                     prior_betaTraitxChill_mu=0,# the mean of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxChill_sigma=1,# the varience of the prior distribution of the effect of trait on the slope of chilling 
                     prior_betaTraitxPhoto_mu=0,# the mean of the prior distribution of the effect of trait on the slope of photo period 
                     prior_betaTraitxPhoto_sigma=1# the varience of the prior distribution of the effect of trait on the slope of photo period 
  ) 
} 