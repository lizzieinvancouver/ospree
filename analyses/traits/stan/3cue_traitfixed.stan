
data {
  int < lower = 1 > n_spec; // number of random effect levels (species) 
  int < lower = 1 > Nph; // Sample size for forcing 
  int < lower = 1, upper = n_spec > species[Nph]; // id of random effect (species)
  vector[Nph] yTrait; // Trait value
  //MODEL 2 ------------------------------------------------
  vector[Nph] yPhenoi; // Outcome phenology
  vector[Nph] forcei; // predictor forcing 
  vector[Nph] chilli; // predictor chilling
  vector[Nph] photoi; // predictor photoperiod

  real prior_muForceSp_mu;
  real prior_muForceSp_sigma;
  real prior_muChillSp_mu;
  real prior_muChillSp_sigma;
  real prior_muPhotoSp_mu;
  real prior_muPhotoSp_sigma;
  real prior_muPhenoSp_mu;
  real prior_muPhenoSp_sigma;
  real prior_sigmaForceSp_mu;
  real prior_sigmaForceSp_sigma;
  real prior_sigmaChillSp_mu;
  real prior_sigmaChillSp_sigma;
  real prior_sigmaPhotoSp_mu;
  real prior_sigmaPhotoSp_sigma;
  real prior_sigmaPhenoSp_mu;
  real prior_sigmaPhenoSp_sigma;
  real prior_betaTraitxForce_mu;
  real prior_betaTraitxForce_sigma;
  real prior_betaTraitxChill_mu;
  real prior_betaTraitxChill_sigma;
  real prior_betaTraitxPhoto_mu;
  real prior_betaTraitxPhoto_sigma;
  real prior_sigmaphenoy_mu;
  real prior_sigmaphenoy_sigma;
}

parameters{
  //MODEL 2 -----------------------------------------------------
  //level 2
    
  real alphaForceSp[n_spec]; //the distribution of species forcing values 
  real muForceSp; // the mean of the effect of forcing
  real <lower = 0> sigmaForceSp; //variation around the mean of the effect of forcing 
    
  real alphaChillSp[n_spec]; //the distribution of species chilling values
  real muChillSp; // the mean of the effect of chilling
  real <lower = 0> sigmaChillSp; //variation around the mean of the effect of chilling

  real alphaPhotoSp[n_spec]; //the distribution of species photoperiod values
  real muPhotoSp; // the mean of the effect of photoperiod
  real <lower = 0> sigmaPhotoSp; //variation around the mean of the effect of photo

  real alphaPhenoSp[n_spec]; //the species level intercept 
  real muPhenoSp; // 
  real <lower = 0> sigmaPhenoSp; 

  real betaTraitxForce; 
  real betaTraitxChill;
  real betaTraitxPhoto;
  // general varience/error
  real <lower =0> sigmapheno_y; // overall variation accross observations
}

transformed parameters{
  //MODEL 2------------------------------------------------
  real betaForceSp[n_spec];     //species level beta forcing 
  real betaPhotoSp[n_spec];     //species level beta photoperiod
  real betaChillSp[n_spec];     //species level beta chilling

  //MODEL 2----------------------------------------
  //get beta-cue-Sp values for each species
  for (isp in 1:n_spec){
    betaForceSp[isp] = alphaForceSp[isp] + betaTraitxForce * (yTrait[isp]);
  }
    
  for (isp in 1:n_spec){
    betaPhotoSp[isp] = alphaPhotoSp[isp] + betaTraitxPhoto* (yTrait[isp]);
  }

  for (isp in 1:n_spec){
    betaChillSp[isp] = alphaChillSp[isp] + betaTraitxChill* (yTrait[isp]);
  }

}

model{ 
  //MODEL 2 -----------------------------------------------
  //likelihood 
  for (i in 1:Nph){
    yPhenoi[i] ~ normal(alphaPhenoSp[species[i]] + betaForceSp[species[i]] * forcei[i] + betaPhotoSp[species[i]] * photoi[i] + betaChillSp[species[i]] * chilli[i], sigmapheno_y);
  }

  //priors - level 1
  sigmapheno_y ~ normal(prior_sigmaphenoy_mu,  prior_sigmaphenoy_sigma); // Aug 13, lizzie suggested 1 or 0.5 instead

  //priors level 2
  alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp);//
  muPhenoSp ~ normal(prior_muPhenoSp_mu, prior_muPhenoSp_sigma);  // mu.pheno.sp = 150
  sigmaPhenoSp ~ normal(prior_sigmaPhenoSp_mu,prior_sigmaPhenoSp_sigma); // sigma.pheno.sp =2  

  alphaForceSp ~ normal(muForceSp, sigmaForceSp);  //
  muForceSp ~ normal(prior_muForceSp_mu,  prior_muForceSp_sigma);//
  sigmaForceSp ~ normal(prior_sigmaForceSp_mu,prior_sigmaForceSp_sigma); //
    
  alphaChillSp ~ normal(muChillSp, sigmaChillSp);  //
  muChillSp ~ normal(prior_muChillSp_mu,prior_muChillSp_sigma);//
  sigmaChillSp ~ normal(prior_sigmaChillSp_mu,prior_sigmaChillSp_sigma); //sigma.chill.sp
   
  alphaPhotoSp ~ normal(muPhotoSp, sigmaPhotoSp);  //
  muPhotoSp ~ normal(prior_muPhotoSp_mu,prior_muPhotoSp_sigma);//
  sigmaPhotoSp ~ normal(prior_sigmaPhotoSp_mu,prior_sigmaPhotoSp_sigma); //sigma.photo.sp
        
  betaTraitxForce ~ normal(prior_betaTraitxForce_mu, prior_betaTraitxForce_sigma); // Aug 5: widen these and see what the impact is
  betaTraitxPhoto ~ normal(prior_betaTraitxPhoto_mu, prior_betaTraitxPhoto_sigma); // # this was 0.5 at first and
  betaTraitxChill ~ normal(prior_betaTraitxChill_mu, prior_betaTraitxChill_sigma); 

}


generated quantities {
} // The posterior predictive distribution


//generated quantities {
//    real y_pred[N];
//    for (i in 1:N) {
//        y_pred[i] = normal_rng(ymu, sigma);
 //   }
    
//    # could have poster for both the trait and the phenology model with two loops in the //generated quantities with different parameters
 //   # could make a 3-D plot, and do indiv comparison, comparing the real trait data to ypre data
//} // The posterior predictive distribution
