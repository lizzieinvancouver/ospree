
data {
  int<lower = 1> n_spec; // number of random effect levels (species) 
  // Traits
  int<lower = 1> N; // Sample size for trait data 
  int<lower = 1, upper = n_spec> trait_species[N]; // id of random effect (species)
  int<lower = 1> n_study; // number of random effect levels (study) 
  int<lower = 1, upper = n_study> study[N]; // id of random effect (study)
  vector[N] yTraiti; // Observed trait
  //Priors
  // real prior_mu_grand_mu; 
  // real prior_mu_grand_sigma;
  // real prior_sigma_sp_mu;
  // real prior_sigma_sp_sigma;
  // real prior_sigma_study_mu;
  // real prior_sigma_study_sigma;
  // real prior_sigma_traity_mu;
  // real prior_sigma_traity_sigma;
  // Phenology
  int<lower = 1> Nph; // Sample size for phenology data
  int<lower = 1, upper = n_spec> phenology_species[Nph]; // id of random effect (species)
  vector[Nph] yPhenoi; // Outcome phenology
  vector[Nph] forcei; // predictor forcing 
  vector[Nph] chilli; // predictor chilling
  vector[Nph] photoi; // predictor photoperiod
  // real prior_muForceSp_mu;
  // real prior_muForceSp_sigma;
  // real prior_muChillSp_mu;
  // real prior_muChillSp_sigma;
  // real prior_muPhotoSp_mu;
  // real prior_muPhotoSp_sigma;
  // real prior_muPhenoSp_mu;
  // real prior_muPhenoSp_sigma;
  // real prior_sigmaForceSp_mu;
  // real prior_sigmaForceSp_sigma;
  // real prior_sigmaChillSp_mu;
  // real prior_sigmaChillSp_sigma;
  // real prior_sigmaPhotoSp_mu;
  // real prior_sigmaPhotoSp_sigma;
  // real prior_sigmaPhenoSp_mu;
  // real prior_sigmaPhenoSp_sigma;
  // real prior_betaTraitxForce_mu;
  // real prior_betaTraitxForce_sigma;
  // real prior_betaTraitxChill_mu;
  // real prior_betaTraitxChill_sigma;
  // real prior_betaTraitxPhoto_mu;
  // real prior_betaTraitxPhoto_sigma;
  // real prior_sigmaphenoy_mu;
  // real prior_sigmaphenoy_sigma;
}

parameters{
  // Traits
  real mu_grand; // grand mean for trait value 
  vector[n_spec] muSp; // species offsets
  vector[n_study] muStudy; // study offsets
  real<lower = 0> sigma_traity; // sd general
  real<lower = 0> sigma_sp; // sd species
  real<lower = 0> sigma_study; // sd study
  // Phenology
  real alphaForceSp[n_spec]; 
  real muForceSp; 
  real<lower = 0> sigmaForceSp;
  real alphaChillSp[n_spec]; 
  real muChillSp; 
  real<lower = 0> sigmaChillSp;
  real alphaPhotoSp[n_spec];
  real muPhotoSp;
  real<lower = 0> sigmaPhotoSp;
  real alphaPhenoSp[n_spec];
  real muPhenoSp;
  real<lower = 0> sigmaPhenoSp; 
  real betaTraitxForce; 
  real betaTraitxChill;
  real betaTraitxPhoto;
  real<lower = 0> sigmapheno_y;
}

transformed parameters{
  // Traits
  vector[N] y_hat; 
  vector[n_spec] mu_grand_sp;
  // Phenology
  real betaForceSp[n_spec];     //species level beta forcing 
  real betaPhotoSp[n_spec];     //species level beta photoperiod
  real betaChillSp[n_spec];     //species level beta chilling
  // Traits
  for(i in 1:n_spec){
    mu_grand_sp[i] = mu_grand + muSp[i];
  }
  for (i in 1:N){
    y_hat[i] = mu_grand + muSp[trait_species[i]] + muStudy[study[i]];
  }
  // Phenology
  for (isp in 1:n_spec){
    betaForceSp[isp] = alphaForceSp[isp] + betaTraitxForce * (mu_grand_sp[isp]);
  }
  for (isp in 1:n_spec){
    betaPhotoSp[isp] = alphaPhotoSp[isp] + betaTraitxPhoto * (mu_grand_sp[isp]);
  }
  for (isp in 1:n_spec){
    betaChillSp[isp] = alphaChillSp[isp] + betaTraitxChill * (mu_grand_sp[isp]);
  }
}

model{
  // Traits
  //// likelihood
  yTraiti ~ normal(y_hat, sigma_traity);
  muSp ~ normal(0, sigma_sp);
  muStudy ~ normal(0, sigma_study);
  //// priors
  mu_grand ~ normal(20,10);
  sigma_sp ~ normal(4,5);
  sigma_study ~ normal(2,5);
  sigma_traity ~ normal(3,5);

  // Phenology
  //// likelihood
  for (i in 1:Nph){
    yPhenoi[i] ~ normal(alphaPhenoSp[phenology_species[i]] + betaForceSp[phenology_species[i]] * forcei[i] + betaPhotoSp[phenology_species[i]] * photoi[i] + betaChillSp[phenology_species[i]] * chilli[i], sigmapheno_y);
  }
  alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp);
  alphaForceSp ~ normal(muForceSp, sigmaForceSp); 
  alphaChillSp ~ normal(muChillSp, sigmaChillSp);
  alphaPhotoSp ~ normal(muPhotoSp, sigmaPhotoSp);
  //// priors
  muPhenoSp ~ normal(40,10);
  sigmaPhenoSp ~ normal(5,5);
  
  sigmapheno_y ~ normal(10,5);

  muForceSp ~ normal(-15,10);
  sigmaForceSp ~ normal(5,5);

  muChillSp ~ normal(-15,10);
  sigmaChillSp ~ normal(5,5);
   
  muPhotoSp ~ normal(-15,10);
  sigmaPhotoSp ~ normal(5,5);
        
  betaTraitxForce ~ normal(0,1);
  betaTraitxPhoto ~ normal(0,1);
  betaTraitxChill ~ normal(0,1); 

}


generated quantities {
} 
