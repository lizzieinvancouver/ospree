
data {
  int<lower = 1> n_spec; // number of random effect levels
  // Traits
  int<lower = 1> N; // Sample size 
  int<lower = 1, upper = n_spec> trait_species[N]; // id of random effect (species)
  int<lower = 1> n_study; // number of random effect levels (study) 
  int<lower = 1, upper = n_study> study[N]; // id of random effect (study)
  vector[N] yTraiti; // Observed trait
  real prior_mu_grand_mu; 
  real prior_mu_grand_sigma;
  real prior_sigma_sp_mu;
  real prior_sigma_sp_sigma;
  real prior_sigma_study_mu;
  real prior_sigma_study_sigma;
  real prior_sigma_traity_mu;
  real prior_sigma_traity_sigma;
  
  // Cue model
  int<lower = 1> Ncue; // Sample size for second part
  int<lower = 1, upper = n_spec> cue_species[Ncue]; // id of random effect (species)
  vector[Ncue] yRespi; // Outcome phenology
  vector[Ncue] cuei; // predictor forcing 
  real prior_muCueSp_mu;
  real prior_muCueSp_sigma;
  real prior_muRespSp_mu;
  real prior_muRespSp_sigma;
  real prior_sigmaCueSp_mu;
  real prior_sigmaCueSp_sigma;
  real prior_sigmaRespSp_mu;
  real prior_sigmaRespSp_sigma;
  real prior_betaTraitxCue_mu;
  real prior_betaTraitxCue_sigma;
  real prior_sigmaRespy_mu;
  real prior_sigmaRespy_sigma;
}

parameters{
  // Traits
  real mu_grand; // grand mean for trait value 
  vector[n_spec] muSp; // species offsets
  vector[n_study] muStudy; // study offsets
  real<lower = 0> sigma_traity; // sd general
  real<lower = 0> sigma_sp; // sd species
  real<lower = 0> sigma_study; // sd study
 
  real alphaCueSp[n_spec]; 
  real muCueSp; 
  real<lower = 0> sigmaCueSp;
  real alphaRespSp[n_spec];
  real muRespSp;
  real<lower = 0> sigmaRespSp; 
  real betaTraitxCue; 
  real<lower = 0> sigmaResp_y;
}

transformed parameters{
  
  vector[N] y_hat; 
  vector[n_spec] mu_grand_sp;
  
  real betaCueSp[n_spec];     //species level beta forcing 
 
  for(i in 1:n_spec){
    mu_grand_sp[i] = mu_grand + muSp[i];
  }
  for (i in 1:N){
    y_hat[i] = mu_grand + muSp[trait_species[i]] + muStudy[study[i]];
  }
  
  for (isp in 1:n_spec){
    betaCueSp[isp] = alphaCueSp[isp] + betaTraitxCue * (mu_grand_sp[isp]);
  }
}

model{
  // Traits
  //// likelihood
  yTraiti ~ normal(y_hat, sigma_traity);
  muSp ~ normal(0, sigma_sp);
  muStudy ~ normal(0, sigma_study);
  //// priors
  mu_grand ~ normal(prior_mu_grand_mu, prior_mu_grand_sigma);
  sigma_sp ~ normal(prior_sigma_sp_mu, prior_sigma_sp_sigma);
  sigma_study ~ normal(prior_sigma_study_mu, prior_sigma_study_sigma);
  sigma_traity ~ normal(prior_sigma_traity_mu, prior_sigma_traity_sigma);

  //// likelihood
  for (i in 1:Ncue){
    yRespi[i] ~ normal(alphaRespSp[cue_species[i]] + betaCueSp[cue_species[i]] * cuei[i], sigmaResp_y);
  }
  alphaRespSp ~ normal(muRespSp, sigmaRespSp);
  alphaCueSp ~ normal(muCueSp, sigmaCueSp); 

  //// priors
  muRespSp ~ normal(prior_muRespSp_mu, prior_muRespSp_sigma);
  sigmaRespSp ~ normal(prior_sigmaRespSp_mu, prior_sigmaRespSp_sigma);
  
  sigmaResp_y ~ normal(prior_sigmaRespy_mu, prior_sigmaRespy_sigma);

  muCueSp ~ normal(prior_muCueSp_mu,  prior_muCueSp_sigma);
  sigmaCueSp ~ normal(prior_sigmaCueSp_mu, prior_sigmaCueSp_sigma);

  betaTraitxCue ~ normal(prior_betaTraitxCue_mu, prior_betaTraitxCue_sigma);
 
}

