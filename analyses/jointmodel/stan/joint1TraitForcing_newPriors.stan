//Model started by Faith in Feb 2020 based on Lizzie's notes 
// running a joint model to try and see how trait variation might help 
//predict phenology. Based off Lizzie's joint model equation 
//priors are centered around values from Geoff's similation code 

data {

  //MODEL 1 ------------------------------------------------
  int < lower = 1 > N; // Sample size for trait data 
  int < lower = 1 > n_study; // number of random effect levels (study) 
  int < lower = 1, upper = n_study > study[N]; // id of random effect (study)
  vector[N] yTraiti; // Outcome trait data 

  //both models --------------------------------------------------------
  int < lower = 1 > n_spec; // number of random effect levels (species) 
  int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)

  //MODEL 2 ------------------------------------------------
  int < lower = 1 > Nph; // Sample size for forcing 
  vector[Nph] yPhenoi; // Outcome phenology
  vector[Nph] forcingi; // predictor forcing 

}

parameters{

  //MODEL 1 ------------------------------------------------
  //level 1
  real <lower =0> sigmaTrait_y; // overall variation across observations
  //level 2
  real <lower = 0> sigma_sp; // variation of intercept amoung species
  real mu_g; // mean of the alpha value for species
  real alphaTraitSp[n_spec]; //The trait effect of each species without stdy 
  real <lower = 0> sigma_stdy; // variation of intercept amoung studies
  real alphaStdy[n_study]; // mean of the alpha value for studies 

  //MODEL 2 -----------------------------------------------------
  real alphaForcingSp[n_spec]; //the distribution of species forcing values
  real muForceSp; // the mean of the effect of forcing
  real <lower = 0> sigmaForceSp; //variation around the mean of the effect of forcing 
  real alphaPhenoSp[n_spec]; //the distribution of species forcing effects 
  real muPhenoSp; // the mean of the effect of phenology
  real <lower = 0> sigmaPhenoSp; //variation around the mean of the effect of phenology  
  real betaTraitxPheno; //the interaction of alphatrait species with phenology?

  // general varience/error
  real <lower =0> sigmapheno_y; // overall variation accross observations

}

transformed parameters{
  //MODEL 1 ----------------------------------------
  /* real ymu[N]; //Individual mean for species and study */
  /* //MODEL 2------------------------------------------------ */
  /* real betaForcingSp[Nph]; //beta forcing  */

  /* //MODEL 1 */
  /* //Mean of measured trait */
  /* for (i in 1:N){ */
  /*   ymu[i] = alphaTraitSp[species[i]] + alphaStdy[study[i]];  //alphaTraitSp is used in 2nd level of model */
  /* } */

  /* //MODEL 2---------------------------------------- */
  /* //get betaForcingSp values */
  /* for (i in 1:Nph){ */
  /*   betaForcingSp[i] = alphaForcingSp[species[i]] + betaTraitxPheno * alphaTraitSp[species[i]]; */
  /* } */

}
model{ 
  //MODEL 1 ---------------------------------------------
  //likelihood
  for (i in 1:N){
    yTraiti[i] ~ normal(alphaTraitSp[species[i]] + alphaStdy[study[i]],
			sigmaTrait_y);
  }
  alphaStdy ~ normal(0, sigma_stdy);//Centered at 0 since alphaTraitSp is center of distribution
  //priors
  sigmaTrait_y ~ normal(0.5, 5); // sigma_obs 0.5
  sigma_stdy ~ normal(1, 5); //sigma_study 1 
    
  //MODEL 2 -----------------------------------------------
  //likelihood 
  for (i in 1:Nph){
    yPhenoi[i] ~ normal(alphaPhenoSp[species[i]] +
			(alphaForcingSp[species[i]] + betaTraitxPheno * alphaTraitSp[species[i]]) * forcingi[i],
			sigmapheno_y);
  }
  alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp); //phenology_a
  alphaForcingSp ~ normal(muForceSp, sigmaForceSp);  //forcing_a
  //priors
  sigmapheno_y ~ normal(3, 5); // sigma_phen 0.5
  sigmaForceSp ~ normal(.5, 5); //sigma_a_forcing 0.4
  muForceSp ~ normal(-.9, 5); // mu_a_forcing -0.9 
  sigmaPhenoSp ~ normal(10, 5); // sigma_a_phen 10
  muPhenoSp ~ normal(100, 5);  //mu_a_phen 100
  betaTraitxPheno ~ normal(-.6, 5); // b_forcing.  

  //SHARED PARAMETERS
  alphaTraitSp ~ normal(mu_g, sigma_sp); //trait1
  mu_g ~ normal(3, 5); // mu_species 3
  sigma_sp ~ normal(1.1, 5); //sigma_species 1.1
  
}


generated quantities {
} // The posterior predictive distribution
