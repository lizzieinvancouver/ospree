//Model started by Faith in Feb 2020 based on Lizzie's notes 
// Modified by Deirdre based on the rangers model 

// running a joint model to try and see how trait variation might help 
    //predict phenology. BAsed off Lizzie's joint model exquation 

//priors are centred around values from Deirdre's similation code: fakedata_3parm_1trait.R   

data {
    int < lower = 1 > N; // Sample size for trait data 
    vector[N] yTraiti; // Outcome trait data 
    int < lower = 1 > n_spec; // number of random effect levels (species) 
    int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)
    //Priors
    real prior_mu_grand; 
    real prior_sigma_grand;
    real prior_mu_sp;
    real prior_sigma_sp_mu;
    real prior_sigma_sp_sigma;
    real prior_sigma_traity_mu;
    real prior_sigma_traity_sigma;
}

parameters{
    //MODEL 1 ------------------------------------------------
    //level 1
    real mu_grand; // Grand mean for trait value 
    vector[n_spec] muSp;
    
    real <lower = 0> sigmaTrait_y; // overall variation accross observations
    real <lower = 0> sigma_sp; // variation of intercept amoung species
    real <lower = 0> sigma_study; // variation of intercept amoung studies
}

transformed parameters{
    vector[N] y_hat;
    
    for (i in 1:N)
      y_hat[i] = mu_grand + muSp[species[i]];
    }

model{ 
    //MODEL 1 ---------------------------------------------
    //assign priors
    sigmaTrait_y ~ normal(prior_sigma_traity_mu, prior_sigma_traity_sigma); // trt.var 0.5
    sigma_sp ~ normal(prior_sigma_sp_mu, prior_sigma_sp_sigma); //sigma_species 10
    mu_grand ~ normal(prior_mu_grand, prior_sigma_grand); // 
    muSp ~ normal(prior_mu_sp, sigma_sp); //

    // run the actual model - likelihood
    yTraiti ~ normal(y_hat, sigmaTrait_y);
}

generated quantities {
    real y_rep[N];
    for(n in 1:N)
      y_rep[n] = normal_rng(y_hat[n], sigmaTrait_y);
} // The posterior predictive distribution
