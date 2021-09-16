//Model started by Faith in Feb 2020 based on Lizzie's notes 
// Modified by Deirdre based on the rangers model 

// running a joint model to try and see how trait variation might help 
    //predict phenology. BAsed off Lizzie's joint model exquation 

//priors are centred around values from Deirdre's similation code: fakedata_3parm_1trait.R   

data {
    //MODEL 1 ------------------------------------------------
    int < lower = 1 > N; // Sample size for trait data 
 
    int < lower = 1 > n_study; // number of random effect levels (study) 
    int < lower = 1, upper = n_study > study[N]; // id of random effect (study)

    vector[N] yTraiti; // Outcome trait data 

    //both models --------------------------------------------------------
    int < lower = 1 > n_spec; // number of random effect levels (species) 
    int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)

    // //MODEL 2 ------------------------------------------------
    // int < lower = 1 > Nph; // Sample size for forcing 
    // 
    // vector[Nph] yPhenoi; // Outcome phenology
    // vector[Nph] forcei; // predictor forcing 
    // vector[Nph] photoi; // predictor photoperiod 
    // vector[Nph] chilli; // predictor chilling
    // 
    // int < lower = 1, upper = n_spec > species2[Nph]; // id of random effect (species)
    
    //Priors
    real prior_mu_grand; 
    real prior_sigma_grand;
    real prior_mu_sp;
    real prior_sigma_sp_mu;
    real prior_sigma_sp_sigma;
    real prior_mu_study;
    real prior_sigma_study_mu;
    real prior_sigma_study_sigma;
    real prior_sigma_traity_mu;
    real prior_sigma_traity_sigma;
}

parameters{

    //MODEL 1 ------------------------------------------------
    //level 1
    vector[n_spec] muSp_raw;
    vector[n_study] muStudy_raw;
    
    real <lower =0> sigmaTrait_y; // overall variation accross observations
    real mu_grand; // Grand mean for trait value 
    //level 2
    real <lower = 0> sigma_sp; // variation of intercept amoung species
    //real muSp[n_spec]; //The trait effect of each species without study 
    real <lower = 0> sigma_study; // variation of intercept amoung studies
    //real muStudy[n_study]; // mean of the alpha value for studies 
}

transformed parameters{
    vector[N] y_hat;
    
    vector[n_spec] muSp = muSp_raw * sigma_sp;
    vector[n_study] muStudy = muStudy_raw * sigma_study;
    
    for (i in 1:N)
    y_hat[i] = mu_grand + muSp[species[i]] + muStudy[study[i]];
    }

model{ 
    //MODEL 1 ---------------------------------------------
    //assign priors
    sigmaTrait_y ~ normal(prior_sigma_traity_mu, prior_sigma_traity_sigma); // trt.var 0.5
    sigma_sp ~ normal(prior_sigma_sp_mu, prior_sigma_sp_sigma); //sigma_species 10
    mu_grand ~ normal(prior_mu_grand, prior_sigma_grand); // 
    muSp_raw ~ normal(prior_mu_sp, sigma_sp); //

    sigma_study ~ normal(prior_sigma_study_mu, prior_sigma_study_sigma); //sigma.study 5
    muStudy_raw ~ normal(prior_mu_study, sigma_study);//
    
    // run the actual model - likihood
        yTraiti ~ normal(y_hat, sigmaTrait_y);
    }

generated quantities {
    real y_rep[N];
    for(n in 1:N)
      y_rep[n] = normal_rng(y_hat[n], sigmaTrait_y);
} // The posterior predictive distribution
