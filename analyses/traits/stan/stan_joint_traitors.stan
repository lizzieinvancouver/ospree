//Model started by Faith in Feb 2020 based on Lizzie's notes 
// running a joint model to try and see how trait variation might help 
    //predict phenology. BAsed off Lizzie's joint model exqation 

//priors are centred around values from Geoffs similation code   

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
    vector[Nph] photoi; // predictor photoperiod 
    vector[Nph] chillingi; // predictor chilling

    int < lower = 1, upper = n_spec > species2[Nph]; // id of random effect (species)

}

parameters{

    //MODEL 1 ------------------------------------------------
    //level 1
    real <lower =0> sigmaTrait_y; // overall variation accross observations
    real mu_grand; // Grand mean for trait value 
    //level 2
    real <lower = 0> sigma_sp; // variation of intercept amoung species
    //real mu_g; // mean of the alpha value for species
    real muSp[n_spec]; //The trait effect of each species without stdy 

    real <lower = 0> sigma_stdy; // variation of intercept amoung studies
    real muStdy[n_study]; // mean of the alpha value for studies 

    //MODEL 2 -----------------------------------------------------
    //level 2
    
    real alphaForcingSp[n_spec]; //the distribution of species forcing values
    real muForceSp; // the mean of the effect of forcing
    real <lower = 0> sigmaForceSp; //variation around the mean of the effect of forcing 
    
    real alphaChillSp[n_spec]; //the distribution of species chilling values
    real muChillSp; // the mean of the effect of chilling
    real <lower = 0> sigmaChillSp; //variation around the mean of the effect of chilling
    
    real alphaPhotoSp[n_spec]; //the distribution of species chilling values
    real muPhotoSp; // the mean of the effect of chilling
    real <lower = 0> sigmaPhotoSp; //variation around the mean of the effect of chilling

    real alphaPhenoSp[n_spec]; //the species level intercept 
    real muPhenoSp; // 
    real <lower = 0> sigmaPhenoSp; 

   //real betaTraitxPheno; //the interaction of alphatrait species with phenology?
    real betaTraitxForcing; 
    real betaTraitxChill; 
    real betaTraitxPhoto; 
    // general varience/error
    real <lower =0> sigmapheno_y; // overall variation accross observations
}

transformed parameters{
    //MODEL 1 ----------------------------------------
    //Individual mean for species and study
    real ymu[N];

    //MODEL 2------------------------------------------------
    real betaForcingSp[n_spec];     //species level beta forcing 
    real betaPhotoSp[n_spec];     //species level beta photoperiod
    real betaChillSp[n_spec];     //species level beta chilling 

    //MODEL 1
    //Individual mean calculation 
    for (i in 1:N){
        ymu[i] = mu_grand + muSp[species[i]] + muStdy[study[i]];  //muSp is used in 2nd level of model
    }

    //MODEL 2----------------------------------------
    //get betaForcingSp values for each species
    for (isp in 1:n_spec){
    betaForcingSp[isp] = alphaForcingSp[isp] + betaTraitxForcing * (mu_grand + muSp[isp]);
    }
    
    for (isp in 1:n_spec){
    betaPhotoSp[isp] = alphaPhotoSp[isp] + betaTraitxPhoto* (mu_grand + muSp[isp]);
    }
    
    for (isp in 1:n_spec){
    betaChillSp[isp] = alphaChillSp[isp] + betaTraitxChill* (mu_grand + muSp[isp]);
    }
}
model{ 
    //MODEL 1 ---------------------------------------------
    //assign priors
    sigmaTrait_y ~ normal(10,10); // sigma_obs 10
    sigma_sp ~ normal(10,20); //sigma_species 10
    mu_grand ~ normal(0, 20); // mu_species 20
    muSp ~ normal(0, sigma_sp); //trait1

    sigma_stdy ~ normal(5, 2);
    muStdy ~ normal(0, sigma_stdy);//sigma_study 0.6. Centred around 0 because muSp acts as the center of the distribution

    // run the actual model - likihood
    for (i in 1:N){
        yTraiti[i] ~ normal(ymu[i], sigmaTrait_y);
    }

    //MODEL 2 -----------------------------------------------
    //priors - level 1
    sigmapheno_y ~ normal(2, 10); // sigma_phen 5

    //priors level 2

    sigmaForceSp ~ normal(5, 1); //sigma_a_forcing 2
    muForceSp ~ normal(0, 1);// mu_a_forcing -0.8 
    alphaForcingSp ~ normal(muForceSp, sigmaForceSp);  //forcing_a

    sigmaPhotoSp ~ normal(5, 1); //sigma_a_forcing 2
    muPhotoSp ~ normal(0, 1);// mu_a_forcing -0.8 
    alphaPhotoSp ~ normal(muPhotoSp, sigmaPhotoSp);  //forcing_a
    
    sigmaChillSp ~ normal(5, 1); //sigma_a_forcing 2
    muChillSp ~ normal(0, 1);// mu_a_forcing -0.8 
    alphaChillSp ~ normal(muChillSp, sigmaChillSp);  //forcing_a
    
    sigmaPhenoSp ~ normal(5, 10); // sigma_a_phen 10
    muPhenoSp ~ normal(100, 50);  //mu_a_phen 100
    alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp);//phenology_a

    betaTraitxForcing ~ normal(0, 10); // b_forcing. Geoff has one for each species though.  
    betaTraitxPhoto ~ normal(0, 10); // b_forcing. Geoff has one for each species though.  
    betaTraitxChill ~ normal(0, 10); // b_forcing. Geoff has one for each species though.  

    //likelihood 
        for (i in 1:Nph){
    yPhenoi[i] ~ normal( alphaPhenoSp[species2[i]] + betaForcingSp[species2[i]] * forcingi[i] + betaPhotoSp[species2[i]] * photoi[i] + betaChillSp[species2[i]] * chillingi[i], sigmapheno_y);
        }

}


generated quantities {
} // The posterior predictive distribution
