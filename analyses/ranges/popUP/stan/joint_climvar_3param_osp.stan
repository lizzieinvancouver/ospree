# this stan code is similar to joint_climvar_3param_emw.stan but with a more reasonable prior for the intercept mu

data {

    //both models --------------------------------------------------------
        int < lower = 1 > N; // Sample size 
    int < lower = 1 > n_spec; // number of random effect levels (species) 
    int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)
        vector[n_spec] climvar; // climate variable for each species

    //MODEL 2 ------------------------------------------------

    
 
    vector[N] yPhenoi; // Outcome phenology
    vector[N] forcingi; // predictor forcing 
    vector[N] photoi; // predictor photoperiod 
    vector[N] chillingi; // predictor chilling
    
}

parameters{


    //MODEL 2 -----------------------------------------------------
    //level 2
    real alphaForcingSp[n_spec]; //the distribution of species forcing values
    real muForceSp; // the mean of the effect of forcing
    real <lower = 0> sigmaForceSp; //variation around the mean of the effect of forcing 

 real alphaPhotoSp[n_spec]; //the distribution of species photo values
    real muPhotoSp; // the mean of the effect of photoperiod
    real <lower = 0> sigmaPhotoSp; //variation around the mean of the effect of photoperiod

 real alphaChillSp[n_spec]; //the distribution of species chilling values
    real muChillSp; // the mean of the effect of chilling
    real <lower = 0> sigmaChillSp; //variation around the mean of the effect of chilling

    real alphaPhenoSp[n_spec]; //the distribution of species alphas?
    real muPhenoSp; // the mean of the effect of phenology
    real <lower = 0> sigmaPhenoSp; //variation around the mean of the effect of phenology  

    real betaTraitxForcing; //the interaction of climvar species with Force? Not sure this is right
  real betaTraitxPhoto; //the interaction of climvar species with Photo?
  real betaTraitxChill; //the interaction of climvar species with Chill?
  
    // general varience/error
    real <lower =0> sigmapheno_y; // overall variation accross observations

}

transformed parameters{

    //MODEL 2------------------------------------------------
    real betaForcingSp[n_spec];     //species level beta forcing
    real betaPhotoSp[n_spec];     //species level beta photoperiod
    real betaChillSp[n_spec];     //species level beta chilling 


    //MODEL 2----------------------------------------
    //get betaCUESp values for each species
    for (isp in 1:n_spec){
    betaForcingSp[isp] = alphaForcingSp[isp] + betaTraitxForcing*climvar[isp];
    }
    
    for (isp in 1:n_spec){
    betaPhotoSp[isp] = alphaPhotoSp[isp] + betaTraitxPhoto*climvar[isp];
    }
    
    for (isp in 1:n_spec){
    betaChillSp[isp] = alphaChillSp[isp] + betaTraitxChill*climvar[isp];
    }
}

model{ 

    //MODEL 2 -----------------------------------------------
    //priors - level 1
    sigmapheno_y ~ normal(0, 5); // prior for general variance around the mean 

    //priors level 2

    sigmaForceSp ~ normal(0, 20);// prior for forcing 
    muForceSp ~ normal(0, 10);
    alphaForcingSp ~ normal(muForceSp, sigmaForceSp);
    
    sigmaPhotoSp ~ normal(0, 5);// prior for photoperiod
    muPhotoSp ~ normal(0, 10);
    alphaPhotoSp ~ normal(muPhotoSp, sigmaPhotoSp); 
    
    sigmaChillSp ~ normal(0, 10);// prior for photoperiod
    muChillSp ~ normal(0, 10);
    alphaChillSp ~ normal(muChillSp, sigmaChillSp); 



   // sigmaPhenoSp ~ normal(0, 5); //priors for phenology 
    muPhenoSp ~ normal(60, 20);
    alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp); 

    betaTraitxForcing~ normal(0, 10);
    betaTraitxPhoto ~ normal(0, 10);
    betaTraitxChill ~ normal(0, 10);

    //likelihood 
        for (i in 1:N){
        yPhenoi[i] ~ normal(alphaPhenoSp[species[i]] + 
           betaForcingSp[species[i]] * forcingi[i] + 
           betaPhotoSp[species[i]] * photoi[i] + 
           betaChillSp[species[i]] * chillingi[i], sigmapheno_y);
        }

}
