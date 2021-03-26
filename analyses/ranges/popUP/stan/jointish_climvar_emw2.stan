data {

    //both models --------------------------------------------------------
        int < lower = 1 > N; // Sample size 
        int < lower = 1 > n_spec; // number of random effect levels (species) 
        int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)
        vector[N] climvar; // climate variable for each species

    //MODEL 2 ------------------------------------------------

    
 
    vector[N] yPhenoi; // Outcome phenology
    vector[N] forcingi; // predictor forcing 

    //int < lower = 1, upper = n_spec > species2[Nph]; // id of random effect (species)


}

parameters{


    //MODEL 2 -----------------------------------------------------
    //level 2
    real alphaForcingSp[n_spec]; //the distribution of species forcing values
    real muForceSp; // the mean of the effect of forcing
    real <lower = 0> sigmaForceSp; //variation around the mean of the effect of forcing 

    real alphaPhenoSp[n_spec]; //the distribution of species forcing effects 
    real muPhenoSp; // the mean of the effect of phenology
    real <lower = 0> sigmaPhenoSp; //variation around the mean of the effect of phenology  

    real betaTraitxPheno; //the interaction of climvar species with phenology?

    // general varience/error
    real <lower =0> sigmapheno_y; // overall variation accross observations

    // real betaForcingSp[n_spec];     //species level beta forcing 


}

model{ 

    //MODEL 2 -----------------------------------------------
    //priors - level 1
    sigmapheno_y ~ normal(0, 5); // prior for general variance around the mean 

    //priors level 2

    sigmaForceSp ~ normal(0, 5);// prior for forcing 
    muForceSp ~ normal(0, 5);
    alphaForcingSp ~ normal(muForceSp, sigmaForceSp); 

    sigmaPhenoSp ~ normal(0, 5); //priors for phenology 
    muPhenoSp ~ normal(150, 5);
    alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp); 

    betaTraitxPheno ~ normal(0, 1);

    //likelihood 
    for (i in 1:N){
    yPhenoi[i] ~ normal( alphaPhenoSp[species[i]] +
        (alphaForcingSp[species[i]] + betaTraitxPheno*climvar[i]) * forcingi[i], sigmapheno_y);
        }

}


