//model started by Faith Feb 2020 and changed oin Dec 2020. 
//2nd half of teh joint model based on Lizzies notes
// running a simple model of the second part of the model 
//focusing only on foring and a singel trait value per species to 
//predict phenology day of year event  


data {
    int < lower = 1 > Nph; // Sample size
 
    vector < lower = 0> [Nph] yPhenoi; // Outcome. No negative days of the year. 
    vector< lower = 0>  [Nph] forcingi; // predictor.  No negative amount of chilling. 

    int < lower = 1 > n_spec; // number of random effect levels (species) 
    int < lower = 1, upper = n_spec > species[Nph]; // id of random effect (species)

    vector [n_spec] alphaTraitSp; //species level trait data from the first level of the model
}


parameters{

    //level 1

    //level 2
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
    
    real betaForcingSp[n_spec];     //species level beta forcing 

    //get betaForcingSp values for each species
    for (isp in 1:n_spec){
        betaForcingSp[isp] = alphaForcingSp[isp] + betaTraitxPheno * alphaTraitSp[isp];
    }

}

model{ 

    //priors - level 1
    sigmapheno_y ~ normal(0, 5); // prior for general variance around the mean 

    //priors level 2

    sigmaForceSp ~ normal(0, 5);// prior for forcing 
    muForceSp ~ normal(0, 5);
    alphaForcingSp ~ normal(muForceSp, sigmaForceSp); 

    sigmaPhenoSp ~ normal(0, 5); //priors for phenology 
    muPhenoSp ~ normal(100, 50);
    alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp); 

    betaTraitxPheno ~ normal(0, 5);

    //likelihood 
        for (i in 1:Nph){
    yPhenoi[i] ~ normal( alphaPhenoSp[species[i]] + betaForcingSp[species[i]] * forcingi[i], sigmapheno_y);
        }

}


generated quantities {
} // The posterior predictive distribution
