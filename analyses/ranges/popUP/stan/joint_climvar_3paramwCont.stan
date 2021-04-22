/* Trying to add continent to rangers model (as opposed to running each dataset separately) */
/* Lizzie started this on 22 April 2021 */

/* Model below is ... day ~ alpha_sp + betaForce + betaChill + betaPhoto + betaSite +  
    betaForce*Site where Site=continent ... 
    if it ever worked, we could add + betaChill*Site + betaForce*Site and ...
    we could add site to intercept but I skipped it as not sure we need it */

data {

    int < lower = 1 > N; // Sample size 
    int < lower = 1 > n_spec; // number of random effect levels (species) 
    int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)
    vector[n_spec] climvar; // climate variable for each species
    vector[n_spec] continent; // continent dummy variable
 
    vector[N] yPhenoi; // Outcome phenology
    vector[N] forcingi; // predictor forcing 
    vector[N] photoi; // predictor photoperiod 
    vector[N] chillingi; // predictor chilling
    
}

transformed data { 	      
    vector[n_spec] inter_fs;
    inter_fs    = climvar .* continent; 
}

parameters{
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
    real betaFS; // attempt at continent*clim effect

    // general varience/error
    real <lower =0> sigmapheno_y; // overall variation accross observations

}

transformed parameters{

    //betas------------------------------------------------
    real betaForcingSp[n_spec];     //species level beta forcing
    real betaPhotoSp[n_spec];     //species level beta photoperiod
    real betaChillSp[n_spec];     //species level beta chilling 


    //get betaCUESp values for each species----------------------------------------
    for (isp in 1:n_spec){
    betaForcingSp[isp] = alphaForcingSp[isp] + betaTraitxForcing*climvar[isp] + 
        betaFS*inter_fs[isp];
    }
    
    for (isp in 1:n_spec){
    betaPhotoSp[isp] = alphaPhotoSp[isp] + betaTraitxPhoto*climvar[isp];
    }
    
    for (isp in 1:n_spec){
    betaChillSp[isp] = alphaChillSp[isp] + betaTraitxChill*climvar[isp];
    }
}

model{ 
    //priors - level 1
    sigmapheno_y ~ normal(0, 5); // prior for general variance around the mean 

    //priors level 2
    sigmaForceSp ~ normal(0, 5);// prior for forcing 
    muForceSp ~ normal(0, 5);
    alphaForcingSp ~ normal(muForceSp, sigmaForceSp);
    
    sigmaPhotoSp ~ normal(0, 5);// prior for photoperiod
    muPhotoSp ~ normal(0, 5);
    alphaPhotoSp ~ normal(muPhotoSp, sigmaPhotoSp); 
    
    sigmaChillSp ~ normal(0, 10);// prior for photoperiod
    muChillSp ~ normal(0, 10);
    alphaChillSp ~ normal(muChillSp, sigmaChillSp); 

   // sigmaPhenoSp ~ normal(0, 5); //priors for phenology 
    muPhenoSp ~ normal(150, 10);
    alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp); 

    betaTraitxForcing~ normal(0, 10);
    betaTraitxPhoto ~ normal(0, 10);
    betaTraitxChill ~ normal(0, 10);
    betaFS ~ normal(0,10);

    //likelihood 
        for (i in 1:N){
        yPhenoi[i] ~ normal(alphaPhenoSp[species[i]] + 
           betaForcingSp[species[i]] * forcingi[i] + 
           betaPhotoSp[species[i]] * photoi[i] + 
           betaChillSp[species[i]] * chillingi[i], sigmapheno_y);
        }

}
