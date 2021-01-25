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

    int < lower = 1, upper = n_spec > species2[Nph]; // id of random effect (species)


}


parameters{

    //MODEL 1 ------------------------------------------------
    //level 1
    real <lower =0> sigmaTrait_y; // overall variation accross observations

    //level 2
    real <lower = 0> sigma_sp; // variation of intercept amoung species
    real mu_g; // mean of the alpha value for species
    real muSp[n_spec]; //The trait effect of each species without stdy 

    real <lower = 0> sigma_stdy; // variation of intercept amoung studies
    real muStdy[n_study]; // mean of the alpha value for studies 

    //MODEL 2 -----------------------------------------------------
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
    //MODEL 1 ----------------------------------------
    //Individual mean for species and study
    real ymu[N];

    //MODEL 2------------------------------------------------
    real betaForcingSp[n_spec];     //species level beta forcing 

    //MODEL 1
    //Individual mean calculation 
    for (i in 1:N){
        ymu[i] = muSp[species[i]] + muStdy[study[i]];  //muSp is used in 2nd level of model
    }

    //MODEL 2----------------------------------------
    //get betaForcingSp values for each species
    for (isp in 1:n_spec){
    betaForcingSp[isp] = alphaForcingSp[isp] + betaTraitxPheno * muSp[isp];
    }

}
model{ 
    //MODEL 1 ---------------------------------------------
    //assign priors
    sigmaTrait_y ~ normal(0,5); // sigma_obs 0.4

    sigma_sp ~ normal(0,5); //sigma_species 1.1
    mu_g ~ normal(0, 5); // mu_species 3
    muSp ~ normal(mu_g, sigma_sp); //trait1

    sigma_stdy ~ normal(0, 5);
    muStdy ~ normal(0, sigma_stdy);//sigma_study 0.6. Centred around 0 because muSp acts as the center of the distribution

    // run the actual model - likihood
    for (i in 1:N){
        yTraiti[i] ~ normal(ymu[i], sigmaTrait_y);
    }

    //MODEL 2 -----------------------------------------------
    //priors - level 1
    sigmapheno_y ~ normal(0, 10); // sigma_phen 0.5

    //priors level 2

    sigmaForceSp ~ normal(0, 5); //sigma_a_forcing 2
    muForceSp ~ normal(0, 5);// mu_a_forcing -0.8 
    alphaForcingSp ~ normal(muForceSp, sigmaForceSp);  //forcing_a

    sigmaPhenoSp ~ normal(0, 10); // sigma_a_phen 10
    muPhenoSp ~ normal(100, 50);  //mu_a_phen 100
    alphaPhenoSp ~ normal(muPhenoSp, sigmaPhenoSp);//phenology_a

    betaTraitxPheno ~ normal(0, 10); // b_forcing. Geoff has one for each species though.  

    //likelihood 
        for (i in 1:Nph){
    yPhenoi[i] ~ normal( alphaPhenoSp[species2[i]] + betaForcingSp[species2[i]] * forcingi[i], sigmapheno_y);
        }

}


generated quantities {
} // The posterior predictive distribution