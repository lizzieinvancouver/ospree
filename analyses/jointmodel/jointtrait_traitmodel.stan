/* running a joint model to try and see how trait variation might help 
predict phenology. BAsed off Lizzie's joint model exqation 
By Faith Jones, extracted from JointModelSim_fj.R on 21 Feb 2020 by Lizzie */


data {
	// Model of trait 
	int < lower = 1 > N; // Sample size for trait data 
 	int < lower = 1 > nstudy; // number of random effect levels (study) 
	int < lower = 1, upper = nstudy > study[N]; // id of random effect (study)
 	vector[N] traitdat; // y trait data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
}


parameters{
	// Model of trait
	real <lower =0> sigma_y; // overall variation accross observations
        real agrand; // grand mean for trait
	real <lower = 0> sigma_sp; // variation of intercept amoung species
	real muaSp[nsp]; // mean of the alpha value for species
	real <lower = 0> sigma_study; // variation of intercept amoung studies
	real muaStudy[nstudy]; // mean of the alpha value for studies 
}

transformed parameters{ 
	real ypred[N];
	for (i in 1:N){
	    ypred[i] = agrand + muaSp[species[i]] + muaStudy[study[i]];  
	}
}
model{ 
	// Model of trait
	muaSp ~ normal(0, sigma_sp);
	muaStudy ~ normal(0, sigma_study);

	sigma_y ~ normal(0, 10);
        agrand ~ normal(0, 20);
	sigma_sp ~ normal(0, 20);
	sigma_study ~ normal(0, 20);

	// likelihood
        traitdat ~ normal(ypred, sigma_y);
}


generated quantities {
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = agrand + muaSp[species[n]] + muaStudy[study[n]];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);
} // The posterior predictive distribution
