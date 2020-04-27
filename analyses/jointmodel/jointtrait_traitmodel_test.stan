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
	vector[nsp] mua_sp; // mean of the alpha value for species
	real <lower = 0> sigma_study; // variation of intercept amoung studies
	vector[nstudy] mua_study; // mean of the alpha value for studies 
}

model{ 
	vector[N] ypred;
	ypred = agrand + mua_sp[species] + mua_study[study];  
	
	// Model of trait priors
	mua_sp ~ normal(0, sigma_sp);
	mua_study ~ normal(0, sigma_study);
	sigma_y ~ normal(0, 3);
        agrand ~ normal(30, 10);
	sigma_sp ~ normal(0, 10);
	sigma_study ~ normal(0, 10);

	// likelihood
        traitdat ~ normal(ypred, sigma_y);
}


generated quantities {
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = agrand + mua_sp[species[n]] + mua_study[study[n]];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);
} // The posterior predictive distribution
