/* First pass at a joint model to best estimate species-level trait values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of trait 
	int < lower = 1 > N; // Sample size for trait data 
 	int < lower = 1 > nstudy; // number of random effect levels (study) 
	int < lower = 1, upper = nstudy > study[N]; // id of random effect (study)
 	vector[N] traitdat; // y trait data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
       // Model of pheno
       	int < lower = 1 > Npheno; // Sample size for pheno data 
 	vector[Npheno] phendat; // y pheno data 
 	vector[Npheno] forcing; // predictor forcing 
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of trait
	real <lower =0> sigma_y; // overall variation accross observations for trait
        real agrand; // grand mean for trait
	real <lower = 0> sigma_sp; // variation of intercept amoung species
	real muaSp[nsp]; // mean of the alpha value for species
	real <lower = 0> sigma_study; // variation of intercept amoung studies
	real muaStudy[nstudy]; // mean of the alpha value for studies 
       // Model of pheno
	real <lower =0> sigma_ypheno; // overall variation accross observations for pheno
	real <lower = 0> sigma_apheno; // variation of intercept amoung species for pheno
	real muaSpheno[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_bforce; // variation of intercept amoung species for pheno
	real muaforce[nsppheno]; // mean of the alpha value for species for pheno
        real bforce_trait; // our beta!
}

transformed parameters{ 
        real ypred[N];
	vector[nsppheno] b_force_final;
	real ypredpheno[Npheno];
	for (i in 1:N){
	    ypred[i] = agrand + muaSp[species[i]] + muaStudy[study[i]];  
	}
	for(isp in 1:nsppheno){
	    b_force_final[isp] =  muaforce[isp] + bforce_trait * muaSp[isp];
	}
	for (iph in 1:Npheno){
	ypredpheno[iph] = muaSpheno[speciespheno[iph]] + b_force_final[speciespheno[iph]] * forcing[iph];
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

        // Model of pheno
        muaSpheno ~ normal(0, sigma_apheno);
        muaforce ~ normal(0, sigma_bforce);
        sigma_ypheno ~ normal(0, 10);
        sigma_apheno ~ normal(0, 10);
        sigma_bforce ~ normal(0, 10);

	// likelihoods (is this bad)?
        traitdat ~ normal(ypred, sigma_y);
        phendat ~ normal(ypredpheno, sigma_ypheno);
}


generated quantities { // Not updated beyond trait ...
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = agrand + muaSp[species[n]] + muaStudy[study[n]];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);
} 
