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
	real mua_sp[nsp]; // mean of the alpha value for species
	real <lower = 0> sigma_study; // variation of intercept amoung studies
	real mua_study[nstudy]; // mean of the alpha value for studies 
       // Model of pheno
	real <lower =0> sigma_ypheno; // overall variation accross observations for pheno
	real <lower = 0> sigma_apheno; // variation of intercept amoung species for pheno
	real mua_spheno[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_bforce; // variation of intercept amoung species for pheno
	real muaforce[nsppheno]; // mean of the alpha value for species for pheno
        real bforce_trait; // our beta!
}


model{ 
        real ypred[N];
	vector[nsppheno] b_force_final;
	real ypredpheno[Npheno];
	for (i in 1:N){
	    ypred[i] = agrand + mua_sp[species[i]] + mua_study[study[i]];  
	}
	for(isp in 1:nsppheno){
	    b_force_final[isp] =  muaforce[isp] + bforce_trait * mua_sp[isp];
	}
	for (iph in 1:Npheno){
	ypredpheno[iph] = mua_spheno[speciespheno[iph]] + b_force_final[speciespheno[iph]] * forcing[iph];

	// Model of trait
	mua_sp ~ normal(0, sigma_sp);
	mua_study ~ normal(0, sigma_study);

	sigma_y ~ normal(0, 3);
        agrand ~ normal(30, 10);
	sigma_sp ~ normal(0, 10);
	sigma_study ~ normal(0, 10);

        // Model of pheno
        mua_spheno ~ normal(0, sigma_apheno);
        muaforce ~ normal(0, sigma_bforce);
        sigma_ypheno ~ normal(0, 5);
        sigma_apheno ~ normal(0, 10);
        sigma_bforce ~ normal(0, 5);

	// likelihoods (is this bad)?
        traitdat ~ normal(ypred, sigma_y);
        phendat ~ normal(ypredpheno, sigma_ypheno);
}


generated quantities { // Not updated beyond trait ...
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = agrand + mua_sp[species[n]] + mua_study[study[n]];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);
} 
