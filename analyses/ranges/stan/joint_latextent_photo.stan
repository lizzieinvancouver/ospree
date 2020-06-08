/* First pass at a joint model to best estimate species-level lat values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
 	int < lower = 1 > nstudy; // number of random effect levels (study) 
	int < lower = 1, upper = nstudy > study[N]; // id of random effect (study)
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
       // Model of pheno
       	int < lower = 1 > Npheno; // Sample size for pheno data 
  vector[N] rangedat; // y lat data 
 	//vector[nsp] extentdat; // y extent data 
 	vector[Npheno] phendat; // y pheno data 
 	vector[Npheno] photoperiod; // predictor photoperiod 
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower =0> sigma_y; // overall variation accross observations for lat
        real agrand_lat; // grand mean for lat
        real agrand_extent; // grand mean for extent
	real <lower = 0> sigma_sp; // variation of intercept amoung species
	vector[nsp] mua_sp; // mean of the alpha value for species
	real <lower = 0> sigma_study; // variation of intercept amoung studies
	vector[nstudy] mua_study; // mean of the alpha value for studies 
       // Model of pheno
	real <lower =0> sigma_ypheno; // overall variation accross observations for pheno
	real <lower = 0> sigma_apheno; // variation of intercept amoung species for pheno
	vector[nsppheno] mua_spheno; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_bphoto; // variation of intercept amoung species for pheno
	vector[nsppheno] muaphoto; // mean of the alpha value for species for pheno
        real bphoto_range; // our beta!
        //real bphoto_extent; // our beta!
  
}

transformed parameters{
	 real b_photo_final =  muaphoto + bphoto_range * mua_sp;
}


model{ 
	real ypredpheno[Npheno];
	
	vector[N] yrange = agrand_lat + agrand_extent + a_sp[species] + a_study[study];  
	
	for (iph in 1:Npheno){
	ypredpheno[iph] = a_spheno[speciespheno[iph]] + b_photo_final[iph] * photoperiod[iph];
        }
	
	mua_sp ~ normal(0, sigma_sp);
	mua_study ~ normal(0, sigma_study);

	sigma_y ~ normal(0, 3);
        agrand_lat ~ normal(40, 10);
        agrand_extent ~ normal(10, 10);
	sigma_sp ~ normal(0, 10);
	sigma_study ~ normal(0, 10);

        // Model of pheno
        mua_spheno ~ normal(0, sigma_apheno);
        muaphoto ~ normal(0, sigma_bphoto);
        sigma_ypheno ~ normal(0, 5);
        sigma_apheno ~ normal(0, 10);
        sigma_bphoto ~ normal(0, 5);

	// likelihoods 
        rangedat ~ normal(yrange, sigma_y);
        phendat ~ normal(ypredpheno, sigma_ypheno);
}

/* Not updated beyond lat ...
generated quantities {
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = agrand + mua_sp[species[n]] + mua_study[study[n]];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);
} */
