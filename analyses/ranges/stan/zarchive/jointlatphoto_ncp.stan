/* First pass at a joint model to best estimate species-level lat values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
 	int < lower = 1 > nstudy; // number of random effect levels (study) 
	int < lower = 1, upper = nstudy > study[N]; // id of random effect (study)
 	vector[N] latdat; // y lat data 
 	vector[N] extentdat; // y extent data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
       // Model of pheno
       	int < lower = 1 > Npheno; // Sample size for pheno data 
 	vector[Npheno] phendat; // y pheno data 
 	vector[Npheno] photoperiod; // predictor photoperiod 
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower =0> sigma_y; // overall variation accross observations for lat
        real agrand; // grand mean for lat
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
        real bphoto_lat; // our beta!
        real bphoto_extent; // our beta!
        
  /*real a_sp_ncp; // NCP mean of alpha value for species 
  real a_study_ncp; // NCP mean of alpha value for study
  real a_spheno_ncp;
  real a_photo_ncp;*/
  
}

transformed parameters{
	
	/*vector[nsp] a_sp = mua_sp + a_sp_ncp * sigma_sp; // mean of the alpha value for species
  vector[nstudy] a_study = mua_study + a_study_ncp * sigma_study; // mean of the alpha value for studies 
	vector[nsppheno] a_spheno = mua_spheno + a_spheno_ncp * sigma_apheno;
	vector[nsppheno] a_photo = mua_photo + a_photo_ncp * sigma_bphoto;*/
	
        
	 real b_photo_final =  muaphoto + bphoto_lat * mua_sp + bphoto_extent * mua_sp;
}


model{ 
        real ypred[N];
	real ypredpheno[Npheno];
	for (i in 1:N){
	    ypred[i] = agrand + a_sp[species[i]] + a_study[study[i]];  
	}
	for (iph in 1:Npheno){
	ypredpheno[iph] = a_spheno[speciespheno[iph]] + b_photo_final[iph] * photoperiod[iph];
        }
	// Model of lat
	a_sp_ncp ~ normal(0, 1);
	a_study_ncp ~ normal(0, 1);
	a_sppheno_ncp ~ normal(0, 1);
	a_photo_ncp ~ normal(0, 1);
	
	
	mua_sp ~ normal(0, sigma_sp);
	mua_study ~ normal(0, sigma_study);

	sigma_y ~ normal(0, 3);
        agrand ~ normal(30, 10);
	sigma_sp ~ normal(0, 10);
	sigma_study ~ normal(0, 10);

        // Model of pheno
        mua_spheno ~ normal(0, sigma_apheno);
        muaphoto ~ normal(0, sigma_bphoto);
        sigma_ypheno ~ normal(0, 5);
        sigma_apheno ~ normal(0, 10);
        sigma_bphoto ~ normal(0, 5);

	// likelihoods 
        latdat ~ normal(ypred, sigma_y);
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
