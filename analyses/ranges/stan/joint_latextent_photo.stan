/* First pass at a joint model to best estimate species-level lat values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
  // Model of pheno
  int < lower = 1 > Npheno; // Sample size for pheno data 
  
  vector[N] mindat; // y min lat data 
  vector[N] maxdat; // y max lat data 
  
 	vector[Npheno] phendat; // y pheno data 
 	vector[Npheno] photoperiod; // predictor photoperiod 
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower =0> sigma_y; // overall variation accross observations for lat
	
  vector[nsp] a_mins_sp; // lower 10% of min latitudes per species
  vector[nsp] a_maxs_sp; // upper 10% of max latitudes per species
	
  // Model of pheno
	real <lower =0> sigma_ypheno; // overall variation accross observations for pheno
	real <lower = 0> sigma_apheno; // variation of intercept amoung species for pheno
	real <lower = 0> sigma_sp; // variation of intercept amoung species
	vector[nsp] mua_sp; // mean of the alpha value for species
	vector[nsppheno] mua_spheno; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_bphoto; // variation of intercept amoung species for pheno
	vector[nsppheno] muaphoto; // mean of the alpha value for species for pheno
	
  real bphoto_minlat; // our beta!
  real bphoto_maxlat; // another beta!
  
}

transformed parameters{
	 vector[nsppheno] b_photomin_final =  muaphoto + bphoto_minlat * mua_sp;
   vector[nsppheno] b_photomax_final =  muaphoto + bphoto_maxlat * mua_sp;
	 
}


model{ 
	real ypredpheno;
	
	vector[N] ymins = a_mins_sp[species]; 
	vector[N] ymaxs = a_maxs_sp[species]; 
	
	for (iph in 1:Npheno){
	ypredpheno = mua_spheno[speciespheno[iph]] + 
	                          b_photomin_final[speciespheno[iph]] * photoperiod[iph] + 
	                    b_photomax_final[speciespheno[iph]] * photoperiod[iph];
        }
	
	//mua_sp ~ normal(0, sigma_sp);

	sigma_y ~ normal(0, 3);
	mua_sp ~ normal(0, sigma_sp);
  sigma_sp ~ normal(0, 10);

  // Model of pheno
  mua_spheno ~ normal(0, sigma_apheno);
  muaphoto ~ normal(0, sigma_bphoto);
  sigma_ypheno ~ normal(0, 5);
  sigma_apheno ~ normal(0, 10);
  sigma_bphoto ~ normal(0, 5);

	// likelihoods 
  mindat ~ normal(ymins, sigma_y);
  maxdat ~ normal(ymaxs, sigma_y);
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
