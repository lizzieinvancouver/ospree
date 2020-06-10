/* First pass at a joint model to best estimate species-level lat values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
  // Model of pheno
  int < lower = 1 > Npheno; // Sample size for pheno data 
  
  vector[N] rangedat; // y lat data 
  
 	vector[Npheno] phendat; // y pheno data 
 	vector[Npheno] photoperiod; // predictor photoperiod 
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower =0> sigma_y; // overall variation accross observations for lat
	
  vector[nsp] a_lat_sp; // grand mean for lat
  vector[nsp] a_extent_sp; // grand mean for latitudinal extent of range
  
	real <lower = 0> sigma_sp; // variation of intercept amoung species
	vector[nsp] mua_sp; // mean of the alpha value for species
	
  // Model of pheno
	real <lower =0> sigma_ypheno; // overall variation accross observations for pheno
	real <lower = 0> sigma_apheno; // variation of intercept amoung species for pheno
	vector[nsppheno] mua_spheno; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_bphoto; // variation of intercept amoung species for pheno
	vector[nsppheno] muaphoto; // mean of the alpha value for species for pheno
  real bphoto_range; // our beta!
  
}

transformed parameters{
	 real b_photo_final =  muaphoto + bphoto_range * mua_sp;
}


model{ 
	real ypredpheno[Npheno];
	
	vector[N] ymins = a_mins_sp[species];  
	
	for (iph in 1:Npheno){
	ypredpheno[iph] = a_spheno[speciespheno[iph]] + b_photo_final[iph] * photoperiod[iph];
        }
	
	mua_sp ~ normal(0, sigma_sp);

	sigma_y ~ normal(0, 3);
  a_lat_sp ~ normal(40, 10);
  a_extent_sp ~ normal(10, 10);
  
	sigma_sp ~ normal(0, 10);

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
