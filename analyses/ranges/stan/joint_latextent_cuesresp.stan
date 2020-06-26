/* First pass at a joint model to best estimate species-level lat values
and use that to predict phenoloy, by Lizzie with help from Faith and Geoff */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
  // Model of pheno
  int < lower = 1 > Npheno; // Sample size for pheno data 
  
  real mindat[N]; // y min lat data 
  real maxdat[N]; // y max lat data 
  
 	vector[Npheno] photodat; // y photo data 
 	vector[Npheno] forcedat; // y photo data 
 	vector[Npheno] chilldat; // y photo data 
 	
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower=0> sigma_y; // overall variation accross observations for lat
	
	//real agrand; // grand mean for trait
	
  real a_mins_sp[nsp]; // lower 10% of min latitudes per species
  real a_maxs_sp[nsp]; // upper 10% of max latitudes per species
	
  // Model of pheno
  real mua_sp[nsp]; // mean of the alpha value for species
  real <lower = 0> sigma_sp; // variation of intercept amoung species
  
  real a_photo[nsppheno]; // mean of the alpha value for species for photo
  
  vector[nsppheno] mu_bphotomin;
  vector[nsppheno] mu_bphotomax;
  
	real <lower=0> sigma_yphoto; // overall variation accross observations for photo

	real a_force[nsppheno]; // mean of the alpha value for species for force
	
	vector[nsppheno] mu_bforcemin;
	vector[nsppheno] mu_bforcemax;
  
	real <lower=0> sigma_yforce; // overall variation accross observations for force

	real a_chill[nsppheno]; // mean of the alpha value for species for chill
	
	vector[nsppheno] mu_bchillmin;
	vector[nsppheno] mu_bchillmax;
  
	real <lower=0> sigma_ychill; // overall variation accross observations for chill
	
	real mua_sforce[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_aforce; // variation of intercept amoung species for pheno
	
	real mua_sphoto[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_aphoto; // variation of intercept amoung species for pheno
	
	real mua_schill[nsppheno]; // mean of the alpha value for species for pheno
	real <lower = 0> sigma_achill; // variation of intercept amoung species for pheno*/
  
}

transformed parameters{
  real b_photomin[nsppheno];
  real b_photomax[nsppheno];
	real b_forcemin[nsppheno];
	real b_forcemax[nsppheno];
	real b_chillmin[nsppheno];
	real b_chillmax[nsppheno];
  
  for(i in 1:nsppheno){
    
    b_photomin[i] =  mua_sphoto[i] + mu_bphotomin * mua_sp[i]; // didn't work, a_photo worked much better but still too much variation among species
    b_photomax[i] =  mua_sphoto[i] + mu_bphotomax * mua_sp[i];
    
	  b_forcemin[i] =  mua_sforce[i] + mu_bforcemin * mua_sp[i];
	  b_forcemax[i] =  mua_sforce[i] + mu_bforcemax * mua_sp[i];
	  
	  b_chillmin[i] =  mua_schill[i] + mu_bchillmin * mua_sp[i];
	  b_chillmax[i] =  mua_schill[i] + mu_bchillmax * mua_sp[i];
	  
	}
}

model{ 
  real ypredphoto[Npheno];
	real ypredforce[Npheno];
	real ypredchill[Npheno];
	//real ypred[N];
	
	real latmins[N] = a_mins_sp[species]; 
	real latmaxs[N] = a_maxs_sp[species]; 
	
	mindat ~ normal(latmins, sigma_y);
  maxdat ~ normal(latmaxs, sigma_y);
	
	for(i in 1:Npheno){
	  
	ypredphoto[i] = a_photo[speciespheno[i]] + mu_bphotomin[speciespheno[i]]*a_mins_sp[speciespheno[i]] + 
	                mu_bphotomax[speciespheno[i]]*a_maxs_sp[speciespheno[i]];
	                
	ypredforce[i] = a_force[speciespheno[i]] + mu_bforcemin[speciespheno[i]]*a_mins_sp[speciespheno[i]] + 
	                mu_bforcemax[speciespheno[i]]*a_maxs_sp[speciespheno[i]];
	                
	ypredchill[i] = a_chill[speciespheno[i]] + mu_bchillmin[speciespheno[i]]*a_mins_sp[speciespheno[i]] + 
	                mu_bchillmax[speciespheno[i]]*a_maxs_sp[speciespheno[i]];
	
	}
  
  
  //ypred = agrand + ypredphoto + ypredforce + ypredchill;
  
  //a_grand ~ normal(30, 10)
  
  a_photo ~ normal(0, sigma_yphoto);
  a_force ~ normal(0, sigma_yforce);
  a_chill ~ normal(0, sigma_ychill);
  
  mua_sphoto ~ normal(0, sigma_aphoto);
  mua_sforce ~ normal(0, sigma_aforce);
  mua_schill ~ normal(0, sigma_achill);
  
  mu_bphotomin ~ normal(0, 2);
  mu_bphotomax ~ normal(0, 2);
  mu_bforcemin ~ normal(0, 2);
  mu_bforcemax ~ normal(0, 2);
  mu_bchillmin ~ normal(0, 2);
  mu_bchillmax ~ normal(0, 2);

  
  mua_sp ~ normal(0, sigma_sp);
  sigma_sp ~ normal(0, 2);
  
  sigma_y ~ normal(0, 3);
  sigma_yphoto ~ normal(0, 1);
  sigma_yforce ~ normal(0, 1);
  sigma_ychill ~ normal(0, 1);
  
  sigma_aphoto ~ normal(0, 2);
  sigma_aforce ~ normal(0, 2);
  sigma_achill ~ normal(0, 2);
  

	// likelihoods 
  
  photodat ~ normal(ypredphoto, sigma_yphoto);
  forcedat ~ normal(ypredforce, sigma_yforce);
  chilldat ~ normal(ypredchill, sigma_ychill);
  
  //yresp ~ normal(ypred, sigma_y);
  
}

generated quantities {
   real y_ppmin[N];
   real y_ppmax[N];
   real y_ppphoto[Npheno];
   real y_ppforce[Npheno];
   real y_ppchill[Npheno];
   //real y_pp[N]
   
   y_ppmin = a_mins_sp[species];
   y_ppmax = a_maxs_sp[species];
   
   y_ppmin = normal_rng(y_ppmin, sigma_y);
   y_ppmax = normal_rng(y_ppmax, sigma_y);
   
   for(i in 1:Npheno){
     
   y_ppphoto[i] = a_photo[speciespheno[i]] + mu_bphotomin[speciespheno[i]]*a_mins_sp[speciespheno[i]] + 
	                mu_bphotomax[speciespheno[i]]*a_maxs_sp[speciespheno[i]];
	                
   y_ppforce[i] = a_force[speciespheno[i]] + mu_bforcemin[speciespheno[i]]*a_mins_sp[speciespheno[i]] + 
	                mu_bforcemax[speciespheno[i]]*a_maxs_sp[speciespheno[i]];
	                
   y_ppchill[i] = a_chill[speciespheno[i]] + mu_bchillmin[speciespheno[i]]*a_mins_sp[speciespheno[i]] + 
	                mu_bchillmax[speciespheno[i]]*a_maxs_sp[speciespheno[i]];
   
   }
   
   //y_pp = agrand + y_ppphoto + y_ppforce + y_ppchill;
   
   y_ppphoto = normal_rng(y_ppphoto, sigma_yphoto);
   y_ppforce = normal_rng(y_ppforce, sigma_yforce);
   y_ppchill = normal_rng(y_ppchill, sigma_ychill);
   
   //y_pp = normal_rng(y_pp, sigma_y);
   
} 
