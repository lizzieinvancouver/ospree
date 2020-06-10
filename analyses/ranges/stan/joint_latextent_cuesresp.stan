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
  
 	vector[Npheno] photodat; // y photo data 
 	vector[Npheno] forcedat; // y photo data 
 	vector[Npheno] chilldat; // y photo data 
 	
	int < lower = 1 > nsppheno; // number of random effect levels (species) 
	int < lower = 1, upper = nsppheno > speciespheno[Npheno]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower=0> sigma_y; // overall variation accross observations for lat
	
  real a_mins_sp[nsp]; // lower 10% of min latitudes per species
  real a_maxs_sp[nsp]; // upper 10% of max latitudes per species
  
  //real agrand; // grand mean for response
	
  // Model of pheno
  real mu_photo_sp;
  real a_photo[nsppheno]; // mean of the alpha value for species for photo
	real <lower=0> sigma_yphoto; // overall variation accross observations for photo
	
	real mu_force_sp;
	real a_force[nsppheno]; // mean of the alpha value for species for force
	real <lower=0> sigma_yforce; // overall variation accross observations for force

  real mu_chill_sp;
	real a_chill[nsppheno]; // mean of the alpha value for species for chill
	real <lower=0> sigma_ychill; // overall variation accross observations for chill
  
}

model{ 
  real ypredphoto[Npheno];
	real ypredforce[Npheno];
	real ypredchill[Npheno];
	//real ypred[N];
	
	real latmins[N] = a_mins_sp[species]; 
	real latmaxs[N] = a_maxs_sp[species]; 
	
	for(i in 1:Npheno){
	  
	ypredphoto[i] = a_photo[speciespheno[i]] + a_mins_sp[speciespheno[i]] + a_maxs_sp[speciespheno[i]];
	ypredforce[i] = a_force[speciespheno[i]] + a_mins_sp[speciespheno[i]] + a_maxs_sp[speciespheno[i]];
	ypredchill[i] = a_chill[speciespheno[i]] + a_mins_sp[speciespheno[i]] + a_maxs_sp[speciespheno[i]];
	
	}
  
  
  //ypred = agrand + ypredphoto + ypredforce + ypredchill;
  
  //agrand ~ normal(50, 10)
  a_photo ~ normal(mu_photo_sp, sigma_yphoto);
  a_force ~ normal(mu_force_sp, sigma_yforce);
  a_chill ~ normal(mu_chill_sp, sigma_ychill);
  
  mu_photo_sp ~ normal(0, 2);
  mu_force_sp ~ normal(0, 5);
  mu_chill_sp ~ normal(0, 10);
  
  sigma_y ~ normal(0, 3);
  sigma_yphoto ~ normal(0, 1);
  sigma_yforce ~ normal(0, 1);
  sigma_ychill ~ normal(0, 1);

	// likelihoods 
  mindat ~ normal(latmins, sigma_y);
  maxdat ~ normal(latmaxs, sigma_y);
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
   
   for(i in 1:Npheno){
     
   y_ppphoto[i] = a_photo[speciespheno[i]] + a_mins_sp[speciespheno[i]] + a_maxs_sp[speciespheno[i]];
   y_ppforce[i] = a_force[speciespheno[i]] + a_mins_sp[speciespheno[i]] + a_maxs_sp[speciespheno[i]];
   y_ppchill[i] = a_chill[speciespheno[i]] + a_mins_sp[speciespheno[i]] + a_maxs_sp[speciespheno[i]];
   
   }
   
   //y_pp = agrand + y_ppphoto + y_ppforce + y_ppchill;
      
   y_ppmin = normal_rng(y_ppmin, sigma_y);
   y_ppmax = normal_rng(y_ppmax, sigma_y);
   y_ppphoto = normal_rng(y_ppphoto, sigma_yphoto);
   y_ppforce = normal_rng(y_ppforce, sigma_yforce);
   y_ppchill = normal_rng(y_ppchill, sigma_ychill);
   
   //y_pp = normal_rng(y_pp, sigma_y);
   
} 
