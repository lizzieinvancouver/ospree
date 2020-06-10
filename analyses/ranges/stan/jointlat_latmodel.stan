/* running a joint model to try and see how range variation might help 
predict phenology. Based off Lizzie's joint model equation 
By Cat, with lots of help from Lizzie and Faith */

data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
 	vector[N] mindat; // y min lat data 
 	vector[N] maxdat; // y max lat data 
 	//vector[N] meandat; // y mean lat data 
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower =0> sigma_y; // overall variation across observations
  real a_mins_sp[nsp]; // lower 10% of min latitudes per species
  real a_maxs_sp[nsp]; // upper 10% of max latitudes per species
  //real a_means_sp[nsp]; // species top 10% of max latitudes per species

}

model{ 
  real ymin[N];
  real ymax[N];
  //real ymean[N];
  
  ymin = a_mins_sp[species];
  ymax = a_maxs_sp[species];
  //ymean = a_means_sp[species];

	sigma_y ~ normal(0, 3);
  //a_lat_sp ~ normal(40, 10);
  //a_extent_sp ~ normal(10, 10);

	// likelihood
        mindat ~ normal(ymin, sigma_y);
        maxdat ~ normal(ymax, sigma_y);
        //meandat ~ normal(ymean, sigma_y);
}


generated quantities {
   real y_ppmin[N];
   real y_ppmax[N];
   //real y_ppmean[N];
   
      y_ppmin = a_mins_sp[species];
      y_ppmax = a_maxs_sp[species];
      //y_ppmean = a_means_sp[species];
      
    y_ppmin = normal_rng(y_ppmin, sigma_y);
    y_ppmax = normal_rng(y_ppmax, sigma_y);
    //y_ppmean = normal_rng(y_ppmean, sigma_y);
    
} 
