/* running a joint model to try and see how range variation might help 
predict phenology. Based off Lizzie's joint model equation 
By Cat, with lots of help from Lizzie and Faith */


data {
	// Model of lat 
	int < lower = 1 > N; // Sample size for lat data 
 	int < lower = 1 > nstudy; // number of random effect levels (study) 
	int < lower = 1, upper = nstudy > study[N]; // id of random effect (study)
 	vector[N] rangedat; // y lat and extent data 
 	//vector[N] extentdat; // y extent data
 	//vector[N] latdat; // y lat data
	int < lower = 1 > nsp; // number of random effect levels (species) 
	int < lower = 1, upper = nsp > species[N]; // id of random effect (species)
}


parameters{
	// Model of lat
	real <lower =0> sigma_y; // overall variation across observations
        real agrand_lat; // grand mean for lat
        real agrand_extent; // grand mean for extent
	real <lower = 0> sigma_sp; // variation of intercept among species
	real <lower = 0> sigma_study; // variation of intercept among studies
	
	vector[nsp] mua_sp; // mean of the alpha value for species
	vector[nstudy] mua_study; // mean of the alpha value for studies 

  
}  

model{ 
  vector[N] yrange;
  
  yrange = agrand_lat + agrand_extent + mua_sp[species] + mua_study[study];

	mua_sp ~ normal(0, sigma_sp);
	mua_study ~ normal(0, sigma_study);
	
	sigma_y ~ normal(0, 3);
        agrand_lat ~ normal(40, 10);
        agrand_extent ~ normal(10, 10);
	sigma_sp ~ normal(0, 10);
	sigma_study ~ normal(0, 10);

	// likelihood
	      //latdat ~ normal(ylat, sigma_y);
	      //extentdat ~ normal(yextent, sigma_y);
        rangedat ~ normal(yrange, sigma_y);
}


generated quantities {
   vector[N] y_ppc;
   
      y_ppc = agrand_lat + agrand_extent + mua_sp[species] + mua_study[study];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);
} // The posterior predictive distribution
