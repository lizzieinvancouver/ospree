// ``Sequential" modeling for range project where we
// 1) Run traditional OSPREE budburst model (nointer_2level.stan)
// Then 2)) make a new model where estimating the effect of climate range variable and continent on species' cue intesity
//WIth current formulate it can initialize


data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	
  vector[n_sp] climvar; // climate variable for each species
  vector[n_sp] continent; // continent dummy variable
 
	}
	
	transformed data {
  vector[n_sp] inter_climcont= climvar .* continent;  
	}

parameters {
  real mu_a_sp;   
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_force[n_sp]; // slope of forcing effect 
  real b_photo[n_sp]; // slope of photoperiod effect
  real b_chill[n_sp]; // slope of chill effect
	
	//real  b_force2[n_sp]; //some dummy version of b_force for second model
	real b_climforce; //climate parameter effect
	real b_contforce; // continent effect
	real b_climcontforce; // slope of continet x climate effect

  real b_climphoto; //climate parameter effect
	real b_contphoto; // continent effect
	real b_climcontphoto; // slope of continet x climate effect
	
	 real b_climchill; //climate parameter effect
	real b_contchill; // continent effect
	real b_climcontchill; // slope of continet x climate effect
	
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i];
			     	}


 

	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
	b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp);
	

        mu_b_force_sp ~ normal(0, 50);
        sigma_b_force_sp ~ normal(0, 10);
        mu_b_photo_sp ~ normal(0, 50);
        sigma_b_photo_sp ~ normal(0, 10);
        mu_b_chill_sp ~ normal(0, 50);
        sigma_b_chill_sp ~ normal(0, 10);
        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);

	b_climforce ~ normal(0, 10);
	b_contforce ~ normal(0, 10);
	b_climcontforce ~ normal(0, 10);
		
	b_climphoto ~ normal(0, 10);
	b_contphoto~ normal(0, 10);
	b_climcontphoto ~ normal(0, 10);
	
	b_climchill~ normal(0, 10);
	b_contchill ~ normal(0, 10);
	b_climcontchill ~ normal(0, 10);

	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);
	
	y ~ normal(yhat, sigma_y);
	
		// will need something like below but I am not sure exactly how to do it without multipl definin b_force
	//Woof my stan coding is rusty
  b_force ~ normal(mu_b_force_sp+ b_climforce * climvar + b_contforce * continent +b_climcontforce *  inter_climcont,sigma_b_force_sp);

  b_photo ~ normal(mu_b_photo_sp+ b_climphoto * climvar + b_contphoto * continent +b_climcontphoto *  inter_climcont,sigma_b_force_sp);

 b_chill~ normal(mu_b_chill_sp+ b_climchill * climvar + b_contchill * continent +b_climcontchill *  inter_climcont,sigma_b_force_sp);



}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_force[sp[n]] * force[n] + 
	      	b_photo[sp[n]] * photo[n] +
		b_chill[sp[n]] * chill[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);


}

