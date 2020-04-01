data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
		
	}

parameters {
  real mu_a_sp;   
  //real mu_b_chill_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 
  //real<lower=0> sigma_b_chill_sp;

  real a_sp[n_sp]; // intercept for species
  real b_chill; // slope of chill effect
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_chill * chill[i];
			     	}

	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	//b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp);

        b_chill ~ normal(0, 50);
        //sigma_b_chill ~ normal(0, 10);
        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);
	
	y ~ normal(yhat, sigma_y);

}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_chill* chill[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}
