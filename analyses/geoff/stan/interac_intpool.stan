
data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	}
	
transformed data {
  // vector[N] inter_cf;           
  // vector[N] inter_cp;           
  // vector[N] inter_fp;                    
  // inter_cf    = chill .* force; 
  // inter_cp    = chill .* photo; 
  // inter_fp    = force .* photo; 
}

parameters {
  real mu_a_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 
  real b_force;
  real b_photo;
  real b_chill;
  real b_cf;
  real b_cp;
  real b_fp;
  
  real a_sp[n_sp]; // intercept for species
}
	
transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force * force[i] + 
	      	b_photo * photo[i] +
		b_chill * chill[i] +
                b_cf * (chill[i] * force[i]) +
                b_cp * (chill[i] * photo[i]) +
                b_fp * (force[i] * photo[i]);
			     	}
}
	
model {
	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(0, 50); 
	b_photo ~ normal(0, 50); 
	b_chill ~ normal(0, 50);
	b_cf ~ normal(0, 50);
	b_cp ~ normal(0, 50);
	b_fp ~ normal(0, 50);

     	mu_a_sp ~ normal(60, 10);
        sigma_a_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}
