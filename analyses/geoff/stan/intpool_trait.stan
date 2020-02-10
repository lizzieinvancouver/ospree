data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] trait;        // predictor
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	}
	
		

transformed data {
}
 

parameters {
  real mu_a_sp;   
  real b_force;
  real b_force_trait;
  real b_photo;   
  real b_chill;
    
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
}

transformed parameters {
	    vector[N] b_force_final;
	    vector[N] yhat;
	    
	    for(i in 1:N){
	    	  b_force_final[i] = b_force + b_force_trait * trait[i];
	    }
	    
	    for(i in 1:N){
            	  yhat[i] = a_sp[sp[i]] + // indexed with species
		  b_force_final[i] * force[i] + 
	      	  b_photo * photo[i] +
		  b_chill * chill[i];
	    }
}
	
model {
	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(0, 50);
	b_force_trait ~ normal(0, 50);
	b_photo ~ normal(0, 50); 
	b_chill ~ normal(0, 50);

     	mu_a_sp ~ normal(60, 10);
        sigma_a_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}

