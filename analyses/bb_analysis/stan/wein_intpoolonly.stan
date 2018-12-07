

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	vector[N] weinberger; //predictor	
	}
	
		

transformed data {
  vector[N] inter_cw;           
  vector[N] inter_fw;           
  vector[N] inter_pw;                  

  
  inter_cw    = weinberger .* chill; 
  inter_fw    = weinberger .* force; 
  inter_pw    = weinberger .* photo; 
}

parameters {
  real mu_a_sp;   
  real b_force;   
  real b_photo;   
  real b_chill;
    
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 
  
    real a_sp[n_sp]; // intercept for species
  real b_weinberger; //slope of weinberger effect
  real b_cw; // slope of chill x weinberger effect
  real b_fw; // slope of force x weinberger effect
  real b_pw; // slope of  photo x weinberger effect
}

	transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force * force[i] + 
	      	b_photo * photo[i] +
		b_chill * chill[i] +
		b_weinberger * weinberger[i] +
                b_cw *  inter_cw[i] +
                b_fw * inter_fw[i] +
                b_pw * inter_pw[i];
			     	}
	}
	
		model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(0,50); 
	b_photo ~ normal(0,50); 
	b_chill ~ normal(0,50);
	b_weinberger ~normal(0,100);
	b_fw ~ normal(0,50);
	b_pw ~ normal(0,50);
	b_cw ~ normal(0,50);

     mu_a_sp ~ normal(0, 50);
    sigma_a_sp ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);
  

	y ~ normal(yhat, sigma_y);

}

