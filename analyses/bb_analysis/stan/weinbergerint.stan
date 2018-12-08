
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
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;
  real mu_b_weinberger_sp;
  real mu_b_cw_sp;   
  real mu_b_fw_sp;   
  real mu_b_pw_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp;
  real<lower=0> sigma_b_weinberger_sp;
  real<lower=0> sigma_b_cw_sp; 
  real<lower=0> sigma_b_fw_sp; 
  real<lower=0> sigma_b_pw_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_force[n_sp]; // slope of forcing effect 
  real b_photo[n_sp]; // slope of photoperiod effect
  real b_chill[n_sp]; // slope of chill effect
  real b_weinberger[n_sp]; //slope of weinberger effect
  real b_cw[n_sp]; // slope of chill x weinberger effect
  real b_fw[n_sp]; // slope of force x weinberger effect
  real b_pw[n_sp]; // slope of  photo x weinberger effect

	}
	
	transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i] +
		b_weinberger[sp[i]] * weinberger[i] +
                b_cw[sp[i]] *  inter_cw[i] +
                b_fw[sp[i]] * inter_fw[i] +
                b_pw[sp[i]] * inter_pw[i];
			     	}
	}
	
	model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
	b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp);
	b_weinberger~ normal(mu_b_weinberger_sp, sigma_b_weinberger_sp);
	b_cw ~ normal(mu_b_cw_sp, sigma_b_cw_sp); 
	b_fw ~ normal(mu_b_cw_sp, sigma_b_cw_sp); 
	b_pw ~ normal(mu_b_cw_sp, sigma_b_cw_sp); 

        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);

        mu_b_force_sp ~ normal(0, 50);
        sigma_b_force_sp ~ normal(0, 10);
        mu_b_photo_sp ~ normal(0, 50);
        sigma_b_photo_sp ~ normal(0, 10);
        mu_b_chill_sp ~ normal(0, 50);
        sigma_b_chill_sp ~ normal(0, 10);
        mu_b_weinberger_sp ~ normal(0, 50);
        sigma_b_weinberger_sp ~ normal(0, 10);

        mu_b_cw_sp ~ normal(0, 50);
        sigma_b_cw_sp ~ normal(0, 10);
        mu_b_fw_sp ~ normal(0, 50);
        sigma_b_fw_sp ~ normal(0, 10);
        mu_b_pw_sp ~ normal(0, 50);
        sigma_b_pw_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}
