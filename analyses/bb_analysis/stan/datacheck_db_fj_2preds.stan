// OSPREE analysis
// adapted for data checking by DAn on March 31 2020. @ predictors
// Level: Species (actually genus) on INTERCEPTS and SLOPES

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] pred1; 	// predictor
	vector[N] pred2; 	// predictor
		
	}

parameters {
  real mu_a_sp;   
  real mu_b_pred1_sp;   
  real mu_b_pred2_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_pred1_sp; 
  real<lower=0> sigma_b_pred2_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_pred1[n_sp]; // slope of forcing effect 
  real b_pred2[n_sp]; // slope of photoperiod effect 
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_pred1[sp[i]] * pred1[i] + 
	      	b_pred2[sp[i]] * pred2[i];
			     	}

	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_pred1 ~ normal(mu_b_pred1_sp, sigma_b_pred1_sp); 
	b_pred1~ normal(mu_b_pred2_sp, sigma_b_pred1_sp); 

        mu_b_pred1_sp ~ normal(0, 50);
        sigma_b_pred1_sp ~ normal(0, 10);
        mu_b_pred2_sp ~ normal(0, 50);
        sigma_b_pred2_sp ~ normal(0, 10);
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
		b_pred1[sp[n]] * pred1[n] + 
	      	b_pred2[sp[n]] * pred2[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}
