// OSPREE analysis
// 3-level model for budburst day a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species and population on INTERCEPTS and SLOPES, just forcing for now

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	int<lower=1> n_pop;
	int<lower=1, upper=n_pop> pop[N];
	vector[N] y; 		// response
	//vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	//vector[N] photo; 	// predictor
		
	}

parameters {
  real<lower=0> sigma_a_pop;
  real<lower=0> sigma_b_pop; 
  real mu_a_sp;   
  real mu_b_force_sp;   
//  real mu_b_photo_sp;   
//  real mu_b_chill_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
//  real<lower=0> sigma_b_photo_sp; 
//  real<lower=0> sigma_b_chill_sp; 
  real<lower=0> sigma_y; 
 
  real a_pop[n_pop]; //estimated intercept for each population
  real b_force_pop[n_pop]; //estimated slope for each population
  real a_sp[n_sp]; // intercept for species
  real b_force[n_sp]; // slope of forcing effect 
//  real b_photo[n_sp]; // slope of photoperiod effect
//  real b_chill[n_sp]; // slope of chill effect
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_pop[pop[i]] + // indexed with population
		b_force_pop[pop[i]] * force[i];
			     	}
}

model {
  for (j in 1:n_pop){
        a_pop[j] ~ normal(a_sp[sp[j]], sigma_a_pop); // indexed with species
        b_force_pop[j] ~ normal(b_force[sp[j]], sigma_b_pop);
	}

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
//	b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
//	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp); 

        sigma_a_pop ~ normal(0, 10);
        sigma_b_pop ~ normal(0, 10);
        mu_b_force_sp ~ normal(0, 50);
        sigma_b_force_sp ~ normal(0, 10);
//      mu_b_photo_sp ~ normal(0, 50);
//      sigma_b_photo_sp ~ normal(0, 10);
//      mu_b_chill_sp ~ normal(0, 50);
//      sigma_b_chill_sp ~ normal(0, 10);
        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
	
	y ~ normal(yhat, sigma_y);

}

generated quantities{

}
