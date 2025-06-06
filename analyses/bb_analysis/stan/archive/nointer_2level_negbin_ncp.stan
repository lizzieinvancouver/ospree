// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in July 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) on INTERCEPTS and SLOPES
// November 2018 work

/* NCP is on force and photo ONLY! */


data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	int y[N]; 		// response
	real chill[N]; 	// predictor
	real force[N]; 	// predictor
	real photo[N]; 	// predictor
		
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

  vector[n_sp] a_sp; // intercept for species
  vector[n_sp] b_force_ncp; // NCP slope of forcing effect 
  vector[n_sp] b_photo_ncp; // NCP slope of photoperiod effect
  vector[n_sp] b_chill; // slope of chill effect
	}

transformed parameters {
   vector[n_sp] b_force; 
   vector[n_sp] b_photo;
   vector[N] yhat;

   b_force = mu_b_force_sp + sigma_b_force_sp*b_force_ncp;
   b_photo = mu_b_photo_sp + sigma_b_photo_sp*b_photo_ncp;

       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i];
			     	}

	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	//b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
	//b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp); 
        b_force_ncp ~ normal(0,10);
        b_photo_ncp ~ normal(0,10);

        mu_b_force_sp ~ normal(0, 50);
        sigma_b_force_sp ~ normal(0, 10);
        mu_b_photo_sp ~ normal(0, 50);
        sigma_b_photo_sp ~ normal(0, 10);
        mu_b_chill_sp ~ normal(0, 50);
        sigma_b_chill_sp ~ normal(0, 10);
        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);
	
	y ~ neg_binomial_2_log(yhat, sigma_y);

}

/* Not working, lots of "Exception: neg_binomial_rng: Shape parameter is -0.759066, but must be > 0!"
generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_force[sp[n]] * force[n] + 
	      	b_photo[sp[n]] * photo[n] +
		b_chill[sp[n]] * chill[n];
    for (n in 1:N)
      y_ppc[n] = neg_binomial_rng(y_ppc[n], sigma_y);

}
*/



