// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in July 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) on INTERCEPTS and SLOPES
// This model has interactions! The two-way type but it does not have species grouping on them!

/* Non-centered parameterization */

/* Our PPC does not look good for overall ypred
   Other distributions we could try: gamma, but then we cannot have zero (could do hurdle with gamma)
   Or treat the data as integers and fit a negative binomial, as I try below. */

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	int y[N]; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
		
	}

transformed data {
  vector[N] inter_cf;           
  vector[N] inter_cp;           
  vector[N] inter_fp;                  

  inter_cf    = chill .* force; 
  inter_cp    = chill .* photo; 
  inter_fp    = force .* photo; 
}

parameters {
  real a0;   
  real mu_a_sp;   
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;    
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp; 
  real<lower=0> sigma_y; 

  vector[n_sp]  a_sp_ncp; // NCP intercept for species
  vector[n_sp]  b_force_ncp; // NCP slope of forcing effect 
  vector[n_sp]  b_photo_ncp; // NCP slope of photoperiod effect
  vector[n_sp]  b_chill_ncp; // NCP slope of chill effect
  real b_cf; // slope of chill x force effect
  real b_cp; // slope of chill x photo effect
  real b_fp; // slope of force x photo effect

	}

transformed parameters {
   vector[n_sp] a_sp; 
   vector[n_sp] b_force; 
   vector[n_sp] b_photo;
   vector[n_sp] b_chill; 
   vector[N] yhat;
 
   a_sp = mu_a_sp + sigma_a_sp*a_sp_ncp;
   b_force = mu_b_force_sp + sigma_b_force_sp*b_force_ncp;
   b_photo = mu_b_photo_sp + sigma_b_photo_sp*b_photo_ncp;
   b_chill = mu_b_chill_sp + sigma_b_chill_sp*b_chill_ncp;

   
       	for(i in 1:N){
            yhat[i] = a0 + a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i] +
                b_cf *  inter_cf[i] +
                b_cp * inter_cp[i] +
                b_fp * inter_fp[i];
			     	}
	}

model {

	// a_sp ~ normal(mu_a_sp, sigma_a_sp); // NCP now 
	// b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); // NCP now 
	// b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); // NCP now 
	// b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp); // NCP now 
       
        a0 ~ normal(50, 50);
        mu_a_sp ~ normal(0, 50); // NCP now 
        sigma_a_sp ~ normal(0, 10);
	b_cf ~ normal(0, 10);
	b_cp ~ normal(0, 10);
	b_fp ~ normal(0, 10);

        mu_b_force_sp ~ normal(0, 10); 
        sigma_b_force_sp ~ normal(0, 10);
        mu_b_photo_sp ~ normal(0, 10); 
        sigma_b_photo_sp ~ normal(0, 10);
        mu_b_chill_sp ~ normal(0, 10);  
        sigma_b_chill_sp ~ normal(0, 15);
    
        a_sp_ncp ~ normal(0,30);
        b_force_ncp ~ normal(0,30);
        b_photo_ncp ~ normal(0,30);
        b_chill_ncp ~ normal(0,30);

	y ~ neg_binomial_2_log(yhat, sigma_y);// 

}
/*
generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_force[sp[n]] * force[n] + 
	      	b_photo[sp[n]] * photo[n] +
		b_chill[sp[n]] * chill[n] +
                b_cf *  inter_cf[n] +
                b_cp * inter_cp[n] +
                b_fp * inter_fp[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);
}
*/
