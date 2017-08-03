// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in July 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) on INTERCEPTS and SLOPES
// This model has interactions! The two-way type. 

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
  vector[N] inter_cf;           
  vector[N] inter_cp;           
  vector[N] inter_fp;                  

  inter_cf    = chill .* force; 
  inter_cp    = chill .* photo; 
  inter_fp    = force .* photo; 
}

parameters {
  real mu_a_sp;   
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;   
  //real mu_b_cf_sp;   
  real mu_b_cp_sp;   
  real mu_b_fp_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp; 
  //real<lower=0> sigma_b_cf_sp; 
  real<lower=0> sigma_b_cp_sp; 
  real<lower=0> sigma_b_fp_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_force[n_sp]; // slope of forcing effect 
  real b_photo[n_sp]; // slope of photoperiod effect
  real b_chill[n_sp]; // slope of chill effect
  //real b_cf[n_sp]; // slope of chill x force effect
  real b_cp[n_sp]; // slope of chill x photo effect
  real b_fp[n_sp]; // slope of force x photo effect

	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i] +
               // b_cf[sp[i]] *  inter_cf[i] +
                b_cp[sp[i]] * inter_cp[i] +
                b_fp[sp[i]] * inter_fp[i];
			     	}
	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
	b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp); 
	//b_cf ~ normal(mu_b_cf_sp, sigma_b_cf_sp); 
	b_cp ~ normal(mu_b_cp_sp, sigma_b_cp_sp); 
	b_fp ~ normal(mu_b_fp_sp, sigma_b_fp_sp); 

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

       // mu_b_cf_sp ~ normal(0, 50);
       //sigma_b_cf_sp ~ normal(0, 10);
        mu_b_cp_sp ~ normal(0, 50);
        sigma_b_cp_sp ~ normal(0, 10);
        mu_b_fp_sp ~ normal(0, 50);
        sigma_b_fp_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}

