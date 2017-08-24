// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in July 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) on INTERCEPTS only 
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
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_force; // slope of forcing effect 
  real b_photo; // slope of photoperiod effect
  real b_chill; // slope of chill effect
  real b_cf; // slope of chill x force effect
  real b_cp; // slope of chill x photo effect
  real b_fp; // slope of force x photo effect

	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force * force[i] + 
	      	b_photo * photo[i] +
		b_chill * chill[i] +
                b_cf *  inter_cf[i] +
                b_cp * inter_cp[i] +
                b_fp * inter_fp[i];
			     	}
	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
       
        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
	b_force ~ normal(0, 10); 
	b_photo ~ normal(0, 10); 
	b_chill ~ normal(0, 10); 
	b_cf ~ normal(0, 10);
	b_cp ~ normal(0, 10);
	b_fp ~ normal(0, 30);

	y ~ normal(yhat, sigma_y);

}

