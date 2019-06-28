// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in Feb 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) only on INTERCEPTS

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	}

parameters {
  real mu_a_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_force; // slope of forcing effect 
  real b_photo; // slope of photoperiod effect
  real b_chill; // slope of chill effect
	
	}
	
transformed parameters {
  real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force * force[i] + 
  	      	b_photo * photo[i] + 
                b_chill * chill[i];
			     	
			 }
}
	

model {
	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
        b_chill ~ normal(0,10);
      	b_force ~ normal(0, 10);
      	b_photo ~ normal(0, 10);
	
	y ~ normal(yhat, sigma_y);

}

