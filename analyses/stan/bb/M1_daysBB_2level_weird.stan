// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in Feb 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) only on SLOPES and INTERCEPTS
// Includes: Interactions 

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
		
	}

transformed data { 			// 4 two-way interaction terms 
	vector[N] inter_fp; 	// forcing x photo                    
	vector[N] inter_fc;           
	vector[N] inter_pc;           

	inter_fp   <- force .* photo;  
	inter_fc   <- force .* chill;  
	inter_pc   <- photo .* chill;  
}

parameters {
  real a_0; // overall intercept, same as "beta_0" in classroom example model
  real b_force_0; // overall forcing effect
  real b_photo_0; // overall photoperiod effect  
  real b_chill_0; // overall chill effect  
  real b_inter_fp_0; // overall forcing x photoperiod effect  
  real b_inter_fc_0; // overall forcing x chill effect  
  real b_inter_pc_0; // overall photoperiod x chill effect  

  real mu_a_sp[n_sp];
  real mu_b_force_sp[n_sp]; 
  real mu_b_photo_sp[n_sp];
  real mu_b_chill_sp[n_sp];
  real mu_b_inter_fp_sp[n_sp];
  real mu_b_inter_fc_sp[n_sp];
  real mu_b_inter_pc_sp[n_sp];
      
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp;
  real<lower=0> sigma_b_chill_sp;
  real<lower=0> sigma_b_inter_fp_sp;
  real<lower=0> sigma_b_inter_fc_sp;
  real<lower=0> sigma_b_inter_pc_sp;

  real<lower=0> sigma_y; 

	real a_sp[n_sp]; // intercept for species
  	real b_force_sp[n_sp]; // slope of forcing effect at species level
	real b_photo_sp[n_sp]; // slope of photoperiod effect, at species level
	real b_chill_sp[n_sp]; // slope of chill effect, at species level
	real b_inter_fp_sp[n_sp]; // slope of forcing x photoperiod effect, at species level
	real b_inter_fc_sp[n_sp]; // slope of forcing x chill effect, at species level
	real b_inter_pc_sp[n_sp]; // slope of photoperiod x chill effect, at species level
	}

model {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
					b_force_sp[sp[i]] * force[i] + // indexed with species
					b_photo_sp[sp[i]] * photo[i]+
					b_chill_sp[sp[i]] * chill[i] +
					b_inter_fp_sp[sp[i]] * inter_fp[i]+
					b_inter_fc_sp[sp[i]] * inter_fc[i]+
					b_inter_pc_sp[sp[i]] * inter_pc[i];
					}
	a_sp ~ normal(0, 50); //
	b_force_0 ~ normal(0, 30);
	b_photo_0 ~ normal(0, 30);
	b_chill_0 ~ normal(0, 30);
	b_inter_fp_0 ~ normal(0, 30);
 	b_inter_fc_0 ~ normal(0, 30);
  	b_inter_pc_0 ~ normal(0, 30);
  	
	mu_a_sp ~ normal(0, sigma_a_sp); // 20 d on either lat at sp level
	mu_b_force_sp ~ normal(0, sigma_b_force_sp); 
	mu_b_photo_sp ~ normal(0, sigma_b_photo_sp);
	mu_b_chill_sp ~ normal(0, sigma_b_chill_sp);
	mu_b_inter_fp_sp ~ normal(0, sigma_b_inter_fp_sp);
	mu_b_inter_fc_sp ~ normal(0, sigma_b_inter_fc_sp);
	mu_b_inter_pc_sp ~ normal(0, sigma_b_inter_pc_sp);
		
	sigma_a_sp ~ normal(0, 20); 
	sigma_b_force_sp ~ normal(0, 20); 
	sigma_b_photo_sp ~ normal(0, 20); 
	sigma_b_chill_sp ~ normal(0, 20); 
	sigma_b_inter_fp_sp ~ normal(0, 20); 
	sigma_b_inter_fc_sp ~ normal(0, 20);
	sigma_b_inter_pc_sp ~ normal(0, 20);
	
	y ~ normal(yhat, sigma_y);

}
