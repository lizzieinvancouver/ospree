// OSPREE analysis
// flynn@fas.harvard.edu
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod, and latitude in a meta-analysis of 100+ studies
// Levels: Species. No labgroup level, see if model behaves.
// no interactions... still bad, why?

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	vector[N] lat;		// predictor
	}

parameters {
  real a_0; // overall intercept, same as "beta_0" in classroom example model
  real b_force_0; // overall forcing effect
  real b_photo_0; // overall photoperiod effect  
  real b_lat_0; // overall lat effect  
  real b_chill_0; // overall chill effect  
 
  real mu_a_sp[n_sp];
  real mu_b_force_sp[n_sp]; 
  real mu_b_photo_sp[n_sp];
  real mu_b_lat_sp[n_sp];
  real mu_b_chill_sp[n_sp];

  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp;
  real<lower=0> sigma_b_lat_sp;
  real<lower=0> sigma_b_chill_sp;

  real<lower=0> sigma_y; 
  }

transformed parameters {
	real y_hat[N];
	
	real a_sp[n_sp]; // intercept for species
  	real b_force_sp[n_sp]; // slope of forcing effect at species level
	real b_photo_sp[n_sp]; // slope of photoperiod effect, at species level
	real b_lat_sp[n_sp]; // slope of photoperiod effect, at species level
	real b_chill_sp[n_sp]; // slope of chill effect, at species level
	
	// Species level. Random intercept (a) and slopes (b) for forcing, photoperiod, chilling, lat, 2-way interax
	for (k in 1:n_sp) {
		
		a_sp[k] 		= a_0 + mu_a_sp[k];
		b_force_sp[k] 	= b_force_0 + mu_b_force_sp[k];
		b_photo_sp[k] 	= b_photo_0 + mu_b_photo_sp[k];
		b_lat_sp[k] 	= b_lat_0 + mu_b_lat_sp[k];
		b_chill_sp[k] 	= b_chill_0 + mu_b_chill_sp[k];
	
		}
	
	// row level 
	for(i in 1:N){

		y_hat[i] = a_sp[sp[i]] + // indexed with species
					b_force_sp[sp[i]] * force[i] + // indexed with species
					b_photo_sp[sp[i]] * photo[i]+
					b_lat_sp[sp[i]] * lat[i] +
					b_chill_sp[sp[i]] * chill[i] 
					;
					}
	}

model {
	a_0 ~ normal(0, 100); // ~~ 26 for budburst, 37 for leafout
	b_force_0 ~ normal(0, 100);
	b_photo_0 ~ normal(0, 100);
	b_lat_0 ~ normal(0, 100);
	b_chill_0 ~ normal(0, 100);

	mu_a_sp ~ normal(0, sigma_a_sp); // 20 d on either lat at sp level
	mu_b_force_sp ~ normal(0, sigma_b_force_sp); 
	mu_b_photo_sp ~ normal(0, sigma_b_photo_sp);
	mu_b_lat_sp ~ normal(0, sigma_b_lat_sp);
	mu_b_chill_sp ~ normal(0, sigma_b_chill_sp);
		
	sigma_a_sp ~ normal(0, 20); 
	sigma_b_force_sp ~ normal(0, 20); 
	sigma_b_photo_sp ~ normal(0, 20); 
	sigma_b_lat_sp ~ normal(0, 20); 
	sigma_b_chill_sp ~ normal(0, 20); 
	
	y ~ normal(y_hat, sigma_y);
}
