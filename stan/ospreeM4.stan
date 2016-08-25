// OSPREE analysis
// flynn@fas.harvard.edu
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod, and latitude in a meta-analysis of 100+ studies
// Levels: Species. 
// Re-doing based on lday models from buds project: no a_0 etc, and now works perfectly
// Next steps: add lab group and interactions back in.

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
  vector[n_sp] a_sp;
  vector[n_sp] b_force;
  vector[n_sp] b_photo;
  vector[n_sp] b_lat;
  vector[n_sp] b_chill;
  
  real mu_a;
  real mu_b_force; 
  real mu_b_photo;
  real mu_b_lat;
  real mu_b_chill;

  real<lower=0> sigma_a; 
  real<lower=0> sigma_b_force; 
  real<lower=0> sigma_b_photo;
  real<lower=0> sigma_b_lat;
  real<lower=0> sigma_b_chill;

  real<lower=0> sigma_y; 
  }

transformed parameters {
	real y_hat[N];

	for(i in 1:N){

		y_hat[i] = a_sp[sp[i]] + // indexed with species
					b_force[sp[i]] * force[i] + // indexed with species
					b_photo[sp[i]] * photo[i]+
					b_lat[sp[i]] * lat[i] +
					b_chill[sp[i]] * chill[i] 
					;
					}
	}

model {
	mu_a ~ normal(0, 20); // 1 sd is 20 d on either side at sp level
	mu_b_force ~ normal(0, 20); 
	mu_b_photo ~ normal(0, 20);
	mu_b_lat ~ normal(0, 20);
	mu_b_chill ~ normal(0, 20);
		
	sigma_a ~ normal(0, 10); // actually half-normal, bounded at 0
	sigma_b_force ~ normal(0, 10); 
	sigma_b_photo ~ normal(0, 10); 
	sigma_b_lat ~ normal(0, 10); 
	sigma_b_chill ~ normal(0, 10); 
	
  // no pooling of a here 	
	b_force ~ normal(mu_b_force, sigma_b_force);
	b_photo ~ normal(mu_b_photo, sigma_b_photo);
	b_lat ~ normal(mu_b_lat, sigma_b_lat);
	b_chill ~ normal(mu_b_chill, sigma_b_chill);
	
	y ~ normal(y_hat, sigma_y);
}
