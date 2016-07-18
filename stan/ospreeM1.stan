// OSPREE analysis
// flynn@fas.harvard.edu
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod, and latitude in a meta-analysis of 100+ studies
// Levels: Species and Study (labgroup)

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1> n_lab;
	int<lower=1, upper=n_sp> sp[N];
	int<lower=1, upper=n_lab> lab[N]; 
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	vector[N] lat;		// predictor
		
	}

transformed data { 			// 6 two-way interaction terms 
	vector[N] inter_fp; 	// forcing x photo           
	vector[N] inter_fl;           
	vector[N] inter_pl;           
	vector[N] inter_fc;           
	vector[N] inter_pc;           
	vector[N] inter_lc;           

	inter_fp	<- force .* photo;  
	inter_fl	<- force .* lat;  
	inter_pl	<- photo .* lat;  
	inter_fc   <- force .* chill;  
	inter_pc   <- photo .* chill;  
	inter_lc   <- lat .* chill;  

}

parameters {
  real a_0; // overall intercept, same as "beta_0" in classroom example model
  real b_force_0; // overall forcing effect
  real b_photo_0; // overall photoperiod effect  
  real b_lat_0; // overall lat effect  
  real b_chill_0; // overall chill effect  
  real b_inter_fp_0; // overall forcing x photoperiod effect  
  real b_inter_fl_0; // overall forcing x lat effect  
  real b_inter_pl_0; // overall photoperiod x lat effect  
  real b_inter_fc_0; // overall forcing x chill effect  
  real b_inter_pc_0; // overall photoperiod x chill effect  
  real b_inter_lc_0; // overall lat x chill effect  

  real mu_a_sp[n_sp];
  real mu_a_lab[n_lab]; // labgroup 
  real mu_b_force_sp[n_sp]; 
  real mu_b_photo_sp[n_sp];
  real mu_b_lat_sp[n_sp];
  real mu_b_chill_sp[n_sp];
  real mu_b_inter_fp_sp[n_sp];
  real mu_b_inter_fl_sp[n_sp];
  real mu_b_inter_pl_sp[n_sp];
  real mu_b_inter_fc_sp[n_sp];
  real mu_b_inter_pc_sp[n_sp];
  real mu_b_inter_lc_sp[n_sp];
      
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_a_lab; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp;
  real<lower=0> sigma_b_lat_sp;
  real<lower=0> sigma_b_chill_sp;
  real<lower=0> sigma_b_inter_fp_sp;
  real<lower=0> sigma_b_inter_fl_sp;
  real<lower=0> sigma_b_inter_pl_sp;
  real<lower=0> sigma_b_inter_fc_sp;
  real<lower=0> sigma_b_inter_pc_sp;
  real<lower=0> sigma_b_inter_lc_sp;

  real<lower=0> sigma_y; 
  }

transformed parameters {
	real y_hat[N];
	
	real a_sp[n_sp]; // intercept for species
	real a_lab[n_lab]; // intercept for labgroup
  	real b_force_sp[n_sp]; // slope of forcing effect at species level
	real b_photo_sp[n_sp]; // slope of photoperiod effect, at species level
	real b_lat_sp[n_sp]; // slope of photoperiod effect, at species level
	real b_chill_sp[n_sp]; // slope of chill effect, at species level
	real b_inter_fp_sp[n_sp]; // slope of forcing x photoperiod effect, at species level
	real b_inter_fl_sp[n_sp]; // slope of forcing x latitude effect, at species level
	real b_inter_pl_sp[n_sp]; // slope of photoperiod x latitude effect, at species level
	real b_inter_fc_sp[n_sp]; // slope of forcing x chill effect, at species level
	real b_inter_pc_sp[n_sp]; // slope of photoperiod x chill effect, at species level
	real b_inter_lc_sp[n_sp]; // slope of latitude x chill effect, at species level
		
	// Species level. Random intercept (a) and slopes (b) for forcing, photoperiod, chilling, lat, 2-way interax
	for (k in 1:n_sp) {
		
		a_sp[k] 		<- a_0 + mu_a_sp[k];
		b_force_sp[k] 	<- b_force_0 + mu_b_force_sp[k];
		b_photo_sp[k] 	<- b_photo_0 + mu_b_photo_sp[k];
		b_lat_sp[k] 	<- b_lat_0 + mu_b_lat_sp[k];
		b_chill_sp[k] 	<- b_chill_0 + mu_b_chill_sp[k];
		b_inter_fp_sp[k] <- b_inter_fp_0 + mu_b_inter_fp_sp[k];
		b_inter_fl_sp[k] <- b_inter_fl_0 + mu_b_inter_fl_sp[k];
		b_inter_pl_sp[k] <- b_inter_pl_0 + mu_b_inter_pl_sp[k];										
		b_inter_fc_sp[k] <- b_inter_fc_0 + mu_b_inter_fc_sp[k];
		b_inter_pc_sp[k] <- b_inter_pc_0 + mu_b_inter_pc_sp[k];										
		b_inter_lc_sp[k] <- b_inter_lc_0 + mu_b_inter_lc_sp[k];

		}
	
	// lab group level
	for (j in 1:n_lab) {
		a_lab[j] <- a_0 + mu_a_lab[j];
		}
	
	// row level 
	for(i in 1:N){

		y_hat[i] <- a_sp[sp[i]] + // indexed with species
					a_lab[lab[i]] + // indexed with labgroup
					b_force_sp[sp[i]] * force[i] + // indexed with species
					b_photo_sp[sp[i]] * photo[i]+
					b_lat_sp[sp[i]] * lat[i] +
					b_chill_sp[sp[i]] * chill[i] +
					b_inter_fp_sp[sp[i]] * inter_fp[i]+
					b_inter_fl_sp[sp[i]] * inter_fl[i]+
					b_inter_pl_sp[sp[i]] * inter_pl[i]+
					b_inter_fc_sp[sp[i]] * inter_fc[i]+
					b_inter_pc_sp[sp[i]] * inter_pc[i]+
					b_inter_lc_sp[sp[i]] * inter_lc[i]
					;
					}
	}

model {
	a_0 ~ normal(0, 100); // ~~ 26 for budburst, 37 for leafout
	b_force_0 ~ normal(0, 100);
	b_photo_0 ~ normal(0, 100);
	b_lat_0 ~ normal(0, 100);
	b_chill_0 ~ normal(0, 100);
	b_inter_fp_0 ~ normal(0, 100);
  	b_inter_fl_0 ~ normal(0, 100);
  	b_inter_pl_0 ~ normal(0, 100);
 	b_inter_fc_0 ~ normal(0, 100);
  	b_inter_pc_0 ~ normal(0, 100);
  	b_inter_lc_0 ~ normal(0, 100);
  	
	mu_a_sp ~ normal(0, sigma_a_sp); // 20 d on either lat at sp level
	mu_a_lab ~ normal(0, sigma_a_lab); 
	mu_b_force_sp ~ normal(0, sigma_b_force_sp); 
	mu_b_photo_sp ~ normal(0, sigma_b_photo_sp);
	mu_b_lat_sp ~ normal(0, sigma_b_lat_sp);
	mu_b_chill_sp ~ normal(0, sigma_b_chill_sp);
	mu_b_inter_fp_sp ~ normal(0, sigma_b_inter_fp_sp);
	mu_b_inter_fl_sp ~ normal(0, sigma_b_inter_fl_sp);
	mu_b_inter_pl_sp ~ normal(0, sigma_b_inter_pl_sp);
	mu_b_inter_fc_sp ~ normal(0, sigma_b_inter_fc_sp);
	mu_b_inter_pc_sp ~ normal(0, sigma_b_inter_pc_sp);
	mu_b_inter_lc_sp ~ normal(0, sigma_b_inter_lc_sp);
		
	sigma_a_sp ~ normal(0, 20); 
	sigma_a_lab ~ normal(0, 20); 
	sigma_b_force_sp ~ normal(0, 20); 
	sigma_b_photo_sp ~ normal(0, 20); 
	sigma_b_lat_sp ~ normal(0, 20); 
	sigma_b_chill_sp ~ normal(0, 20); 
	sigma_b_inter_fp_sp ~ normal(0, 20); 
	sigma_b_inter_fl_sp ~ normal(0, 20); 
	sigma_b_inter_pl_sp ~ normal(0, 20); 
	sigma_b_inter_fc_sp ~ normal(0, 20);
	sigma_b_inter_pc_sp ~ normal(0, 20);
	sigma_b_inter_lc_sp ~ normal(0, 20);
	
	y ~ normal(y_hat, sigma_y);
}