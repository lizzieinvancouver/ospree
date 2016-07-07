// OSPREE analysis
// flynn@fas.harvard.edu, aettinger@fas.harvard.edu
// starting with dan's days to bb model and modifying it for percent budburst
// 2 level model for percent budburst as a function of days to budburst, days x forcing temperature, daysXchilling units, daysXphotoperiod, 43 studies/datasets use, 4874 rows of data, 40 species
// Levels: Species, Study

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1> n_study;
	int<lower=1, upper=n_sp> sp[N];
	int<lower=1, upper=n_study> study[N]; 
	vector[N] y; 		// response=percbb
	vector[N] days; 	// predictor
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor		
	}

transformed data { 			// 3 two-way interaction terms 
	vector[N] inter_dc; 	// days x chill          
	vector[N] inter_df;      // days x forcing     
	vector[N] inter_dp;      // days x photo  
	       

	inter_dc	<- days .* photo;  
	inter_df	<- days .* force;  
	inter_dp	<- days .* photo;  

}

parameters {
  real a_0; // overall intercept, same as "beta_0" in classroom example model
  real b_days_0; // overall days effect
  real b_inter_dc_0; // overall days x chilling effect  
  real b_inter_df_0; // overall days x forcing effect  
  real b_inter_dp_0; // overall days x photoperiod effect  
 
  real mu_a_sp[n_sp];
  real mu_a_study[n_study]; // study
  real mu_b_days_sp[n_sp]; 
  real mu_b_inter_dc_sp[n_sp];
  real mu_b_inter_df_sp[n_sp];
  real mu_b_inter_dp_sp[n_sp];
    
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_a_study; 
  real<lower=0> sigma_b_days_sp; 
  real<lower=0> sigma_b_inter_dc_sp;
  real<lower=0> sigma_b_inter_df_sp;
  real<lower=0> sigma_b_inter_dp_sp;
  real<lower=0> sigma_y; 
  }

transformed parameters {
	real y_hat[N];
	
	real a_sp[n_sp]; // intercept for species
	real a_study[n_study]; // intercept for study
  	real b_days_sp[n_sp]; // slope of days effect at species level
	real b_inter_dc_sp[n_sp]; // slope of days x chilling effect, at species level
	real b_inter_df_sp[n_sp]; // slope of days x forcing effect, at species level
	real b_inter_dp_sp[n_sp]; // slope of days x photoperiod effect, at species level
		
	// Species level. Random intercept (a) and slopes (b) for days, 2-way interax between days and photoperiod, chilling, and forcing
	for (k in 1:n_sp) {
		
		a_sp[k] 		<- a_0 + mu_a_sp[k];
		b_days_sp[k] 	<- b_days_0 + mu_b_days_sp[k];
		b_inter_dc_sp[k] <- b_inter_dc_0 + mu_b_inter_dc_sp[k];
		b_inter_df_sp[k] <- b_inter_df_0 + mu_b_inter_df_sp[k];
		b_inter_dp_sp[k] <- b_inter_dp_0 + mu_b_inter_dp_sp[k];										

		}
	
	// study level
	for (j in 1:n_study) {
		a_study[j] <- a_0 + mu_a_study[j];
		}
	
	// row level 
	for(i in 1:N){

		y_hat[i] <- a_sp[sp[i]] + // indexed with species
					a_study[study[i]] + // indexed with labgroup
					b_days_sp[sp[i]] * days[i] + // indexed with species
					b_inter_dc_sp[sp[i]] * inter_dc[i]+
					b_inter_df_sp[sp[i]] * inter_df[i]+
					b_inter_dp_sp[sp[i]] * inter_dp[i]
					;
					}
	}

model {
	a_0 ~ normal(0, 100); // 
	b_days_0 ~ normal(0, 100);
	b_inter_dc_0 ~ normal(0, 100);
  	b_inter_df_0 ~ normal(0, 100);
  	b_inter_dp_0 ~ normal(0, 100);
  	
	mu_a_sp ~ normal(0, sigma_a_sp); // 
	mu_a_study ~ normal(0, sigma_a_study); 
	mu_b_days_sp ~ normal(0, sigma_b_days_sp); 
	mu_b_inter_dc_sp ~ normal(0, sigma_b_inter_dc_sp);
	mu_b_inter_df_sp ~ normal(0, sigma_b_inter_df_sp);
	mu_b_inter_dp_sp ~ normal(0, sigma_b_inter_dp_sp);
		
	sigma_a_sp ~ normal(0, 20); 
	sigma_a_study ~ normal(0, 20); 
	sigma_b_days_sp ~ normal(0, 20); 
	sigma_b_inter_dc_sp ~ normal(0, 20); 
	sigma_b_inter_df_sp ~ normal(0, 20); 
	sigma_b_inter_dp_sp ~ normal(0, 20); 
	
	y ~ normal(y_hat, sigma_y);
}