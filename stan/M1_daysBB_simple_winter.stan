// OSPREE analysis
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod, and latitude in a meta-analysis of 100+ studies
// Levels: Species. 
// Next steps: add lab group back in.

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

transformed data { 		// 6 two-way interaction terms 
	vector[N] inter_fp; 	// forcing x photo           
	vector[N] inter_fl;           
	vector[N] inter_pl;           
	vector[N] inter_fc;           
	vector[N] inter_pc;           
	vector[N] inter_lc;           

	inter_fp	= force .* photo;  
	inter_fl	= force .* lat;  
	inter_pl	= photo .* lat;  
	inter_fc   = force .* chill;  
	inter_pc   = photo .* chill;  
	inter_lc   = lat .* chill;  

}

parameters {
  vector[n_sp] a_sp;
  vector[n_sp] b_force;
  vector[n_sp] b_photo;
  vector[n_sp] b_lat;
  vector[n_sp] b_chill;
  vector[n_sp] b_inter_fp;
  vector[n_sp] b_inter_fl;
  vector[n_sp] b_inter_pl;
  vector[n_sp] b_inter_fc;
  vector[n_sp] b_inter_pc;
  vector[n_sp] b_inter_lc;

  real mu_a;
  real mu_b_force; 
  real mu_b_photo;
  real mu_b_lat;
  real mu_b_chill;
  real mu_b_inter_fp;
  real mu_b_inter_fl;
  real mu_b_inter_pl;
  real mu_b_inter_fc;
  real mu_b_inter_pc;
  real mu_b_inter_lc;

  real<lower=0> sigma_a; 
  real<lower=0> sigma_b_force; 
  real<lower=0> sigma_b_photo;
  real<lower=0> sigma_b_lat;
  real<lower=0> sigma_b_chill;
  real<lower=0> sigma_b_inter_fp; 
  real<lower=0> sigma_b_inter_fl; 
  real<lower=0> sigma_b_inter_pl; 
  real<lower=0> sigma_b_inter_fc; 
  real<lower=0> sigma_b_inter_pc; 
  real<lower=0> sigma_b_inter_lc; 

  real<lower=0> sigma_y; 
  }

transformed parameters {
	real y_hat[N];

	for(i in 1:N){

		y_hat[i] = a_sp[sp[i]] + // indexed with species
					b_force[sp[i]] * force[i] + // indexed with species
					b_photo[sp[i]] * photo[i]+
					b_lat[sp[i]] * lat[i] +
					b_chill[sp[i]] * chill[i] +
					b_inter_fp[sp[i]] * inter_fp[i]+
					b_inter_fl[sp[i]] * inter_fl[i]+
					b_inter_pl[sp[i]] * inter_pl[i]+
					b_inter_fc[sp[i]] * inter_fc[i]+
					b_inter_pc[sp[i]] * inter_pc[i]+
					b_inter_lc[sp[i]] * inter_lc[i]
					;
					}
	}

model {
	mu_a ~ normal(0, 20); // 1 sd is 20 d on either side at sp level
	mu_b_force ~ normal(0, 20); 
	mu_b_photo ~ normal(0, 20);
	mu_b_lat ~ normal(0, 20);
	mu_b_chill ~ normal(0, 20);

	mu_b_inter_fp ~ normal(0, 20);
	mu_b_inter_fl ~ normal(0, 20);
	mu_b_inter_pl ~ normal(0, 20);
	mu_b_inter_fc ~ normal(0, 20);
	mu_b_inter_pc ~ normal(0, 20);
	mu_b_inter_lc ~ normal(0, 20);
		
	sigma_a ~ normal(0, 10); // actually half-normal, bounded at 0
	sigma_b_force ~ normal(0, 10); 
	sigma_b_photo ~ normal(0, 10); 
	sigma_b_lat ~ normal(0, 10); 
	sigma_b_chill ~ normal(0, 10); 

	sigma_b_inter_fp ~ normal(0, 10); 
	sigma_b_inter_fl ~ normal(0, 10); 
	sigma_b_inter_pl ~ normal(0, 10); 
	sigma_b_inter_fc ~ normal(0, 10); 
	sigma_b_inter_pc ~ normal(0, 10); 
	sigma_b_inter_lc ~ normal(0, 10); 

	
  // no pooling of a here 	
	b_force ~ normal(mu_b_force, sigma_b_force);
	b_photo ~ normal(mu_b_photo, sigma_b_photo);
	b_lat ~ normal(mu_b_lat, sigma_b_lat);
	b_chill ~ normal(mu_b_chill, sigma_b_chill);
	b_inter_fp ~ normal(mu_b_inter_fp, sigma_b_inter_fp);
	b_inter_fl ~ normal(mu_b_inter_fl, sigma_b_inter_fl);
	b_inter_pl ~ normal(mu_b_inter_pl, sigma_b_inter_pl);
	b_inter_pc ~ normal(mu_b_inter_pc, sigma_b_inter_pc);
	b_inter_fc ~ normal(mu_b_inter_fc, sigma_b_inter_fc);
	b_inter_fc ~ normal(mu_b_inter_fc, sigma_b_inter_fc);
	b_inter_lc ~ normal(mu_b_inter_lc, sigma_b_inter_lc);
	
	y ~ normal(y_hat, sigma_y);
}
