// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in July 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) on INTERCEPTS and SLOPES
// This model has interactions! The two-way type but it does not have species grouping on them!

data {
	int<lower=0> N;
	int<lower=0> n_sp;
	int<lower=0, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
	vector[N] lat; // predictor
		
	}

transformed data {
  vector[N] inter_cf;           
  vector[N] inter_cp;           
  vector[N] inter_fp;                  
  vector[N] inter_cl;
  vector[N] inter_pl;
  vector[N] inter_fl;
  inter_cf    = chill .* force; 
  inter_cp    = chill .* photo; 
  inter_fp    = force .* photo; 
  inter_cl    = chill .* lat; 
  inter_pl    = photo .* lat; 
  inter_fl    = force .* lat; 
}

parameters {
  real mu_a_sp;   
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;
  real mu_b_lat_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp;
  real<lower=0> sigma_b_lat_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_force[n_sp]; // slope of forcing effect 
  real b_photo[n_sp]; // slope of photoperiod effect
  real b_chill[n_sp]; 
  real b_lat[n_sp]; // 
  real b_cf; // slope of chill x force effect
  real b_cp; // slope of chill x photo effect
  real b_fp; // slope of force x photo effect
  real b_cl;
  real b_pl;
  real b_fl;
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i] +
		b_lat[sp[i]] * lat[i] +
                b_cf *  inter_cf[i] +
                b_cp * inter_cp[i] +
                b_fp * inter_fp[i]+
                b_cl * inter_cl[i] +
                b_pl * inter_pl[i] +
                b_fl * inter_fl[i];
       	}
	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
	b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp); 
	b_lat ~ normal(mu_b_lat_sp, sigma_b_lat_sp); 
       
        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
	b_cf ~ normal(0, 10);
	b_cp ~ normal(0, 10);
	b_fp ~ normal(0, 30);
	b_cl ~ normal(0, 30);
	b_pl ~ normal(0, 30);
	b_fl ~ normal(0, 30);

        mu_b_force_sp ~ normal(0, 50);
        sigma_b_force_sp ~ normal(0, 10);
        mu_b_photo_sp ~ normal(0, 50);
        sigma_b_photo_sp ~ normal(0, 10);
        mu_b_chill_sp ~ normal(0, 50);
        sigma_b_chill_sp ~ normal(0, 10);
        mu_b_lat_sp ~ normal(0, 50);
        sigma_b_lat_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}

