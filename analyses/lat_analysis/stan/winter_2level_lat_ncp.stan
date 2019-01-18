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
	vector[N] lat; // predictor
		
	}
	
transformed data {
  vector[N] inter_pl;                  

  inter_pl    = photo .* lat; 
}

parameters {
  real mu_a_sp;   
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;
  real mu_b_lat_sp;
  real mu_b_pl_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp;
  real<lower=0> sigma_b_lat_sp;
  real<lower=0> sigma_b_pl_sp;
  real<lower=0> sigma_y; 

  vector[n_sp] a_sp; // intercept for species
  vector[n_sp] b_force; // slope of forcing effect 
  vector[n_sp] b_photo; // slope of photoperiod effect
  vector[n_sp] b_chill; // slope of chill effect
  vector[n_sp] b_lat; // slope of lat effect
  vector[n_sp] b_pl_ncp; // slope of photo x lat effect
  //vector[n_sp] b_pl_ncp; // slope of lat effect

	}

transformed parameters {
  vector[n_sp] b_pl; // slope of interaction 
  vector[N] yhat;
  
  //b_photo = mu_b_photo_sp + sigma_b_photo_sp*b_photo_ncp;
  //b_lat = mu_b_lat_sp + sigma_b_lat_sp*b_lat_ncp;
  b_pl = mu_b_pl_sp + sigma_b_pl_sp*b_pl_ncp;
  
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i] +
		      b_lat[sp[i]] * lat[i] +
                b_pl[sp[i]] *  inter_pl[i];
	}
}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
	b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp);
	b_lat ~ normal(mu_b_lat_sp, sigma_b_lat_sp);
	//b_pl ~ normal(mu_b_pl_sp, sigma_b_pl_sp); 

        mu_a_sp ~ normal(0, 50);
        sigma_a_sp ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 10);
	//b_lat ~ normal(0, 10);
	b_pl_ncp ~ normal(0, 10);

        mu_b_force_sp ~ normal(0, 50);
        sigma_b_force_sp ~ normal(0, 10);
        mu_b_photo_sp ~ normal(0, 50);
        sigma_b_photo_sp ~ normal(0, 10);
        mu_b_chill_sp ~ normal(0, 50);
        sigma_b_chill_sp ~ normal(0, 10);
        mu_b_lat_sp ~ normal(0, 50);
        sigma_b_lat_sp ~ normal(0, 10);

        mu_b_pl_sp ~ normal(0, 50);
        sigma_b_pl_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);

}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_force[sp[n]] * force[n] + 
	      	b_photo[sp[n]] * photo[n] +
		b_chill[sp[n]] * chill[n] +
		      b_lat[sp[n]] * lat[n] +
		b_pl[sp[n]] * inter_pl[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}

