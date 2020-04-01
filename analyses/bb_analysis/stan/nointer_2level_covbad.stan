// OSPREE analysis
// Started by flynn@fas.harvard.edu, code updated by Lizzie in July 2017 to try running.
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) on INTERCEPTS and SLOPES

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
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;   
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_force[n_sp]; // slope of forcing effect 
  real b_photo[n_sp]; // slope of photoperiod effect
  real b_chill[n_sp]; // slope of chill effect
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp;   corr_matrix[4] Omega;// cov matrix for slopes/intercepts
	}

transformed parameters {
  vector[4] vec_ab[n_sp];
  vector[4] Mu_ab;
  vector[4] Sigma_ab;
  cov_matrix[4] SRS_sigma_speciesOmega;
  real yhat[N];
  for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + 
	      	b_photo[sp[i]] * photo[i] +
		b_chill[sp[i]] * chill[i];
			     	}
  for (j in 1:n_sp) {
  vec_ab[j, 1] = a_sp[j];
  vec_ab[j, 2] = b_force[j];
  vec_ab[j, 3] = b_photo[j];
  vec_ab[j, 4] = b_chill[j];
    for (l in 1:4){
    Mu_ab[1] = mu_a_sp;
    Mu_ab[2] = mu_b_force_sp;
    Mu_ab[3] = mu_b_photo_sp;
    Mu_ab[4] = mu_b_chill_sp;
    }         }
    for (k in 1:4){
    Sigma_ab[1] = sigma_a_sp;
    Sigma_ab[2] = sigma_b_force_sp;
    Sigma_ab[3] = sigma_b_photo_sp;
    Sigma_ab[4] = sigma_b_chill_sp;
    }
    SRS_sigma_speciesOmega = quad_form_diag(Omega,Sigma_ab);

	}

model {
	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
	b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
	b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp); 
	Omega ~ lkj_corr(4);  // prior for correlation matrix
	vec_ab ~ multi_normal(Mu_ab, SRS_sigma_speciesOmega);
   mu_b_force_sp ~ normal(0, 50);
   sigma_b_force_sp ~ normal(0, 10);
   mu_b_photo_sp ~ normal(0, 50);
   sigma_b_photo_sp ~ normal(0, 10);
   mu_b_chill_sp ~ normal(0, 50);
   sigma_b_chill_sp ~ normal(0, 10);
   mu_a_sp ~ normal(0, 50);
   sigma_a_sp ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);
	
	y ~ normal(yhat, sigma_y);

}

generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_force[sp[n]] * force[n] + 
	      	b_photo[sp[n]] * photo[n] +
		b_chill[sp[n]] * chill[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}
