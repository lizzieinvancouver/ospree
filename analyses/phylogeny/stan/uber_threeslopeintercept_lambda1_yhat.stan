
functions {
  matrix lambda_vcv(matrix vcv, real lambda, real sigma){
    matrix[rows(vcv),cols(vcv)] local_vcv;
    matrix[rows(vcv),cols(vcv)] sigma_mat;  
    local_vcv = vcv * lambda;
    for(i in 1:rows(local_vcv))
      local_vcv[i,i] = vcv[i,i];
    sigma_mat = diag_matrix(rep_vector(sigma, rows(vcv)));
    return(sigma_mat * local_vcv * sigma_mat);
  }
}

data {
  int<lower=1> N;
  int<lower=1> n_sp;
  int<lower=1, upper=n_sp> sp[N];
  vector[N] y; 		// response
  vector[N] x1; 	// predictor (forcing)
  vector[N] x2; 	// predictor (chilling)
  vector[N] x3; 	// predictor (photoperiod)
  matrix[n_sp,n_sp]Vphy;     // phylogeny
  // Priors
  real a_z_prior_mu;
  real a_z_prior_sigma;
  real sigma_interceptsa_prior_mu;
  real sigma_interceptsa_prior_sigma;
  real b_zf_prior_mu;
  real b_zf_prior_sigma;
  real sigma_interceptsbf_prior_mu;
  real sigma_interceptsbf_prior_sigma;
  real b_zc_prior_mu;
  real b_zc_prior_sigma;
  real sigma_interceptsbc_prior_mu; 
  real sigma_interceptsbc_prior_sigma; 
  real b_zp_prior_mu;
  real b_zp_prior_sigma;
  real sigma_interceptsbp_prior_mu; 
  real sigma_interceptsbp_prior_sigma; 
  real sigma_y_mu_prior;
  real sigma_y_mu_sigma;  
	
}

transformed data{
  real lam_interceptsa = 1;       
  real lam_interceptsbf = 1;       
  real lam_interceptsbc = 1;
  real lam_interceptsbp = 1;
}

parameters {
  real<lower=0> sigma_y;    
  real<lower=0> sigma_interceptsa;
  real<lower=0> sigma_interceptsbf;   
  real<lower=0> sigma_interceptsbc; 
  real<lower=0> sigma_interceptsbp; 
  vector[n_sp] b_force; // slope of forcing effect
  real b_zf;
  vector[n_sp] b_chill; // slope of chilling effect
  real b_zc;
  vector[n_sp] b_photo; // slope of photo effect
  real b_zp;
  vector[n_sp] a; // intercept
  real a_z;

}

model {
       real yhat[N];
       	for(i in 1:N){
            yhat[i] = 
	      a[sp[i]] + b_force[sp[i]] * x1[i] + b_chill[sp[i]] * x2[i] + b_photo[sp[i]] * x3[i];
			     	}
  a ~ multi_normal(rep_vector(a_z,n_sp), lambda_vcv(Vphy, lam_interceptsa, sigma_interceptsa)); 
  b_force ~ multi_normal(rep_vector(b_zf,n_sp), lambda_vcv(Vphy, lam_interceptsbf, sigma_interceptsbf)); 
  b_chill ~ multi_normal(rep_vector(b_zc,n_sp), lambda_vcv(Vphy, lam_interceptsbc, sigma_interceptsbc));
  b_photo ~ multi_normal(rep_vector(b_zp,n_sp), lambda_vcv(Vphy, lam_interceptsbp, sigma_interceptsbp));
  
  y ~ normal(yhat, sigma_y);

 // Priors
  a_z ~ normal(a_z_prior_mu, a_z_prior_sigma);
  sigma_interceptsa ~ normal(sigma_interceptsa_prior_mu, sigma_interceptsa_prior_sigma);
  b_zf ~ normal(b_zf_prior_mu, b_zf_prior_sigma);
  sigma_interceptsbf ~ normal(sigma_interceptsbf_prior_mu, sigma_interceptsbf_prior_sigma);
  b_zc ~ normal(b_zc_prior_mu, b_zc_prior_sigma);
  sigma_interceptsbc ~ normal(sigma_interceptsbc_prior_mu, sigma_interceptsbc_prior_sigma);
  b_zp ~ normal(b_zp_prior_mu, b_zp_prior_sigma);
  sigma_interceptsbp ~ normal(sigma_interceptsbp_prior_mu, sigma_interceptsbp_prior_sigma);
  sigma_y ~ normal(sigma_y_mu_prior, sigma_y_mu_sigma);

  
}

generated quantities {
  real yhat[N];
       	for(i in 1:N){
            yhat[i] = 
	      a[sp[i]] + b_force[sp[i]] * x1[i] + b_chill[sp[i]] * x2[i] + b_photo[sp[i]] * x3[i];
			     	}
}
