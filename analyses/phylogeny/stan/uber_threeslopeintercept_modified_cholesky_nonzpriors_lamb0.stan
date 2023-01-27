// Started 25 January 2023 by Lizzie 
// based on uber_threeslopeintercept_modified_cholesky_updatedpriors.stan
// Tweaked the priors for non-z-scored predictors (beta slopes and a)

functions {
  matrix lambda_vcv(matrix vcv, real lambda, real sigma){
    matrix[rows(vcv),cols(vcv)] local_vcv;
   // matrix[rows(vcv),cols(vcv)] sigma_mat;  
    local_vcv = vcv * lambda;
    for(i in 1:rows(local_vcv))
      local_vcv[i,i] = vcv[i,i];
      return(quad_form_diag(local_vcv, rep_vector(sigma, rows(vcv))));
    //sigma_mat = diag_matrix(rep_vector(sigma, rows(vcv)));
    //return(sigma_mat * local_vcv * sigma_mat);
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
}

transformed data{
  real lam_interceptsa = 0;       
  real lam_interceptsbf = 0;       
  real lam_interceptsbc = 0;
  real lam_interceptsbp = 0;
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
       
        matrix[n_sp,n_sp] vcv_a;     // phylogeny
        matrix[n_sp,n_sp] vcv_bf;     // phylogeny
        matrix[n_sp,n_sp] vcv_bc;     // phylogeny
        matrix[n_sp,n_sp] vcv_bp;     // phylogeny

       
       	for(i in 1:N){
            yhat[i] = 
	      a[sp[i]] + b_force[sp[i]] * x1[i] + b_chill[sp[i]] * x2[i] + b_photo[sp[i]] * x3[i];
			     	}
			     	
	vcv_a = cholesky_decompose(lambda_vcv(Vphy, lam_interceptsa, sigma_interceptsa));
  vcv_bf = cholesky_decompose(lambda_vcv(Vphy, lam_interceptsbf, sigma_interceptsbf));
  vcv_bc = cholesky_decompose(lambda_vcv(Vphy, lam_interceptsbc, sigma_interceptsbc));
  vcv_bp = cholesky_decompose(lambda_vcv(Vphy, lam_interceptsbp, sigma_interceptsbp));


  a ~ multi_normal_cholesky(rep_vector(a_z,n_sp), vcv_a); 
  b_force ~ multi_normal_cholesky(rep_vector(b_zf,n_sp), vcv_bf); 
  b_chill ~ multi_normal_cholesky(rep_vector(b_zc,n_sp), vcv_bc);
  b_photo ~ multi_normal_cholesky(rep_vector(b_zp,n_sp),vcv_bp);
  
  y ~ normal(yhat, sigma_y);

 // Priors -- keep in Stan code, better for reproducibility and runs faster
    a_z ~ normal(60, 10); // updated prior
    b_zf ~ normal(0, 10); // updated prior
    b_zc ~ normal(0, 10); // updated prior
    b_zp ~ normal(0, 10); // 

    
    // I don't have a good sense of how to set these, so keeping a little wide
    sigma_interceptsa ~ normal(30, 20);
    sigma_interceptsbf ~ normal(1, 5);
    sigma_interceptsbc ~ normal(1, 5);
    sigma_interceptsbp ~ normal(1, 5);
    
    sigma_y ~ normal(10, 10); // 
  
}
