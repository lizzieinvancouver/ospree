/* Started 23 September 2022
By Lizzie, using betan_phylogeny_synchrony_cp.stan and older PMM code together
Trying to implement the speed-ups from Mike B. into our PMM model
I udpated some names (lam_intercepts -> lamba) and...
there's no sigma, so it's tau now (e.g., sigma_interceptsa -> tau_a) */

functions {
   // Phylogeny matrix with off-diagonal scaling -- Mike made this unscaled, so no sigma here!
   matrix unscaled_lambda_vcv(matrix vcv, real lambda){
    matrix[rows(vcv), cols(vcv)] local_vcv = vcv * lambda;
    for (i in 1:rows(local_vcv)) local_vcv[i,i] = vcv[i,i];
    return(local_vcv);
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

parameters {
  real<lower=0> sigma_y; 
  real<lower=0, upper=1> lambda_a;       
  real<lower=0> tau_a; // Updating based on Mike's code 
  real<lower=0, upper=1> lambda_bf;       
  real<lower=0> tau_bf;   
  real<lower=0, upper=1> lambda_bc;       
  real<lower=0> tau_bc; 
  real<lower=0, upper=1> lambda_bp;       
  real<lower=0> tau_bp; 
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
    vector[N] yhat = a[sp];	// Adding intercept here, following Mike's code
    matrix[n_sp,n_sp] vcv_a;     // phylogeny
    matrix[n_sp,n_sp] vcv_bf;     // phylogeny
    matrix[n_sp,n_sp] vcv_bc;     // phylogeny
    matrix[n_sp,n_sp] vcv_bp;     // phylogeny
       
    for(n in 1:N){
        yhat[n] += 
	    b_force[sp[n]] * x1[n] + b_chill[sp[n]] * x2[n] + b_photo[sp[n]] * x3[n];
 	    }

    vcv_a = tau_a * cholesky_decompose(unscaled_lambda_vcv(Vphy, lambda_a));
    vcv_bf = tau_bf * cholesky_decompose(unscaled_lambda_vcv(Vphy, lambda_bf));
    vcv_bc = tau_bc * cholesky_decompose(unscaled_lambda_vcv(Vphy, lambda_bc));
    vcv_bp = tau_bp * cholesky_decompose(unscaled_lambda_vcv(Vphy, lambda_bp));

    // The below is basically unchanged when using Mike's code!
    a ~ multi_normal_cholesky(rep_vector(a_z,n_sp), vcv_a); 
    b_force ~ multi_normal_cholesky(rep_vector(b_zf,n_sp), vcv_bf); 
    b_chill ~ multi_normal_cholesky(rep_vector(b_zc,n_sp), vcv_bc);
    b_photo ~ multi_normal_cholesky(rep_vector(b_zp,n_sp),vcv_bp);
  
    y ~ normal(yhat, sigma_y);

    // Priors
    a_z ~ normal(30, 10); // Same as before, seems okay
    b_zf ~ normal(-2, 10); // updated prior ... I think we should also try 0, 10
    b_zc ~ normal(-2, 10); // updated prior
    b_zp ~ normal(0, 5); // updated prior

    // All below: same as before, seems okay
    lambda_a ~ beta(1, 1);
    lambda_bf ~ beta(1, 1);
    lambda_bc ~ beta(1, 1);
    lambda_bp ~ beta(1, 1);

    // For now, I picked the below against Mike's code (and I checked them visually)
    tau_a ~ normal(30, 20);
    tau_bf ~ normal(-1, 5);
    tau_bc ~ normal(-1, 5);
    tau_bp ~ normal(-1, 5);
    
    sigma_y ~ normal(10, 10); // updated prior

  
}


