// OSPREE analysis -- updated with Will's new code (ubermini_2) in Sep 2020
// Also, skip the partial pooling on intercepts for now, maybe we should add back in
// Simplified version of nointe_2level.stan 
// with phylogeny on the one slope (force)



functions {
  matrix lambda_vcv(matrix vcv, real lambda, real sigma){
    matrix[rows(vcv),cols(vcv)] local_vcv;
    local_vcv = vcv * lambda;
    for(i in 1:rows(local_vcv))
      local_vcv[i,i] = vcv[i,i];
    return(local_vcv * sigma);
  }
}

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] force; 	// predictor
        matrix[n_sp,n_sp]Vphy;     // phylogeny
		
	}

parameters {
  
  //Level 1 
  real<lower=0> sigma_y;                            // variation of predicted values around mean predictions 
  real b_f;                                         //grand slope effect of forcing (grand beta forcing)
  real<lower=0> a_f ;                                         // grand intercept 
  
  //level 2 - alpha 
  real <lower=0>sigma_af;                                         // Standard deviation of how much species intercepts generaly vary 
  vector[n_sp] a_sp;                                // list of how each species differes from grand intercept a_f
  
  
  //level 2 - beta 
  real<lower=0,upper=200> other_interceptsbf;       //used in covariance matrix 
  real<lower=0,upper=200> lam_interceptsbf;         //used in covariance matrix 
  vector[n_sp] b_force; // slope of forcing effect 
	}

model {
  
  real yhat[N];
  
  // Priors on parameters
  sigma_y ~ normal(0,100);
  a_f ~ normal(100,100);
  b_f ~ normal(0, 5);
  
  //Level 2 priors 
  other_interceptsbf ~ normal(0, 50);
	lam_interceptsbf ~ normal(0, 50);

  a_sp ~ normal(0, sigma_af);
	sigma_af ~ normal(0,100);
  	
	
  //liklyhood 
  b_force ~ multi_normal_lpdf(rep_vector(b_f,n_sp), lambda_vcv(Vphy, lam_interceptsbf, other_interceptsbf)); 

  for(i in 1:N){
            yhat[i] = a_f + a_sp[sp[i]] + b_force[sp[i]] * force[i] ; //
			     	}
	y ~ normal(yhat, sigma_y);

}

