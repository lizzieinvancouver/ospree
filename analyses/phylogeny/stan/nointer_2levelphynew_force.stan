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
  real<lower=0> sigma_y;     
  real<lower=0,upper=200> other_interceptsbf;       
  real<lower=0,upper=200> lam_interceptsbf;      
  real b_f;
  vector[n_sp] a_sp; // intercept for species
  vector[n_sp] b_force; // slope of forcing effect 
	}

model {
       real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] ; //
			     	}

	b_force ~ multi_normal(rep_vector(b_f,n_sp), lambda_vcv(Vphy, lam_interceptsbf, other_interceptsbf)); 

        b_f ~ normal(0, 5);
        other_interceptsbf ~ normal(0, 50);
	lam_interceptsbf ~ normal(0, 50);
 
	y ~ normal(yhat, sigma_y);

}
