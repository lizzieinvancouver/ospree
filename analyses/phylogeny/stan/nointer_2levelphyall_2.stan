// OSPREE analysis -- updated with Will's new code (ubermini_2) in Sep 2020
// Also, skip the partial pooling on intercepts for now
// Simplified version of nointe_2level.stan 
// with phylogeny on intercept and on all the slopes

// If you want a PMM, see this:
// See https://groups.google.com/forum/#!topic/stan-users/Irv9RWDCpQE


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
	//vector[N] chill; 	// predictor
	//vector[N] photo; 	// predictor
        matrix[n_sp,n_sp]Vphy;     // phylogeny
		
	}

parameters {
  real<lower=0> sigma_y; 
  // real<lower=0,upper=200> other_intercepts;       
  // real<lower=0,upper=200> lam_intercepts;       
  real<lower=0,upper=200> other_interceptsbf;       
  real<lower=0,upper=200> lam_interceptsbf;    
  //real<lower=0,upper=200> other_interceptsbc;       
  //real<lower=0,upper=200> lam_interceptsbc;    
  //real<lower=0,upper=200> other_interceptsbp;       
  //real<lower=0,upper=200> lam_interceptsbp;    
  real b_f;
  //real b_c;
  // real b_p;
  // real a; // grand mean intercept
  // real a_here; 
  vector[n_sp] a_sp; // intercept for species
  vector[n_sp] b_force; // slope of forcing effect 
  //vector[n_sp] b_chill; // slope of chill effect 
  //vector[n_sp] b_photo; // slope of photo effect 
	}

model {
       real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] ; //+ b_chill[sp[i]] * chill[i] + b_photo[sp[i]] * photo[i]
			     	}

	// a_sp ~ multi_normal(rep_vector(a_here,n_sp), lambda_vcv(Vphy, lam_intercepts, other_intercepts)); 
	b_force ~ multi_normal(rep_vector(b_f,n_sp), lambda_vcv(Vphy, lam_interceptsbf, other_interceptsbf)); 
	//b_chill ~ multi_normal(rep_vector(b_c,n_sp), lambda_vcv(Vphy, lam_interceptsbc, other_interceptsbc)); 
//	b_photo ~ multi_normal(rep_vector(b_p,n_sp), lambda_vcv(Vphy, lam_interceptsbp, other_interceptsbp)); 

        // a_sp ~ normal(10, 30);
        b_f ~ normal(0, 5);
        // b_c ~ normal(0, 5);
        // b_p ~ normal(0, 5);
        // a_here ~ normal(0, 10); 
        // other_intercepts ~ normal(0, 50);
	// lam_intercepts ~ normal(0, 50);
       // other_interceptsbf ~ normal(0, 50);
	//lam_interceptsbf ~ normal(0, 50);
        //other_interceptsbc ~ normal(0, 50);
	//lam_interceptsbc ~ normal(0, 50);
        //other_interceptsbp ~ normal(0, 50);
	//lam_interceptsbp ~ normal(0, 50);
	y ~ normal(yhat, sigma_y);

}
