// OSPREE analysis
// Simplified version of nointe_2level.stan 
// with phylogeny on intercept and on all the slopes

// If you want a PMM, see this:
// See https://groups.google.com/forum/#!topic/stan-users/Irv9RWDCpQE


data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] force; 	// predictor
	vector[N] chill; 	// predictor
	vector[N] photo; 	// predictor
        matrix[n_sp,n_sp]Vphy;     // phylogeny
		
	}

parameters {
  real<lower=0> sigma_y; 
  real<lower=0,upper=200> null_intercepts;       
  real<lower=0,upper=200> lam_intercepts;       
  real<lower=0,upper=200> null_interceptsbf;       
  real<lower=0,upper=200> lam_interceptsbf;    
  real<lower=0,upper=200> null_interceptsbc;       
  real<lower=0,upper=200> lam_interceptsbc;    
  real<lower=0,upper=200> null_interceptsbp;       
  real<lower=0,upper=200> lam_interceptsbp;    
  real a; // grand mean intercept
  vector[n_sp] a_sp; // intercept for species
  vector[n_sp] b_force; // slope of forcing effect 
  vector[n_sp] b_chill; // slope of chill effect 
  vector[n_sp] b_photo; // slope of photo effect 
	}

model {
       real yhat[N];
       	for(i in 1:N){
            yhat[i] = a + a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i] + b_chill[sp[i]] * chill[i] + 
                b_photo[sp[i]] * photo[i];
			     	}

	a_sp ~ multi_normal(rep_vector(0,n_sp), diag_matrix(rep_vector(null_intercepts, n_sp)) + lam_intercepts*Vphy); 
	b_force ~ multi_normal(rep_vector(0,n_sp), diag_matrix(rep_vector(null_interceptsbf, n_sp)) + lam_interceptsbf*Vphy); 
	b_chill ~ multi_normal(rep_vector(0,n_sp), diag_matrix(rep_vector(null_interceptsbc, n_sp)) + lam_interceptsbc*Vphy); 
	b_photo ~ multi_normal(rep_vector(0,n_sp), diag_matrix(rep_vector(null_interceptsbp, n_sp)) + lam_interceptsbp*Vphy); 

        a ~ normal(10, 30);
        null_intercepts ~ normal(0, 50);
	lam_intercepts ~ normal(0, 50);
        null_interceptsbf ~ normal(0, 50);
	lam_interceptsbf ~ normal(0, 50);
        null_interceptsbc ~ normal(0, 50);
	lam_interceptsbc ~ normal(0, 50);
        null_interceptsbp ~ normal(0, 50);
	lam_interceptsbp ~ normal(0, 50);
	y ~ normal(yhat, sigma_y);

}
