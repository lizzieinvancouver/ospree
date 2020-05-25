// OSPREE analysis
// Simplified version of nointe_2level.stan 
// with phylogeny on intercept and partial pooling on the slopes via phy also

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
  real<lower=0,upper=100> null_intercepts;       
  real<lower=0,upper=100> lam_intercepts;       
  real<lower=0,upper=100> null_interceptsb;       
  real<lower=0,upper=100> lam_interceptsb;    
  real a; // grand mean intercept
  vector[n_sp] a_sp; // intercept for species
  vector[n_sp] b_force; // slope of forcing effect 
	}

model {
       real yhat[N];
       	for(i in 1:N){
            yhat[i] = a + a_sp[sp[i]] + // indexed with species
		b_force[sp[i]] * force[i];
			     	}

	a_sp ~ multi_normal(rep_vector(0,n_sp), diag_matrix(rep_vector(null_intercepts, n_sp)) + lam_intercepts*Vphy); 
	b_force ~ multi_normal(rep_vector(0,n_sp), diag_matrix(rep_vector(null_interceptsb, n_sp)) + lam_interceptsb*Vphy); 

        a ~ normal(10, 20);
        null_intercepts ~ normal(0, 20);
	lam_intercepts ~ normal(0, 20);
        null_interceptsb ~ normal(0, 20);
	lam_interceptsb ~ normal(0, 20);
	y ~ normal(yhat, sigma_y);

}

