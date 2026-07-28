// OSPREE analysis for single species ... 
// Started by Lizzie 
// On 28 July 2026, building off nointer_2level.stan ...
// SUPER SIMPLE! No partial pooling, nothing 

data {
	int<lower=1> N;
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
		
	}

parameters {
  real a;   
  real b_force;   
  real b_photo;   
  real b_chill;   
  real<lower=0> sigma_y; 
}

transformed parameters {
   array[N] real yhat;
       	for(i in 1:N){
            yhat[i] = a + // indexed with study
					b_force * force[i] + 
	      		b_photo * photo[i] + 
					b_chill * chill[i];
			     	}

	}

model {
   a ~ normal(0, 50);
	b_force ~ normal(0, 10);
	b_photo ~ normal(0, 10);
	b_chill ~ normal(0, 30);
	
	y ~ normal(yhat, sigma_y);
}
