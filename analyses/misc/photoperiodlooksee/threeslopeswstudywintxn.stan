// OSPREE analysis for single species ... 
// Started by Lizzie 
// On 28 July 2026, building off nointer_2level.stan 
// Just study partially pooled on intercept

data {
	int<lower=1> N;
   int<lower=1> n_study;
	array[N] int<lower=1, upper=n_study> study;
	vector[N] y; 		// response
	vector[N] chill; 	// predictor
	vector[N] force; 	// predictor
	vector[N] photo; 	// predictor
		
	}

transformed data { 	 
	vector[N] chillphoto; 
	chillphoto    = chill .* photo; 
}

parameters {
  real mu_a;   
  real b_force;   
  real b_photo;   
  real b_chill;   
  real b_cp; 
  real<lower=0> sigma_a_study; 
  real<lower=0> sigma_y; 

  array[n_study] real a_study; // intercept for study
	}

transformed parameters {
   array[N] real yhat;
       	for(i in 1:N){
            yhat[i] = a_study[study[i]] + // indexed with study
					b_force * force[i] + 
	      		b_photo * photo[i] + 
					b_chill * chill[i] +
					b_cp * chillphoto[i];
			     	}

	}

model {

	a_study ~ normal(mu_a, sigma_a_study); 
   mu_a ~ normal(0, 50);
   sigma_a_study ~ normal(0, 10);
	b_force ~ normal(0, 10);
	b_photo ~ normal(0, 10);
	b_chill ~ normal(0, 30);
	b_cp ~ normal(0, 20);
	
	y ~ normal(yhat, sigma_y);

}
