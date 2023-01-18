// Started January 18 by Lizzie
// Just a regular partial pooling model 

data {
  int<lower=1> N;
  int<lower=1> n_sp;
  int<lower=1, upper=n_sp> sp[N];
  vector[N] y; 		// response
  vector[N] x1; 	// predictor (springtemp)
}


parameters {
  real<lower=0> sigma_y;    
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;   
  real mu_a;
  real mu_b;
  vector[n_sp] b_raw; // slope 
  vector[n_sp] a; // intercept

}

transformed parameters{
  vector[n_sp] b;
  b = mu_b + sigma_b * b_raw;
}

model {
       real yhat[N];
       
       	for(i in 1:N){
            yhat[i] = 
	              a[sp[i]] + b[sp[i]] * x1[i];
			     	}
			     	
	a ~ normal(mu_a, sigma_a);
  // b ~ normal(mu_b, sigma_b); // NCP -- deleted

  y ~ normal(yhat, sigma_y);

 // Priors
    mu_a ~ normal(100, 20); 
    mu_b ~ normal(0,10);
    sigma_a ~ normal(30, 20);
    sigma_b ~ normal(1, 5);
    sigma_y ~ normal(10, 20); 
  
}


