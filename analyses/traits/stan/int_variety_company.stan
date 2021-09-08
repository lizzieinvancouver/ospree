
data {
  int<lower=1> N;
  vector[N] y; 		// response
  int<lower=1> n_company;
  int<lower=1, upper=n_company> company[N];
  int<lower=1> n_variety;
  int<lower=1, upper=n_variety> variety[N];
  // Priors
  real prior_a_grand_mu;
  real prior_a_grand_sigma;
  real prior_sigma_a_company_mu;
  real prior_sigma_a_company_sigma;
  real prior_sigma_a_variety_mu;
  real prior_sigma_a_variety_sigma;
  real prior_sigma_y_mu;
  real prior_sigma_y_sigma;
}
	
parameters {
  real a_grand;   
  real<lower=0> sigma_a_company;
  real<lower=0> sigma_a_variety;
  real<lower=0> sigma_y; 
  real a_company[n_company]; // intercept for company
  real a_variety[n_variety]; // intercept for varieties
  
}

transformed parameters {
  real mu_y[N];

  for(i in 1:N){
    mu_y[i] = a_grand + a_company[company[i]] + a_variety[variety[i]];
	}
 
}
	
model {   	
  // likelihood
  y ~ normal(mu_y, sigma_y);
  
  a_company ~ normal(0, sigma_a_company);
  a_variety ~ normal(0, sigma_a_variety);
  
  // priors
  a_grand ~ normal(prior_a_grand_mu, prior_a_grand_sigma);
  sigma_a_company ~ normal(prior_sigma_a_company_mu, prior_sigma_a_company_sigma);
  sigma_a_variety ~ normal(prior_sigma_a_variety_mu, prior_sigma_a_variety_sigma);
  sigma_y ~ normal(prior_sigma_y_mu, prior_sigma_y_sigma);
}

generated quantities {
  /* vector[N] y_pred; */

  /* for(i in 1:N){ */
  /*   y_pred[i] = normal_rng(mu_y[i], sigma_y); */
  /* }   */
}
