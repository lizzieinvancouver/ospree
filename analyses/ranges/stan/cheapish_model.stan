/* Copied from nointer_2level_2dfs and cheap_model.stan 
By Lizzie so far 
Not correct so far, see jointtraitphen.stan and follow that method */

data {
  int<lower=1> N;
  int<lower=1> X; // length of range data
  int<lower=1> n_sp;
  int<lower=1, upper=n_sp> sp[N];
  vector[N] y; 		// response
  vector[N] chill; 	// experiment predictor
  vector[N] force; 	// experiment predictor
  vector[N] photo; 	// experiment predictor
  vector[X] climvar; 	// climate predictor

  
}

parameters {
  real mu_a_sp;   
  real mu_b_force_sp;   
  real mu_b_photo_sp;   
  real mu_b_chill_sp;   
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_force_sp; 
  real<lower=0> sigma_b_photo_sp; 
  real<lower=0> sigma_b_chill_sp; 
  real<lower=0> sigma_y; 

  real alpha;
  real beta;jointtraitphen.stan and follow that method
  real<lower=0> sigma;
  
  real a_sp[n_sp]; // intercept for species
  real b_force[n_sp]; // slope of forcing effect 
  real b_photo[n_sp]; // slope of photoperiod effect
  real b_chill[n_sp]; // slope of chill effect
}

transformed parameters {
  real yhat[N];
  for(i in 1:N){
    yhat[i] = a_sp[sp[i]] + // indexed with species
    b_force[sp[i]] * force[i] + 
      b_photo[sp[i]] * photo[i] +
      b_chill[sp[i]] * chill[i];
  }

  
}

model {

  a_sp ~ normal(mu_a_sp, sigma_a_sp); 
  b_force ~ normal(mu_b_force_sp, sigma_b_force_sp); 
  b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp); 
  b_chill ~ normal(mu_b_chill_sp, sigma_b_chill_sp); 
  
  mu_b_force_sp ~ normal(0, 25);
  sigma_b_force_sp ~ normal(0, 10);
  mu_b_photo_sp ~ normal(0, 25);
  sigma_b_photo_sp ~ normal(0, 10);
  mu_b_chill_sp ~ normal(0, 35);
  sigma_b_chill_sp ~ normal(0, 10);
  mu_a_sp ~ normal(0, 30);
  sigma_a_sp ~ normal(0, 10);
  
  y ~ normal(yhat, sigma_y);
  b_force ~ normal(alpha + beta * climvar, sigma);

  
}

generated quantities{

}
