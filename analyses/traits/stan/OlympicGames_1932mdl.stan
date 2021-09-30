
data {
  int<lower=0> N;//number of observation
  int<lower=0> n_pair;//number of pairs
  int<lower=0> n_judge;//number of judges
  int<lower=1,upper = n_pair> pair[N];//vector of pairs
  int<lower=1,upper = n_judge> judge[N];//vector of judges
  vector[N] y;
}

parameters {
  vector[n_pair] gamma;
  vector[n_judge] delta;
  real<lower=0> mu;
  real<lower=0> sigma_gamma;
  real<lower=0> sigma_delta;
  real<lower=0> sigma_y;
}

transformed parameters {
  vector[N] y_hat;

  for (i in 1:N)
  y_hat[i] = mu + gamma[pair[i]] + delta[judge[i]];
}

model {
  mu ~ normal(0,10);
  sigma_gamma ~ normal(0,10);
  sigma_delta ~ normal(0,10);
  sigma_y ~ normal(0,1);

  gamma ~ normal(0, sigma_gamma);
  delta ~ normal(0, sigma_delta);
  //likelihood
  y ~ normal(y_hat, sigma_y);
}
generated quantities {
  real y_rep[N];
  for(n in 1:N)
    y_rep[n] = normal_rng(y_hat[n],sigma_y);
}

