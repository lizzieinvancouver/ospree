// Major original edits by M Kosmala
// from file called synchrony1_notype_randslops_wcovar.stan in lizzieinvancouver/synchrony_lizzieinvancouver repo
// 5 November 2017: Using for PEP data now, added back in pooled intercepts and changes species -> sites

data {
  int N;                                // # data points
  int J;                                // # sites
  vector[N] y;                          // DOY of pheno event
  int sites[N];                       // sites identity, coded as int
  vector[N] year;                       // year of data point
  int nVars;					//number of predictors
  matrix[nVars, nVars] Imat;
}
parameters {
  vector[J] a;                          // the intercept for each sites
  vector[J] b;                          // the slope for each sites
  real<lower=0> sigma_y;                // measurement error, noise, etc.
  cov_matrix[nVars] Omega3;
 
  // hyperparameters
  real mu_a;                            // mean intercept across sites
  real<lower=0> sigma_a;                // variation of intercept among sites; implicit uniform prior
  real mu_b;                            // mean slope across sites
  real<lower=0> sigma_b;                // variation of slope among sites; implicit uniform prior
}


model {
	real ypred[N];
	//priors for covariance matrix:
	 Omega3 ~ inv_wishart(nVars+1, Imat);  // inverse-Wishart prior for correlations, 	
	 	
  for (i in 1:N){
    ypred[i] = a[sites[i]] + b[sites[i]] * year[i];
  }
  y ~ normal(ypred, sigma_y);
  a ~ normal(mu_a, sigma_a); 
  b ~ normal(mu_b, sigma_b);
} 

