// Major original edits by M Kosmala
// from file called synchrony1_notype_randslops_wcovar.stan in lizzieinvancouver/synchrony_lizzieinvancouver repo
// 6 November 2017: Using for PEP data now, added back in pooled intercepts and changes species -> sites
// 7 November 2017: Tried turning off covariance for now and ADDED NCP to b
// And I added some priors

data {
  int N;                                // # data points
  int J;                                // # sites
  vector[N] y;                          // DOY of pheno event
  int sites[N];                       // sites identity, coded as int
  vector[N] year;                       // year of data point
 // int nVars;					//number of predictors
 // matrix[nVars, nVars] Imat;
}
parameters {
  vector[J] a;                          // the intercept for each sites
  vector[J] b_ncp;                          // the slope for each sites, NCP version
  real<lower=0> sigma_y;                // measurement error, noise, etc.
//  cov_matrix[nVars] Omega3;
 
  // hyperparameters
  real mu_a;                            // mean intercept across sites
  real<lower=0> sigma_a;                // variation of intercept among sites; implicit uniform prior
  real mu_b;                            // mean slope across sites
  real<lower=0> sigma_b;                // variation of slope among sites; implicit uniform prior
  
}

transformed parameters{
  vector[J] b;                          // the slope for each sites
  b = mu_b + sigma_b*b_ncp;
}

model {
	real ypred[N];
	 	
  for (i in 1:N){
    ypred[i] = a[sites[i]] + b[sites[i]] * year[i];
  }
  y ~ normal(ypred, sigma_y);
  a ~ normal(mu_a, sigma_a); 
  // b ~ normal(mu_b, sigma_b); // NCP 

  //priors for covariance matrix:
  //Omega3 ~ inv_wishart(nVars+1, Imat);  // inverse-Wishart prior for correlations, 	
  // Other priors
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 10);
  sigma_y ~ normal(0, 30);
  sigma_b ~ normal(0, 30);
} 

