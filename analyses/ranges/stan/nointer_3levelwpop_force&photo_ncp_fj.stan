// OSPREE analysis
// 3-level model for budburst day a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species and population on INTERCEPTS and SLOPES, just forcing for now

 data {
   // Define variables in data
   // Number of level-1 observations (an integer)
   int<lower=0> N;
   // Number of level-2 clusters
   int<lower=0> n_sp;
   int<lower=0> n_study;
   // Number of level-3 clusters
   int<lower=0> n_pop;
   
   //Cue parameters
   vector[N] force;
   vector[N] photo;
 
   // Cluster IDs
   int<lower=1, upper=n_sp> sp[N];
   int<lower=1, upper=n_study> study[N];
   int<lower=1, upper=n_pop> pop[N]; //wait should this be by n_sp rather than N???
 
   // Level 3 look up vector for level 2
   //int<lower=1> popLookup[n_sp];
 
   // Continuous outcome
   real y[N];
   
 }
 
 parameters {
   // Define parameters to estimate
   // Population intercept (a real number)
   real mu_a_sp;
   real mu_a_study;
   // Population slope
   real mu_b_force_sp;
   real mu_b_photo_sp;
 
   // Level-1 errors
   real<lower=0> sigma_y;
   real<lower=0> sigma_a_sp;
   real<lower=0> sigma_a_study;
 
   // Level-2 random effect
   //real mu_a_pop[n_sp];
   real<lower=0> sigma_b_force_sp;
   real<lower=0> sigma_b_photo_sp;
 
   // Level-3 random effect
   //real u_0k[Nk];
   // Population slope
   real mu_b_force_pop;
   real mu_b_photo_pop;
   real<lower=0> sigma_b_force_pop;
   real<lower=0> sigma_b_photo_pop;
   real<lower=0> sigma_a_pop;
   
   // Varying intercepts
   real a_sppop[n_pop];
   //real b_force_sppop[n_pop];
   real b_force_sppop_raw[n_pop]; //do NCP here
   //real b_photo_sppop[n_pop];
   real b_photo_sppop_raw[n_pop]; //do NCP here
 
   // Individual mean
   real a_sp[n_sp];
   real a_study[n_study];
   real b_force_raw[n_sp]; //do NCP here 
   //real b_force[n_sp];
   real b_photo_raw[n_sp]; //do NCP here 
   //real b_photo[n_sp];
   
   real alpha; // 'grand mean' ... needed when you have more than one level
   
 }
 
 transformed parameters  {
   real yhat[N];
   real b_photo[n_sp];
   real b_force[n_sp];
   real b_force_sppop[n_pop];
   real b_photo_sppop[n_pop];
   
   
  for (j in 1:n_sp){
    b_photo[j] = mu_b_photo_sp + sigma_b_photo_sp * b_photo_raw[j];
    b_force[j] = mu_b_force_sp + sigma_b_force_sp * b_force_raw[j];
        }
        
  for (j in 1:n_pop){
    b_photo_sppop[j] = mu_b_photo_pop + sigma_b_photo_pop * b_photo_sppop_raw[j];
    b_force_sppop[j] = mu_b_force_pop + sigma_b_force_pop * b_force_sppop_raw[j];
        }
   
   // Individual mean
   for(i in 1:N){
            yhat[i] = alpha +
            a_study[study[i]] + a_sppop[pop[i]] + // indexed with population
		b_force_sppop[pop[i]] * force[i] +
		b_photo_sppop[pop[i]] * photo[i];
			     	}
 }
 
 model {
   // Prior part of Bayesian inference
   // Flat prior for mu (no need to specify if non-informative)
   
   // Varying intercepts definition
   // Level-3 (10 level-3 random intercepts)
   for (j in 1:n_pop) {
     a_sppop[j] ~ normal(a_sp[sp[j]], sigma_a_pop);
   }
   // Level-2 (100 level-2 random intercepts)
   for (k in 1:n_pop) {
     b_force_sppop[k] ~ normal(b_force[sp[k]], sigma_b_force_pop);
   }
   
   for (l in 1:n_pop) {
     b_photo_sppop[l] ~ normal(b_photo[sp[l]], sigma_b_photo_pop);
   }
 
   // Random effects distribution
   a_sp  ~ normal(mu_a_sp, sigma_a_sp);
   a_study  ~ normal(mu_a_study, sigma_a_study);
   //Faith commnted these lines out because this bit is handled in the loop with the raw ncp parameters
   //b_force ~ normal(mu_b_force_sp, sigma_b_force_sp);
   //b_photo ~ normal(mu_b_photo_sp, sigma_b_photo_sp);
   
   mu_b_force_sp ~ normal(0, 20);
   sigma_b_force_sp ~ normal(0, 10); 
   
   //Faith added priors for the raw 0 to 1 bit of ncp
   b_photo_raw ~ normal(0, 1);
   b_force_raw ~ normal(0, 1);
   
   mu_b_photo_sp ~ normal(0, 20);
   sigma_b_photo_sp ~ normal(0, 10); 
   
   mu_a_sp ~ normal(0, 40);
   sigma_a_sp ~ normal(0, 10);
   
   mu_a_study ~ normal(0, 30);
   sigma_a_study ~ normal(0, 10);
   
   sigma_a_pop ~ normal(0, 5);
   sigma_b_force_pop ~ normal(0, 5);
   sigma_b_photo_pop ~ normal(0, 5);
   sigma_y ~ normal(0, 10);
 
   // Likelihood part of Bayesian inference
   // Outcome model N(mu, sigma^2) (use SD rather than Var)
   for (i in 1:N) {
     y[i] ~ normal(yhat[i], sigma_y);
   }
 }
