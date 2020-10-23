// OSPREE analysis
// 3-level model for budburst day a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species and population on INTERCEPTS and SLOPES, just forcing for now

 data {
   // Define variables in data
   // Number of level-1 observations (an integer)
   int<lower=0> N;
   // Number of level-2 clusters
   int<lower=0> n_sp;
   // Number of level-3 clusters
   int<lower=0> n_pop;
   
   //Cue parameters
   vector[N] force; 
 
   // Cluster IDs
   int<lower=1> sp[N];
   int<lower=1> pop[N];
 
   // Level 3 look up vector for level 2
   //int<lower=1> popLookup[n_sp];
 
   // Continuous outcome
   real y[N];
   
 }
 
 parameters {
   // Define parameters to estimate
   // Population intercept (a real number)
   real mu_a_sp;
   // Population slope
   real mu_b_force_sp;
 
   // Level-1 errors
   real<lower=0> sigma_y;
   real<lower=0> sigma_a_sp;
 
   // Level-2 random effect
   //real mu_a_pop[n_sp];
   real<lower=0> sigma_b_force_sp;
 
   // Level-3 random effect
   //real u_0k[Nk];
   real<lower=0> sigma_b_pop;
   real<lower=0> sigma_a_pop;
   
   // Varying intercepts
   real a_pop[n_pop];
   real b_force_pop[n_pop];
 
   // Individual mean
   real a_sp[n_sp];
   real b_force[n_sp];
   
 }
 
 transformed parameters  {
   real yhat[N];
   // Individual mean
   for(i in 1:N){
            yhat[i] = a_pop[pop[i]] + // indexed with population
		b_force_pop[pop[i]] * force[i];
			     	}
 }
 
 model {
   // Prior part of Bayesian inference
   // Flat prior for mu (no need to specify if non-informative)
   
   // Varying intercepts definition
   // Level-3 (10 level-3 random intercepts)
   for (k in 1:n_pop) {
     a_pop[k] ~ normal(a_sp[sp[k]], sigma_a_pop);
   }
   // Level-2 (100 level-2 random intercepts)
   for (j in 1:n_pop) {
     b_force_pop[j] ~ normal(b_force[sp[j]], sigma_b_pop);
   }
 
   // Random effects distribution
   a_sp  ~ normal(mu_a_sp, sigma_a_sp);
   b_force ~ normal(mu_b_force_sp, sigma_b_force_sp);
   
   mu_b_force_sp ~ normal(0, 50);
   sigma_b_force_sp ~ student_t(3, 0, 35); 
   
   mu_a_sp ~ normal(0, 50);
   sigma_a_sp ~ student_t(3, 0, 35);
   
   sigma_a_pop ~ student_t(3, 0, 35);
   sigma_b_pop ~ student_t(3, 0, 35);
   sigma_y ~ student_t(3, 0, 35);
 
   // Likelihood part of Bayesian inference
   // Outcome model N(mu, sigma^2) (use SD rather than Var)
   for (i in 1:N) {
     y[i] ~ normal(yhat[i], sigma_y);
   }
 }