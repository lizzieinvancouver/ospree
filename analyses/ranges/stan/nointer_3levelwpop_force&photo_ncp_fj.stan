// OSPREE analysis
// 3-level model for budburst day a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species and population on INTERCEPTS and SLOPES, just forcing and photoperiod for now
// using BRMS stancode, incorporating 'functions' block and cholesky matrix
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
   int<lower=1, upper=n_pop> pop[N]; 
 
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
   //real mu_a_sppop;
   //real mu_b_force_sppop;
   //real mu_b_photo_sppop;
 
   // Level-1 errors
   real<lower=0> sigma_y;
   real<lower=0> sigma_a_sp;
   real<lower=0> sigma_a_study;
 
   // Level-2 random effect
   real<lower=0> sigma_b_force_sp;
   real<lower=0> sigma_b_photo_sp;
 
   // Level-3 random effect
   // Population slope
   real<lower=0> sigma_b_force_sppop;
   real<lower=0> sigma_b_photo_sppop;
   real<lower=0> sigma_a_pop;
   
   // Varying intercepts
   real a_sppop[n_pop];
   //vector[n_pop] a_sppop_raw;
   real b_force_sppop[n_pop];
   //vector[n_pop] b_force_sppop_raw; //do NCP here
   real b_photo_sppop[n_pop];
   //vector[n_pop] b_photo_sppop_raw; //do NCP here
 
   // Individual mean
   //real a_sp[n_sp];
   //real a_study[n_study];
   vector[n_sp] a_sp_raw;
   vector[n_study] a_study_raw;
   vector[n_sp] b_force_raw; //do NCP here 
   //real b_force[n_sp];
   vector[n_sp] b_photo_raw; //do NCP here 
   //real b_photo[n_sp];
   
   real alpha; // 'grand mean' ... needed when you have more than one level
   
 }
 
 transformed parameters  {
   vector[n_sp] b_photo = mu_b_photo_sp + sigma_b_photo_sp * b_photo_raw;
   vector[n_sp] b_force = mu_b_force_sp + sigma_b_force_sp * b_force_raw;
   
   vector[n_sp] a_sp = mu_a_sp + sigma_a_sp * a_sp_raw;
   vector[n_study] a_study = mu_a_study + sigma_a_study * a_study_raw;
   
   //vector[n_pop] a_sppop = mu_a_sppop + sigma_a_pop * a_sppop_raw;
   
   //vector[n_pop] b_force_sppop = mu_b_force_sppop + sigma_b_photo_sppop * b_photo_sppop_raw;
   //vector[n_pop] b_photo_sppop = mu_b_force_sppop + sigma_b_force_sppop * b_force_sppop_raw;
   
   
  /*for (j in 1:n_sp){
    b_photo[j] = mu_b_photo_sp + sigma_b_photo_sp * b_photo_raw[j];
    b_force[j] = mu_b_force_sp + sigma_b_force_sp * b_force_raw[j];
        }
        
  for (j in 1:n_pop){
    b_photo_sppop[j] = sigma_b_photo_sppop * b_photo_sppop_raw[j];
    b_force_sppop[j] = sigma_b_force_sppop * b_force_sppop_raw[j];
        }*/
   
   
 }
 
 model {
   vector[N] yhat;
   // Individual mean
   for(i in 1:N){
            yhat[i] = alpha +
            a_study[study[i]] + a_sppop[pop[i]] + // indexed with population
		b_force_sppop[pop[i]] * force[i] +
		b_photo_sppop[pop[i]] * photo[i];
			     	}
   
   
   // Prior part of Bayesian inference
   // Flat prior for mu (no need to specify if non-informative)
   // Varying intercepts definition
   // Level-3 (10 level-3 random intercepts)
   for (j in 1:n_pop) {
     target += normal_lpdf(a_sppop[j] | a_sp[sp[j]], sigma_a_pop);
     
     // Level-2 (100 level-2 random intercepts)
     target += normal_lpdf(b_force_sppop[j] | b_force[sp[j]], sigma_b_force_sppop);
     target += normal_lpdf(b_photo_sppop[j] | b_photo[sp[j]], sigma_b_photo_sppop);
   }
   
   // Random effects distribution of raw (ncp) priors
   target += normal_lpdf(to_vector(a_sp_raw) | 0, 1);
   target += normal_lpdf(to_vector(a_study_raw) | 0, 1);
   //target += normal_lpdf(to_vector(a_sppop_raw) | 0, 1);
   
   target += normal_lpdf(to_vector(b_photo_raw) | 0, 1);
   target += normal_lpdf(to_vector(b_force_raw) | 0, 1);
   
   // Random effects distribution of remaining priors
   target += normal_lpdf(to_vector(b_photo_sppop) | 0, 20);
   target += normal_lpdf(to_vector(b_force_sppop) | 0, 20);
   
   target += normal_lpdf(to_vector(a_sp) | 0, 20);
	 target += normal_lpdf(to_vector(a_study) | 0, 20);
   
   //target += normal_lpdf(to_vector(a_sppop) | 0, 10);
   target += normal_lpdf(to_vector(a_sppop) | 0, 20);
   
   target += normal_lpdf(to_vector(b_photo) | 0, 20);
	 target += normal_lpdf(to_vector(b_force) | 0, 20);
   
   /*mu_b_force_sp ~ normal(0, 20);
   //sigma_b_force_sp ~ normal(0, 10); 
   
   mu_b_photo_sp ~ normal(0, 20);
   sigma_b_photo_sp ~ normal(0, 10); 
   
   mu_a_study ~ normal(0, 30);
   sigma_a_study ~ normal(0, 10);*/
   
   //sigma_a_pop ~ normal(0, 5);
   //target += normal_lpdf(mu_a_sp | 0, 40);
   //target += normal_lpdf(sigma_a_sp | 0, 10);
   //target += normal_lpdf(mu_a_sppop | 0, 10);
   target += normal_lpdf(sigma_a_pop | 0, 5);
   
   target += normal_lpdf(sigma_b_force_sppop | 0, 5);
   target += normal_lpdf(sigma_b_photo_sppop | 0, 5);
   target += normal_lpdf(sigma_y | 0, 10);
 
   // Likelihood part of Bayesian inference
   for (i in 1:N) {
     target += normal_lpdf(y[i] | yhat[i], sigma_y);
     //y[i] ~ normal(yhat[i], sigma_y);
   }
 }
