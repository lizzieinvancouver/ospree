// OSPREE analysis
// 3-level model for budburst day a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species and population on INTERCEPTS and SLOPES, just forcing and photoperiod for now since chilling covarys with site

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
   vector[n_pop] a_sppop_raw; // reparameterize here
   vector[n_pop] b_force_sppop_raw; // reparameterize here
   vector[n_pop] b_photo_sppop_raw; //reparameterize here
 
   // Individual mean
   vector[n_sp] a_sp_raw; // reparameterize here
   vector[n_study] a_study_raw; // reparameterize here
   vector[n_sp] b_force_raw; // reparameterize here
   vector[n_sp] b_photo_raw; // reparameterize here
   
   real alpha; // 'grand mean' ... needed when you have more than one level
   
 }
 
 transformed parameters  {
   vector[n_sp] b_photo = mu_b_photo_sp + sigma_b_photo_sp * b_photo_raw;
   vector[n_sp] b_force = mu_b_force_sp + sigma_b_force_sp * b_force_raw;
   
   vector[n_sp] a_sp = mu_a_sp + sigma_a_sp * a_sp_raw;
   vector[n_study] a_study = mu_a_study + sigma_a_study * a_study_raw;

   vector[n_pop] a_sppop0 = sigma_a_pop * a_sppop_raw; // You need to seperate out the population effect from the species effect mean to make things easier for the indexing
   vector[n_pop] a_sppop;
   
   vector[n_pop] b_force_sppop0 = sigma_b_force_sppop * b_force_sppop_raw; 
   vector[n_pop] b_force_sppop;
   
   vector[n_pop] b_photo_sppop0 = sigma_b_photo_sppop * b_photo_sppop_raw;
   vector[n_pop] b_photo_sppop;
   
   for (j in 1:n_pop){ //
     a_sppop[j] = a_sp[sp[j]] + a_sppop0[j];
     b_force_sppop[j] = b_force[sp[j]] + b_force_sppop0[j];
     b_photo_sppop[j] = b_photo[sp[j]] + b_photo_sppop0[j];
   }
   
   
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
   
   
   // Random effects distribution of raw (ncp) priors, they should always be (0, 1)
   target += normal_lpdf(to_vector(a_sp_raw) | 0, 1);
   target += normal_lpdf(to_vector(a_study_raw) | 0, 1);
   target += normal_lpdf(to_vector(a_sppop_raw) | 0, 1);
   
   target += normal_lpdf(to_vector(b_photo_raw) | 0, 1);
   target += normal_lpdf(to_vector(b_force_raw) | 0, 1);
   
   target += normal_lpdf(to_vector(b_photo_sppop_raw) | 0, 1);
   target += normal_lpdf(to_vector(b_force_sppop_raw) | 0, 1);
   
   // Random effects distribution of remaining priors
   target += normal_lpdf(to_vector(a_sp) | 0, 20);
	 target += normal_lpdf(to_vector(a_study) | 0, 20);
   target += normal_lpdf(to_vector(a_sppop) | 0, 20);
   
   target += normal_lpdf(to_vector(b_photo) | 0, 20);
	 target += normal_lpdf(to_vector(b_force) | 0, 20);
	 target += normal_lpdf(to_vector(b_photo_sppop) | 0, 20);
   target += normal_lpdf(to_vector(b_force_sppop) | 0, 20);
   
   target += normal_lpdf(sigma_y | 0, 10);
 
   // Likelihood part of Bayesian inference
   for (i in 1:N) {
     target += normal_lpdf(y[i] | yhat[i], sigma_y);
   }
 }
