//model based on stan_Part1.stan writen by Faith in Feb 2020, but splir
//from that model Dec 2020 because Faith wanted to try a grand alpha
	// it should get species specific trait effect values 
//thsi only works on one trait at a time at the moment 

data {
	int < lower = 1 > N; // Sample size
 
 	//level 1
 	vector[N] yTraiti; // 

 	//level 2
	int < lower = 1 > n_spec; // number of random effect levels (species) 
	int < lower = 1, upper = n_spec > species[N]; // id of random effect (species)

	int < lower = 1 > n_study; // number of random effect levels (study) 
	int < lower = 1, upper = n_study > study[N]; // id of random effect (study)

}


parameters{

	//level 1
	// general varience/error
	real <lower =0> sigmaTrait_y; // overall variation accross observations
  real mu_grand; //Grand mean trait value 


	//level 2
	real <lower = 0> sigma_sp; // variation of intercept amoung species
	real muSp[n_spec]; // mean of the alpha value for species

	real <lower = 0> sigma_stdy; // variation of intercept amoung studies
	real muStdy[n_study]; // mean of the alpha value for studies 

}

transformed parameters{
	//Individual mean for species and study
	//real ymu[N];

	//Individual mean calculation 
	//for (i in 1:N){
	//	ymu[i] = mu_grand + muSp[species[i]] + muStdy[study[i]];  
	//}
}
model{ 
	//assign priors - these are just related to the data I simulated at the moment,
	//they will need work to match real trait data 

	 real ymu[N];
	  
	sigmaTrait_y ~ normal(0,5);
  mu_grand ~ normal(20, 10);

	sigma_sp ~ normal(0,10);
	muSp ~ normal(0, sigma_sp);

	sigma_stdy ~ normal(0, 5);
	muStdy ~ normal(0, sigma_stdy);

	// run the actual model - likihood
	
	for (i in 1:N){
	  ymu[i] = mu_grand + muSp[species[i]] + muStdy[study[i]];
		yTraiti[i] ~ normal(ymu[i], sigmaTrait_y);
	}


}


generated quantities {
} 
