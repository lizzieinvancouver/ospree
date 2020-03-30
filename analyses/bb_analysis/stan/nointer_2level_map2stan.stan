// OSPREE analysis
// Stancode from map2stan model fit from bb_analysis/models_getcov.R
// poor model fit!! and no covariance matrix included
// 2 level model for budburst day or percent budburst as a function of forcing temperature, chilling units, photoperiod in a meta-analysis of 100+ studies
// Level: Species (actually genus) on INTERCEPTS and SLOPES

data{
    int<lower=1> N;
    int<lower=1> N_sp;
    real chill[N];
    real force[N];
    real photo[N];
    int sp[N];
}
parameters{
    real resp;
    vector[N_sp] bc;
    vector[N_sp] bp;
    vector[N_sp] bf;
    vector[N_sp] a;
    vector[4] mu_species;
    real<lower=0> sigma;
    vector<lower=0>[4] sigma_species;
    corr_matrix[4] Rho;
}
transformed parameters{
    vector[4] v_abfbpbc[N_sp];
    cov_matrix[4] SRS_sigma_speciesRho;
    for ( j in 1:N_sp ) {
        v_abfbpbc[j,1] = a[j];
        v_abfbpbc[j,2] = bf[j];
        v_abfbpbc[j,3] = bp[j];
        v_abfbpbc[j,4] = bc[j];
    }
    SRS_sigma_speciesRho = quad_form_diag(Rho,sigma_species);
}
model{
    vector[N] mu;
    Rho ~ lkj_corr( 4 );
    sigma_species ~ normal( 0 , 10 );
    sigma ~ normal( 0 , 10 );
    mu_species ~ normal( 0 , 50 );
    v_abfbpbc ~ multi_normal( mu_species , SRS_sigma_speciesRho );
    for ( i in 1:N ) {
        mu[i] = a[sp[i]] + bf[sp[i]] * force[i] + bp[sp[i]] * photo[i] + bc[sp[i]] * chill[i];
    }
    resp ~ normal( mu , sigma );
}
generated quantities{
    vector[N] mu;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        mu[i] = a[sp[i]] + bf[sp[i]] * force[i] + bp[sp[i]] * photo[i] + bc[sp[i]] * chill[i];
    }
}