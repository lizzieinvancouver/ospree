// BRMS Output for Latitude Analysis - Poisson

// generated with brms 2.5.0
functions { 
} 
data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  int<lower=1> K;  // number of population-level effects 
  matrix[N, K] X;  // population-level design matrix 
  // data for group-level effects of ID 1
  int<lower=1> J_1[N];
  int<lower=1> N_1;
  int<lower=1> M_1;
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  vector[N] Z_1_3;
  vector[N] Z_1_4;
  vector[N] Z_1_5;
  vector[N] Z_1_6;
  vector[N] Z_1_7;
  vector[N] Z_1_8;
  vector[N] Z_1_9;
  vector[N] Z_1_10;
  vector[N] Z_1_11;
  int<lower=1> NC_1;
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
  int Kc = K - 1; 
  matrix[N, K - 1] Xc;  // centered version of X 
  vector[K - 1] means_X;  // column means of X before centering 
  for (i in 2:K) { 
    means_X[i - 1] = mean(X[, i]); 
    Xc[, i - 1] = X[, i] - means_X[i - 1]; 
  } 
} 
parameters { 
  vector[Kc] b;  // population-level effects 
  real temp_Intercept;  // temporary intercept 
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // unscaled group-level effects
  // cholesky factor of correlation matrix
  cholesky_factor_corr[M_1] L_1;
} 
transformed parameters { 
  // group-level effects 
  matrix[N_1, M_1] r_1 = (diag_pre_multiply(sd_1, L_1) * z_1)';
  vector[N_1] r_1_1 = r_1[, 1];
  vector[N_1] r_1_2 = r_1[, 2];
  vector[N_1] r_1_3 = r_1[, 3];
  vector[N_1] r_1_4 = r_1[, 4];
  vector[N_1] r_1_5 = r_1[, 5];
  vector[N_1] r_1_6 = r_1[, 6];
  vector[N_1] r_1_7 = r_1[, 7];
  vector[N_1] r_1_8 = r_1[, 8];
  vector[N_1] r_1_9 = r_1[, 9];
  vector[N_1] r_1_10 = r_1[, 10];
  vector[N_1] r_1_11 = r_1[, 11];
} 
model { 
  vector[N] mu = temp_Intercept + Xc * b;
  for (n in 1:N) { 
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n] + r_1_3[J_1[n]] * Z_1_3[n] + r_1_4[J_1[n]] * Z_1_4[n] + r_1_5[J_1[n]] * Z_1_5[n] + r_1_6[J_1[n]] * Z_1_6[n] + r_1_7[J_1[n]] * Z_1_7[n] + r_1_8[J_1[n]] * Z_1_8[n] + r_1_9[J_1[n]] * Z_1_9[n] + r_1_10[J_1[n]] * Z_1_10[n] + r_1_11[J_1[n]] * Z_1_11[n];
  } 
  // priors including all constants 
  target += normal_lpdf(b | 0, 1); 
  target += student_t_lpdf(temp_Intercept | 3, 3, 10); 
  target += student_t_lpdf(sd_1 | 3, 0, 10)
    - 11 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(to_vector(z_1) | 0, 1);
  target += lkj_corr_cholesky_lpdf(L_1 | 1); 
  // likelihood including all constants 
  if (!prior_only) { 
    target += poisson_log_lpmf(Y | mu);
  } 
} 
generated quantities { 
  // actual population-level intercept 
  real b_Intercept = temp_Intercept - dot_product(means_X, b); 
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // take only relevant parts of correlation matrix
  cor_1[1] = Cor_1[1,2]; 
  cor_1[2] = Cor_1[1,3]; 
  cor_1[3] = Cor_1[2,3]; 
  cor_1[4] = Cor_1[1,4]; 
  cor_1[5] = Cor_1[2,4]; 
  cor_1[6] = Cor_1[3,4]; 
  cor_1[7] = Cor_1[1,5]; 
  cor_1[8] = Cor_1[2,5]; 
  cor_1[9] = Cor_1[3,5]; 
  cor_1[10] = Cor_1[4,5]; 
  cor_1[11] = Cor_1[1,6]; 
  cor_1[12] = Cor_1[2,6]; 
  cor_1[13] = Cor_1[3,6]; 
  cor_1[14] = Cor_1[4,6]; 
  cor_1[15] = Cor_1[5,6]; 
  cor_1[16] = Cor_1[1,7]; 
  cor_1[17] = Cor_1[2,7]; 
  cor_1[18] = Cor_1[3,7]; 
  cor_1[19] = Cor_1[4,7]; 
  cor_1[20] = Cor_1[5,7]; 
  cor_1[21] = Cor_1[6,7]; 
  cor_1[22] = Cor_1[1,8]; 
  cor_1[23] = Cor_1[2,8]; 
  cor_1[24] = Cor_1[3,8]; 
  cor_1[25] = Cor_1[4,8]; 
  cor_1[26] = Cor_1[5,8]; 
  cor_1[27] = Cor_1[6,8]; 
  cor_1[28] = Cor_1[7,8]; 
  cor_1[29] = Cor_1[1,9]; 
  cor_1[30] = Cor_1[2,9]; 
  cor_1[31] = Cor_1[3,9]; 
  cor_1[32] = Cor_1[4,9]; 
  cor_1[33] = Cor_1[5,9]; 
  cor_1[34] = Cor_1[6,9]; 
  cor_1[35] = Cor_1[7,9]; 
  cor_1[36] = Cor_1[8,9]; 
  cor_1[37] = Cor_1[1,10]; 
  cor_1[38] = Cor_1[2,10]; 
  cor_1[39] = Cor_1[3,10]; 
  cor_1[40] = Cor_1[4,10]; 
  cor_1[41] = Cor_1[5,10]; 
  cor_1[42] = Cor_1[6,10]; 
  cor_1[43] = Cor_1[7,10]; 
  cor_1[44] = Cor_1[8,10]; 
  cor_1[45] = Cor_1[9,10]; 
  cor_1[46] = Cor_1[1,11]; 
  cor_1[47] = Cor_1[2,11]; 
  cor_1[48] = Cor_1[3,11]; 
  cor_1[49] = Cor_1[4,11]; 
  cor_1[50] = Cor_1[5,11]; 
  cor_1[51] = Cor_1[6,11]; 
  cor_1[52] = Cor_1[7,11]; 
  cor_1[53] = Cor_1[8,11]; 
  cor_1[54] = Cor_1[9,11]; 
  cor_1[55] = Cor_1[10,11]; 
} 