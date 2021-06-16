data {
  int<lower=1>              Ni; // number of individuals
  int<lower=1>              Nij; // number of observations, total
  int<lower=1>              Nk; // number of pipelines
  vector[Nk]               Y[Nij]; // design matrix for each region
  vector[Nij]               DAYS; // days since initial visit
  int<lower=1>              ID[Nij]; // 1-d array of each individual id
  vector[Nij]               MCI; // MCI indicator
  vector[Nij]               AD; // AD indicator
}


parameters {
  real<lower=0>            sigma;
  vector[Ni]               alpha_0_intercept;
  vector[Ni]               alpha_1_intercept;
  real                     alpha_0;
  real                     alpha_1;
  real<lower=0>            lambda_0;
  real<lower=0>            lambda_1;
  real                     beta_mci;
  real                     beta_ad;
  real                     beta_mci_t;
  real                     beta_ad_t;
  vector[Nij]              W;
  vector<lower=0>[Nk]      tau;
  cholesky_factor_corr[Nk] L_Omega;
}


transformed parameters {
  matrix[Nk, Nk] SIGMA;

  SIGMA = diag_pre_multiply(tau, L_Omega) * diag_pre_multiply(tau, L_Omega)';
  
}


model{
  
  alpha_0 ~ normal(7, 2);
  alpha_1 ~ normal(0, 2);
  lambda_0 ~ normal(0, 1);
  lambda_1 ~ normal(0, 0.1);
  
  beta_mci ~ normal(0, 1.5); 
  beta_mci_t ~ normal(0, 0.5); 
  beta_ad ~ normal(0, 1.5); 
  beta_ad_t ~ normal(0, 0.5); 
  
  sigma ~ exponential(1);
  L_Omega ~ lkj_corr_cholesky(2);
  tau ~ cauchy(0, 2.5);
  
  alpha_0_intercept ~ normal(alpha_0, lambda_0); 
  alpha_1_intercept ~ normal(alpha_1, lambda_1); 

  W ~ normal(
    (beta_mci * MCI + beta_ad * AD + alpha_0_intercept[ID]) +
    (beta_mci_t * MCI + beta_ad_t * AD + alpha_1_intercept[ID]) .* DAYS, 
    sigma
    ); 

// likelihood
  {
    vector[Nk] Warray[Nij];
    for(ij in 1:Nij){
      Warray[ij] = rep_vector(W[ij], 7);
    }
    Y ~ multi_normal(Warray, SIGMA);
  }

}

generated quantities {}
