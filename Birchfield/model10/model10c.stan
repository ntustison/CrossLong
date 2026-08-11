data {
  
  int<lower=1>       I; // number of individuals
  int<lower=1>       N; // number of observations, total
  int<lower=1>       K; // number of pipelines
  int<lower=1>       ID[N]; // 1-d array of each individual id
  vector[N]          YEARS; // days since initial visit
  vector[N]          INITIAL_AGE; // age at first visit  
  vector[N]          MALE; // male indicator
  vector[N]          MCI; // MCI indicator
  vector[N]          AD; // AD indicator
  vector[N]          MM_SCORE; // score on cognitive test
  matrix[N, K]       Y; // observation of L + R ERC thickness
  
}

parameters {
  
  vector[I]               alpha_0_subject;
  vector[I]               alpha_1_subject;
  real                    alpha_0;
  real                    alpha_1;
  real<lower=0>           lambda_0;
  real<lower=0>           lambda_1;
  real                    beta_mci;
  real                    beta_ad;
  real                    beta_age;
  real                    beta_male;
  real                    beta_ct;
  real                    beta_mci_t;
  real                    beta_ad_t;
  real<lower=0>           sigma;
  vector<lower=0>[N]      CT;
  vector[K]               offset;
  vector<lower=0>[K]      tau; // scale
  vector<lower=0>[K]      nu; // df
  cholesky_factor_corr[K] L_Omega; // Omega is CT error correlation matrix
  vector<lower=0>[K]      chisqnu[N];
  
}

transformed parameters {

  vector[K] Y_array[N]; 
  vector[K] q[N];
  vector[K] sqrtinvq_tau[N];

  for(n in 1:N){
    Y_array[n] = rep_vector(CT[n], 7) + offset;
    q[n] = chisqnu[n] ./ nu;
    sqrtinvq_tau[n] = tau ./ sqrt(q[n]);
  }
      
}

model{
  
  // priors
  
  alpha_0_subject ~ normal(alpha_0, lambda_0);
  alpha_1_subject ~ normal(alpha_1, lambda_1);
  
  alpha_0 ~ normal(15, 15);
  lambda_0 ~ normal(0, 10);
  alpha_1 ~ normal(0, 5);
  lambda_1 ~ normal(0, 10);
  
  beta_mci ~ normal(0, 10); 
  beta_ad ~ normal(0, 10); 
  beta_age ~ normal(0, 10);
  beta_male ~ normal(0, 10);
  beta_ct ~ normal(0, 10);
  beta_mci_t ~ normal(0, 10);
  beta_ad_t ~ normal(0, 10);
  sigma ~ normal(0, 1);
  
  CT ~ normal(7, 2);
  offset ~ normal(0, 3);
  tau ~ normal(0, 3); 
  nu ~ exponential(1.0/30.0);
  L_Omega ~ lkj_corr_cholesky(2);
  
  // likelihood
  
  MM_SCORE ~ normal(
    alpha_0_subject[ID] 
    + beta_mci * MCI 
    + beta_ad * AD 
    + beta_age * INITIAL_AGE 
    + beta_male * MALE 
    + beta_ct * CT
    + (alpha_1_subject[ID] 
       + beta_mci_t * MCI 
       + beta_ad_t * AD) .* YEARS,
    sigma
  );
  
  for(n in 1:N){
    chisqnu[n] ~ chi_square(nu);
    row(Y, n) ~ multi_normal(Y_array[n], 
      diag_pre_multiply(sqrtinvq_tau[n], L_Omega) * diag_pre_multiply(sqrtinvq_tau[n], L_Omega)');
  }
  
}

generated quantities {
  
  matrix[K, K] Omega;
  Omega = L_Omega * L_Omega';
    
}

