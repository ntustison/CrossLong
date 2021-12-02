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

transformed data {
  matrix[N, 6] X;
  
  X[,1] = MCI;
  X[,2] = AD;
  X[,3] = INITIAL_AGE;
  X[,4] = MALE;
  X[,5] = MCI .* YEARS;
  X[,6] = AD .* YEARS;
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
  // This looks like expensive I/O
  vector[K] q[N];
  vector[K] sqrtinvq_tau[N];

  for(n in 1:N){
    q[n] = chisqnu[n] ./ nu;
    sqrtinvq_tau[n] = tau ./ sqrt(q[n]);
  }
      
}

model{
  
  // priors
  // My suspicion is these should be non-centered but I'll leave that alone
  // since just trying to make code fast
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
  
  // This might need to go up at top of block. In earlier Stans variable
  // definitions had to go first but I don't think that's a requirement anymore.
  vector[6] beta = [
    beta_mci,
    beta_ad,
    beta_age,
    beta_male,
    beta_mci_t,
    beta_ad_t
  ]';
  
  // This does some optimizations courtsey the X * b term to make the autodiff tree smaller
  MM_SCORE ~ normal_id_glm(X, beta_ct * CT + alpha_0_subject[ID] + alpha_1_subject[ID] .* YEARS, beta, sigma);
  
  for(n in 1:N){
    chisqnu[n] ~ chi_square(nu);
    // Diag * L * L' Diag -- if L was cholesky factor, Diag * L will be too I think
    // I moved Y_array out of transformed parameters and just put it here?
    // I'm pretty sure this won't count for anything, but you could move it
    // back and check.
    Y[n] ~ multi_normal_cholesky(CT[n] + offset, diag_pre_multiply(sqrtinvq_tau[n], L_Omega));
  }
  
}

generated quantities {
  vector[K] Y_array[N];
  matrix[K, K] Omega = L_Omega * L_Omega';
  for(n in 1:N) {
    Y_array[n] = CT[n] + offset;
  }
}

