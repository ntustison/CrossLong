data {
  
  int<lower=1>              Ni; // number of individuals
  int<lower=1>              Nij; // number of observations, total
  int<lower=1>              Nk; // number of pipelines
  int<lower=1>              ID[Nij]; // 1-d array of each individual id
  vector[Nij]               YEARS; // days since initial visit
  vector[Nij]               INITIAL_AGE; // age at first visit  
  vector[Nij]               MALE; // male indicator
  vector[Nij]               MM_SCORE; // score on cognitive test
  matrix[Nij, Nk]           Y; // observation of L + R ERC thickness
  
}

parameters {

  vector[Ni]               alpha_0_subject;
  vector[Ni]               alpha_1_subject;
  real                     alpha_0;
  real                     alpha_1;
  real<lower=0>            lambda_0;
  real<lower=0>            lambda_1;
  real                     beta_age;
  real                     beta_male;
  real                     beta_ct;
  real<lower=0>            sigma;
  vector[Nij]              CT;
  vector[Nk-1]             bias;
  vector<lower=0>[Nk]      nu;
  vector<lower=0>[Nk]      tau;
  
}

model{
  
  // priors
  
  alpha_0_subject ~ normal(alpha_0, lambda_0);
  alpha_1_subject ~ normal(alpha_1, lambda_1);
  
  alpha_0 ~ normal(20, 20);
  lambda_0 ~ normal(0, 10);
  alpha_1 ~ normal(0, 5);
  lambda_1 ~ normal(0, 10);

  beta_age ~ normal(0, 10);
  beta_male ~ normal(0, 10);
  beta_ct ~ normal(0, 10);
  
  sigma ~ normal(0, 1);
  
  bias ~ normal(0, 0.5);
  tau ~ normal(0, 1); 
  nu ~ exponential(1.0/30.0);

  // likelihood

  MM_SCORE ~ normal(
    alpha_0_subject[ID] 
    + alpha_1_subject[ID] .* YEARS
    + beta_age * INITIAL_AGE
    + beta_male * MALE
    + beta_ct * CT,
    sigma
  );
  
  for(k in 1:Nk-1)
    col(Y, k) ~ student_t(nu[k], CT + bias[k], tau[k]);
  col(Y, Nk) ~ student_t(nu[Nk], CT, tau[Nk]);

} // end of model