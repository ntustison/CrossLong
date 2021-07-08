data {
  int<lower=1>      Ni; // number of individuals
  int<lower=1>      Nij; // number of total observations
  int<lower=1>      Nk; // number of pipelines
  int<lower=1>      ID[Nij]; // 1-d array of each individual id
  vector[Nij]       INITIAL_AGE; // age at baseline
  vector[Nij]       MALE; // indicator
  vector[Nij]       MCI; // indicator
  vector[Nij]       AD; // indicator
  vector[Nij]       YEARS; // visit date minus first visit date
  matrix[Nij, Nk]   Y; // outcome (ERC thickness sum) matrix; rows are observations, columns are pipelines
}

parameters {
  real<lower=0>          sigma;
  real                   alpha_0;
  real                   alpha_1;
  vector[Ni]             alpha_0_subject;
  vector[Ni]             alpha_1_subject;
  real<lower=0>          lambda_0;
  real<lower=0>          lambda_1;
  real                   beta_mci;
  real                   beta_ad;
  real                   beta_age;
  real                   beta_male;
  real                   beta_mci_t;
  real                   beta_ad_t;
  vector<lower=0>[Nk]    tau;
  vector<lower=0>[Nk]    nu;
  vector[Nij]            W;
}

model{

  alpha_0 ~ normal(6.4, 0.6);
  alpha_1 ~ normal(0, 0.2);
  lambda_0 ~ normal(0, 1);
  lambda_1 ~ normal(0, 0.1);
  
  beta_mci ~ normal(0, 1.5); 
  beta_ad ~ normal(0, 1.5); 
  beta_mci_t ~ normal(0, 0.5); 
  beta_ad_t ~ normal(0, 0.5); 

  sigma ~ normal(0, 1);
  tau ~ normal(0, 1); 
  nu ~ exponential(1.0/30.0);
  
  alpha_0_subject ~ normal(alpha_0, lambda_0); 
  alpha_1_subject ~ normal(alpha_1, lambda_1);

  W ~ normal(
      ( alpha_0_subject[ID] + beta_mci * MCI + beta_ad * AD + beta_age * INITIAL_AGE + beta_male * MALE ) 
    + ( alpha_1_subject[ID] + beta_mci_t * MCI + beta_ad_t * AD ) .* YEARS,
    sigma
  );

// likelihood

  for(k in 1:Nk){ // for each pipeline k
    col(Y, k) ~ student_t(nu[k], W, tau[k]);
  }

} // end of model
