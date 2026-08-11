data {
  int<lower=1>              Ni; // number of individuals
  int<lower=1>              Nij; // number of observations, total
  int<lower=1>              Nk; // number of pipelines
  matrix[Nij, Nk]           Y; // design matrix for each region
  vector[Nij]               timePoints; // time points
  int<lower=1>              ids[Nij]; // 1-d array of each individual id
  vector[Nij]               LMCI;
  vector[Nij]               AD;
  vector[Nij]               AGE;
}

parameters {
  real<lower=0>          sigma;
  real                   alpha_0;
  vector[Ni]             alpha_0_intercept;
  real<lower=0>          lambda_0;
  real                   beta_lmci;
  real                   beta_ad;
  real                   beta_CT;
  real                   beta_ad_t;
  vector[Nk-1]           bias;
  vector<lower=0>[Nk]    tau;
  vector<lower=0>[Nk]    nu;
  vector[Nij]            Z;
}

model{

  alpha_0 ~ normal(70, 5);
  lambda_0 ~ normal(0, 10);

  beta_lmci ~ normal(0, 10); 
  beta_ad ~ normal(0, 10); 


  sigma ~ normal(0,  1);
  tau ~ normal(0, 1); 
  nu ~ exponential(1.0/30.0);
  
  alpha_0_intercept ~ normal(alpha_0, lambda_0); 

  AGE ~ normal(
    alpha_0_intercept[ids]  
    + beta_lmci * LMCI  
    + beta_ad * AD  
    + beta_CT * Z, 
    sigma
  );

// likelihood

  bias ~ normal(0, 0.5);

  for(k in 1:Nk-1){
    col(Y, k) ~ student_t(nu[k], Z + bias[k], tau[k]);
  }
  col(Y, Nk) ~ student_t(nu[Nk], Z, tau[Nk]);

} // end of model

generated quantities {}
