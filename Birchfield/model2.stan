data {
  int<lower=1>              Ni; // number of individuals
  int<lower=1>              Nij; // number of observations, total
  int<lower=1>              Nk; // number of pipelines
  matrix[Nij, Nk]           Y; // design matrix for each region
  vector[Nij]               timePoints; // time points
  int<lower=1>              ids[Nij]; // 1-d array of each individual id
  vector[Nij]               LMCI;
  vector[Nij]               AD;
}

parameters {
  real<lower=0>          sigma;
  real                   alpha_0;
  real                   alpha_1;
  vector[Ni]             alpha_0_intercept;
  vector[Ni]             alpha_1_intercept;
  real<lower=0>          lambda_0;
  real<lower=0>          lambda_1;
  real                   beta_lmci;
  real                   beta_ad;
  real                   beta_lmci_t;
  real                   beta_ad_t;
  vector<lower=0>[Nk]    tau;
  vector[Nij]            Z;
}

model{
  
  vector[Nij]  alpha_0_intercept_s;
  vector[Nij]  alpha_1_intercept_s;
  
  int  counter;

  alpha_0 ~ normal(6.4, 0.6);
  alpha_1 ~ normal(0, 0.2);
  lambda_0 ~ cauchy(0, 1);
  lambda_1 ~ cauchy(0, 0.1);
  
  beta_lmci ~ normal(0, 1.5); // AH: this seems tight
  beta_ad ~ normal(0, 1.5); // AH: this seems tight
  beta_lmci_t ~ normal(0, 10); // vague, maybe change later
  beta_ad_t ~ normal(0, 10); // vague, maybe change later

  sigma ~ cauchy(0,  1.2); // AH: this seems tight, maybe 5?
  tau ~ cauchy(0, 1); // ad hoc, maybe change later
  
  counter = 1;
  
  alpha_0_intercept ~ normal(alpha_0, lambda_0); 
  alpha_1_intercept ~ normal(alpha_1, lambda_1); 
  for(ij in 1:Nij){
    alpha_0_intercept_s[ij] = alpha_0_intercept[ids[ij]]; // AH: this seems strange. should it be ids[ij] or counter?
    alpha_1_intercept_s[ij] = alpha_1_intercept[counter]; // AH: this seems strange. should it be ids[ij] or counter?
    if(ij != Nij && ids[ij] != ids[ij+1]) { counter = counter + 1; }
  }

  Z ~ normal(
    alpha_0_intercept_s 
    + alpha_1_intercept_s .* timePoints 
    + beta_lmci * LMCI  
    + beta_lmci_t * LMCI .* timePoints
    + beta_ad * AD  
    + beta_ad_t * AD .* timePoints, 
    sigma
  );

// likelihood

  for(k in 1:Nk){ // for each pipeline k
    col(Y, k) ~ normal(Z, tau[k]);
  }

} // end of model

generated quantities {}