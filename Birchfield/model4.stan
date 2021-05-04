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
  vector[2]              alpha;
  vector[2]              alpha_intercept_cn[Ni];
  vector[2]              alpha_intercept_lmci[Ni];
  vector[2]              alpha_intercept_ad[Ni];
  cov_matrix[2]          Sigma_cn;
  cov_matrix[2]          Sigma_lmci;
  cov_matrix[2]          Sigma_ad;
  real                   beta_lmci;
  real                   beta_ad;
  real                   beta_lmci_t;
  real                   beta_ad_t;
  vector<lower=0>[Nk]    tau;
  vector<lower=0>[Nk]    nu;
  vector[Nij]            Z;

}

model{
  
  vector[2] alpha_intercept_s[Nij];
  
  alpha ~ multi_normal( [6.4, 0.6], [[1, 0], [0, 1]] ); 
  
  Sigma_cn ~ inv_wishart( 3, [[1,0], [0,1]] );
  Sigma_lmci ~ inv_wishart( 3, [[1,0], [0,1]] );  
  Sigma_ad ~ inv_wishart( 3, [[1,0], [0,1]] );
  
  alpha_intercept_cn ~ multi_normal(alpha, Sigma_cn);
  alpha_intercept_lmci ~ multi_normal(alpha, Sigma_lmci);  
  alpha_intercept_ad ~ multi_normal(alpha, Sigma_ad);  

  beta_lmci ~ normal(0, 1.5); 
  beta_ad ~ normal(0, 1.5); 
  beta_lmci_t ~ normal(0, 0.5); 
  beta_ad_t ~ normal(0, 0.5); 

  sigma ~ normal(0,  1);
  tau ~ normal(0, 1); 
  nu ~ exponential(1.0/30.0);
  
  for(ij in 1:Nij){
    alpha_intercept_s[ij] = 
      alpha_intercept_cn[ids[ij]]*(1-LMCI[ij])*(1-AD[ij]) + 
      alpha_intercept_lmci[ids[ij]]*LMCI[ij] + 
      alpha_intercept_ad[ids[ij]]*AD[ij];
  }

  Z ~ normal(
    alpha_intercept_s[1]  # this is alpha_0_intercept_s
    + alpha_intercept_s[2] .* timePoints  # this is alpha_1_intercept_s
    + beta_lmci * LMCI  
    + beta_lmci_t * LMCI .* timePoints
    + beta_ad * AD  
    + beta_ad_t * AD .* timePoints, 
    sigma
  );

// likelihood

  for(k in 1:Nk){ // for each pipeline k
    col(Y, k) ~ student_t(nu[k], Z, tau[k]);
  }

} // end of model

generated quantities {}
