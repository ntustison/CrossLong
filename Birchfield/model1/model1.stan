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
  vector<lower=0>[Nk]    sigma; 
  vector[Nk]             alpha_0;         
  vector[Nk]             alpha_1;         
  vector[Ni]             alpha_0_intercept[Nk];    
  vector[Ni]             alpha_1_intercept[Nk];    
  vector<lower=0>[Nk]    lambda_0;
  vector<lower=0>[Nk]    lambda_1;
  vector[Nk]             beta_lmci;
  vector[Nk]             beta_ad;
  vector[Nk]             beta_lmci_t;
  vector[Nk]             beta_ad_t; 
  
}

model{
  
  vector[Nij]  alpha_0_intercept_s[Nk];
  vector[Nij]  alpha_1_intercept_s[Nk];
  
  int  counter;

  alpha_0 ~ normal( 6.4, 0.6 );
  alpha_1 ~ normal( 0, 0.2 );
  beta_lmci ~ normal( 0, 1.5 );
  beta_ad ~ normal( 0, 1.5 );
  beta_lmci_t ~ normal(0, 10); // vague, maybe change later
  beta_ad_t ~ normal(0, 10); // vague, maybe change later
  lambda_0   ~ cauchy( 0,  1 );
  lambda_1   ~ cauchy( 0,  0.1 );
  sigma   ~ cauchy( 0,  1.2 ); 
  
  counter = 1;
  
  for(k in 1: Nk){   // for each pipeline k

    alpha_0_intercept[k] ~ normal( alpha_0[k], lambda_0[k] ); 
    // alpha_0 is Nk-vector, elements iid
    alpha_1_intercept[k] ~ normal( alpha_1[k], lambda_1[k] ); 
    // alpha_1 is Nk-vector, elements iid 
  
    for(ij in 1:Nij){   // for each observation ij
    
      alpha_0_intercept_s[k][ij] = alpha_0_intercept[k][ids[ij]]; 
      // every "observation intercept" alpha_0_intercept_s[ij] is 
      // the "individal intercept" alpha_0_intercept[ids[ij]], and
      // this is true for each pipeline k
      
      alpha_1_intercept_s[k][ij] = alpha_1_intercept[k][counter];
      // every "observation intercept" alpha_1_intercept_s[ij] is 
      // the "individal intercept" alpha_1_intercept[ids[ij]], and
      // this is true for each pipeline k
  
      if(ij != Nij && ids[ij] != ids[ij+1]) { counter = counter + 1; }
      // if this is not the very last individual in the dataset, and
      // if this is the last observation on this individual, increment counter
    
    } // end of loop over observations
    
    counter = 1; // reset counter
  
  } // end of loop over pipelines

// likelihood

  for(k in 1:Nk) { // for each pipeline k
    col(Y, k) ~ normal(
      alpha_0_intercept_s[k]  
      + alpha_1_intercept_s[k] .* timePoints 
      + beta_lmci[k] * LMCI  
      + beta_lmci_t[k] * LMCI .* timePoints
      + beta_ad[k] * AD  
      + beta_ad_t[k] * AD .* timePoints, 
      sigma[k] 
    );
  }

} // end of model

generated quantities {}