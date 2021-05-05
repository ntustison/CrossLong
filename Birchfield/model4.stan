data {
  int<lower=1>            Ni; // number of individuals
  int<lower=1>            Nij; // number of observations, total
  int<lower=1>            Nk; // number of pipelines
  int<lower=1>            cnLength;
  int<lower=1>            lmciLength;
  int<lower=1>            adLength;
  matrix[Nij, Nk]         Y; // design matrix for each region
  vector[Nij]             timePoints; // time points
  int<lower=1>            groupID[Nij];
  vector[Nij]             LMCI;
  vector[Nij]             AD;
}

parameters {
  real<lower=0>          sigma;
  vector[2]              alpha;
  vector[2]              alphaInterceptCN[cnLength];
  vector[2]              alphaInterceptLMCI[lmciLength];
  vector[2]              alphaInterceptAD[adLength];
  cov_matrix[2]          SigmaCN;
  cov_matrix[2]          SigmaLMCI;
  cov_matrix[2]          SigmaAD;
  real                   betaLMCI;
  real                   betaAD;
  real                   betaLMCI_t;
  real                   betaAD_t;
  vector<lower=0>[Nk]    tau;
  vector<lower=0>[Nk]    nu;
  vector[Nij]            Z;

}

model{
  
  vector[2] alphaIntercept_s[Nij];
  
  alpha ~ multi_normal( [6.4, 0], [[.6, 0], [0, .2]] ); 
  
  SigmaCN ~ inv_wishart( 3, [[1,0], [0,1]] );
  SigmaLMCI ~ inv_wishart( 3, [[1,0], [0,1]] );  
  SigmaAD ~ inv_wishart( 3, [[1,0], [0,1]] );
  
  alphaInterceptCN ~ multi_normal( alpha, SigmaCN );
  alphaInterceptLMCI ~ multi_normal( alpha, SigmaLMCI );  
  alphaInterceptAD ~ multi_normal( alpha, SigmaAD );  

  betaLMCI ~ normal( 0, 1.5 ); 
  betaAD ~ normal( 0, 1.5 ); 
  betaLMCI_t ~ normal( 0, 0.5 ); 
  betaAD_t ~ normal( 0, 0.5 ); 

  sigma ~ normal( 0,  1 );
  tau ~ normal( 0, 1 ); 
  nu ~ exponential( 1.0/30.0 );
  
  for( ij in 1:Nij ){
    if( LMCI[ij] == 1 ){
      alphaIntercept_s[ij] = alphaInterceptLMCI[groupID[ij]]; 
    } else if( AD[ij] == 1 ){
      alphaIntercept_s[ij] = alphaInterceptAD[groupID[ij]]; 
    } else {
      alphaIntercept_s[ij] = alphaInterceptCN[groupID[ij]]; 
    }
  }

  // Z ~ normal(
  //   alphaIntercept_s[1]  // formerly alpha_0_intercept_s
  //   + alphaIntercept_s[2] .* timePoints  // formerly alpha_1_intercept_s
  //   + betaLMCI * LMCI  
  //   + betaLMCI_t * LMCI .* timePoints
  //   + betaAD * AD  
  //   + betaAD_t * AD .* timePoints, 
  //   sigma
  // );
  
  for(ij in 1:Nij){
    Z[ij] ~ normal(
      alphaIntercept_s[ij][1]
      + alphaIntercept_s[ij][2] * timePoints[ij]
      + betaLMCI * LMCI[ij]
      + betaLMCI_t * LMCI[ij] * timePoints[ij]
      + betaAD * AD[ij]
      + betaAD_t * AD[ij] * timePoints[ij],
      sigma
    );
  }

  // likelihood

  for(k in 1:Nk){ // for each pipeline k
    col(Y, k) ~ student_t(nu[k], Z, tau[k]);
  }

} // end of model

generated quantities {}
