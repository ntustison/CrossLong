data {
  int<lower=1>              Ni;   // number of individuals 
  int<lower=1>              Nij;  // number of observations
  int<lower=1>              Nk;   // number of pipelines
  int<lower=1>              Na1;  // number of subjects with multiple observations
  
  matrix[Nij, Nk]           Y;          // design matrix for each region
  vector[Nij]               timePoints; // time point
  vector[Nij]               m;          // bool = 1 if multiple observations from individual i
  
  int<lower=1>              ids[Nij];       // vector of each individual id      
  int<lower=1>              slopeIds[Na1];  // each subject with multiple observations has own number in {1, ..., Na1}
  vector[Nij]               LMCI;
  vector[Nij]               AD;
}

parameters {
  vector<lower=0>[Nk]    sigma; 

  vector[Nk]             alpha_0;         
  vector[Nk]             alpha_1;         

  vector[Ni]             alpha_0_intercept[Nk];    
  vector[Na1]            alpha_1_intercept[Nk];    

  vector<lower=0>[Nk]    tau_0;
  vector<lower=0>[Nk]    tau_1;
  
  vector[Nk] beta_lmci; // added
  vector[Nk] beta_ad; // added
}

model {
  vector[Nij]        alpha_0_intercept_s[Nk];
  vector[Nij]        alpha_1_intercept_s[Nk];
  int                counter;
  int                firstIndivWithSlopeDone;

  counter = 1;
  firstIndivWithSlopeDone = 0;

  alpha_0 ~ normal( 0, 10 );
  alpha_1 ~ normal( 0, 10 );
  beta_lmci ~ normal( 0, 10 ); // added
  beta_ad ~ normal( 0, 10 ); // added
  tau_0   ~ cauchy( 0,  5 );
  tau_1   ~ cauchy( 0,  5 );
  sigma   ~ cauchy( 0,  5 ); 
  
  for( k in 1:Nk )
    {
    alpha_0_intercept[k] ~ normal( alpha_0[k], tau_0[k] );
    alpha_1_intercept[k] ~ normal( alpha_1[k], tau_1[k] );
    
    for( ij in 1:Nij )
      {
      alpha_0_intercept_s[k][ij] = alpha_0_intercept[k][ids[ij]];

      if( m[ij] == 1 )
        {
        if( firstIndivWithSlopeDone == 1 )
          {
          if( ids[ij] != ids[ij-1] )
            {
              counter = counter + 1;
            }
          }
        alpha_1_intercept_s[k][ij] = alpha_1_intercept[k][slopeIds[counter]];
        if( counter == 1 )
          {
          if( ids[ij] != ids[ij+1] )
            {
              firstIndivWithSlopeDone = 1;
            }
          }
        } else {
        alpha_1_intercept_s[k][ij] = 0;  
        }
      }
    counter = 1;
    firstIndivWithSlopeDone = 0;
    }
  
  for( k in 1:Nk ) 
    { 
  //col( Y, k ) ~ normal( alpha_0_intercept_s[k] + alpha_1_intercept_s[k] .* timePoints, sigma[k] );
    col( Y, k ) ~ normal( alpha_0_intercept_s[k] + alpha_1_intercept_s[k] .* timePoints + beta_lmci[k] * LMCI + beta_ad[k] * AD, sigma[k] );
    }
}

generated quantities {
  vector[Nk]  var_ratio;
  vector[Nk]  var_ratio_experimental;
  
  var_ratio = tau_0 ./ sigma;
  var_ratio_experimental = ( tau_0 + tau_1 ) ./ sigma;
}
