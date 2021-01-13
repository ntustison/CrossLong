data {
  int<lower=1>                                     numberOfIndividuals; 
  int<lower=1>                                     numberOfObservations;
  int<lower=1>                                     numberOfRegions;
  matrix[numberOfObservations, numberOfRegions]    scaledThickness;
  vector[numberOfObservations]                     timePoints;
  int ids[numberOfObservations];     
}

parameters {
  vector[numberOfRegions]             beta_1;    
  vector<lower=0>[numberOfRegions]    sigma; 
  vector[numberOfIndividuals]         alpha_0_intercept[numberOfRegions];    
  vector[numberOfRegions]             alpha_0;         
  vector<lower=0>[numberOfRegions]    tau_0;
}

model {
  vector[numberOfObservations]        alpha_0_intercept_s[numberOfRegions];
  
  alpha_0 ~ normal( 0, 10 );
  tau_0   ~ cauchy( 0,  5 );
  sigma   ~ cauchy( 0,  5 ); 
  beta_1  ~ normal( 0, 10 );
  
  for( i in 1:numberOfRegions )
    {
    alpha_0_intercept[i] ~ normal( alpha_0[i], tau_0[i] );
    for( j in 1:numberOfObservations )
      {
      alpha_0_intercept_s[i][j] = alpha_0_intercept[i][ids[j]];
      }
    }
  
  for( i in 1:numberOfRegions ) 
    { 
    col( scaledThickness, i ) ~ normal( alpha_0_intercept_s[i] + beta_1[i] * timePoints, sigma[i] );
    }
}

generated quantities {
  vector[numberOfRegions] var_ratio;
  var_ratio = tau_0 ./ sigma;
}
