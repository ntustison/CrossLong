NOTE: in the later models, I have not yet rethought the priors on the slopes after changing the timepoints from months to years.


NOTE: any of the .rmd files for summary displays may be out of date.


>> dataManipulation.r
Creates an RDS object called corticalThicknessData.rds.
This is a list of 7 data frames) with the following variables:
 ID [no longer study ID, now as integer from 1:663]
 VISIT_NUMBER [integer]
 DAYS [calculated as visit date minus first visit date]
 YEARS [calculated as DAYS/365]
 MCI [indicator]
 AD [indicator]
 INITIAL_AGE [calculated as first visit date minus birth date]
 MALE [indicator]
 MM_SCORE [integer]
 THICKNESS_SUM [left ERC thickness plus right ERC thickness]


The pipelines are numbered as
1  FSCross
2  FSLong
3  ANTsCross
4  ANTsNative
5  ANTsSST
6  ANTsXNetCross
7  ANTsXNetLong


>> model1.stan
A hierarchical model with different parameters for each of the k=7 pipelines.
Predictors are AD, LMCI, and interaction with time. 
Estimated variables:
 alpha_0_intercept[1:7][1:663]
 alpha_0[1:7]
 alpha_1_intercept[1:7][1:663]
 alpha_1[1:7]
 lambda_0[1:7]
 lambda_1[1:7]
 beta_lmci[1:7]
 beta_ad[1:7]
 beta_lmci_t[1:7]
 beta_ad_t[1:7]
 sigma[1:7]


>> model2.stan
Different pipeline error tau[k] for each of the k=7 pipelines. 
Z is a latent variable, modeling the true outcome.
Predictors of Z are AD, LMCI, and interaction with time, as in model 1. 
Z is the mean of each pipeline, and the (normally distributed) errors are tau[1:7].
Parameters of interest:
 alpha_0
 alpha_1
 lambda_0
 lambda_1
 beta_lmci
 beta_ad
 beta_lmci_t
 beta_ad_t
 sigma
 tau[1:7]
 Z


>> model3.stan
The latent variable is renamed W.
Time points are now YEARS (recalculated from raw data).
Predictors of W are now AD, MCI, INITIAL_AGE, and MALE.
Y (data) now has student t distribution with kurtosis (df) parameter nu. 
Prior is nu ~ Exp(1/30) (rate).
Parameters of interest:
 alpha_0
 alpha_1
 lambda_0
 lambda_1
 beta_mci [renamed]
 beta_ad
 beta_mci_t [renamed]
 beta_ad_t
 beta_age
 beta_male
 sigma
 tau[1:7]
 nu[1:7]
 W

>>model4.stan
This model does not include the covariates INITIAL_AGE and MALE.
Time points are still YEARS.
New parameters bias[1:7] ~ normal(0, 0.5).
Y still has student t distribution.
We tried to separate pipeline bias from pipeline error: the means of the Y[k] are now W + bias[k].
For identifiability, we forced bias[7] to be 0. Only relative biases are identifiable. 
W no longer represents "truth" as in model 3.
Parameters of interest:
 alpha_0
 alpha_1
 lambda_0
 lambda_1
 beta_mci
 beta_ad
 beta_mci_t
 beta_ad_t
 sigma
 bias[1:6]
 tau[1:7]
 nu[1:7]
 W

>>model5.stan
This model does not include the covariates INITIAL_AGE and MALE.
Time points are still YEARS.
We modeled the likelihood completely differently from model 4.
The columns of Y still have mean W. 
However, the rows are no longer student t but multivariate normal with covariance matrix SIGMA.
We factor SIGMA into Tau * L_Omega * L_Omega' * Tau, where 
  Tau is the diagonal matrix with taus (now representing normally distributed error scales) as diagonal elements, 
  L_Omega is the lower Cholesky factor of the correlation matrix Omega. 
The code for this is vectorized. 
Parameters of interest:
 alpha_0
 alpha_1
 lambda_0
 lambda_1
 beta_mci
 beta_ad
 beta_mci_t
 beta_ad_t
 sigma
 W
 tau[1:7]
 L_Omega
 Omega
 SIGMA

  