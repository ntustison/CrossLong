These files based on 
  stan_plotResults_hierarchical.r
  stan_corticalThicknessModel.stan

data_manipulation.r
Creates an RDS object (list of 7 data frames) with outcome thickness.sum (sum of left ERC and right ERC), and time points in months after first visit.

model1.stan
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

model2.stan
A hierarchical model with single set of parameters different pipeline error tau[k] for each of the k=7 pipelines. 
Predictors are AD, LMCI, and interaction with time, as in model 1. 
Estimated variables:
 alpha_0_intercept[1:663]
 alpha_0
 alpha_1_intercept[1:663]
 alpha_1
 lambda_0
 lambda_1
 beta_lmci
 beta_ad
 beta_lmci_t
 beta_ad_t
 sigma
 tau[1:7]