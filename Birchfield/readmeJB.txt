These files based on 
  stan_plotResults_hierarchical.r
  stan_corticalThicknessModel.stan


data_manipulation.r
Creates an RDS object (list of 7 data frames) with outcome thickness.sum (sum of left ERC and right ERC), and time points in months after first visit.


The pipelines are numbered as
1  FSCross
2  FSLong
3  ANTsCross
4  ANTsNative
5  ANTsSST
6  ANTsXNetCross
7  ANTsXNetLong


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


model2Summary
8 chains, 40000 iterations each, of which first half were warmup.
R displayed the following warnings:
1: In system(paste(CXX, ARGS), ignore.stdout = TRUE, ignore.stderr = TRUE) :
  '-E' not found
2: There were 2789 divergent transitions after warmup. See
http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
to find out why this is a problem and how to eliminate them.
3: There were 157211 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
4: There were 8 chains where the estimated Bayesian Fraction of Missing Information was low. See
http://mc-stan.org/misc/warnings.html#bfmi-low
5: Examine the pairs() plot to diagnose sampling problems
6: The largest R-hat is 1.42, indicating chains have not mixed.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#r-hat
7: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#bulk-ess
8: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#tail-ess