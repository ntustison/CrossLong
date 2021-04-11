library(tidyverse)
library(rstan) 
# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- 'C:/Users/Admin/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd(BirchfieldDirectory)

corticalThicknessData <- readRDS("corticalThicknessData.rds", refhook=NULL)

stanModelFile <- paste0(BirchfieldDirectory, 'model1.stan')

Ni <- length( unique( corticalThicknessData[[1]]$ID ) ) # number of unique IDs
Nij <- nrow( corticalThicknessData[[1]] ) # number of total observations
LMCI <- corticalThicknessData[[1]]$LMCI
AD <- corticalThicknessData[[1]]$AD
Nk <- length( corticalThicknessData )
Y <- matrix(NA, nrow=Nij, ncol=Nk)
for(k in 1:Nk){
  Y[, k] <- corticalThicknessData[[k]]$thickness.sum
}
timePoints <- corticalThicknessData[[1]]$VISIT
ids <- as.numeric( as.factor( corticalThicknessData[[1]]$ID ) ) # gives each unique ID an integer from 1 to 663
stanData <- list(
  Ni=Ni,
  Nij=Nij,
  Nk=Nk,
  Y=Y,
  timePoints=timePoints,
  ids=ids,
  LMCI=LMCI,
  AD=AD )

set.seed(1)
model1fit <- stan(
  file=stanModelFile,
  data=stanData,
  cores=16L,
  verbose=TRUE,
  iter=1200,
  chains=1,
  warmup=200 )

saveRDS(model1fit, "model1fit1200.rds")

summary(model1fit)[[1]]
