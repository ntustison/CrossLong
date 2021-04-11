library(tidyverse)
library(rstan) 
# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- 'C:/Users/Admin/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd(BirchfieldDirectory)

corticalThicknessData <- readRDS("corticalThicknessData.rds", refhook=NULL)

stanModelFile <- paste0(BirchfieldDirectory, 'model2.stan')

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
stanData <- list( # specifies the data for the model
  Ni=Ni,
  Nij=Nij,
  Nk=Nk,
  Y=Y,
  timePoints=timePoints,
  ids=ids,
  LMCI=LMCI,
  AD=AD )

set.seed(1)
model2fit <- stan(
  file=stanModelFile,
  data=stanData,
  cores=4,
  verbose=TRUE,
  iter=21000,
  chains=4,
  warmup=1000)

saveRDS(fitStan2, "model2fit.rds")

# 5 min for 1200 samples