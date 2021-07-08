library(tidyverse)
library(rstan) 
library(gt)

# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- "~/CrossLong/Birchfield/"#'C:/Users/Admin/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd(BirchfieldDirectory)

corticalThicknessData <- readRDS("corticalThicknessData.rds", refhook=NULL)

stanModelFile <- paste0(BirchfieldDirectory, 'measurementErrorModel.stan')

Ni <- length( unique( corticalThicknessData[[1]]$ID ) ) # number of unique IDs
Nij <- nrow( corticalThicknessData[[1]] ) # number of total observations
LMCI <- corticalThicknessData[[1]]$MCI
AD <- corticalThicknessData[[1]]$AD
Nk <- length( corticalThicknessData )
Y <- matrix(NA, nrow=Nij, ncol=Nk)
for(k in 1:Nk){
  Y[, k] <- corticalThicknessData[[k]]$THICKNESS_SUM
}
timePoints <- corticalThicknessData[[1]]$DAYS
ids <- as.numeric( as.factor( corticalThicknessData[[1]]$ID ) ) # gives each unique ID an integer from 1 to 663
stanData <- list( # specifies the data for the model
  Ni=Ni,
  Nij=Nij,
  Nk=Nk,
  Y=Y,
  timePoints=timePoints,
  ids=ids,
  LMCI=LMCI,
  AD=AD,
  AGE=corticalThicknessData[[1]]$AGE)

set.seed(1)
model3Fit <- stan(
  file=stanModelFile,
  data=stanData,
  cores=1,
  verbose=TRUE,
  iter=2000,
  chains=1,
  warmup=1000)

saveRDS(model3Fit, "model3Fit.rds")