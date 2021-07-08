library(tidyverse)
library(rstan) 

# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- 'D:/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd(BirchfieldDirectory)

corticalThicknessData <- readRDS("corticalThicknessData.rds", refhook=NULL)

stanModelFile <- paste0(BirchfieldDirectory, 'model3/model3.stan')

Ni <- length( unique( corticalThicknessData[[1]]$ID ) ) # number of unique IDs
Nij <- nrow( corticalThicknessData[[1]] ) # number of total observations
Nk <- length( corticalThicknessData ) # number of pipelines
ID <- corticalThicknessData[[1]]$ID
INITIAL_AGE <- corticalThicknessData[[1]]$INITIAL_AGE
MALE <- corticalThicknessData[[1]]$MALE
MCI <- corticalThicknessData[[1]]$MCI
AD <- corticalThicknessData[[1]]$AD
YEARS <- corticalThicknessData[[1]]$YEARS
MM_SCORE <- corticalThicknessData[[1]]$MM_SCORE

Y <- matrix(NA, nrow=Nij, ncol=Nk)
for(k in 1:Nk){
  Y[, k] <- corticalThicknessData[[k]]$THICKNESS_SUM
}


stanData <- list(
  Ni=Ni,
  Nij=Nij,
  Nk=Nk,
  Y=Y,
  YEARS=YEARS,
  INITIAL_AGE=INITIAL_AGE,
  MALE=MALE,
  ID=ID,
  MCI=MCI,
  AD=AD,
  MM_SCORE=MM_SCORE
  )

startTime <- proc.time()
model3Fit <- stan(
  seed=1,
  file=stanModelFile,
  data=stanData,
  cores=4,
  verbose=TRUE,
  iter=200,
  chains=1,
  warmup=100,
  algorithm='HMC')
endTime <- proc.time()
runTime <- endTime - startTime
runTime

saveRDS(model3Fit, "model3Fit.rds")
