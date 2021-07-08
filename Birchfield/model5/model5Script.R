library(tidyverse)
library(rstan) 

# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- 'D:/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd( paste0(BirchfieldDirectory, 'model5') )

corticalThicknessData <- readRDS("../corticalThicknessData.rds", refhook=NULL)

stanModelFile <- paste0(getwd(), '/model5.stan')

Ni <- length(unique(corticalThicknessData[[1]]$ID)) # number of unique IDs
Nij <- nrow(corticalThicknessData[[1]]) # number of total observations
Nk <- length(corticalThicknessData) # number of pipelines
ID <- corticalThicknessData[[1]]$ID # id from 1-663
YEARS <- corticalThicknessData[[1]]$YEARS # days since initial visit
MCI <- corticalThicknessData[[1]]$MCI # MCI indicator
AD <- corticalThicknessData[[1]]$AD # AD indicator

Y <- matrix(NA, nrow=Nij, ncol=Nk)
for(k in 1:Nk){
  Y[, k] <- corticalThicknessData[[k]]$THICKNESS_SUM
}

stanData <- list( 
  Ni=Ni,
  Nij=Nij,
  Nk=Nk,
  ID=ID,
  YEARS=YEARS,
  MCI=MCI,
  AD=AD,
  Y=Y
)

startTime <- proc.time()
model5Fit <- stan(
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

saveRDS(model5Fit, "model5Fit.rds")