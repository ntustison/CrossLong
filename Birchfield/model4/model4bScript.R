library(tidyverse)
library(rstan) 

# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- 'D:/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd(BirchfieldDirectory)

corticalThicknessData <- readRDS("corticalThicknessData.rds", refhook=NULL)

stanModelFile <- paste0(BirchfieldDirectory, 'model4/model4b.stan')

Ni <- length(unique(corticalThicknessData[[1]]$ID)) # number of unique IDs
Nij <- nrow(corticalThicknessData[[1]]) # number of total observations
Nk <- length(corticalThicknessData) # number of pipelines
ID <- corticalThicknessData[[1]]$ID # id from 1-663
DAYS <- corticalThicknessData[[1]]$DAYS # days since initial visit
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
  DAYS=DAYS,
  MCI=MCI,
  AD=AD,
  Y=Y,
  identity=diag(Nk)
)

set.seed(1)
model4bFit <- stan(
  file=stanModelFile,
  data=stanData,
  cores=8,
  verbose=TRUE,
  iter=5000,
  chains=1,
  warmup=1000)

saveRDS(model4bFit, "model4/model4bFit.rds")


