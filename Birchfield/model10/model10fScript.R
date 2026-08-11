modelFolder <- "model10"
modelName <- "model10f"

iter = 50000
warmup = 25000
thin = 2
chains = 1
cores = chains
seed = 12

library(tidyverse)
library(rstan) 
# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

# user-defined function
stringexec <- function(x){
  eval(parse(text=x), envir=globalenv())
}

BirchfieldDirectory <- 'D:/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd( paste0(BirchfieldDirectory, modelFolder) )
corticalThicknessData <- readRDS("../model6/model6Data.rds", refhook=NULL)
stanModelFile <- paste0(getwd(), '/', modelName,'.stan')

I <- length(unique(corticalThicknessData[[1]]$ID)) # number of unique IDs
N <- nrow(corticalThicknessData[[1]]) # number of total observations
K <- length(corticalThicknessData) # number of pipelines
ID <- corticalThicknessData[[1]]$ID # id from 1-663
YEARS <- corticalThicknessData[[1]]$YEARS # years since initial visit
INITIAL_AGE <- corticalThicknessData[[1]]$INITIAL_AGE # age at first visit
MALE <- corticalThicknessData[[1]]$MALE # male indicator
MCI <- corticalThicknessData[[1]]$MCI # MCI indicator
AD <- corticalThicknessData[[1]]$AD # AD indicator
MM_SCORE <- corticalThicknessData[[1]]$MM_SCORE # MM score (integer)

Y <- matrix(NA, nrow=N, ncol=K)
for(k in 1:K){
  Y[, k] <- corticalThicknessData[[k]]$THICKNESS_SUM
}

rm(corticalThicknessData)

stanData <- list( 
  I = I,
  N = N,
  K = K,
  ID = ID,
  YEARS = YEARS,
  INITIAL_AGE = INITIAL_AGE,
  MALE = MALE,
  MCI = MCI,
  AD = AD,
  MM_SCORE = MM_SCORE,
  Y=Y
)

runCommand <- paste0(
  modelName, 
  'Fit <- stan(
  seed = seed,
  file = stanModelFile,
  data = stanData,
  cores = cores,
  verbose = TRUE,
  iter = iter,
  chains = chains,
  warmup = warmup,
  thin=thin,
  control = list(max_treedepth = 20, adapt_delta = 0.99)
  )'
)
cat(runCommand)
stringexec(runCommand)

saveCommand <- paste0(
  'saveRDS(',
  modelName,
  'Fit, "',
  modelName,
  'Fit', 
  seed, 
  '.rds")'
) 
cat(saveCommand)
stringexec(saveCommand)
