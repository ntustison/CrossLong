library(tidyverse)
library(rstan) 

# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- 'D:/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd( paste0(BirchfieldDirectory, 'model6') )

corticalThicknessData <- readRDS("model6Data.rds", refhook=NULL)

stanModelFile <- paste0(getwd(), '/model6a.stan')

Ni <- length(unique(corticalThicknessData[[1]]$ID)) # number of unique IDs
Nij <- nrow(corticalThicknessData[[1]]) # number of total observations
Nk <- length(corticalThicknessData) # number of pipelines
ID <- corticalThicknessData[[1]]$ID # id from 1-663
YEARS <- corticalThicknessData[[1]]$YEARS # days since initial visit
INITIAL_AGE <- corticalThicknessData[[1]]$INITIAL_AGE
MALE <- corticalThicknessData[[1]]$MALE
MCI <- corticalThicknessData[[1]]$MCI # MCI indicator
AD <- corticalThicknessData[[1]]$AD # AD indicator
MM_SCORE <- corticalThicknessData[[1]]$MM_SCORE

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
  INITIAL_AGE = INITIAL_AGE,
  MALE = MALE,
  MCI=MCI,
  AD=AD,
  MM_SCORE = MM_SCORE,
  Y=Y
)

startTime <- proc.time()
model6aFit <- stan(
  seed=1,
  file=stanModelFile,
  data=stanData,
  cores=4,
  verbose=TRUE,
  iter=5000,
  chains=4,
  warmup=2000)
endTime <- proc.time()
runTime <- endTime - startTime
runTime

saveRDS(model6aFit, "model6aFit.rds")

# summary(model6aFit)[[1]] %>% View()

get_sampler_params(model6aFit)[[1]] %>% View()

summary(model6aFit)[[1]] %>% View()

str(model6aFit)
str(summary(model6aFit))

a <- model6aFit@stan_args
chains <- a %>% length()
iter <- a[[1]]$iter
warmup <- a[[1]]$warmup
thin <- a[[1]]$thin
runtime <- get_elapsed_time(model6aFit)
