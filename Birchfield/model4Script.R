library(tidyverse)
library(rstan) 

# Since tidyverse contains extract(), specify rstan::extract() when needed

options(mc.cores = parallel::detectCores())

BirchfieldDirectory <- 'D:/Documents/_UCLA/Holbrook/CrossLong/Birchfield/'
setwd(BirchfieldDirectory)

corticalThicknessData <- readRDS("corticalThicknessData.rds", refhook=NULL)

stanModelFile <- paste0(BirchfieldDirectory, 'model4.stan')

## give IDs by group so that the model can assign different parameters by group

d <- corticalThicknessData[[1]]
cnID <- d %>% filter(LMCI==0 & AD==0) %>% pull(ID) %>% as.factor() %>% as.numeric()
lmciID <- d %>% filter(LMCI==1) %>% pull(ID) %>% as.factor() %>% as.numeric()
adID <- d %>% filter(AD==1) %>% pull(ID) %>% as.factor() %>% as.numeric()
groupID <- c(cnID, lmciID, adID)
cnLength <- length(cnID)
lmciLength <- length(lmciID)
adLength <- length(adID)
rm(d, cnID, lmciID, adID)

for(i in 1:length(corticalThicknessData)){
  corticalThicknessData[[i]] <- corticalThicknessData[[i]] %>% arrange(AD, LMCI)
  corticalThicknessData[[i]]$groupID <- groupID
}

##

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

stanData <- list( # specifies the data for the model
  Ni=Ni,
  Nij=Nij,
  Nk=Nk,
  cnLength=cnLength,
  lmciLength=lmciLength,
  adLength=adLength,
  Y=Y,
  timePoints=timePoints,
  groupID=groupID,
  LMCI=LMCI,
  AD=AD )

set.seed(1)
model4Fit <- stan(
  file=stanModelFile,
  data=stanData,
  cores=8,
  verbose=TRUE,
  iter=100,
  chains=1,
  warmup=10)

saveRDS(model4Fit, "model4Fit.rds")