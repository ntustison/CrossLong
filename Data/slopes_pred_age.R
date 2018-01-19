# get ss_slopes

library(nlme)
library(ggplot2)
library(randomForest)

baseDir <- '/Users/ntustison/Data/Public/CrossLong/'

# get all rows featuring missing cortical thickness measurements
CombinedLong1 <- read.csv(paste0( baseDir, "data/adniCrossSectionalAntsMergeSubset_WithScr.csv") )
miss <- which(rowSums(is.na(as.matrix(CombinedLong1[,11:72]))) > 0)
CombinedLong2 <- read.csv(paste0( baseDir, "data/adniLongitudinalAntsMergeSubset_WithScr.csv") )
miss <- c(miss,which(rowSums(is.na(as.matrix(CombinedLong2[,11:72]))) > 0))
CombinedLong3 <- read.csv(paste0( baseDir, "data/adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv") )
miss <- c(miss,which(rowSums(is.na(as.matrix(CombinedLong3[,11:72]))) > 0))
# now freesurfer
CombinedLong4 <- read.csv(paste0( baseDir, "data/adniCrossSectionalFreeSurferMergeSubset_WithScr.csv") )
miss <- c(miss,which(rowSums(is.na(as.matrix(CombinedLong4[,11:72]))) > 0))
CombinedLong5 <- read.csv(paste0( baseDir, "data/adniLongitudinalFreeSurferMergeSubset_WithScr.csv") )
miss <- c(miss,which(rowSums(is.na(as.matrix(CombinedLong5[,11:72]))) > 0))

MSE <- c()
 
#
####
######## long2
####
#

CombinedLong <- read.csv(paste0( baseDir, "data/adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv") )
CombinedLong <- CombinedLong[-miss,] #remove rows with missings

#get months
months <- as.numeric(gsub("[^\\d]+", "",CombinedLong$VISIT , perl=TRUE))
months <- sapply(months, substring,first=1)
months <- as.numeric(months)
months[is.na(months)] <- 0
CombinedLong$months <- months

CombinedLong <- CombinedLong[order(CombinedLong$ID,CombinedLong$months),]

#number indivs
Ni <- length(unique(CombinedLong$ID))

Nk=62
Q <- as.matrix(CombinedLong[,11:72])

ss_slopes <- matrix(0,Ni,Nk)
for(m in 1:Nk){
  df            <- data.frame(Y=Q[,m],months=CombinedLong$months,
                              ID=CombinedLong$ID)
  fit           <- lme(Y ~ months, random = ~ months -1 |ID,
                       data = df)
  ss_slopes[,m] <- as.numeric(fit$coefficients$fixed[2]) +
    as.vector(fit$coefficients$random[[1]][,1])
}

ages <- CombinedLong$AGE[1]
for(i in 2:dim(CombinedLong)[1]){
  if(CombinedLong$ID[i]!=CombinedLong$ID[i-1]){
    ages <- c(ages,CombinedLong$AGE[i])
  }
}

rf <- randomForest(x=ss_slopes,y=ages,ntree = 100)
MSE <- c(MSE,rf$mse[100])


#
####
######## long1
####
#

CombinedLong <- read.csv(paste0( baseDir, "data/adniLongitudinalAntsMergeSubset_WithScr.csv") )
CombinedLong <- CombinedLong[-miss,] #remove rows with missings

#get months
months <- as.numeric(gsub("[^\\d]+", "",CombinedLong$VISIT , perl=TRUE))
months <- sapply(months, substring,first=1)
months <- as.numeric(months)
months[is.na(months)] <- 0
CombinedLong$months <- months

CombinedLong <- CombinedLong[order(CombinedLong$ID,CombinedLong$months),]

#number indivs
Ni <- length(unique(CombinedLong$ID))

Nk=62
Q <- as.matrix(CombinedLong[,11:72])

ss_slopes <- matrix(0,Ni,Nk)
for(m in 1:Nk){
  df            <- data.frame(Y=Q[,m],months=CombinedLong$months,
                              ID=CombinedLong$ID)
  fit           <- lme(Y ~ months, random = ~ months -1 |ID,
                       data = df)
  ss_slopes[,m] <- as.numeric(fit$coefficients$fixed[2]) +
    as.vector(fit$coefficients$random[[1]][,1])
}

ages <- CombinedLong$AGE[1]
for(i in 2:dim(CombinedLong)[1]){
  if(CombinedLong$ID[i]!=CombinedLong$ID[i-1]){
    ages <- c(ages,CombinedLong$AGE[i])
  }
}

rf <- randomForest(x=ss_slopes,y=ages,ntree = 100)
MSE <- c(MSE,rf$mse[100])

#
####
######## CS
####
#

CombinedLong <- read.csv(paste0( baseDir, "data/adniCrossSectionalAntsMergeSubset_WithScr.csv") )
CombinedLong <- CombinedLong[-miss,] #remove rows with missings

#get months
months <- as.numeric(gsub("[^\\d]+", "",CombinedLong$VISIT , perl=TRUE))
months <- sapply(months, substring,first=1)
months <- as.numeric(months)
months[is.na(months)] <- 0
CombinedLong$months <- months

CombinedLong <- CombinedLong[order(CombinedLong$ID,CombinedLong$months),]

#number indivs
Ni <- length(unique(CombinedLong$ID))

Nk=62
Q <- as.matrix(CombinedLong[,11:72])

ss_slopes <- matrix(0,Ni,Nk)
for(m in 1:Nk){
  df            <- data.frame(Y=Q[,m],months=CombinedLong$months,
                              ID=CombinedLong$ID)
  fit           <- lme(Y ~ months, random = ~ months -1 |ID,
                       data = df)
  ss_slopes[,m] <- as.numeric(fit$coefficients$fixed[2]) +
    as.vector(fit$coefficients$random[[1]][,1])
}

ages <- CombinedLong$AGE[1]
for(i in 2:dim(CombinedLong)[1]){
  if(CombinedLong$ID[i]!=CombinedLong$ID[i-1]){
    ages <- c(ages,CombinedLong$AGE[i])
  }
}

rf <- randomForest(x=ss_slopes,y=ages,ntree = 100)
MSE <- c(MSE,rf$mse[100])


#
####
######## FS long
####
#

CombinedLong <- read.csv(paste0( baseDir, "data/adniLongitudinalFreeSurferMergeSubset_WithScr.csv") )
CombinedLong <- CombinedLong[-miss,] #remove rows with missings

#get months
months <- as.numeric(gsub("[^\\d]+", "",CombinedLong$VISIT , perl=TRUE))
months <- sapply(months, substring,first=1)
months <- as.numeric(months)
months[is.na(months)] <- 0
CombinedLong$months <- months

CombinedLong <- CombinedLong[order(CombinedLong$ID,CombinedLong$months),]

#number indivs
Ni <- length(unique(CombinedLong$ID))

Nk=62
Q <- as.matrix(CombinedLong[,11:72])

ss_slopes <- matrix(0,Ni,Nk)
for(m in 1:Nk){
  df            <- data.frame(Y=Q[,m],months=CombinedLong$months,
                              ID=CombinedLong$ID)
  fit           <- lme(Y ~ months, random = ~ months -1 |ID,
                       data = df)
  ss_slopes[,m] <- as.numeric(fit$coefficients$fixed[2]) +
    as.vector(fit$coefficients$random[[1]][,1])
}

ages <- CombinedLong$AGE[1]
for(i in 2:dim(CombinedLong)[1]){
  if(CombinedLong$ID[i]!=CombinedLong$ID[i-1]){
    ages <- c(ages,CombinedLong$AGE[i])
  }
}

rf <- randomForest(x=ss_slopes,y=ages,ntree = 100)
MSE <- c(MSE,rf$mse[100])

#
####
######## FS CS
####
#

CombinedLong <- read.csv(paste0( baseDir, "data/adniCrossSectionalFreeSurferMergeSubset_WithScr.csv") )
CombinedLong <- CombinedLong[-miss,] #remove rows with missings

#get months
months <- as.numeric(gsub("[^\\d]+", "",CombinedLong$VISIT , perl=TRUE))
months <- sapply(months, substring,first=1)
months <- as.numeric(months)
months[is.na(months)] <- 0
CombinedLong$months <- months

CombinedLong <- CombinedLong[order(CombinedLong$ID,CombinedLong$months),]

#number indivs
Ni <- length(unique(CombinedLong$ID))

Nk=62
Q <- as.matrix(CombinedLong[,11:72])

ss_slopes <- matrix(0,Ni,Nk)
for(m in 1:Nk){
  df            <- data.frame(Y=Q[,m],months=CombinedLong$months,
                              ID=CombinedLong$ID)
  fit           <- lme(Y ~ months, random = ~ months -1 |ID,
                       data = df)
  ss_slopes[,m] <- as.numeric(fit$coefficients$fixed[2]) +
    as.vector(fit$coefficients$random[[1]][,1])
}

ages <- CombinedLong$AGE[1]
for(i in 2:dim(CombinedLong)[1]){
  if(CombinedLong$ID[i]!=CombinedLong$ID[i-1]){
    ages <- c(ages,CombinedLong$AGE[i])
  }
}

rf <- randomForest(x=ss_slopes,y=ages,ntree = 100)
MSE <- c(MSE,rf$mse[100])
MSE <- MSE/max(MSE)

#
#####
######### figure
#####
#

Methods <- c('ANTs Native','ANTs SST','ANTs Cross', 'FS Long', 'FS Cross')
dfgg <- data.frame(Methods,MSE)
gg <- ggplot(data=dfgg,aes(x=Methods,y=MSE,fill=Methods))+
  geom_bar(stat = 'identity') + scale_x_discrete(limits=Methods)+
  scale_fill_discrete(guide=FALSE) + ylab('Mean squared error')
gg

ggsave('pred_age',gg,device='pdf')


