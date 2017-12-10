library( rstan )

baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'data/' )

stanModelFile <- paste0( dataDirectory, 'stan_corticalThicknessModel.stan' )

corticalThicknessCsvs <- list()
corticalThicknessCsvs[[1]] <- paste0( dataDirectory, 'adniCrossSectionalAntsMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[2]] <- paste0( dataDirectory, 'adniLongitudinalAntsMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[3]] <- paste0( dataDirectory, 'adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[4]] <- paste0( dataDirectory, 'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[5]] <- paste0( dataDirectory, 'adniLongitudinalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[6]] <- paste0( dataDirectory, 'newLongitudinalThicknessANTsNativeSpace.csv' )

corticalThicknessPipelineNames <- c( 'ANTsCross', 'ANTsSST', 'ANTsNative', 'FSCross', 'FSLong', 'newANTsNative' )
numberOfRegions <- 62

##############################################################################
#
#

corticalThicknessData <- list()
intersectImageIds <- c()
for( i in 1:length( corticalThicknessCsvs ) )
  {
  corticalThicknessData[[i]] <- read.csv( corticalThicknessCsvs[[i]] )
  if( i == 1 )
    {
    intersectImageIds <- corticalThicknessData[[i]]$IMAGE_ID    
    }
  intersectImageIds <- intersect( corticalThicknessData[[i]]$IMAGE_ID, intersectImageIds )
  }

missingData <- c()
for( i in 1:length( corticalThicknessData ) )
  {
  corticalThicknessData[[i]] <- corticalThicknessData[[i]][which( corticalThicknessData[[i]]$IMAGE_ID %in% intersectImageIds ), ]
  corticalThicknessData[[i]]$IMAGE_ID <- factor( corticalThicknessData[[i]]$IMAGE_ID, levels = intersectImageIds )
  corticalThicknessData[[i]] <- corticalThicknessData[[i]][order( corticalThicknessData[[i]]$IMAGE_ID ), ]

  thicknessColumns <- ( ncol( corticalThicknessData[[i]] ) - numberOfRegions + 1 ):ncol( corticalThicknessData[[i]] )
  missingData <- append( missingData, which( rowSums ( is.na( as.matrix( corticalThicknessData[[i]][, thicknessColumns] ) ) ) > 0 ) )
  }
missingData <- unique( missingData )  

for( i in 1:length( corticalThicknessData ) )
  {
  corticalThicknessData[[i]] <- corticalThicknessData[[i]][-missingData, ]    
  }

timePoints <- corticalThicknessData[[1]]$VISIT
timePoints <- as.numeric( gsub( "[^\\d]+", "", timePoints, perl = TRUE ) )
timePoints[is.na( timePoints )] <- 0

for( i in 1:length( corticalThicknessData ) )
  {
  thicknessColumns <- ( ncol( corticalThicknessData[[i]] ) - numberOfRegions + 1 ):ncol( corticalThicknessData[[i]] )
  corticalThicknessData[[i]]$ID <- factor( corticalThicknessData[[i]]$ID )
  corticalThicknessData[[i]] <- data.frame( ID = corticalThicknessData[[i]]$ID, 
    IMAGE_ID = corticalThicknessData[[i]]$IMAGE_ID,
    VISIT = timePoints,
    corticalThicknessData[[i]][,thicknessColumns] )
  corticalThicknessData[[i]] <- 
    corticalThicknessData[[i]][order( corticalThicknessData[[i]]$ID, corticalThicknessData[[i]]$VISIT ),]  
  }

##############################################################################
#
#

i <- 1
# for( i in 1:length( corticalThicknessData ) )
#   {
  stanFile <- paste0( dataDirectory, 'stan_', corticalThicknessPipelineNames[i], '_data.R' )    
  cat( "stan_rdump:  ", stanFile, "\n" )
  thicknessColumns <- ( ncol( corticalThicknessData[[i]] ) - numberOfRegions + 1 ):ncol( corticalThicknessData[[i]] )
  scaledThickness <- scale( as.matrix( corticalThicknessData[[i]][, thicknessColumns] ) )

  numberOfIndividuals <- length( unique( corticalThicknessData[[i]]$ID ) )
  numberOfObservations <- nrow( corticalThicknessData[[i]] )
  
  ids <- as.numeric( as.factor( corticalThicknessData[[i]]$ID ) )

  stanData <- list( numberOfRegions, numberOfIndividuals, numberOfObservations, 
    timePoints, ids, scaledThickness ) 
  fitStan <- stan( file = stanModelFile, data = stanData )
  print( fitStan )
  # }  
