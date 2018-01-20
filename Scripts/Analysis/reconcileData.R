library( ggplot2 )
library( reshape2 )

baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )

numberOfRegions <- 62

corticalThicknessPipelineNames <- c( 'FSCross', 'FSLong', 'ANTsCross', 'ANTsNative', 'ANTsSST' )

corticalThicknessCsvs <- list()
corticalThicknessCsvs[[1]] <- paste0( dataDirectory, 'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[2]] <- paste0( dataDirectory, 'adniLongitudinalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[3]] <- paste0( dataDirectory, 'newLongitudinalThicknessCrossSectionalANTs.csv' )
corticalThicknessCsvs[[4]] <- paste0( dataDirectory, 'newLongitudinalThicknessANTsNativeSpace.csv' )
corticalThicknessCsvs[[5]] <- paste0( dataDirectory, 'newLongitudinalThicknessANTsSST.csv' )

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

singleTimePointData <- c()
subjects <- unique( corticalThicknessData[[1]]$ID )
for( j in 1:length( subjects ) )
  {
  subject <- subjects[j]
  subjectData <- corticalThicknessData[[1]][which( corticalThicknessData[[1]]$ID == subject ),]
  if( length( subjectData$ID ) == 1 )
    {
    singleTimePointData <- append( singleTimePointData, subjects[j] )
    }
  }  

for( i in 1:length( corticalThicknessData ) )
  {
  for( j in 1:length( singleTimePointData ) )
    {
    corticalThicknessData[[i]] <- corticalThicknessData[[i]][-which( corticalThicknessData[[i]]$ID == singleTimePointData[j] ), ]    
    }
  }

timePoints <- corticalThicknessData[[1]]$VISIT
timePoints <- as.numeric( gsub( "[^\\d]+", "", timePoints, perl = TRUE ) )
timePoints[is.na( timePoints )] <- 0

diagnosis <- corticalThicknessData[[1]]$DIAGNOSIS

for( i in 1:length( corticalThicknessData ) )
  {
  thicknessColumns <- ( ncol( corticalThicknessData[[i]] ) - numberOfRegions + 1 ):ncol( corticalThicknessData[[i]] )
  corticalThicknessData[[i]]$ID <- factor( corticalThicknessData[[i]]$ID )
  corticalThicknessData[[i]] <- data.frame( ID = corticalThicknessData[[i]]$ID, 
    IMAGE_ID = corticalThicknessData[[i]]$IMAGE_ID,
    VISIT = timePoints,
    AGE = corticalThicknessData[[1]]$AGE,
    DIAGNOSIS = factor( corticalThicknessData[[1]]$DIAGNOSIS ), 
    corticalThicknessData[[i]][,thicknessColumns] )
  corticalThicknessData[[i]] <- 
    corticalThicknessData[[i]][order( corticalThicknessData[[i]]$ID, corticalThicknessData[[i]]$VISIT ),]  

  write.csv( corticalThicknessData[[i]], quote = FALSE, row.names = FALSE, 
             file = paste0( dataDirectory, "/reconciled_", corticalThicknessPipelineNames[i], ".csv" ) )  
  }





