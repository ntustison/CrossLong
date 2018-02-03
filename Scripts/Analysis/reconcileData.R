library( ADNIMERGE )

baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
sandboxDirectory <- paste0( baseDirectory, 'Sandbox/' )
dataDirectory <- paste0( baseDirectory, 'Data/' )

numberOfRegions <- 62

corticalThicknessPipelineNames <- c( 'FSCross', 'FSLong', 'ANTsCross', 'ANTsNative', 'ANTsSST' )

corticalThicknessCsvs <- list()
corticalThicknessCsvs[[1]] <- paste0( sandboxDirectory, 'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[2]] <- paste0( sandboxDirectory, 'adniLongitudinalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[3]] <- paste0( sandboxDirectory, 'newLongitudinalThicknessCrossSectionalANTs.csv' )
corticalThicknessCsvs[[4]] <- paste0( sandboxDirectory, 'newLongitudinalThicknessANTsNativeSpace.csv' )
corticalThicknessCsvs[[5]] <- paste0( sandboxDirectory, 'newLongitudinalThicknessANTsSST.csv' )

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


cat( "Getting adnimerge Dx\n" )
pb <- txtProgressBar( min = 0, max = nrow( corticalThicknessData[[1]] ), style = 3 )

naIndices <- c()
adniMergeDx <- c()
adniMergeExamDate <- c()
for( i in 1:nrow( corticalThicknessData[[1]] ) )
  {
  visitCode <- corticalThicknessData[[1]]$VISIT[i]
  if( visitCode == 'scr' )
    {
    visitCode <- 'bl'  
    }
  index <- which( adnimerge$PTID == corticalThicknessData[[1]]$ID[i] & adnimerge$VISCODE == visitCode )

  adniMergeDx[i] <- 'NA'
  adniMergeExamDate[i] <- 'NA'
  if( length( index ) > 0 )
    {
    adniMergeDx[i] <- levels( adnimerge$DX.bl )[adnimerge$DX.bl[index]]
    adniMergeExamDate[i] <- as.character( adnimerge$EXAMDATE[index] )
    } else {
    naIndices <- append( naIndices, corticalThicknessData[[1]]$ID[i] ) 
    }
  setTxtProgressBar( pb, i )
  }

cat( "\n\nCreating new .csv files\n" )
for( i in 1:length( corticalThicknessData ) )
  {
  thicknessColumns <- ( ncol( corticalThicknessData[[i]] ) - numberOfRegions + 1 ):ncol( corticalThicknessData[[i]] )
  corticalThicknessData[[i]]$ID <- factor( corticalThicknessData[[i]]$ID )

  corticalThicknessData[[i]] <- data.frame( ID = corticalThicknessData[[i]]$ID, 
    IMAGE_ID = corticalThicknessData[[i]]$IMAGE_ID,
    VISIT = timePoints,
    EXAM_DATE = as.character( adniMergeExamDate ),
    AGE = corticalThicknessData[[1]]$AGE,
    SEX = corticalThicknessData[[1]]$SEX,
    MMSCORE = corticalThicknessData[[1]]$MMSCORE,
    DIAGNOSIS = factor( adniMergeDx ), 
    corticalThicknessData[[i]][,thicknessColumns] )
  }

for( i in 1:length( corticalThicknessData ) )
  {
  corticalThicknessData[[i]] <- 
    corticalThicknessData[[i]][order( corticalThicknessData[[i]]$ID, corticalThicknessData[[i]]$VISIT ),]  

  corticalThicknessData[[i]] <- corticalThicknessData[[i]][-which( corticalThicknessData[[i]]$ID == naIndices ),]
  write.csv( corticalThicknessData[[i]], quote = FALSE, row.names = FALSE, 
             file = paste0( dataDirectory, "/reconciled_", corticalThicknessPipelineNames[i], ".csv" ) )  
  }





