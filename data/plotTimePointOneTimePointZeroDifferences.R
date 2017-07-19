library( ggplot2 )
library( reshape2 )

originalDataFiles <- c( 'adniCrossSectionalAntsMergeSubset_WithScr.csv',
                        'adniLongitudinalAntsMergeSubset_WithScr.csv',
                        'adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv',
                        'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv',
                        'adniLongitudinalFreeSurferMergeSubset_WithScr.csv'
                       )

pipelineTypes <- c( 'ANTsCrossSectional',
                    'ANTsSST',
                    'ANTsNative',
                    'FreeSurferCrossSectional',
                    'FreeSurferLongitudinal'
                  )

plotDir <- './'

dataList <- list()
for( i in 1:length( originalDataFiles ) )
  {
  cat( "Reading ", originalDataFiles[i], "\n" )
  dataList[[i]] <- read.csv( originalDataFiles[i] )

  timePointsString <- c( 'scr', 'bl', 'm06', 'm12', 'm18', 'm24', 'm36' )
  timePointsNumeric <- c( 0, 0, 6, 12, 18, 24, 36 )

  numericVisit <- rep( 'NA', length( dataList[[i]]$VISIT ) )
  for( m in 1:length( timePointsString ) )
    {
    numericVisit[dataList[[i]]$VISIT == timePointsString[m]] <- timePointsNumeric[m]
    }
  dataList[[i]]$VISIT <- as.numeric( numericVisit )
  }


thicknessColumns <- grep( "thickness", colnames( dataList[[1]] ) )
thicknessColumnNames <- gsub( 'thickness.', '', colnames( dataList[[1]][, thicknessColumns] ) )


roiThicknessList <- list()

uniqueIDs <- unique( dataList[[i]]$ID )
uniqueIDsIndices <- list()
for( k in 1:length( uniqueIDs ) )
  {
  uniqueIDsIndices[[k]] <- which( dataList[[i]]$ID == uniqueIDs[k] )
  }

count <- 1
pb <- txtProgressBar( min = 0, max = length( uniqueIDs ), style = 3 )
for( k in 1:length( uniqueIDs ) )
  {
  if( length( uniqueIDsIndices[[k]] ) > 1 )
    {
    for( i in 1:length( originalDataFiles ) )
      {
      subjectSpecificData <- dataList[[i]][uniqueIDsIndices[[k]], thicknessColumns]

      difference <- abs( subjectSpecificData[2,] - subjectSpecificData[1,] )

      pipelineDataFrame <- data.frame( PipelineType = rep( pipelineTypes[i], length( thicknessColumnNames ) ),
                                       Region = thicknessColumnNames,
                                       ThicknessDifference = as.numeric( difference )
                                     )
      roiThicknessList[[count]] <- pipelineDataFrame
      count <- count+1
#       roiThicknessDataFrame <- rbind( roiThicknessDataFrame, pipelineDataFrame )
      }
    }
  setTxtProgressBar( pb, k )
  }

roiThicknessDataFrame <- do.call( rbind, roiThicknessList )

roiThicknessPlot <- ggplot( data = roiThicknessDataFrame ) +
  geom_boxplot( aes( x = Region, y = ThicknessDifference, fill = PipelineType ), outlier.colour="black", outlier.size=1 ) +
  ggtitle( "Normalized absolute thickness difference between the first two time points" ) +
  scale_y_continuous( "Delta thickness (mm)" ) +
  theme( axis.text.x = element_text( angle = 60, hjust = 1 ) )
ggsave( paste0( plotDir, '/timePointOneTimePointZeroDifferences.pdf' ),
        roiThicknessPlot, width = 30, height = 8, unit = 'in' )

