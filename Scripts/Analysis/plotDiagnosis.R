library( ggplot2 )
library( reshape2 )

baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
plotDir <- paste0( dataDirectory, '/RegionalThicknessDiagnosticPlots/' )

corticalThicknessPipelineNames <- c( 'ANTsCross', 'ANTsNative', 'ANTsSST', 'FSCross', 'FSLong' )
numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

corticalThicknessCsvs <- list()
for( i in 1:length( corticalThicknessPipelineNames ) )
  {
  corticalThicknessCsvs[[i]] <- paste0( dataDirectory, 'reconciled_', corticalThicknessPipelineNames[i], '.csv' )
  }

dataList <- list()
for( i in 1:length( corticalThicknessCsvs ) )
  {
  cat( "Reading ", corticalThicknessCsvs[[i]], "\n" )
  dataList[[i]] <- read.csv( corticalThicknessCsvs[[i]] )
  }

thicknessColumns <- grep( "thickness", colnames( dataList[[1]] ) )
uniqueSubjectIds <- unique( dataList[[1]]$ID )

# Normalize to first time point

cat( "Renormalzing data to first time point ... \n" )
pb <- txtProgressBar( min = 0, max = length( uniqueSubjectIds ), style = 3 )

for( j in 1:length( uniqueSubjectIds ) )
  {
  for( i in 1:length( dataList ) )
    {
    dataListSubject <- dataList[[i]][which( dataList[[i]]$ID == uniqueSubjectIds[j] ),]
    dataListSubject <- dataListSubject[order( dataListSubject$VISIT ),]

    # if( nrow( dataListSubject ) > 1 )
    #   {
    #   meanThickness <- colMeans( dataListSubject[, thicknessColumns] )
    #   for( k in 2:nrow( dataListSubject ) )
    #     {
    #     dataListSubject$VISIT[k] <- dataListSubject$VISIT[k] - dataListSubject$VISIT[1]
    #     dataListSubject[k, thicknessColumns] <- dataListSubject[k, thicknessColumns] - meanThickness  
    #     }
    #   }
    # dataListSubject$VISIT[1] <- 0
    # dataListSubject[1, thicknessColumns] <- dataListSubject[1, thicknessColumns] - meanThickness  

    for( k in 2:nrow( dataListSubject ) )
      {
      dataListSubject$VISIT[k] <- dataListSubject$VISIT[k] - dataListSubject$VISIT[1]
      dataListSubject[k, thicknessColumns] <- dataListSubject[k, thicknessColumns] - dataListSubject[1, thicknessColumns]  
      }
    dataListSubject$VISIT[1] <- 0
    dataListSubject[1, thicknessColumns] <- 0

    dataList[[i]][which( dataList[[i]]$ID == uniqueSubjectIds[j] ),] <- dataListSubject  
    }
  setTxtProgressBar( pb, j )
  }

cat( "Done.\n" )
cat( "Making regional plots ... \n" )
pb <- txtProgressBar( min = 0, max = length( thicknessColumns ), style = 3 )

for( j in 1:length( thicknessColumns ) )
  {
  roiThicknessDataFrame <- data.frame( ID = factor(), Diagnosis = factor(), Visit = double(), Thickness = double(), PipelineType = factor()  )
  for( i in 1:length( dataList ) )
    {
    combinedDiagnosis <- dataList[[i]]$DIAGNOSIS

    pipelineDataFrame <- data.frame( ID = dataList[[i]]$ID,
                                     Diagnosis = combinedDiagnosis,
                                     Visit = dataList[[i]]$VISIT,
                                     Thickness = dataList[[i]][,thicknessColumns[j]],
                                     PipelineType = rep( corticalThicknessPipelineNames[i], length( nrow( dataList[[i]] ) ) )
                                   )
    roiThicknessDataFrame <- rbind( roiThicknessDataFrame, pipelineDataFrame )
    }

  roiThicknessPlot <- ggplot( data = roiThicknessDataFrame ) +
    # geom_point( aes( x = Visit, y = Thickness, group = ID, colour = Diagnosis ), alpha = 0.5, size = 0.1 ) +
    geom_smooth( aes( x = Visit, y = Thickness, colour = Diagnosis, fill = Diagnosis ), alpha = 0.5, size = 0, method = lm ) +
    facet_wrap( ~ PipelineType, ncol = 5, scales = "free" ) +
    ggtitle( colnames( dataList[[i]] )[thicknessColumns[j]] ) +
    scale_y_continuous( "Thickness change (mm)" ) +
    scale_x_continuous( "Time point" )
  ggsave( paste0( plotDir, '/', colnames( dataList[[i]] )[thicknessColumns[j]], '_byDiagnosis.pdf' ),
          roiThicknessPlot, width = 12, height = 3.5, unit = 'in' )

  setTxtProgressBar( pb, j )
  }
cat( "Done.\n" )


