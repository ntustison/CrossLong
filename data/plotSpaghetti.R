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

plotDir <- './RegionalThicknessSpaghettiPlots/'

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

pb <- txtProgressBar( min = 0, max = length( thicknessColumns ), style = 3 )

for( j in 1:length( thicknessColumns ) )
  {
  roiThicknessDataFrame <- data.frame( ID = factor(), Diagnosis = factor(), Visit = double(), Thickness = double(), PipelineType = factor()  )
  for( i in 1:length( originalDataFiles ) )
    {
    combinedDiagnosis <- dataList[[i]]$DIAGNOSIS
#     levels( combinedDiagnosis )[levels( combinedDiagnosis ) == "Normal"] <- "Normal_MCI"
#     levels( combinedDiagnosis )[levels( combinedDiagnosis ) == "MCI"] <- "Normal_MCI"
#     levels( combinedDiagnosis )[levels( combinedDiagnosis ) == "LMCI"] <- "LMCI_AD"
#     levels( combinedDiagnosis )[levels( combinedDiagnosis ) == "AD"] <- "LMCI_AD"

    pipelineDataFrame <- data.frame( ID = dataList[[i]]$ID,
                                     Diagnosis = combinedDiagnosis,
                                     Visit = dataList[[i]]$VISIT,
                                     Thickness = dataList[[i]][,thicknessColumns[j]],
                                     PipelineType = rep( pipelineTypes[i], length( nrow( dataList[[i]] ) ) )
                                   )
    roiThicknessDataFrame <- rbind( roiThicknessDataFrame, pipelineDataFrame )
    }

  roiThicknessPlot <- ggplot( data = roiThicknessDataFrame ) +
    geom_line( aes( x = Visit, y = Thickness, group = ID, colour = Diagnosis ), alpha = 0.25, size = 0.25 ) +
    facet_wrap( ~ PipelineType ) +
    ggtitle( colnames( dataList[[i]] )[thicknessColumns[j]] ) +
    scale_y_continuous( "Thickness (mm)", limits = c( 0, 5.0 ) )
  ggsave( paste0( plotDir, '/', colnames( dataList[[i]] )[thicknessColumns[j]], '.pdf' ),
          roiThicknessPlot, width = 6, height = 4, unit = 'in' )

  setTxtProgressBar( pb, j )
  }

