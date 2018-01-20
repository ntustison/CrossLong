library( ggplot2 )
library( reshape2 )

baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
plotDir <- paste0( dataDirectory, '/RegionalThicknessSpaghettiPlots/' )

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

pb <- txtProgressBar( min = 0, max = length( thicknessColumns ), style = 3 )

for( j in 1:length( thicknessColumns ) )
  {
  roiThicknessDataFrame <- data.frame( ID = factor(), Diagnosis = factor(), Visit = double(), Thickness = double(), PipelineType = factor()  )
  for( i in 1:length( dataList ) )
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
                                     PipelineType = rep( corticalThicknessPipelineNames[i], length( nrow( dataList[[i]] ) ) )
                                   )
    roiThicknessDataFrame <- rbind( roiThicknessDataFrame, pipelineDataFrame )
    }

  roiThicknessPlot <- ggplot( data = roiThicknessDataFrame ) +
    geom_line( aes( x = Visit, y = Thickness, group = ID, colour = Diagnosis ), alpha = 0.75, size = 0.25 ) +
    facet_wrap( ~ PipelineType, ncol = 5 ) +
    ggtitle( colnames( dataList[[i]] )[thicknessColumns[j]] ) +
    scale_y_continuous( "Thickness (mm)", limits = c( 0, 5.0 ) ) +
    scale_x_continuous( "Visit (months)" )
  ggsave( paste0( plotDir, '/', colnames( dataList[[i]] )[thicknessColumns[j]], '.pdf' ),
          roiThicknessPlot, width = 6, height = 1.75, unit = 'in' )

  setTxtProgressBar( pb, j )
  }

