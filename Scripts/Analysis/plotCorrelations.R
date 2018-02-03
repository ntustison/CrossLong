library( ggplot2 )

baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
sandboxDirectory <- paste0( baseDirectory, 'Sandbox/' )

pipelineNames <- c( 'ANTsCross', 'ANTsNative', 'ANTsSST', 'FSCross', 'FSLong' )
numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

stanResults <- read.csv( paste0( dataDirectory, "stan_All_Results.csv" ) )
ageResults <- read.csv( paste0( dataDirectory, "predictionAgeBl_DataAll.csv" ) )
mmseResults <- read.csv( paste0( dataDirectory, "predictionMmseBl_DataAll.csv" ) )

ageDataFrame <- data.frame( VarianceRatio = stanResults$variance.ratio.50.,
                            PredictionAccuracy = ageResults$AccuracyMean,
                            Pipeline = ageResults$Pipeline,
                            DktRegion = as.factor( ageResults$DktRegion )
                          )

agePlot <- ggplot( data = ageDataFrame ) +
    geom_point( aes( x = VarianceRatio, y = PredictionAccuracy, colour = Pipeline ), alpha = 0.8, size = 4 ) +
    geom_smooth( aes( x = VarianceRatio, y = PredictionAccuracy ), alpha = 0.5, size = 0, method = lm ) +
    facet_wrap( ~ DktRegion, ncol = 8, scales = "free" ) +
    scale_y_continuous( "Age prediction RMSE (years)" ) +
    scale_x_continuous( "Variance ratio" )
ggsave( paste0( dataDirectory, 'agePrediction.pdf' ), agePlot, width = 20, height = 20, unit = 'in' )

cat( "Age prediction\n")
for( i in 1:nlevels( ageDataFrame$DktRegion ) )
  {
  varRatio <- ageDataFrame$VarianceRatio[which( ageDataFrame$DktRegion == ageDataFrame$DktRegion[i] )]
  predAccur <- ageDataFrame$PredictionAccuracy[which( ageDataFrame$DktRegion == ageDataFrame$DktRegion[i] )]  
  ageCorTest <- cor.test( varRatio, predAccur )
  
  if( ageCorTest$p.value < 0.1 )
    {
    cat( levels( ageDataFrame$DktRegion )[ageDataFrame$DktRegion[i]], ": ", ageCorTest$estimate, "\n" )  
    }
  }

mmseDataFrame <- data.frame( VarianceRatio = stanResults$variance.ratio.50.,
                             PredictionAccuracy = mmseResults$AccuracyMean,
                             Pipeline = mmseResults$Pipeline,
                             DktRegion = as.factor( mmseResults$DktRegion )
                            )

mmsePlot <- ggplot( data = mmseDataFrame ) +
    geom_point( aes( x = VarianceRatio, y = PredictionAccuracy, colour = Pipeline ), alpha = 0.8, size = 4 ) +
    geom_smooth( aes( x = VarianceRatio, y = PredictionAccuracy ), alpha = 0.5, size = 0, method = lm ) +
    facet_wrap( ~ DktRegion, ncol = 8, scales = "free" ) +
    scale_y_continuous( "MMSE RMSE" ) +
    scale_x_continuous( "Variance ratio" )
ggsave( paste0( dataDirectory, 'mmsePrediction.pdf' ), mmsePlot, width = 20, height = 20, unit = 'in' )

cat( "\nMMSE prediction\n")

for( i in 1:nlevels( mmseDataFrame$DktRegion ) )
  {
  varRatio <- mmseDataFrame$VarianceRatio[which( mmseDataFrame$DktRegion == mmseDataFrame$DktRegion[i] )]
  predAccur <- mmseDataFrame$PredictionAccuracy[which( mmseDataFrame$DktRegion == mmseDataFrame$DktRegion[i] )]  
  mmseCorTest <- cor.test( varRatio, predAccur )
  if( mmseCorTest$p.value < 0.1 )
    {
    cat( levels( mmseDataFrame$DktRegion )[mmseDataFrame$DktRegion[i]], ": ", mmseCorTest$estimate, "\n" )  
    }
  }


