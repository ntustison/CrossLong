library( ggplot2 )
library( plyr )

cross <- read.csv( "adniCrossSectionalAntsMergeSubset_WithScr.csv" )
longNative <- read.csv( "adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv" )
longSST <- read.csv( "adniLongitudinalAntsMergeSubset_WithScr.csv" )

thicknessColumns <- grep( 'thickness', colnames( cross ), value = FALSE )

sstGreaterThanZero <- 0
nativeGreaterThanZero <- 0
for( i in 1:length( thicknessColumns ) )
  {
  cat( colnames( cross )[thicknessColumns[i]], "\n" )
  sstDifference <- cross[,thicknessColumns[i]] - longSST[,thicknessColumns[i]]
  nativeDifference <- cross[,thicknessColumns[i]] - longNative[,thicknessColumns[i]]
  thicknessDataFrame <- data.frame( Cross = cross[,thicknessColumns[i]],
                                    LongSSTDifference = sstDifference,
                                    LongNativeDifference = nativeDifference )
  sstMeanDiff <- mean( sstDifference, na.rm = TRUE )
  sstSdDiff <- sd( sstDifference, na.rm = TRUE )

  if( sstMeanDiff > 0 )
    {
    sstGreaterThanZero <- sstGreaterThanZero + 1
    }

  nativeMeanDiff <- mean( nativeDifference, na.rm = TRUE )
  nativeSdDiff <- sd( nativeDifference, na.rm = TRUE )

  if( nativeMeanDiff > 0 )
    {
    nativeGreaterThanZero <- nativeGreaterThanZero + 1
    }

  baSstPlot <- ggplot( thicknessDataFrame, aes( x = Cross, y = LongSSTDifference ) ) +
               geom_point( alpha = 0.25 ) +
               geom_hline( yintercept = sstMeanDiff, linetype = 1  ) +
               geom_hline( yintercept = sstMeanDiff - sstSdDiff, linetype = 2  ) +
               geom_hline( yintercept = sstMeanDiff + sstSdDiff, linetype = 2  ) +
               ggtitle( colnames( cross )[thicknessColumns[i]] ) +
               xlab( 'Cross thickness' ) +
               ylab( 'Thickness difference (Cross - SST)' ) +
               scale_y_continuous( limits = c( -2, 2 ) )
  ggsave( file = paste0( "BlandAltmanDifferencePlots/baSstPlot_", colnames( cross )[thicknessColumns[i]], ".pdf" ), plot = baSstPlot, width = 7, height = 4 )

  baNativePlot <- ggplot( thicknessDataFrame, aes( x = Cross, y = LongNativeDifference ) ) +
               geom_point( alpha = 0.25 ) +
               geom_hline( yintercept = nativeMeanDiff, linetype = 1  ) +
               geom_hline( yintercept = nativeMeanDiff - nativeSdDiff, linetype = 2  ) +
               geom_hline( yintercept = nativeMeanDiff + nativeSdDiff, linetype = 2  ) +
               ggtitle( colnames( cross )[thicknessColumns[i]] ) +
               xlab( 'Cross thickness' ) +
               ylab( 'Thickness difference (Cross - Native)' ) +
               scale_y_continuous( limits = c( -2, 2 ) )
  ggsave( file = paste0( "BlandAltmanDifferencePlots/baNativePlot_", colnames( cross )[thicknessColumns[i]], ".pdf" ), plot = baNativePlot, width = 7, height = 4 )
  }

cat( "SST: greater than 0 --> ", sstGreaterThanZero, "\n", sep = '' )  
cat( "Native: greater than 0 --> ", nativeGreaterThanZero, "\n", sep = '' )  