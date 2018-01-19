library( reshape2 )
library( ggplot2 )
library( caret )
library( e1071 )
library( grid )
library( xgboost )
library( randomForest )
library( plyr )

##
#
#  Create the "slope" files from the original cortical thickness files.  Each
#
#

dataFiles <- c( 'adniCrossSectionalAntsMergeSubset_WithScr.csv', 'adniLongitudinalAntsMergeSubset_WithScr.csv', 'adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv', 'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv', 'adniLongitudinalFreeSurferMergeSubset_WithScr.csv' )

thicknessData <- list()
for( i in 1:length( dataFiles ) )
  {
  thicknessData[[i]] <- read.csv( dataFiles[i] )
  thicknessData[[i]] <- thicknessData[[i]][which( thicknessData[[i]]$VISIT == "bl" | thicknessData[[i]]$VISIT == "scr" ),]
  }

##
#
#  Do prediction
#
#

nPermutations <- 1000

trainingPortions <- c( 0.9 )

pipelineTypes <- c( "ANTs cross-sectional", "ANTs longitudinal-SST", "ANTs longitudinal-native", "FreeSurfer cross-sectional", "FreeSurfer longitudinal" )

count <- 1
for( p in trainingPortions )
  {
  trainingPortion <- p
  cat( "trainingPortion = ", trainingPortion, "\n", sep = '' )

  resultsData <- data.frame( Pipeline = character( 0 ), Accuracy = numeric( 0 ) )

  featureImpMean <- list()
  featureImpSd <- list()
  for( n in seq( 1, nPermutations, by = 1 ) )
    {
    cat( "  Permutation ", n, "\n", sep = '' )

    trainingIndices <- createDataPartition( thicknessData[[4]]$DIAGNOSIS, p = trainingPortion, list = FALSE, times = 1 )

    for( d in 1:length( pipelineTypes ) )
      {

      trainingData <- thicknessData[[d]][trainingIndices,]
      trainingData <- trainingData[complete.cases( trainingData ),]
      trainingData$DIAGNOSIS <- as.numeric( trainingData$DIAGNOSIS )
      testingData <- thicknessData[[d]][-trainingIndices,]
      testingData <- testingData[complete.cases( testingData ),]
      testingData$DIAGNOSIS <- as.numeric( testingData$DIAGNOSIS )

      predictorColumns <- sort( c( grep( "thickness", colnames( trainingData ) ) ) )

#       modelDataXgb <- xgb.DMatrix( as.matrix( trainingData[, predictorColumns] ),
#                                            label = trainingData$AGE )
#       paramXgb <- list( max.depth = 6, eta = 0.3, silent = 0, objective = "reg:linear" )
#       modelXgb <- xgb.train( paramXgb, modelDataXgb, nrounds = 100, nthread = 8, verbose = 0 )
#       predictedAge <- predict( modelXgb, as.matrix( testingData[, grep( "thickness", colnames( trainingData ) )] ) )
#
#       xgbImp <- xgb.importance( model = modelXgb )
#       sorted <- sort( as.numeric( xgbImp$Feature ), index.return = TRUE )
#       if( n == 1 )
#         {
#         featureImpMean[[d]] <- xgbImp$Gain[sorted$ix]
#         featureImpSd[[d]] <- 0.0
#         } else {
#         featureImpPreviousMean <- featureImpMean[[d]]
#         featureImpMean[[d]] <- featureImpPreviousMean + ( xgbImp$Gain[sorted$ix] - featureImpPreviousMean ) / ( n - 1 )
#         featureImpSd[[d]] <- featureImpSd[[d]] + ( xgbImp$Gain[sorted$ix] - featureImpPreviousMean ) * ( xgbImp$Gain[sorted$ix] - featureImpMean[[d]] )
#         }

      ageRF <- randomForest( x = trainingData[, predictorColumns],
                                   y = trainingData$AGE, importance = TRUE,
                                   na.action = na.omit, replace = FALSE, ntree = 500 )
      if( n == 1 )
        {
        featureImpMean[[d]] <- importance( ageRF, type = 1 )
        featureImpSd[[d]] <- 0.0
        } else {
        featureImpPreviousMean <- featureImpMean[[d]]
        imp <-  importance( ageRF, type = 1 )
        featureImpMean[[d]] <- featureImpPreviousMean + ( imp - featureImpPreviousMean ) / ( n - 1 )
        featureImpSd[[d]] <- featureImpSd[[d]] + ( imp - featureImpPreviousMean ) * ( imp - featureImpMean[[d]] )
        }
      predictedAge <- predict( ageRF, testingData )


      oneAccuracy <- sqrt( sum( ( predictedAge - testingData$AGE )^2 / length( predictedAge ) ) )
      cat( "    ", pipelineTypes[d], ": rmse = ", oneAccuracy, "\n", sep = '' )


      oneData <- data.frame( Pipeline = pipelineTypes[d],
                             Accuracy = oneAccuracy )
      resultsData <- rbind( resultsData, oneData )
      }

    if( n %% 10 == 0 )
      {
      rmsePlot <- ggplot( resultsData, aes( x = Accuracy, fill = Pipeline ) ) +
                          scale_y_continuous( "Density" ) +
                          scale_x_continuous( "Accuracy" ) +
                         geom_density( alpha = 0.5 )
      ggsave( filename = paste( "~/Desktop/accuracy", p, ".png", sep = "" ), plot = rmsePlot, width = 6, height = 6, units = 'in' )

      resultsDataBarPlot <- ddply( resultsData, "Pipeline", summarise, meanAccuracy = mean( Accuracy ), sdAccuracy = sd( Accuracy ) )
      rmseBarPlot <- ggplot( resultsDataBarPlot, aes( x = Pipeline, y = meanAccuracy ) ) +
                     geom_bar( aes( fill = Pipeline ), stat = "identity" ) +
                     geom_errorbar( aes( ymin = meanAccuracy - sdAccuracy, ymax = meanAccuracy + sdAccuracy ), width = 0.35 ) +
                     scale_y_continuous( "Accuracy" ) +
                     theme( legend.position = "none" )
      ggsave( filename = paste( "~/Desktop/accuracyBarPlot", p, ".png", sep = "" ), plot = rmseBarPlot, width = 8, height = 5, units = 'in' )
      }
    }

  for( n in 1:length( pipelineTypes ) )
    {
    predictorColumns <- sort( c( grep( "thickness", colnames( trainingData[[n]] ) ) ) )
    featureImp.df <- data.frame( Statistic = colnames( thicknessData[[n]] )[predictorColumns],
                                 Importance = featureImpMean[[n]],
                                 ImportanceSd = featureImpSd[[n]] )

#     featureImp.df <- data.frame( Statistic = names( featureImpMean[[n]][,1] ),
#                                  Importance = as.numeric( featureImpMean[[n]][,1] ),
#                                  ImportanceSd = as.numeric( featureImpSd[[n]] ) )

    featureImp.df <- featureImp.df[order( featureImp.df$Importance ),]

    modifiedStatistic <- gsub( 'thickness.', '', featureImp.df$Statistic )

    featureImp.df$Statistic <- factor( x = modifiedStatistic, levels = modifiedStatistic )

    vPlot <- ggplot( data = featureImp.df, aes( x = Importance, y = Statistic ) ) +
             geom_point( aes( color = Importance ) ) +
             geom_errorbarh( aes( xmax = Importance + ImportanceSd, xmin = Importance - ImportanceSd, color = Importance ) ) +
             ylab( "" ) +
             scale_x_continuous( "Gain" ) +
#              scale_color_continuous( low = "navyblue", high = "darkred" ) +
             theme( axis.text.y = element_text( size = 8 ) ) +
             theme( plot.margin = unit( c( 0.1, 0.1, 0.1, -0.5 ), "cm" ) ) +
             theme( axis.title = element_text( size = 9 ) ) +
             theme( legend.position = "none" )

    ggsave( file = paste( "~/Desktop/importanceCombined", pipelineTypes[n], p, ".png", sep = "" ), plot = vPlot, width = 4, height = 8 )
    }

  myAov <- aov( Accuracy ~ Pipeline, data = resultsData )
  TukeyHSD( myAov, c( "Pipeline" ) )


#  Xgboost
# > TukeyHSD( myAov, c( "Pipeline" ) )
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
#
# Fit: aov(formula = Accuracy ~ Pipeline, data = resultsData)
#
# $Pipeline
#                                             diff          lwr         upr    p adj
# Longitudinal-SST-Cross-sectional      0.01194526  0.005469428  0.01842108 1.31e-05
# Longitudinal-native-Cross-sectional   0.02701698  0.020541147  0.03349280 0.00e+00
# Random-Cross-sectional               -0.09583287 -0.102308701 -0.08935704 0.00e+00
# Longitudinal-native-Longitudinal-SST  0.01507172  0.008595891  0.02154755 0.00e+00
# Random-Longitudinal-SST              -0.10777813 -0.114253956 -0.10130230 0.00e+00
# Random-Longitudinal-native           -0.12284985 -0.129325676 -0.11637402 0.00e+00

  }




