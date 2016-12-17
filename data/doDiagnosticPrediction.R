library( reshape2 )
library( ggplot2 )
library( caret )
library( e1071 )
library( grid )
library( xgboost )

##
#
#  Create the "slope" files from the original cortical thickness files.  Each
#
#

crossSlopeDataFile <- "adniCrossSlopeData.csv"
long1SlopeDataFile <- "adniLong1SlopeData.csv"
long2SlopeDataFile <- "adniLong2SlopeData.csv"

if( ! file.exists( crossSlopeDataFile ) || ! file.exists( long1SlopeDataFile ) || ! file.exists( long2SlopeDataFile ) )
  {

  crossDataFile <- "adniCrossSectionalAntsMergeSubset.csv"
  long1DataFile <- "adniLongitudinalAntsMergeSubset.csv"
  long2DataFile <- "adniLongitudinalNativeSpaceAntsMergeSubset.csv"

  crossData <- read.csv( crossDataFile )
  long1Data <- read.csv( long1DataFile )
  long2Data <- read.csv( long2DataFile )

  subjects <- crossData$ID
  visit <- crossData$VISIT
  diagnosis <- crossData$DIAGNOSIS
  crossData <- crossData[,grep( "thickness", colnames( crossData ) )]
  crossData <- crossData[,-grep( "total.mean.thickness", colnames( crossData ) )]
  crossData$ID <- subjects
  crossData$VISIT <- visit
  crossData$DIAGNOSIS <- diagnosis
  long1Data <- long1Data[,grep( "thickness", colnames( long1Data ) )]
  long1Data <- long1Data[,-grep( "total.mean.thickness", colnames( long1Data ) )]
  long1Data$ID <- subjects
  long1Data$VISIT <- visit
  long1Data$DIAGNOSIS <- diagnosis
  long2Data <- long2Data[,grep( "thickness", colnames( long2Data ) )]
  long2Data <- long2Data[,-grep( "total.mean.thickness", colnames( long2Data ) )]
  long2Data$ID <- subjects
  long2Data$VISIT <- visit
  long2Data$DIAGNOSIS <- diagnosis

  subjects <- levels( crossData$ID )
  timePointsString <- c( 'bl', 'm06', 'm12', 'm18', 'm24', 'm36' )
  timePointsNumeric <- c( 0, 6, 12, 18, 24, 36 )

  numericVisit <- rep( 'NA', length( crossData$VISIT ) )
  for( m in 1:length( timePointsString ) )
    {
    numericVisit[crossData$VISIT == timePointsString[m]] <- timePointsNumeric[m]
    }
  crossData$VISIT <- as.numeric( numericVisit )
  long1Data$VISIT <- as.numeric( numericVisit )
  long2Data$VISIT <- as.numeric( numericVisit )

  thicknessColumns <- grep( "thickness", colnames( crossData ) )

  crossSlopeData <- matrix( NA, nrow = length( subjects ), ncol = length( thicknessColumns ) )
  long1SlopeData <- matrix( NA, nrow = length( subjects ), ncol = length( thicknessColumns ) )
  long2SlopeData <- matrix( NA, nrow = length( subjects ), ncol = length( thicknessColumns ) )

  pb <- txtProgressBar( min = 0, max = nrow( crossSlopeData ) * ncol( crossSlopeData ), style = 3 )

  diagnoses <- rep( NA, length( subjects ) )

  for( i in 1:length( subjects ) )
    {
    subject <- subjects[i]
    crossSubjectData <- crossData[which( crossData$ID == subject ),]
    long1SubjectData <- long1Data[which( long1Data$ID == subject ),]
    long2SubjectData <- long2Data[which( long2Data$ID == subject ),]

    diagnoses[i] <- crossSubjectData$DIAGNOSIS[1]

    for( j in 1:length( thicknessColumns ) )
      {
      crossLmResults <- lm( crossSubjectData[,j] ~ crossSubjectData$VISIT, na.action=na.omit )
      long2LmResults <- lm( long2SubjectData[,j] ~ long2SubjectData$VISIT, na.action=na.omit )

      if( prod( is.na( crossSubjectData[,j]  ) ) == 0 )
        {
        crossLmResults <- lm( crossSubjectData[,j] ~ crossSubjectData$VISIT, na.action=na.omit )
        crossSlopeData[i, j] <- crossLmResults$coefficients[2]
        }

      if( prod( is.na( long1SubjectData[,j]  ) ) == 0 )
        {
        long1LmResults <- lm( long1SubjectData[,j] ~ long1SubjectData$VISIT, na.action=na.omit )
        long1SlopeData[i, j] <- long1LmResults$coefficients[2]
        }

      if( prod( is.na( long2SubjectData[,j]  ) ) == 0 )
        {
        long2LmResults <- lm( long2SubjectData[,j] ~ long2SubjectData$VISIT, na.action=na.omit )
        long2SlopeData[i, j] <- long2LmResults$coefficients[2]
        }

      setTxtProgressBar( pb, i * length( thicknessColumns ) + j )
      }

    }
  crossSlopeData <- as.data.frame( crossSlopeData )
  long1SlopeData <- as.data.frame( long1SlopeData )
  long2SlopeData <- as.data.frame( long2SlopeData )

  colnames( crossSlopeData ) <- colnames( crossData )[thicknessColumns]
  colnames( long1SlopeData ) <- colnames( crossData )[thicknessColumns]
  colnames( long2SlopeData ) <- colnames( crossData )[thicknessColumns]

  crossSlopeData$DIAGNOSIS <- diagnoses
  long1SlopeData$DIAGNOSIS <- diagnoses
  long2SlopeData$DIAGNOSIS <- diagnoses

  write.csv( crossSlopeData, "adniCrossSlopeData.csv" )
  write.csv( long1SlopeData, "adniLong1SlopeData.csv" )
  write.csv( long2SlopeData, "adniLong2SlopeData.csv" )
  } else {

  crossSlopeData <- read.csv( crossSlopeDataFile )
  long1SlopeData <- read.csv( long1SlopeDataFile )
  long2SlopeData <- read.csv( long2SlopeDataFile )

  crossSlopeData$X <- NULL
  long1SlopeData$X <- NULL
  long2SlopeData$X <- NULL

  }


##
#
#  Do random forest prediction for
#
#

nPermutations <- 1000

trainingPortions <- c( 0.9 )

slopeData <- list()
slopeData[[1]] <- crossSlopeData
slopeData[[2]] <- long1SlopeData
slopeData[[3]] <- long2SlopeData

slopeTypes <- c( "Cross", "Long1", "Long2" )

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

    trainingIndices <- createDataPartition( slopeData[[1]]$DIAGNOSIS, p = trainingPortion, list = FALSE, times = 1 )

    for( d in 1:length( slopeTypes ) )
      {
      trainingData <- slopeData[[d]][trainingIndices,]
      trainingData <- trainingData[complete.cases( trainingData ),]
      testingData <- slopeData[[d]][-trainingIndices,]
      testingData <- testingData[complete.cases( testingData ),]

      modelDataXgb <- xgb.DMatrix( as.matrix( trainingData[, !( names( trainingData ) %in% c( "DIAGNOSIS" ) )] ),
                                           label = trainingData$DIAGNOSIS - 1 )
      paramXgb <- list( max.depth = 6, eta = 0.3, silent = 0, objective = "multi:softmax", num_class = 4 )
      modelXgb <- xgb.train( paramXgb, modelDataXgb, nrounds = 100, nthread = 8, verbose = 0 )
      predictedDiagnosis <- predict( modelXgb, as.matrix( testingData[, !( names( testingData ) %in% c( "DIAGNOSIS" ) )] ) ) + 1

      xgbImp <- xgb.importance( model = modelXgb )
      sorted <- sort( as.numeric( xgbImp$Feature ), index.return = TRUE )
      if( n == 1 )
        {
        featureImpMean[[d]] <- xgbImp$Gain[sorted$ix]
        featureImpSd[[d]] <- 0.0
        } else {
        featureImpPreviousMean <- featureImpMean[[d]]
        featureImpMean[[d]] <- featureImpPreviousMean + ( xgbImp$Gain[sorted$ix] - featureImpPreviousMean ) / ( n - 1 )
        featureImpSd[[d]] <- featureImpSd[[d]] + ( xgbImp$Gain[sorted$ix] - featureImpPreviousMean ) * ( xgbImp$Gain[sorted$ix] - featureImpMean[[d]] )
        }

#       diagnosisRF <- randomForest( x = trainingData[, !( names( trainingData ) %in% c( "DIAGNOSIS" ) )],
#                                    y = as.factor( trainingData$DIAGNOSIS ), importance = TRUE,
#                                    na.action = na.omit, replace = FALSE, ntree = 200 )
#       if( n == 1 )
#         {
#         featureImpMean[[d]] <- importance( diagnosisRF, type = 1 )
#         featureImpSd[[d]] <- 0.0
#         } else {
#         featureImpPreviousMean <- featureImpMean[[d]]
#         imp <-  importance( diagnosisRF, type = 1 )
#         featureImpMean[[d]] <- featureImpPreviousMean + ( imp - featureImpPreviousMean ) / ( n - 1 )
#         featureImpSd[[d]] <- featureImpSd[[d]] + ( imp - featureImpPreviousMean ) * ( imp - featureImpMean[[d]] )
#         }
#       predictedDiagnosis <- predict( diagnosisRF, testingData )

      cMatrix <- confusionMatrix( predictedDiagnosis, testingData$DIAGNOSIS, mode = "everything" )
      cat( "    ", slopeTypes[d], ": accuracy = ", cMatrix$overall[[1]], "\n", sep = '' )

      oneData <- data.frame( Pipeline = slopeTypes[d],
                             Accuracy = cMatrix$overall[[1]] )
      resultsData <- rbind( resultsData, oneData )
      }
    }

  for( n in 1:length( slopeTypes ) )
    {
    featureImp.df <- data.frame( Statistic = colnames( crossSlopeData )[grep( 'thickness', colnames( crossSlopeData))],
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

    ggsave( file = paste( "../Figures/importanceCombined", slopeTypes[n], p, ".png", sep = "" ), plot = vPlot, width = 4, height = 8 )
    }

  rmsePlot <- ggplot( resultsData, aes( x = Accuracy, fill = Pipeline ) ) +
                      scale_y_continuous( "Density" ) +
                      scale_x_continuous( "Accuracy" ) +
                     geom_density( alpha = 0.5 )
  ggsave( filename = paste( "../Figures/accuracy", p, ".png", sep = "" ), plot = rmsePlot, width = 6, height = 6, units = 'in' )

  myAov <- aov( Accuracy ~ Pipeline, data = resultsData )
  TukeyHSD( myAov, c( "Pipeline" ) )


# > myAov <- aov( Accuracy ~ Pipeline, data = resultsData )
# > TukeyHSD( myAov, c( "Pipeline" ) )
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
#
# Fit: aov(formula = Accuracy ~ Pipeline, data = resultsData)
#
# $Pipeline
#                   diff         lwr        upr     p adj
# Long1-Cross 0.01184493 0.003475161 0.02021470 0.0026503
# Long2-Cross 0.02740797 0.019038196 0.03577774 0.0000000
# Long2-Long1 0.01556303 0.007193263 0.02393281 0.0000409

  }




