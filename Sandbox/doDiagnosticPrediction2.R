library( ggplot2 )
library( randomForest )
library( caret )
library( plyr )
library( nlme )

############################################################################################################
#
#     This is the core script for using the subject-specific slopes of the longitudinal cortical thickness 
#     for predicting diagnosis.  The major steps performed below are as follows:
#

baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'data/' )

corticalThicknessPipelineNames <- c( 'ANTsCross', 'ANTsSST', 'ANTsNative', 'FSCross', 'FSLong', 'newANTsNative', 'newANTsSST', 'newANTsCross' )
numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 


##########
#
# First, reconcile the data.  Make sure image ids are the same and in the same order.
# Also, remove NA subjects and subjects with single time points.
# 
##########

corticalThicknessCsvs <- list()
corticalThicknessCsvs[[1]] <- paste0( dataDirectory, 'adniCrossSectionalAntsMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[2]] <- paste0( dataDirectory, 'adniLongitudinalAntsMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[3]] <- paste0( dataDirectory, 'adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[4]] <- paste0( dataDirectory, 'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[5]] <- paste0( dataDirectory, 'adniLongitudinalFreeSurferMergeSubset_WithScr.csv' )
corticalThicknessCsvs[[6]] <- paste0( dataDirectory, 'newLongitudinalThicknessANTsNativeSpace.csv' )
corticalThicknessCsvs[[7]] <- paste0( dataDirectory, 'newLongitudinalThicknessANTsSST.csv' )
corticalThicknessCsvs[[8]] <- paste0( dataDirectory, 'newLongitudinalThicknessCrossSectionalANTs.csv' )

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
  # write.csv( corticalThicknessData[[i]], quote = FALSE, row.names = FALSE, 
  #            file = paste0( "/Users/ntustison/Desktop/", corticalThicknessPipelineNames[i], ".csv" ) )  
  }


##########
#
# Read the slope files, if they exist.  Otherwise, create them.
# 
##########

slopeFiles <- c()
slopeDataList <- list()
for( i in 1:length( corticalThicknessData ) )
  {
  slopeFiles[i] <- paste0( corticalThicknessPipelineNames[i], "_slopes.csv" )

  if( ! file.exists( slopeFiles[i] ) )
    {
    cat( "Creating ", slopeFiles[i], "\n" )

    subjects <- unique( corticalThicknessData[[i]]$ID )    
    diagnoses <- rep( NA, length( subjects ) )
    ages <- rep( NA, length( subjects ) )
    for( j in 1:length( subjects ) )
      {
      subjectData <- corticalThicknessData[[i]][which( corticalThicknessData[[i]]$ID == subjects[j] ),]
      diagnoses[j] <- subjectData$DIAGNOSIS[1]
      ages[j] <- subjectData$AGE[1]
      }

    thicknessColumns <- grep( "thickness", colnames( corticalThicknessData[[i]] ) )
    slopeDataList[[i]] <- matrix( NA, nrow = length( subjects ), ncol = length( thicknessColumns ) )

    pb <- txtProgressBar( min = 0, max = ncol( slopeDataList[[i]] ), style = 3 )

    for( j in 1:length( thicknessColumns ) )
      {
      corticalThicknessDataFrame <- data.frame( Y = corticalThicknessData[[i]][, thicknessColumns[j]], 
                                                VISIT = corticalThicknessData[[i]]$VISIT,
                                                ID = corticalThicknessData[[i]]$ID )
      lmeModel <- lme( Y ~ VISIT, random = ~ VISIT - 1 | ID, data = corticalThicknessDataFrame )

      slopeDataList[[i]][, j] <- as.numeric( lmeModel$coefficients$fixed[2]) + as.vector( lmeModel$coefficients$random[[1]][,1] )
      setTxtProgressBar( pb, j )
      }
    cat( "\n" )

    slopeDataList[[i]] <- as.data.frame( slopeDataList[[i]] )
    colnames( slopeDataList[[i]] ) <- colnames( corticalThicknessData[[i]] )[thicknessColumns]
    slopeDataList[[i]]$DIAGNOSIS <- diagnoses
    slopeDataList[[i]]$AGE <- ages

    write.csv( slopeDataList[[i]], slopeFiles[i], quote = FALSE, row.names = FALSE )
    } else {
    cat( "Reading ", slopeFiles[i], "\n" )
    slopeDataList[[i]] <- read.csv( slopeFiles[i] )
    }
  }

##########
#
# Do prediction
# 
##########

nPermutations <- 1000

trainingPortions <- c( 0.9 )


slopeTypes <- c( "ANTs cross", "ANTs SST", "ANTs native", "FreeSurfer cross", "FreeSurfer long", "new ANTs native", "new ANTs SST", "new ANTs cross" )

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

    trainingIndices <- createDataPartition( slopeDataList[[1]]$DIAGNOSIS, p = trainingPortion, list = FALSE, times = 1 )

    numberOfSampledRegions <- 10
    randomColumns <- sample.int( n = numberOfRegions, size = numberOfSampledRegions, replace = FALSE )

    for( d in 1:length( slopeTypes ) )
      {

      if( slopeTypes[d] == "Random" )
        {

        # do Random case
        testingData <- slopeDataList[[1]][-trainingIndices,]
        testingData <- testingData[complete.cases( testingData ),]
        predictedDiagnosis <- sample.int( n = 4,  size = length( testingData$DIAGNOSIS ), replace = TRUE )

        } else {

        trainingData <- slopeDataList[[d]][trainingIndices,]
        trainingData <- trainingData[complete.cases( trainingData ),]
        testingData <- slopeDataList[[d]][-trainingIndices,]
        testingData <- testingData[complete.cases( testingData ),]

        # trainingData$DIAGNOSIS[which( trainingData$DIAGNOSIS == 3 )] <- 2
        # testingData$DIAGNOSIS[which( testingData$DIAGNOSIS == 3 )] <- 2
        # trainingData$DIAGNOSIS[which( trainingData$DIAGNOSIS == 4 )] <- 3
        # testingData$DIAGNOSIS[which( testingData$DIAGNOSIS == 4 )] <- 3

        # predictorColumns <- sort( c( grep( "thickness", colnames( trainingData ) ), which( colnames( trainingData ) == "AGE" ) ) )
        predictorColumns <- sort( grep( "thickness", colnames( trainingData ) ) )
        predictorColumns <- predictorColumns[randomColumns]

        # modelDataXgb <- xgb.DMatrix( as.matrix( trainingData[, predictorColumns] ),
        #                                      label = trainingData$DIAGNOSIS - 1 )
        # paramXgb <- list( max.depth = 9, eta = 0.86, silent = 0, objective = "multi:softmax", num_class = 4 )
        # modelXgb <- xgb.train( paramXgb, modelDataXgb, nrounds = 25, nthread = 8, verbose = 0 )
        # predictedDiagnosis <- predict( modelXgb, as.matrix( testingData[, !( names( testingData ) %in% c( "DIAGNOSIS", "thickness.left.entorhinal", "thickness.right.entorhinal" ) )] ) ) + 1
        #
        # xgbImp <- xgb.importance( model = modelXgb )
        # sorted <- sort( as.numeric( xgbImp$Feature ), index.return = TRUE )
        # if( n == 1 )
        #   {
        #   featureImpMean[[d]] <- xgbImp$Gain[sorted$ix]
        #   featureImpSd[[d]] <- 0.0
        #   } else {
        #   featureImpPreviousMean <- featureImpMean[[d]]
        #   featureImpMean[[d]] <- featureImpPreviousMean + ( xgbImp$Gain[sorted$ix] - featureImpPreviousMean ) / ( n - 1 )
        #   featureImpSd[[d]] <- featureImpSd[[d]] + ( xgbImp$Gain[sorted$ix] - featureImpPreviousMean ) * ( xgbImp$Gain[sorted$ix] - featureImpMean[[d]] )
        #  }

        predictedRF <- randomForest( x = trainingData[, predictorColumns],
                                     y = trainingData$AGE, importance = TRUE,
                                     na.action = na.omit, replace = FALSE, ntree = 100 )
        predictedAge <- predict( predictedRF, testingData )
        # if( n == 1 )
        #   {
        #   featureImpMean[[d]] <- importance( predictedRF, type = 1 )
        #   featureImpSd[[d]] <- 0.0
        #   } else {
        #   featureImpPreviousMean <- featureImpMean[[d]]
        #   imp <-  importance( predictedRF, type = 1 )
        #   featureImpMean[[d]] <- featureImpPreviousMean + ( imp - featureImpPreviousMean ) / ( n - 1 )
        #   featureImpSd[[d]] <- featureImpSd[[d]] + ( imp - featureImpPreviousMean ) * ( imp - featureImpMean[[d]] )
        #   }
        }

      accuracyAge <- sqrt( sum( ( predictedAge - testingData$AGE )^2 ) / length( testingData$AGE ) )
      cat( "    ", slopeTypes[d], ": accuracy = ", accuracyAge, "\n", sep = '' )
      oneData <- data.frame( Pipeline = slopeTypes[d],
                             Accuracy = accuracyAge )

      # cMatrix <- confusionMatrix( predictedDiagnosis, testingData$DIAGNOSIS, mode = "everything" )
      # cat( "    ", slopeTypes[d], ": accuracy = ", cMatrix$overall[[1]], "\n", sep = '' )
      # oneData <- data.frame( Pipeline = slopeTypes[d],
      #                        Accuracy = cMatrix$overall[[1]] )

      resultsData <- rbind( resultsData, oneData )
      }

    if( n %% 10 == 0 )
      {
      rmsePlot <- ggplot( resultsData, aes( x = Accuracy, fill = Pipeline ) ) +
                          scale_y_continuous( "Density" ) +
                          scale_x_continuous( "Accuracy" ) +
                         geom_density( alpha = 0.5 )
      ggsave( filename = paste( "~/Desktop/accuracy", p, "_WithScr.png", sep = "" ), plot = rmsePlot, width = 6, height = 6, units = 'in' )

      resultsDataBarPlot <- ddply( resultsData, "Pipeline", summarise, meanAccuracy = mean( Accuracy ), sdAccuracy = sd( Accuracy ) )
      rmseBarPlot <- ggplot( resultsDataBarPlot, aes( x = Pipeline, y = meanAccuracy ) ) +
                     geom_bar( aes( fill = Pipeline ), stat = "identity" ) +
                     geom_errorbar( aes( ymin = meanAccuracy - sdAccuracy, ymax = meanAccuracy + sdAccuracy ), width = 0.35 ) +
                     scale_y_continuous( "Accuracy" ) +
                     theme( legend.position = "none" )
      ggsave( filename = paste( "~/Desktop/accuracyBarPlot", p, "_WithScr.png", sep = "" ), plot = rmseBarPlot, width = 8, height = 5, units = 'in' )
      }
    }

#   for( n in 1:length( slopeTypes ) )
#     {
#     if( slopeTypes[n] == "Random" )
#       {
#       next
#       }
#     predictorColumns <- sort( c( grep( "thickness", colnames( slopeDataList[[i]] ) ), which( colnames( slopeDataList[[i]] ) == "AGE" ) ) )

#     # featureImp.df <- data.frame( Statistic = colnames( slopeDataList[[i]] )[which( colnames( slopeDataList[[i]] ) != 'DIAGNOSIS' & colnames( slopeDataList[[i]] ) != 'X' )],
#     #                              Importance = featureImpMean[[n]],
#     #                              ImportanceSd = featureImpSd[[n]] )

#     featureImp.df <- data.frame( Statistic = names( featureImpMean[[n]][,1] ),
#                                  Importance = as.numeric( featureImpMean[[n]][,1] ),
#                                  ImportanceSd = as.numeric( featureImpSd[[n]] ) )

#     featureImp.df <- featureImp.df[order( featureImp.df$Importance ),]

#     modifiedStatistic <- gsub( 'thickness.', '', featureImp.df$Statistic )

#     featureImp.df$Statistic <- factor( x = modifiedStatistic, levels = modifiedStatistic )

#     vPlot <- ggplot( data = featureImp.df, aes( x = Importance, y = Statistic ) ) +
#              geom_point( aes( color = Importance ) ) +
#             #  geom_errorbarh( aes( xmax = Importance + ImportanceSd, xmin = Importance - ImportanceSd, color = Importance ) ) +
#              ylab( "" ) +
#              scale_x_continuous( "MeanDecreaseAccuracy" ) +
# #              scale_color_continuous( low = "navyblue", high = "darkred" ) +
#              theme( axis.text.y = element_text( size = 8 ) ) +
#              theme( plot.margin = unit( c( 0.1, 0.1, 0.1, -0.5 ), "cm" ) ) +
#              theme( axis.title = element_text( size = 9 ) ) +
#              theme( legend.position = "none" )

#     ggsave( file = paste( "~/Desktop/importanceCombined", slopeTypes[n], p, "_WithScr.png", sep = "" ), plot = vPlot, width = 4, height = 8 )
#     }

  myAov <- aov( Accuracy ~ Pipeline, data = resultsData )
  TukeyHSD( myAov, c( "Pipeline" ) )
  }
