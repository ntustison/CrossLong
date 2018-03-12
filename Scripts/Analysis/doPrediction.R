library( ggplot2 )
library( plyr )
library( nlme )
library( lubridate )

# baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
plotDir <- paste0( dataDirectory, '/RegionalAgePredictionPlots/' )

corticalThicknessPipelineNames <- c( 'FSCross', 'FSLong', 'ANTsCross', 'ANTsNative', 'ANTsSST'  )
numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

##########
#
# Read the slope files, if they exist.  Otherwise, create them.
# 
##########

slopeFiles <- c()
slopeDataList <- list()
for( i in 1:length( corticalThicknessData ) )
  {
  slopeFiles[i] <- paste0( dataDirectory, 'slopes_', corticalThicknessPipelineNames[i], '.csv' )

  if( ! file.exists( slopeFiles[i] ) )
    {
    cat( "Creating ", slopeFiles[i], "\n" )

    subjects <- unique( corticalThicknessData[[i]]$ID )    
    diagnoses <- rep( NA, length( subjects ) )
    ages <- rep( NA, length( subjects ) )
    mmse.bl <- rep( NA, length( subjects ) )
    cdrsb.bl <- rep( NA, length( subjects ) )
    for( j in 1:length( subjects ) )
      {
      subjectData <- corticalThicknessData[[i]][which( corticalThicknessData[[i]]$ID == subjects[j] ),]
      diagnoses[j] <- subjectData$DIAGNOSIS[1]
      ages[j] <- subjectData$AGE[1]
      mmse.bl[j] <- subjectData$MMSE.bl[1]
      cdrsb.bl[j] <- subjectData$CDRSB.bl[1]

      for( k in 2:nrow( subjectData ) )
        {
        span <- interval( ymd( subjectData$EXAM_DATE[1] ), ymd( subjectData$EXAM_DATE[k] ) )
        subjectData$VISIT[k] <- as.numeric( as.period( span ), "months" )
        }
      subjectData$VISIT[1] <- 0.0  

      corticalThicknessData[[i]]$VISIT[which( corticalThicknessData[[i]]$ID == subjects[j] )] <- subjectData$VISIT
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
    slopeDataList[[i]]$CDRSB.bl <- cdrsb.bl
    slopeDataList[[i]]$MMSE.bl <- mmse.bl

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

nPermutations <- 100

trainingPortions <- c( 0.9 )

slopeTypes <- corticalThicknessPipelineNames

for( p in trainingPortions )
  {
  trainingPortion <- p
  cat( "trainingPortion = ", trainingPortion, "\n", sep = '' )

  predictionDataAll <- data.frame( Hemisphere = character( 0 ), 
    DktRegion = character( 0 ), Pipeline = character( 0 ), Accuracy = numeric( 0 ) )

  trainingIndices <- list()
  for( i in 1:numberOfRegions )
    {

    if( i > 31 )
      {
      hemisphere <- "Right"
      dktRegion <- sub( "r", '', dktBrainGraphRegions[i] )
      } else {
      hemisphere <- "Left"
      dktRegion <- sub( "l", '', dktBrainGraphRegions[i] )
      }

    for( d in 1:length( slopeTypes ) )
      {
      cat( "Pipeline = ", slopeTypes[[d]], "\n" )  

      predictorColumns <- grep( "thickness", colnames( slopeDataList[[d]] ) )[i]

      cat( "i: ", i, colnames( slopeDataList[[d]] )[predictorColumns], ":" , "\n" )
      pb <- txtProgressBar( min = 0, max = nPermutations, style = 3 )
      for( n in seq( 1, nPermutations, by = 1 ) )
        {
        if( d == 1 )
          {
          trainingIndices[[n]] <- createDataPartition( slopeDataList[[1]]$DIAGNOSIS, p = trainingPortion, list = FALSE, times = 1 )
          }

        trainingData <- slopeDataList[[d]][trainingIndices[[n]],]
        trainingData <- trainingData[complete.cases( trainingData ),]
        testingData <- slopeDataList[[d]][-trainingIndices[[n]],]
        testingData <- testingData[complete.cases( testingData ),]

        lmFormula <- as.formula( paste0( 'AGE ~ DIAGNOSIS + ', paste0( colnames( trainingData )[predictorColumns], collapse = '+' )  ) )
        predictedLm <- lm( lmFormula, data = trainingData )
        predicted <- predict( predictedLm, testingData )
        accuracy <- sqrt( sum( ( predicted - testingData$AGE )^2 ) / length( testingData$AGE ) )

        oneData <- data.frame( Hemisphere = hemisphere, DktRegion = dktRegion, Pipeline = slopeTypes[d], Accuracy = accuracy )
        predictionDataAll <- rbind( predictionDataAll, oneData )
        setTxtProgressBar( pb, n, title = paste0( "i = ", i ) )
        }
      cat( "\n" )  
      }

    predictionDataAll$Pipeline <- factor( predictionDataAll$Pipeline, levels = 
      corticalThicknessPipelineNames )
    regionalData <- predictionDataAll[which( predictionDataAll$DktRegion == dktRegion ),]
    rmseBarPlot <- ggplot( regionalData, aes( x = Pipeline, y = Accuracy ) ) +
                  geom_boxplot( aes( fill = Pipeline ), alpha = 0.75, outlier.size = 0.1 ) +
                  scale_y_continuous( "Accuracy" ) +
                  ggtitle( paste0( "Age prediction: ", dktBrainGraphRegions[i] ) ) +
                  theme( legend.position = "none" )
    ggsave( filename = paste0( plotDir, "accuracy.", 
      dktBrainGraphRegions[i], ".png" ), plot = rmseBarPlot, width = 5, height = 3, units = 'in' )
    }
  write.csv( predictionDataAll, paste0( dataDirectory, 'predictionDataAll.csv' ), row.names = FALSE )  

  predictionDataAll$Pipeline <- factor( predictionDataAll$Pipeline, levels = 
    corticalThicknessPipelineNames )
  rmseBarPlot <- ggplot( predictionDataAll, aes( x = DktRegion, y = Accuracy ) ) +
                geom_boxplot( aes( fill = Pipeline ), alpha = 0.75, outlier.size = 0.1 ) +
                  ggtitle( "Age prediction" ) +
                scale_y_continuous( "Prediction error (years)" ) +
                scale_x_discrete( "DKT Region" ) +
                theme( axis.text.x = element_text( face="bold", size = 10, angle = 45, hjust = 1 ) ) +
                facet_wrap( ~ Hemisphere, ncol = 1 )
  ggsave( filename = paste0( plotDir, "accuracyTotal.png" ), plot = rmseBarPlot, 
    width = 12, height = 8, units = 'in' )
  }

 
