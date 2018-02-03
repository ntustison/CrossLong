library( ggplot2 )
library( randomForest )
library( caret )
library( plyr )
library( nlme )
library( lubridate )
library( e1071 )

############################################################################################################
#
#     This is the core script for using the subject-specific slopes of the longitudinal cortical thickness 
#     for predicting diagnosis.  The major steps performed below are as follows:
#

# baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )

corticalThicknessPipelineNames <- c( 'ANTsCross', 'ANTsNative', 'ANTsSST', 'FSCross', 'FSLong' )
numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

# Get Brian's .csv file
adniNfl <- read.csv( paste0( dataDirectory, "adni_elecsys_csf_nfl.csv" ) )

corticalThicknessCsvs <- list()
corticalThicknessData <- list()
for( i in 1:length( corticalThicknessPipelineNames ) )
  {
  corticalThicknessCsvs[[i]] <- paste0( dataDirectory, 'reconciled_', corticalThicknessPipelineNames[i], '.csv' )
  cat( "Reading ", corticalThicknessCsvs[[i]], "\n" )
  corticalThicknessData[[i]] <- read.csv( corticalThicknessCsvs[[i]] )
  }

cat( "Finding Nfl indices.\n")

pb <- txtProgressBar( min = 1, max = nrow( corticalThicknessData[[1]] ), style = 3 )
for( i in 1:nrow( corticalThicknessData[[1]] ) )
  {

  if( corticalThicknessData[[1]]$VISIT[i] == 0 )
    {
    visitCode <- 'bl'  
    } else if( corticalThicknessData[[1]]$VISIT[i] == 3 ) {
    visitCode <- 'm03'
    } else if( corticalThicknessData[[1]]$VISIT[i] == 6 ) {
    visitCode <- 'm06'
    } else {
    visitCode <- paste0( 'm', corticalThicknessData[[1]]$VISIT[i] )
    }

  indices <- which( adniNfl$PTID == corticalThicknessData[[1]]$ID[i] & adniNfl$VISCODE == visitCode )  
  
  if( length( indices ) == 1 )
    {
    for( j in 1:length( corticalThicknessData ) )  
      {
      corticalThicknessData[[j]]$CDRSB.bl[i] <- adniNfl$CDRSB.bl[indices[1]]
      corticalThicknessData[[j]]$MMSE.bl[i] <- adniNfl$MMSE.bl[indices[1]]
      }
    } else {
    for( j in 1:length( corticalThicknessData ) )  
      {
      corticalThicknessData[[j]]$CDRSB.bl[i] <- NA
      corticalThicknessData[[j]]$MMSE.bl[i] <- NA
      }
    }

  setTxtProgressBar( pb, i )
  }
cat( "\n")  

return;


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

slopeTypes <- c( "ANTsCross", "ANTsNative", "ANTsSST", "FSCross", "FSLong" )

for( p in trainingPortions )
  {
  trainingPortion <- p
  cat( "trainingPortion = ", trainingPortion, "\n", sep = '' )

  predictionDataAll <- data.frame( DktRegion = character( 0 ), Pipeline = character( 0 ), AccuracyMean = numeric( 0 ), AccuracyStd = numeric( 0 ) )

  trainingIndices <- list()
  for( d in 1:length( slopeTypes ) )
    {
    cat( "Pipeline = ", slopeTypes[[d]], "\n" )  
    for( i in 1:numberOfRegions )
      {
      predictorColumns <- sort( grep( "thickness", colnames( slopeDataList[[d]] ) ) )[i]

      resultsData <- data.frame( DktRegion = character( 0 ), Pipeline = character( 0 ), Accuracy = numeric( 0 ) )

      cat( "i: ", i, "\n" )
      pb <- txtProgressBar( min = 0, max = nPermutations, style = 3 )
      for( n in seq( 1, nPermutations, by = 1 ) )
        {
        if( d == 1 )
          {
          trainingIndices[[n]] <- createDataPartition( slopeDataList[[1]]$DIAGNOSIS, p = trainingPortion, list = FALSE, times = 1 )
          }

        # cat( "  Permutation ", n, "\n", sep = '' )

        trainingData <- slopeDataList[[d]][trainingIndices[[n]],]
        trainingData <- trainingData[complete.cases( trainingData ),]
        testingData <- slopeDataList[[d]][-trainingIndices[[n]],]
        testingData <- testingData[complete.cases( testingData ),]

        # lmFormula <- as.formula( paste0( 'MMSE.bl ~ AGE + ', paste0( colnames( trainingData )[predictorColumns], collapse = '+' )  ) )
        lmFormula <- as.formula( paste0( 'AGE ~ DIAGNOSIS + ', paste0( colnames( trainingData )[predictorColumns], collapse = '+' )  ) )
        predictedLm <- lm( lmFormula, data = trainingData )
        predicted <- predict( predictedLm, testingData )

        # accuracy <- sqrt( sum( ( predicted - testingData$MMSE.bl )^2 ) / length( testingData$MMSE.bl ) )
        accuracy <- sqrt( sum( ( predicted - testingData$AGE )^2 ) / length( testingData$AGE ) )

        # cat( "    ", i, ': ', 
        #   slopeTypes[d], ": accuracy = ", accuracy, "\n", sep = '' )
        oneData <- data.frame( Pipeline = slopeTypes[d], Accuracy = accuracy )

        resultsData <- rbind( resultsData, oneData )
        setTxtProgressBar( pb, n, title = paste0( "i = ", i ) )
        }
      cat( "\n" )  
      onePredictionData <- data.frame( DktRegion = dktBrainGraphRegions[i], Pipeline = slopeTypes[d], 
        AccuracyMean = mean( resultsData$Accuracy ), AccuracyStd = sd( resultsData$Accuracy ) )     
      predictionDataAll <- rbind( predictionDataAll, onePredictionData )


      # if( n %% 250 == 0 )
      #   {
      #   rmseBarPlot <- ggplot( resultsData, aes( x = Pipeline, y = Accuracy ) ) +
      #                 geom_boxplot( aes( fill = Pipeline ) ) +
      #                 scale_y_continuous( "Accuracy" ) +
      #                 ggtitle( colnames( trainingData )[predictorColumns] ) +
      #                 theme( legend.position = "none" )
      #   ggsave( filename = paste( "~/Desktop/RoiPlots/accuracy.", 
      #     colnames( trainingData )[predictorColumns], ".png", sep = "" ), plot = rmseBarPlot, width = 8, height = 5, units = 'in' )
      #  }

      }
    }
  write.csv( predictionDataAll, paste0( dataDirectory, 'predictionDataAll.csv' ), row.names = FALSE )  
  }
