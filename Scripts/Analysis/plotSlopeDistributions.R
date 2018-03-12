library( ggplot2 )
library( randomForest )
library( caret )
library( plyr )
library( nlme )
library( lubridate )
library( e1071 )

# baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
plotDir <- paste0( dataDirectory, '/RegionalThicknessSlopeDistributions/' )

corticalThicknessPipelineNames <- c( 'ANTsCross', 'ANTsNative', 'ANTsSST', 'FSCross', 'FSLong' )
numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

corticalThicknessCsvs <- list()
corticalThicknessData <- list()
for( i in 1:length( corticalThicknessPipelineNames ) )
  {
  corticalThicknessCsvs[[i]] <- paste0( dataDirectory, 'reconciled_', corticalThicknessPipelineNames[i], '.csv' )
  cat( "Reading ", corticalThicknessCsvs[[i]], "\n" )
  corticalThicknessData[[i]] <- read.csv( corticalThicknessCsvs[[i]] )
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
# Plot the distributions
# 
##########

thicknessColumns <- grep( "thickness", colnames( slopeDataList[[1]] ) )

pb <- txtProgressBar( min = 0, max = length( thicknessColumns ), style = 3 )

for( j in 1:length( thicknessColumns ) )
  {
  roiThicknessDataFrame <- data.frame( Diagnosis = factor(), 
    ThicknessSlopes = double(), PipelineType = factor()  )
  for( i in 1:length( slopeDataList ) )
    {
    combinedDiagnosis <- slopeDataList[[i]]$DIAGNOSIS

    pipelineDataFrame <- data.frame(
      Diagnosis = factor( combinedDiagnosis, levels = c( 'CN', 'LMCI', 'AD' ) ),
      ThicknessSlope = slopeDataList[[i]][,thicknessColumns[j]],
      PipelineType = rep( corticalThicknessPipelineNames[i], length( nrow( slopeDataList[[i]] ) ) )
      )
    roiThicknessDataFrame <- rbind( roiThicknessDataFrame, pipelineDataFrame )
    }

  roiThicknessPlot <- ggplot( data = roiThicknessDataFrame ) +
    geom_density( aes( x = ThicknessSlope, fill = Diagnosis ), alpha = 0.5 ) +
    facet_wrap( ~ PipelineType, ncol = 3 ) +
    ggtitle( colnames( slopeDataList[[i]] )[thicknessColumns[j]] ) +
    scale_x_continuous( "Thickness slope" )
  ggsave( paste0( plotDir, '/', colnames( slopeDataList[[i]] )[thicknessColumns[j]], '.pdf' ),
          roiThicknessPlot, width = 8, height = 3, unit = 'in' )

  setTxtProgressBar( pb, j )
  }




