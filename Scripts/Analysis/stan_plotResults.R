library( rstan )
library( ggplot2 )

############################################################################################################
#
#     This is the core script for calculating the point estimates of the LME model
#     for cortical thickness across the different pipelines.  The major steps performed
#     below are as follows:
#
#       1.  Check to see if the results file already exists (called 'stan_ResultsAll.csv').  
#       2.  If the file exists, plot the results.
#       3.  If the file does not exist, perform the following steps using Rstan
#           3a.  Read in the cortical thickness .csv files and reconcile the data, i.e. make sure that 
#                the image IDs are identical and in identical order across the pipeline types.  Also,
#                remove any time points from all pipelines which have NA's in one or more pipeline.
#           3b.  Fit the data using Rstan with the model specified in the file 'stan_corticalThicknessModel.stan'
#           3c.  Calculate the quantiles = c( 0.0, 0.25, 0.5, 0.75, 1.00 ), for each pipeline and write to
#                a file (per pipeline).  Also, cbind all the results and write to a file ('stan_ResultsAll.csv')
#           3d.  Plot results. 
#

baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
sandboxDirectory <- paste0( baseDirectory, 'Sandbox/' )
figuresDirectory <- paste0( baseDirectory, 'Figures/' )

corticalThicknessPipelineNames <- c( 'ANTsCross', 'ANTsNative', 'ANTsSST', 'FSCross', 'FSLong'  )
numberOfRegions <- 62


dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

stanAllResultsFile <- paste0( dataDirectory, 'stan_ResultsAll.csv' )

if( file.exists( stanAllResultsFile ) )
  {

  stanResultsAll <- read.csv( stanAllResultsFile )

  } else {

  ##########
  #
  # Read in the data and reconcile based on NA's and overlapping imageIDs
  # 
  ##########

  corticalThicknessCsvs <- list()
  corticalThicknessCsvs[[1]] <- paste0( sandboxDirectory, 'newLongitudinalThicknessCrossSectionalANTs.csv' )
  corticalThicknessCsvs[[2]] <- paste0( sandboxDirectory, 'newLongitudinalThicknessANTsNativeSpace.csv' )
  corticalThicknessCsvs[[3]] <- paste0( sandboxDirectory, 'newLongitudinalThicknessANTsSST.csv' )
  corticalThicknessCsvs[[4]] <- paste0( sandboxDirectory, 'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv' )
  corticalThicknessCsvs[[5]] <- paste0( sandboxDirectory, 'adniLongitudinalFreeSurferMergeSubset_WithScr.csv' )

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

  timePoints <- corticalThicknessData[[4]]$VISIT
  timePoints[which( timePoints == 'bl' | timePoints == 'scr' )] <- 0
  timePoints <- as.numeric( gsub( "[^\\d]+", "", timePoints, perl = TRUE ) )
  timePoints[is.na( timePoints )] <- 0

  for( i in 1:length( corticalThicknessData ) )
    {
    thicknessColumns <- ( ncol( corticalThicknessData[[i]] ) - numberOfRegions + 1 ):ncol( corticalThicknessData[[i]] )
    corticalThicknessData[[i]]$ID <- factor( corticalThicknessData[[i]]$ID )
    corticalThicknessData[[i]] <- data.frame( ID = corticalThicknessData[[i]]$ID, 
      IMAGE_ID = corticalThicknessData[[i]]$IMAGE_ID,
      VISIT = timePoints,
      corticalThicknessData[[i]][,thicknessColumns] )
    corticalThicknessData[[i]] <- 
      corticalThicknessData[[i]][order( corticalThicknessData[[i]]$ID, corticalThicknessData[[i]]$VISIT ),]  
    # write.csv( corticalThicknessData[[i]], quote = FALSE, row.names = FALSE, 
    #            file = paste0( "/Users/ntustison/Desktop/", corticalThicknessPipelineNames[i], ".csv" ) )  
    }

  ##########
  #
  # Calculate the LME and point estimates using Rstan.
  # Compute the quantiles and write results to file.
  # 
  ##########

  stanResultsFiles <- c()
  for( i in 1:length( corticalThicknessCsvs ) )
    {
    stanResultsFiles[i] <- paste0( dataDirectory, 'stan_', corticalThicknessPipelineNames[i], '_Results.csv' )    
    }  

  stanModelFile <- paste0( dataDirectory, 'stan_corticalThicknessModel.stan' )

  stanResults <- list()
  for( i in 1:length( corticalThicknessData ) )
    {
    if( file.exists( stanResultsFiles[i] ) )
      {
      cat( "Reading stan:  ", corticalThicknessPipelineNames[i], "\n" )
      stanResults[[i]] <- read.csv( stanResultsFiles[i] )
      } else {
      cat( "Fitting stan:  ", corticalThicknessPipelineNames[i], "\n" )
      thicknessColumns <- ( ncol( corticalThicknessData[[i]] ) - numberOfRegions + 1 ):ncol( corticalThicknessData[[i]] )
      scaledThickness <- scale( as.matrix( corticalThicknessData[[i]][, thicknessColumns] ) )

      numberOfIndividuals <- length( unique( corticalThicknessData[[i]]$ID ) )
      numberOfObservations <- nrow( corticalThicknessData[[i]] )
      
      ids <- as.numeric( as.factor( corticalThicknessData[[i]]$ID ) )

      stanData <- list( numberOfRegions, numberOfIndividuals, numberOfObservations, 
        timePoints, ids, scaledThickness ) 
      fitStan <- stan( file = stanModelFile, data = stanData, verbose = TRUE )

      fitStanExtracted <- extract( fitStan, permuted = TRUE )

      sigma <- t( apply( fitStanExtracted$sigma, 2, quantile ) )
      colnames( sigma ) <- paste0( 'sigma.', colnames( sigma ) )
      sigmaSd <- apply( fitStanExtracted$sigma, 2, sd )

      tau <- t( apply( fitStanExtracted$tau_0, 2, quantile ) )
      colnames( tau ) <- paste0( 'tau.', colnames( tau ) )
      tauSd <- apply( fitStanExtracted$tau_0, 2, sd )

      varianceRatio <- t( apply( fitStanExtracted$var_ratio, 2, quantile ) )
      colnames( varianceRatio ) <- paste0( 'variance.ratio.', colnames( varianceRatio ) )
      varianceRatioSd <- apply( fitStanExtracted$var_ratio, 2, sd )

      stanResults[[i]] <- data.frame( DktRegion = as.factor( dktBrainGraphRegions ), 
                                      Pipeline = rep( corticalThicknessPipelineNames[i], numberOfRegions ),
                                      sigma, sigma.sd = sigmaSd,
                                      tau, tau.sd = tauSd,
                                      varianceRatio, variance.ratio.sd = varianceRatioSd
                                    )
      write.csv( stanResults[[i]], stanResultsFiles[i], row.names = FALSE )  
      }                              

    if( i == 1 )
      {
      stanResultsAll <- stanResults[[i]]
      } else {
      stanResultsAll <- rbind( stanResultsAll, stanResults[[i]] )
      }                        
    }
  }

write.csv( stanResultsAll, quote = FALSE, row.names = FALSE, 
           file = paste0( dataDirectory, "stan_ResultsAll.csv" ) )  


############################################################################################################
#
#     Plot the results
#
#

sigmaPlot <- ggplot( data = stanResultsAll, aes( y = sigma.50., x = DktRegion, colour = Pipeline, shape = Pipeline ) ) +
              geom_errorbar( aes( ymin = sigma.50. - 1.97 * sigma.sd, ymax = sigma.50. + 1.97 * sigma.sd ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face = "bold", size = 8, angle = 60, hjust = 1 ) ) +
              labs( x = 'Cortical region', y = 'Within-subject variability', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( paste0( figuresDirectory, "sigma_FINALX.png" ), sigmaPlot, width = 10, height = 3 )


tauPlot <- ggplot( data = stanResultsAll, aes( y = tau.50., x = DktRegion, colour = Pipeline, shape = Pipeline ) ) +
              geom_errorbar( aes( ymin = tau.50. - 1.97 * tau.sd, ymax = tau.50. + 1.97 * tau.sd ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face = "bold", size = 8, angle = 60, hjust = 1 ) ) +
              labs( x = 'Cortical region', y = 'Between-subject variability', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( paste0( figuresDirectory, "tau_FINALX.png" ), tauPlot, width = 10, height = 3 )


variance.ratioPlot <- ggplot( data = stanResultsAll, aes( y = variance.ratio.50., x = DktRegion, colour = Pipeline, shape = Pipeline ) ) +
              geom_errorbar( aes( ymin = variance.ratio.50. - 1.97 * variance.ratio.sd, ymax = variance.ratio.50. + 1.97 * variance.ratio.sd ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face = "bold", size = 8, angle = 60, hjust = 1 ) ) +
              labs( x = 'Cortical region', y = 'Variance ratio', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( paste0( figuresDirectory, "variance.ratio_FINALX.png" ), variance.ratioPlot, width = 10, height = 3 )


allDataResults <- data.frame( Pipeline = rep( stanResultsAll$Pipeline, 3 ), 
                              Measurement = factor( c( rep( 1, length( stanResultsAll$Pipeline ) ), 
                                                       rep( 2, length( stanResultsAll$Pipeline ) ), 
                                                       rep( 3, length( stanResultsAll$Pipeline ) ) ) ),
                              X50. = c( stanResultsAll$sigma.50., stanResultsAll$tau.50., stanResultsAll$variance.ratio.50. ) )
levels( allDataResults$Measurement ) <- c( 'Within-subject variability', 'Between-subject variability', 'Variance ratio' ) 
allDataResults <- transform( allDataResults, Pipeline = reorder( Pipeline, X50. ) )

boxPlot <- ggplot( data = allDataResults, aes( x = Pipeline, y = X50., fill = Pipeline ) ) +
              geom_boxplot( notch = FALSE ) +
#               scale_fill_manual( "", values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              facet_wrap( ~Measurement, scales = 'free', ncol = 3 ) +
              theme( legend.position='none' ) +
              theme( axis.text.x = element_text( face="bold", size = 10, angle = 45, hjust = 1 ) ) +
              labs( x = '', y = '' )
ggsave( paste0( figuresDirectory, "allData_FINALX.png" ), boxPlot, width = 10, height = 4 )

