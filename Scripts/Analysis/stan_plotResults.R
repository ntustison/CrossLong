library( rstan )
library( lubridate )
library( ggplot2 )

Sys.setenv( TZ = 'America/Los_Angeles' )

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
#           3c.  Calculate the quantiles = c( 0.0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.00 ), for each pipeline 
#                and write to a file (per pipeline).  Also, cbind all the results and write to a file 
#                ('stan_ResultsAll.csv')
#           3d.  Plot results. 
#

baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
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
  # Read in the reconnciled data
  # 
  ##########

  cat( "Reading reconciled data.\n" )

  corticalThicknessCsvs <- list()
  corticalThicknessData <- list()
  for( i in 1:length( corticalThicknessPipelineNames ) )
    {
    corticalThicknessCsvs[[i]] <- paste0( dataDirectory, 'reconciled_', corticalThicknessPipelineNames[i], '.csv' )
    corticalThicknessData[[i]] <- read.csv( corticalThicknessCsvs[[i]] )
    }

  # We renormalize the visits based on exam date

  cat( "Renormalizing visit information based on exam date.\n" )
 
  uniqueSubjectIds <- unique( corticalThicknessData[[1]]$ID )
  thicknessColumns <- grep( "thickness", colnames( corticalThicknessData[[1]] ) )

  pb <- txtProgressBar( min = 0, max = length( uniqueSubjectIds ), style = 3 )

  multipleTimePointSubjectsIds <- c()
  isMultipleTimePointSubject <- rep( 0, nrow( corticalThicknessData[[1]] ) )

  for( j in 1:length( uniqueSubjectIds ) )
    {
    for( i in 1:length( corticalThicknessData ) )
      {
      corticalThicknessDataSubject <- corticalThicknessData[[i]][which( corticalThicknessData[[i]]$ID == uniqueSubjectIds[j] ),]
      corticalThicknessDataSubject <- corticalThicknessDataSubject[order( corticalThicknessDataSubject$VISIT ),]

      if( nrow( corticalThicknessDataSubject ) > 1 )
        {
        for( k in 2:nrow( corticalThicknessDataSubject ) )
          {
          span <- interval( ymd( corticalThicknessDataSubject$EXAM_DATE[1] ), ymd( corticalThicknessDataSubject$EXAM_DATE[k] ) )
          corticalThicknessDataSubject$VISIT[k] <- as.numeric( as.period( span ), "months" )
          }
        if( i == 1 )
          {  
          multipleTimePointSubjectsIds <- append( multipleTimePointSubjectsIds, corticalThicknessDataSubject$ID[1] )  
          isMultipleTimePointSubject[j] <- append( isMultipleTimePointSubject, rep( 1, nrow( corticalThicknessDataSubject ) ) )
          }
        } else {
        if( i == 1 )
          {  
          multipleTimePointSubjectsIds <- append( multipleTimePointSubjectsIds, corticalThicknessDataSubject$ID[1] )  
          isMultipleTimePointSubject[j] <- append( isMultipleTimePointSubject, rep( 0, nrow( corticalThicknessDataSubject ) ) )
          }
        }
      corticalThicknessDataSubject$VISIT[1] <- 0
      corticalThicknessData[[i]][which( corticalThicknessData[[i]]$ID == uniqueSubjectIds[j] ),] <- corticalThicknessDataSubject  
      }
    setTxtProgressBar( pb, j )  
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

      Ni <- length( unique( corticalThicknessData[[i]]$ID ) )
      Nij <- nrow( corticalThicknessData[[i]] )
      Nk <- numberOfRegions
      Na1 <- length( multipleTimePointSubjectsIds )

      Y <- scale( as.matrix( corticalThicknessData[[i]][, thicknessColumns] ) )
      timePoints <- corticalThicknessData[[1]]$VISIT	
      m <- isMultipleTimePointSubject

      ids <- as.numeric( as.factor( corticalThicknessData[[i]]$ID ) )
      slopeIds <- as.numeric( as.factor( multipleTimePointSubjectsIds ) )

      stanData <- list( Ni, Nij, Nk, Na1, Y, timePoints, m, ids, slopeIds ) 
      fitStan <- stan( file = stanModelFile, data = stanData, 
        cores = 16L, verbose = TRUE )

      fitStanExtracted <- extract( fitStan, permuted = TRUE )
 
      probs = c( 0.0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.00 )

      sigma <- t( apply( fitStanExtracted$sigma, 2, quantile, probs ) )
      colnames( sigma ) <- paste0( 'sigma.', colnames( sigma ) )
      sigmaSd <- apply( fitStanExtracted$sigma, 2, sd )

      tau_0 <- t( apply( fitStanExtracted$tau_0, 2, quantile, probs ) )
      colnames( tau_0 ) <- paste0( 'tau0.', colnames( tau_0 ) )
      tau_0Sd <- apply( fitStanExtracted$tau_0, 2, sd )

      tau_1 <- t( apply( fitStanExtracted$tau_1, 2, quantile, probs ) )
      colnames( tau_1 ) <- paste0( 'tau1.', colnames( tau_1 ) )
      tau_1Sd <- apply( fitStanExtracted$tau_1, 2, sd )

      varianceRatio <- t( apply( fitStanExtracted$var_ratio, 2, quantile, probs ) )
      colnames( varianceRatio ) <- paste0( 'variance.ratio.', colnames( varianceRatio ) )
      varianceRatioSd <- apply( fitStanExtracted$var_ratio, 2, sd )

      varianceRatioExp <- t( apply( fitStanExtracted$var_ratio_experimental, 2, quantile, probs ) )
      colnames( varianceRatioExp ) <- paste0( 'variance.ratio.exp.', colnames( varianceRatioExp ) )
      varianceRatioExpSd <- apply( fitStanExtracted$var_ratio_experimental, 2, sd )

      stanResults[[i]] <- data.frame( DktRegion = as.factor( dktBrainGraphRegions ), 
                                      Pipeline = rep( corticalThicknessPipelineNames[i], numberOfRegions ),
                                      sigma, sigma.sd = sigmaSd,
                                      tau_0, tau_0.sd = tau_0Sd,
                                      tau_1, tau_1.sd = tau_1Sd,
                                      varianceRatio, variance.ratio.sd = varianceRatioSd,
                                      varianceRatioExp, variance.ratio.exp.sd = varianceRatioExpSd
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


tauPlot <- ggplot( data = stanResultsAll, aes( y = tau0.50., x = DktRegion, colour = Pipeline, shape = Pipeline ) ) +
              geom_errorbar( aes( ymin = tau0.2.5., ymax = tau0.97.5. ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face = "bold", size = 8, angle = 60, hjust = 1 ) ) +
              labs( x = 'Cortical region', y = 'Between-subject variability', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( paste0( figuresDirectory, "tau_FINALX.png" ), tauPlot, width = 10, height = 3 )


variance.ratioPlot <- ggplot( data = stanResultsAll, aes( y = variance.ratio.50., x = DktRegion, colour = Pipeline, shape = Pipeline ) ) +
              geom_errorbar( aes( ymin = variance.ratio.2.5., ymax = variance.ratio.97.5. ), width = 0.5 ) +
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

