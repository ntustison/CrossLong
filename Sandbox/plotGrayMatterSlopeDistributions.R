library( ggplot2 )
library( ADNIMERGE )
library( dplyr )
library( nlme )
library( lubridate )
library( knitr )
library( kableExtra )

# baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
manuscriptDirectory <- paste0( baseDirectory, 'Manuscript/' )
plotDir <- paste0( dataDirectory, '/RegionalVolumeSlopeDistributions/' )

numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

corticalVolumePipelineNames <- c( 'ANTsNative', 'ANTsSST' )

# Read in cortical volume data and reconcile with thickness data demographics

corticalVolumeCsvs <- list()
corticalVolumeData <- list()
for( i in 1:length( corticalVolumePipelineNames ) )
  {
  corticalVolumeCsvs[[i]] <- paste0( dataDirectory, 'adniGrayMatterVolumesDkt', corticalVolumePipelineNames[i], '.csv' )
  cat( "Reading ", corticalVolumeCsvs[[i]], "\n" )
  corticalVolumeDataAll <- read.csv( corticalVolumeCsvs[[i]] )
  corticalVolumeDataAll$image.id <- sub( "I", "", corticalVolumeDataAll$image.id )

  volumeColumns <- grep( "volume", colnames( corticalVolumeDataAll ) )

  corticalThicknessCsv <- paste0( dataDirectory, 'reconciled_', corticalVolumePipelineNames[i], '.csv' )
  corticalThicknessData <- read.csv( corticalThicknessCsv )

  thicknessColumns <- grep( "thickness", colnames( corticalThicknessData ) )

  reconciledImageIds <- intersect( corticalThicknessData$IMAGE_ID, corticalVolumeDataAll$image.id )
  reconciledIndices <- match( reconciledImageIds, corticalVolumeDataAll$image.id )

  corticalVolumeData[[i]] <- cbind( corticalThicknessData[, -thicknessColumns], 
    corticalVolumeDataAll[reconciledIndices, volumeColumns] )
  }

##########
#
# Read the slope files, if they exist.  Otherwise, create them.
# 
##########

slopeFiles <- c()
slopeDataList <- list()
for( i in 1:length( corticalVolumeData ) )
  {
  slopeFiles[i] <- paste0( dataDirectory, 'slopesVolumes_', corticalVolumePipelineNames[i], '.csv' )

  if( ! file.exists( slopeFiles[i] ) )
    {
    cat( "Creating ", slopeFiles[i], "\n" )

    subjects <- unique( corticalVolumeData[[i]]$ID )    
    diagnoses <- rep( NA, length( subjects ) )
    ages <- rep( NA, length( subjects ) )

    for( j in 1:length( subjects ) )
      {
      subjectData <- corticalVolumeData[[i]][which( corticalVolumeData[[i]]$ID == subjects[j] ),]
      diagnoses[j] <- subjectData$DIAGNOSIS[1]
      ages[j] <- subjectData$AGE[1]

      for( k in 2:nrow( subjectData ) )
        {
        span <- interval( ymd( subjectData$EXAM_DATE[1] ), ymd( subjectData$EXAM_DATE[k] ) )
        subjectData$VISIT[k] <- as.numeric( as.period( span ), "months" )
        }
      subjectData$VISIT[1] <- 0.0  

      corticalVolumeData[[i]]$VISIT[which( corticalVolumeData[[i]]$ID == subjects[j] )] <- subjectData$VISIT
      }

    volumeColumns <- grep( "volume", colnames( corticalVolumeData[[i]] ) )
    slopeDataList[[i]] <- matrix( NA, nrow = length( subjects ), ncol = length( volumeColumns ) )

    pb <- txtProgressBar( min = 0, max = ncol( slopeDataList[[i]] ), style = 3 )

    for( j in 1:length( volumeColumns ) )
      {
      corticalVolumeDataFrame <- data.frame( Y = corticalVolumeData[[i]][, volumeColumns[j]], 
                                             VISIT = corticalVolumeData[[i]]$VISIT,
                                             ID = corticalVolumeData[[i]]$ID )
      lmeModel <- lme( Y ~ VISIT, random = ~ VISIT - 1 | ID, data = corticalVolumeDataFrame )

      slopeDataList[[i]][, j] <- as.numeric( lmeModel$coefficients$fixed[2]) + as.vector( lmeModel$coefficients$random[[1]][,1] )
      setTxtProgressBar( pb, j )
      }
    cat( "\n" )

    slopeDataList[[i]] <- as.data.frame( slopeDataList[[i]] )
    colnames( slopeDataList[[i]] ) <- colnames( corticalVolumeData[[i]] )[volumeColumns]
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
# Plot the distributions
# 
##########

volumeColumns <- grep( "volume", colnames( slopeDataList[[1]] ) )

roiVolumeDataFrame <- data.frame( Pipeline = factor(), Diagnosis = factor(),
  VolumeSlope = double(), Region = factor() )
for( i in 1:length( slopeDataList ) )
  {
  for( j in 1:length( volumeColumns ) )
    {
    combinedDiagnosis <- slopeDataList[[i]]$DIAGNOSIS

    if( j > 31 )
      {
      hemisphere <- rep( "Right", length( combinedDiagnosis ) )
      region <- sub( "r", '', dktBrainGraphRegions[j] )
      } else {
      hemisphere <- rep( "Left", length( combinedDiagnosis ) )
      region <- sub( "l", '', dktBrainGraphRegions[j] )
      }

    singleDataFrame <- data.frame( 
      Pipeline = factor( corticalVolumePipelineNames[i], levels = corticalVolumePipelineNames ),
      Diagnosis = factor( slopeDataList[[i]]$DIAGNOSIS, levels = c( 'CN', 'LMCI', 'AD' ) ),
      VolumeSlope = slopeDataList[[i]][, volumeColumns[j]], 
      Region = rep( region, length( slopeDataList[[i]]$DIAGNOSIS ) ),
      Hemisphere = hemisphere )
    roiVolumeDataFrame <- rbind( roiVolumeDataFrame, singleDataFrame )
    }
  volumePlot <- ggplot( data = 
    roiVolumeDataFrame[which( roiVolumeDataFrame$Pipeline == corticalVolumePipelineNames[i] ),] ) +
    geom_boxplot( aes( y = VolumeSlope, x = Region, fill = Diagnosis ), 
      alpha = 0.75, outlier.size = 0.1 ) +
    ggtitle( corticalVolumePipelineNames[i] ) +
    scale_y_continuous( "Volume slope", limits = c( -0.05, 0.02 ) ) +
    scale_x_discrete( "Cortical region" ) +
    coord_flip() +
    facet_wrap( ~Hemisphere, ncol = 2 )
  ggsave( paste0( plotDir, '/', corticalVolumePipelineNames[i], 'Volume.pdf' ),
          volumePlot, width = 6, height = 8, unit = 'in' )
  }

tukeyResultsDataFrame <- data.frame( Pipeline = factor(), Region = factor(), 
  Hemisphere = factor(), DiagnosticPair = factor(), Pvalue = double() )

tukeyLeft <- matrix( NA, nrow = 31, ncol = 3 * length( corticalVolumePipelineNames ) )
tukeyRight <- matrix( NA, nrow = 31, ncol = 3 * length( corticalVolumePipelineNames ) )

tukeyLeftCI <- matrix( NA, nrow = 31, ncol = 3 * length( corticalVolumePipelineNames ) )
tukeyRightCI <- matrix( NA, nrow = 31, ncol = 3 * length( corticalVolumePipelineNames ) )

for( i in 1:length( slopeDataList ) )
  {
  for( j in 1:length( volumeColumns ) )
    {
    if( j > 31 )
      {
      hemisphere <- "Right"
      dktRegion <- sub( "r", '', dktBrainGraphRegions[j] )
      row <- j - 31
      } else {
      hemisphere <- "Left"
      dktRegion <- sub( "l", '', dktBrainGraphRegions[j] )
      row <- j
      }

    indices <- which( roiVolumeDataFrame$Hemisphere == hemisphere & 
      roiVolumeDataFrame$Region == dktRegion & 
      roiVolumeDataFrame$Pipeline == corticalVolumePipelineNames[i] )
    regionalDataFrame <- roiVolumeDataFrame[indices,]  

    fitLm <- lm( formula = "VolumeSlope ~ Diagnosis", 
      data = regionalDataFrame )
    anovaResults <- aov( fitLm )
    tukeyResults <- as.data.frame( TukeyHSD( anovaResults )$Diagnosis )

    for( k in 1:nrow( tukeyResults ) )
      {
      tukeyResultsDataFrame <- rbind( tukeyResultsDataFrame, data.frame( 
        Pipeline = corticalVolumePipelineNames[i],
        Region = dktRegion,
        Hemisphere = hemisphere,
        DiagnosticPair = rownames( tukeyResults )[k],
        Pvalue = tukeyResults$`p adj`[k] ) )

      col <- ( i - 1 ) * 3 + k
      if( j > 31 )
        {
        tukeyLeft[row, col] <- as.double( tukeyResults$`p adj`[k] )
        tukeyLeftCI[row, col] <- paste0( as.character( round( tukeyResults$lwr[k], 3 ) ), 
          ",", as.character( round( tukeyResults$upr[k], 3 ) ) )
        } else {
        tukeyRight[row, col] <- as.double( tukeyResults$`p adj`[k] )
        tukeyRightCI[row, col] <- paste0( as.character( round( tukeyResults$lwr[k], 3 ) ), 
          ",", as.character( round( tukeyResults$upr[k], 3 ) ) ) 
        }
      }  
    }
  }  

tukeyLeft <- data.frame( cbind( dktBrainGraphRegions[1:31] ), tukeyLeft )
tukeyRight <- data.frame( cbind( dktBrainGraphRegions[32:62] ), tukeyRight )







leftFile <- paste0( manuscriptDirectory, "leftAovTableVolume.tex" )
tukeyLeft %>% 
  # mutate_if( is.numeric, funs( round( ., 2 ) ) ) %>%
  mutate_if( is.numeric, function( x ) {
    cell_spec( x, "latex", bold = F, color = "black", 
    background = spec_color( x, begin = 0.65, end = 1.0, option = "B", 
      alpha = 0.9, na_color = "#FFFFFF", scale_from = c( 0.0, 0.1 ), direction = 1 ) )
    } ) %>%
  kable( format = "latex", escape = F, 
    col.names = c( "DKT", rep( rownames( tukeyResults ), length( corticalVolumePipelineNames ) ) ), linesep = "", 
    align = "c", booktabs = T, caption = 
    paste0( "95\\% confidence intervals for the difference in slope values for the ", 
            "three diagnoses (CN, LMCI, AD) of the ADNI-1 data set for each DKT region ",
            "of the left hemisphere.  Each cell is color-coded based on the adjusted $p$-value ",
            "significance from dark orange ($p < 1\\mathrm{e}-5$) to yellow ($p$ = 0.1). ",
            "Absence of color denotes nonsignificance." ) ) %>%
  column_spec( 1, bold = T ) %>%
  row_spec( 0, angle = 45, bold = F ) %>%
  kable_styling( position = "center", latex_options = c( "scale_down" ) ) %>%
  add_header_above( c( " ", "ANTsNative" = 3, "ANTsSST" = 3 ), bold = T ) %>%
  cat( file = leftFile, sep = "\n" )

rightFile <- paste0( manuscriptDirectory, "rightAovTableVolume.tex" )
tukeyRight %>% 
  # mutate_if( is.numeric, funs( round( ., 2 ) ) ) %>%
  mutate_if( is.numeric, function( x ) {
    cell_spec( x, "latex", bold = F, color = "black", 
    background = spec_color( x, begin = 0.65, end = 1.0, option = "B", 
      alpha = 0.9, na_color = "#FFFFFF", scale_from = c( 0.0, 0.1 ), direction = 1 ) )
    } ) %>%
  kable( format = "latex", escape = F, 
    col.names = c( "DKT", rep( rownames( tukeyResults ), length( corticalVolumePipelineNames ) ) ), linesep = "", 
    align = "c", booktabs = T, caption = 
    paste0( "95\\% confidence intervals for the difference in slope values for the ", 
            "three diagnoses (CN, LMCI, AD) of the ADNI-1 data set for each DKT region ",
            "of the right hemisphere.  Each cell is color-coded based on the adjusted $p$-value ",
            "significance from dark orange ($p < 1\\mathrm{e}-5$) to yellow ($p$ = 0.1). ",
            "Absence of color denotes nonsignificance." ) ) %>%
  column_spec( 1, bold = T ) %>%
  row_spec( 0, angle = 45, bold = F ) %>%
  kable_styling( position = "center", latex_options = c( "scale_down" ) ) %>%
  add_header_above( c( " ", "ANTsNative" = 3, "ANTsSST" = 3 ), bold = T ) %>%
  cat( file = rightFile, sep = "\n" )

## Now replace the adjusted p-values with the actual confidence
## intervals

leftFile2 <- paste0( manuscriptDirectory, "leftAovTableVolume2.tex" )
rightFile2 <- paste0( manuscriptDirectory, "rightAovTableVolume2.tex" )

inputFiles <- c( leftFile, rightFile )
outputFiles <- c( leftFile2, rightFile2 )

tukeyPairResults <- list( tukeyLeft, tukeyRight )
tukeyPairResultsCI <- list( tukeyLeftCI, tukeyRightCI )

for( i in 1:2 )
  {
  fileId <- file( inputFiles[i], "r" )
  file2Id <- file( outputFiles[i], "w" )

  currentRow <- 1
  fileRow <- 0
  while( TRUE )
    {
    line <- readLines( fileId, n = 1 )
    if( length( line ) == 0 ) 
      {
      break  
      }

    fileRow <- fileRow + 1
    if( fileRow >= 11 && fileRow <= 41 )
      {
      tokens <- unlist( strsplit( line, '&' ) )
      for( j in 2:length( tokens ) )
        {
        tokens[j] <- gsub( tukeyPairResults[[i]][currentRow, j], 
          tukeyPairResultsCI[[i]][currentRow, j-1], tokens[j], fixed = TRUE )  
        }
      currentRow <- currentRow + 1  
      line <- paste( tokens, collapse = " & ")
      }
    writeLines( line, file2Id )
    }
  close( fileId )
  close( file2Id )
  }

