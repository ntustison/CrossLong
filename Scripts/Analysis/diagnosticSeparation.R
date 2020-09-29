library( ggplot2 )
library( dplyr )
library( emmeans )
library( lme4 )
library( lmerTest )
library( knitr )
library( kableExtra )
library( ADNIMERGE )
library( multcomp )

baseDirectory <- '/Users/ntustison/Data/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
figuresDirectory <- paste0( baseDirectory, 'Figures/' )
manuscriptDirectory <- paste0( baseDirectory, 'Manuscript/' )

corticalThicknessPipelineNames <- c( 'FSCross', 'FSLong', 'ANTsCross', 'ANTsNative', 'ANTsSST', 'ANTsXNet' )
numberOfPipelines <- length( corticalThicknessPipelineNames )
numberOfRegions <- 62

diagnosticLevels <- c( "CN", "LMCI", "AD" )
visits <- c( 0, 6, 12, 18, 24, 36 )
visitsCode <- c( "bl", "m06", "m12", "m18", "m24", "m36" )

doTemporalSmoothing <- FALSE

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions )

corticalThicknessData <- list()
demographicsData <- data.frame()
for( i in seq.int( numberOfPipelines ) )
  {
  corticalThicknessCsv <- paste0( dataDirectory, 'reconciled_', corticalThicknessPipelineNames[i], '.csv' )
  cat( "Reading ", corticalThicknessCsv, "\n" )
  corticalThicknessData[[i]] <- read.csv( corticalThicknessCsv )

  if( i == 1 )
    {
    naColumn <- rep( NA, nrow( corticalThicknessData[[i]] ) )
    visitCode <- naColumn
    for( j in 1:length( visits ) )
      {
      visitCode[which( corticalThicknessData[[i]]$VISIT == visits[j] )] <- visitsCode[j]
      }
    demographicsData <- data.frame( corticalThicknessData[[i]][, 1:8],
                                    VISCODE = visitCode,
                                    AGE.bl = naColumn,
                                    GENDER = naColumn,
                                    ICV.bl = naColumn,
                                    DX.bl = naColumn,
                                    SITE = naColumn,
                                    APOE4.bl = naColumn,
                                    ABETA.bl = naColumn,
                                    TAU.bl = naColumn,
                                    AV45.bl = naColumn,
                                    CDRSB = naColumn,
                                    CDRSB.bl = naColumn,
                                    LDELTOTAL = naColumn,
                                    LDELTOTAL.bl = naColumn,
                                    RAVLT.immediate = naColumn,
                                    RAVLT.immediate.bl = naColumn,
                                    MMSE = naColumn,
                                    MMSE.bl = naColumn,
                                    FAQ = naColumn,
                                    FAQ.bl = naColumn,
                                    mPACCdigit = naColumn,
                                    mPACCdigit.bl = naColumn,
                                    mPACCtrailsB = naColumn,
                                    mPACCtrailsB.bl = naColumn
                                  )

    for( j in seq_len( nrow( demographicsData ) ) )
      {
      subjectAdniMergeIndices.bl <-
        which( adnimerge$PTID == demographicsData$ID[j] &
          adnimerge$VISCODE == 'bl' )
      subjectAdniMergeIndices <-
        which( adnimerge$PTID == demographicsData$ID[j] &
          adnimerge$VISCODE == demographicsData$VISCODE[j] )

      demographicsData$AGE.bl[j] <- adnimerge$AGE[subjectAdniMergeIndices.bl]
      demographicsData$GENDER[j] <- adnimerge$PTGENDER[subjectAdniMergeIndices.bl]
      demographicsData$ICV.bl[j] <- adnimerge$ICV.bl[subjectAdniMergeIndices.bl]
      demographicsData$DX.bl[j] <- levels( adnimerge$DX.bl )[adnimerge$DX.bl[subjectAdniMergeIndices.bl]]
      demographicsData$SITE[j] <- adnimerge$SITE[subjectAdniMergeIndices]

      demographicsData$APOE4.bl[j] <- adnimerge$APOE4[subjectAdniMergeIndices.bl]
      demographicsData$ABETA.bl[j] <- adnimerge$ABETA.bl[subjectAdniMergeIndices.bl]
      demographicsData$TAU.bl[j] <- adnimerge$TAU.bl[subjectAdniMergeIndices.bl]
      demographicsData$AV45.bl[j] <- adnimerge$AV45.bl[subjectAdniMergeIndices.bl]
      demographicsData$CDRSB[j] <- adnimerge$CDRSB[subjectAdniMergeIndices]
      demographicsData$CDRSB.bl[j] <- adnimerge$CDRSB.bl[subjectAdniMergeIndices.bl]
      demographicsData$LDELTOTAL[j] <- adnimerge$LDELTOTAL[subjectAdniMergeIndices]
      demographicsData$LDELTOTAL.bl[j] <- adnimerge$LDELTOTAL.bl[subjectAdniMergeIndices.bl]
      demographicsData$RAVLT.immediate[j] <- adnimerge$RAVLT.immediate[subjectAdniMergeIndices]
      demographicsData$RAVLT.immediate.bl[j] <- adnimerge$RAVLT.immediate.bl[subjectAdniMergeIndices.bl]
      demographicsData$MMSE[j] <- adnimerge$MMSE[subjectAdniMergeIndices]
      demographicsData$MMSE.bl[j] <- adnimerge$MMSE.bl[subjectAdniMergeIndices.bl]
      demographicsData$FAQ[j] <- adnimerge$FAQ[subjectAdniMergeIndices]
      demographicsData$FAQ.bl[j] <- adnimerge$FAQ.bl[subjectAdniMergeIndices.bl]
      demographicsData$mPACCdigit[j] <- adnimerge$mPACCdigit[subjectAdniMergeIndices]
      demographicsData$mPACCdigit.bl[j] <- adnimerge$mPACCdigit.bl[subjectAdniMergeIndices.bl]
      demographicsData$mPACCtrailsB[j] <- adnimerge$mPACCtrailsB[subjectAdniMergeIndices]
      demographicsData$mPACCtrailsB.bl[j] <- adnimerge$mPACCtrailsB.bl[subjectAdniMergeIndices.bl]
      }
    demographicsData$dTIME  <- demographicsData$AGE - demographicsData$AGE.bl

    demographicsData$ID <- factor( demographicsData$ID )
    demographicsData$SITE <- factor( demographicsData$SITE )
    demographicsData$GENDER <- factor( demographicsData$GENDER )
    demographicsData$DX.bl <- factor( demographicsData$DX.bl, levels = c( "CN", "LMCI", "AD" ) )
    demographicsData$APOE4.bl <- factor( demographicsData$APOE4.bl )
    }
  }

thicknessColumns <- 9:ncol( corticalThicknessData[[1]] )
thicknessNames <- colnames( corticalThicknessData[[1]] )[thicknessColumns]

# Smooth the data (if requested).  Also, add the baseline thickness measurements and
# delta thickness measurements to the corticalThicknessData data frames

for( p in seq.int( numberOfPipelines ) )
  {
  if( doTemporalSmoothing )
    {
    numberOfSmoothingIterations <- 1
    smoothingSigma <- 5.5

    subjects <- unique( demographicsData$ID )

    for( i in seq_len( numberOfSmoothingIterations ) )
      {
      for( s in seq_len( length( subjects ) ) )
        {
        subjectIndices <- which( demographicsData$ID == subjects[s] )

        if( length( subjectIndices ) > 1 )
          {
          subjectMatrix <- data.matrix( corticalThicknessData[[p]][subjectIndices, thicknessColumns] )
          subjectTime <- matrix( demographicsData$dTIME[subjectIndices], ncol = 1 )
          timeDistance <- as.matrix(
            dist( subjectTime, method = "euclidean", diag = TRUE, upper = TRUE, p = 2 ) )
          gaussianDistance <-
            as.matrix( exp( -1.0 * timeDistance / ( smoothingSigma * sd( subjectMatrix ) ) ) )
          gaussianDistance <- gaussianDistance / colSums( gaussianDistance )
          smoothedThickness <- gaussianDistance %*% subjectMatrix
          corticalThicknessData[[p]][subjectIndices, thicknessColumns] <- smoothedThickness
          }
        }
      }
    }

  baselineThickness <- 0 * corticalThicknessData[[p]][,thicknessColumns]
  colnames( baselineThickness ) <- paste0( thicknessNames, ".bl" )
  for( i in seq_len( nrow( corticalThicknessData[[p]] ) ) )
    {
    subjectIndices <- which( corticalThicknessData[[p]]$ID == corticalThicknessData[[p]]$ID[i] )
    subjectIndices.bl <- which( corticalThicknessData[[p]]$ID == corticalThicknessData[[p]]$ID[i] &
      corticalThicknessData[[p]]$VISIT == 0 )

    if( length( subjectIndices.bl ) == 1 )
      {
      baselineThickness[subjectIndices,] <- corticalThicknessData[[p]][subjectIndices.bl, thicknessColumns]
      }
    }
  deltaThickness <- corticalThicknessData[[p]][, thicknessColumns] - baselineThickness
  colnames( deltaThickness ) <- paste0( "d", thicknessNames )

  corticalThicknessData[[p]] <- cbind( corticalThicknessData[[p]], baselineThickness, deltaThickness )
  }


##########
#
# Create the slope-based diagnostic separation
#
##########

tukeyResultsDataFrame <- data.frame( Pipeline = factor(), Region = factor(),
  Hemisphere = factor(), DiagnosticPair = factor(), Pvalue = double() )

tukeyLeft <- matrix( NA, nrow = 31, ncol = 3 * numberOfPipelines )
tukeyRight <- matrix( NA, nrow = 31, ncol = 3 * numberOfPipelines )

tukeyLeftCI <- matrix( NA, nrow = 31, ncol = 3 * numberOfPipelines )
tukeyRightCI <- matrix( NA, nrow = 31, ncol = 3 * numberOfPipelines )

for( i in seq.int( numberOfPipelines ) )
  {
  cat( "Creating results for ", corticalThicknessPipelineNames[i], "\n" )

  pb <- txtProgressBar( min = 0, max = length( thicknessColumns ), style = 3 )

  for( j in 1:length( thicknessColumns ) )
    {
    corticalThicknessDataFrame <- data.frame( Y = corticalThicknessData[[i]][, thicknessColumns[j]],
                                              Y.bl = corticalThicknessData[[i]][, thicknessColumns[j] + 62],
                                              dY = corticalThicknessData[[i]][, thicknessColumns[j] + 62 + 62],
                                              VISIT = corticalThicknessData[[i]]$VISIT/12,
                                              ID = corticalThicknessData[[i]]$ID,
                                              DX.bl = factor( demographicsData$DX.bl, levels = c( "CN", "LMCI", "AD" ) ),
                                              AGE.bl = demographicsData$AGE.bl,
                                              GENDER = demographicsData$GENDER,
                                              APOE4.bl = demographicsData$APOE4.bl,
                                              SITE = demographicsData$SITE,
                                              ICV.bl = demographicsData$ICV.bl
                                            )
    lmeFormula <- as.formula( dY ~ scale( Y.bl ) + scale( AGE.bl ) + scale( ICV.bl ) + APOE4.bl + GENDER + DX.bl +
      VISIT:DX.bl + ( 1 | ID ) + ( 1 | SITE ) )
    lmeModel <- lmer( lmeFormula, data = corticalThicknessDataFrame, REML = FALSE )
    lmeTukey <- summary(
      glht( lmeModel, linfct = mcp( DX.bl = "Tukey" ), alternative = "less", test = adjusted( "fdr" ) ) )

    # lmeEmmeans <- emmeans( lmeModel, "DX.bl"  )
    # lmePairs <- pairs( lmeEmmeans )

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

    comparisonPairs <- c( "LMCI-CN", "AD-CN", "AD-LMCI" )
    for( k in seq_len( length( comparisonPairs ) ) )
      {
      tukeyResultsDataFrame <- rbind( tukeyResultsDataFrame, data.frame(
        Pipeline = corticalThicknessPipelineNames[i],
        Region = dktRegion,
        Hemisphere = hemisphere,
        DiagnosticPair = comparisonPairs[k],
        Pvalue = lmeTukey$test$pvalues[k] ) )

      col <- ( c( 1, 3, 2 )[k] - 1 ) * numberOfPipelines + i

      lowerBound <- as.numeric( lmeTukey$test$coefficients[k] ) - 1.96 * as.numeric( lmeTukey$test$sigma[k] )
      upperBound <- as.numeric( lmeTukey$test$coefficients[k] ) + 1.96 * as.numeric( lmeTukey$test$sigma[k] )

      if( j > 31 )
        {
        tukeyLeft[row, col] <- as.double( lmeTukey$test$pvalues[k] )
        tukeyLeftCI[row, col] <- paste0( as.character( round( lowerBound, 3 ) ),
          ",", as.character( round( upperBound, 3 ) ) )
        } else {
        tukeyRight[row, col] <- as.double( lmeTukey$test$pvalues[k] )
        tukeyRightCI[row, col] <- paste0( as.character( round( lowerBound, 3 ) ),
          ",", as.character( round( upperBound, 3 ) ) )
        }

      }
    setTxtProgressBar( pb, j )
    }
  cat( "\n" )
  }

tukeyLeftLog10 <- data.frame( cbind( dktBrainGraphRegions[1:31] ), log10( tukeyLeft + 1e-10 ) )
tukeyRightLog10 <- data.frame( cbind( dktBrainGraphRegions[32:62] ), log10( tukeyRight + 1e-10 ) )


## Create box plots

tukeyBoxPlotDataFrame <- data.frame( Pipeline = factor(), Diagnoses = factor(),
  Hemisphere = factor(), Region = factor(), pValues = double() )
for( i in 2:ncol( tukeyLeftLog10 ) )
  {
  whichPipeline <- corticalThicknessPipelineNames[( i - 2 ) %% length( corticalThicknessData ) + 1]

  whichDiagnoses <- comparisonPairs[1]
  if( ( i - 1 ) > numberOfPipelines && ( i - 1 ) <= 2 * numberOfPipelines )
    {
    whichDiagnoses <- comparisonPairs[3]
    } else if( ( i - 1 ) > 2 * numberOfPipelines ) {
    whichDiagnoses <- comparisonPairs[2]
    }

  tukeyBoxPlotDataFrame <- rbind( tukeyBoxPlotDataFrame,
                                  data.frame(
                                    Pipeline = rep( whichPipeline, nrow( tukeyLeftLog10 ) ),
                                    Diagnoses = rep( whichDiagnoses, nrow( tukeyLeftLog10 ) ),
                                    Hemisphere = rep( 'Left', nrow( tukeyLeftLog10 ) ),
                                    Region = dktBrainGraphRegions[1:31],
                                    pValues = -tukeyLeftLog10[,i]
                                  )
                                )

  tukeyBoxPlotDataFrame <- rbind( tukeyBoxPlotDataFrame,
                                  data.frame( 
                                    Pipeline = rep( whichPipeline, nrow( tukeyRightLog10 ) ),
                                    Diagnoses = rep( whichDiagnoses, nrow( tukeyLeftLog10 ) ),
                                    Hemisphere = rep( 'Right', nrow( tukeyRightLog10 ) ),
                                    Region = dktBrainGraphRegions[32:62],
                                    pValues = -tukeyRightLog10[,i]
                                  )
                                )
  }

tukeyBoxPlotDataFrame$Pipeline <- factor( tukeyBoxPlotDataFrame$Pipeline, levels = corticalThicknessPipelineNames )
tukeyBoxPlotDataFrame$Diagnoses <- factor( tukeyBoxPlotDataFrame$Diagnoses, levels = comparisonPairs[c( 1, 3, 2 )] )

boxPlot <- ggplot( data = tukeyBoxPlotDataFrame, aes( x = Pipeline, y = pValues, fill = Hemisphere ) ) +
              geom_boxplot( notch = FALSE ) +
#               scale_fill_manual( "", values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              facet_wrap( ~Diagnoses, scales = 'free', ncol = 3 ) +
              theme( axis.text.x = element_text( face = "bold", size = 10, angle = 45, hjust = 1 ) ) +
              labs( x = '', y = '-log10( pvalues )' )
ggsave( paste0( figuresDirectory, "logPvalues.pdf" ), boxPlot, width = 10, height = 4 )




leftFile <- paste0( manuscriptDirectory, "leftAovTable.tex" )
tukeyLeftLog10 %>%
  # mutate_if( is.numeric, funs( round( ., 2 ) ) ) %>%
  mutate_if( is.numeric, function( x ) {
    cell_spec( x, "latex", bold = F, color = "black",
    background = spec_color( x, begin = 0.65, end = 1.0, option = "B",
      alpha = 0.9, na_color = "#FFFFFF", scale_from = c( -10.0, -1.0 ), direction = 1 ) )
    } ) %>%
  kable( format = "latex", escape = F,
    # col.names = c( "DKT", rep( rownames( tukeyResults ), 5 ) ), linesep = "",
    col.names = c( "DKT", rep( corticalThicknessPipelineNames, 3 ) ), linesep = "",
    align = "c", booktabs = T, caption =
    paste0( "95\\% confidence intervals for the",
            "diagnostic contrasts (LMCI$-$CN, AD$-$LMCI, AD$-$CN) of the ADNI-1 data set for each DKT region ",
            "of the left hemisphere.  Each cell is color-coded based on the adjusted log-scaled $p$-value ",
            "significance from dark orange ($p$ < 1e-10) to yellow ($p$ = 0.1). ",
            "Absence of color denotes nonsignificance." ) ) %>%
  column_spec( 1, bold = T ) %>%
  row_spec( 0, angle = 45, bold = F ) %>%
  kable_styling( position = "center", latex_options = c( "scale_down" ) ) %>%
  # add_header_above( c( " ", "FSCross" = 3, "FSLong" = 3, "ANTsCross" = 3, "ANTsNative" = 3, "ANTsSST" = 3 ), bold = T ) %>%
  add_header_above( c( " ", "LMCI$-$CN" = 5, "AD$-$LMCI" = 5, "AD$-$CN" = 5 ), bold = T ) %>%
  cat( file = leftFile, sep = "\n" )

rightFile <- paste0( manuscriptDirectory, "rightAovTable.tex" )
tukeyRightLog10 %>%
  # mutate_if( is.numeric, funs( round( ., 2 ) ) ) %>%
  mutate_if( is.numeric, function( x ) {
    cell_spec( x, "latex", bold = F, color = "black",
    background = spec_color( x, begin = 0.65, end = 1.0, option = "B",
      alpha = 0.9, na_color = "#FFFFFF", scale_from = c( -10.0, -1.0 ), direction = 1 ) )
    } ) %>%
  kable( format = "latex", escape = F,
    # col.names = c( "DKT", rep( rownames( tukeyResults ), 5 ) ), linesep = "",
    col.names = c( "DKT", rep( corticalThicknessPipelineNames, 3 ) ), linesep = "",
    align = "c", booktabs = T, caption =
    paste0( "95\\% confidence intervals for the",
            "diagnostic contrasts (LMCI$-$CN, AD$-$LMCI, AD$-$CN) of the ADNI-1 data set for each DKT region ",
            "of the right hemisphere.  Each cell is color-coded based on the adjusted log-scaled $p$-value ",
            "significance from dark orange ($p$ < 1e-10) to yellow ($p$ = 0.1). ",
            "Absence of color denotes nonsignificance." ) ) %>%
  column_spec( 1, bold = T ) %>%
  row_spec( 0, angle = 45, bold = F ) %>%
  kable_styling( position = "center", latex_options = c( "scale_down" ) ) %>%
  # add_header_above( c( " ", "FSCross" = 3, "FSLong" = 3, "ANTsCross" = 3, "ANTsNative" = 3, "ANTsSST" = 3 ), bold = T ) %>%
  add_header_above( c( " ", "LMCI$-$CN" = 5, "AD$-$LMCI" = 5, "AD$-$CN" = 5 ), bold = T ) %>%
  cat( file = rightFile, sep = "\n" )

## Now replace the adjusted p-values with the actual confidence
## intervals

leftFile2 <- paste0( manuscriptDirectory, "leftAovTable2.tex" )
rightFile2 <- paste0( manuscriptDirectory, "rightAovTable2.tex" )

inputFiles <- c( leftFile, rightFile )
outputFiles <- c( leftFile2, rightFile2 )

tukeyPairResults <- list( tukeyLeftLog10, tukeyRightLog10 )
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
