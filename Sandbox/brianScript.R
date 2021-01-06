library( ADNIMERGE )
library( lme4 )
library( lmerTest )
library( pheatmap )


# baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
manuscriptDirectory <- paste0( baseDirectory, 'Manuscript/' )

corticalThicknessPipelineNames <- c(  'FSCross', 'FSLong', 'ANTsCross', 'ANTsNative', 'ANTsSST' )
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
for( i in 1:length( corticalThicknessPipelineNames ) )
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

for( p in seq_len( length( corticalThicknessPipelineNames ) ) )
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
          gaussianDistance <- gaussianDistance / rowSums( gaussianDistance )  
          smoothedThickness <- gaussianDistance %*% subjectMatrix
          }
        }
      }
    corticalThicknessData[[p]][,thicknessColumns] <- smoothedThickness
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

##
#  Generate the PACC measurements
#

adniMergeIndices.bl <- which( adnimerge$VISCODE == 'bl' )
sd.LDELTOTAL.bl <- sd( adnimerge$LDELTOTAL.bl[adniMergeIndices.bl], na.rm = TRUE )
sd.mPACCdigit.bl <- sd( adnimerge$mPACCdigit.bl[adniMergeIndices.bl], na.rm = TRUE )
sd.RAVLT.immediate.bl <- sd( adnimerge$RAVLT.immediate.bl[adniMergeIndices.bl], na.rm = TRUE )
# sd.mPACCtrailsB.bl <- sd( adnimerge$mPACCtrailsB.bl[adniMergeIndices.bl], na.rm = TRUE )
# sd.MMSE.bl <- sd( adnimerge$MMSE.bl[adniMergeIndices.bl], na.rm = TRUE )

demographicsData$PACC <- demographicsData$LDELTOTAL / sd.LDELTOTAL.bl +
  demographicsData$mPACCdigit / sd.mPACCdigit.bl +
  demographicsData$RAVLT.immediate / sd.RAVLT.immediate.bl
demographicsData$PACC.bl <- demographicsData$LDELTOTAL.bl / sd.LDELTOTAL.bl +
  demographicsData$mPACCdigit.bl / sd.mPACCdigit.bl +
  demographicsData$RAVLT.immediate.bl / sd.RAVLT.immediate.bl
demographicsData$dPACC <- demographicsData$PACC - demographicsData$PACC.bl


##
#
# Create the linear models 
#

draw_colnames_45 <- function (coln, gaps, ...) {
    coord <- pheatmap:::find_coordinates(length(coln), gaps)
    x     <- coord$coord - 0.5 * coord$size
    res   <- grid::textGrob(
      coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"),
      vjust = 0.75, hjust = 1, rot = 45, gp = grid::gpar(...)
    )
    return(res)
}
assignInNamespace(
  x = "draw_colnames",
  value = "draw_colnames_45",
  ns = asNamespace( "pheatmap" )
)

anovaValuesData <- list()
pValuesData <- list()
tValuesData <- list()

for( p in seq_len( length( corticalThicknessPipelineNames ) ) )
  {
  cat( "Doing", corticalThicknessPipelineNames[p], "\n" )
  pb <- txtProgressBar( min = 0, max = length( thicknessNames ), style = 3 )

  anovaValuesData[[p]] <- data.frame( 
    ThicknessRegions = sub( "thickness.", "", thicknessNames ), 
    Values = rep( NA, length( thicknessNames ) ) )
  pValuesData[[p]] <- data.frame( matrix( nrow = length( thicknessNames ), ncol = 3 ) )
  colnames( pValuesData[[p]] ) <- c( "CN", "LMCI", "AD" )
  rownames( pValuesData[[p]] ) <- sub( "thickness.", "", thicknessNames )
  tValuesData[[p]] <- data.frame( matrix( nrow = length( thicknessNames ), ncol = 3 ) )
  colnames( tValuesData[[p]] ) <- c( "CN", "LMCI", "AD" )
  rownames( tValuesData[[p]] ) <- sub( "thickness.", "", thicknessNames )

  randomEffects <- '(1|ID) + (1|SITE)'

  for( name in thicknessNames )
    {
    baselineFormulaString <- paste0( "scale( dPACC ) ~ ", paste0( name ), 
      " + scale( AGE.bl ) + scale( ICV.bl ) + APOE4.bl * GENDER + ",
      "scale( PACC.bl ) + stats::poly( dTIME, 2 ) + " )
    baselineFormula2String <- paste0( "scale( dPACC ) ~ ", paste0( name ), 
      " + scale( AGE.bl ) + scale( ICV.bl ) + APOE4.bl * GENDER + ",
      "scale( PACC.bl ) + stats::poly( dTIME, 2 ) + ", paste0( "d", name ), " + " )
    deltaFormulaString <- paste0( paste0( "d", name ), " ~ scale( ", paste0( name, ".bl" ),
      ") + scale( AGE.bl ) + scale( ICV.bl ) + APOE4.bl * GENDER + DX.bl + ",
      "scale( dTIME ):DX.bl + " )  
    deltaFormulaString2 <- paste0( "scale( dPACC ) ~ ", 
      "scale( ", paste0( "d", name ), " )",
      " + scale( AGE.bl ) + scale( ICV.bl ) + APOE4.bl * GENDER + DX.bl + ",
      "scale( dTIME ):DX.bl + " )  

    baselineFormula <- as.formula( paste( baselineFormulaString, randomEffects ) )
    baselineFormula2 <- as.formula( paste( baselineFormula2String, randomEffects ) )
    deltaFormula <- as.formula( paste( deltaFormulaString2, randomEffects ) )

    combinedData <- cbind( demographicsData, corticalThicknessData[[p]] )

    lm.bl <- lmer( baselineFormula, data = combinedData, REML = FALSE )
    lm.bl2 <- lmer( baselineFormula2, data = combinedData, REML = FALSE )
    lm.delta <- lmer( deltaFormula, data = combinedData, REML = FALSE )

    lmCoefficients <- coefficients( summary( lm.delta ) )

    roiName <- sub( "thickness.", "", name )
    pValuesData[[p]][roiName,] <- 
      as.numeric( lmCoefficients[grep( "dTIME", rownames( lmCoefficients ) ), "Pr(>|t|)"] )
    tValuesData[[p]][roiName,] <- 
      as.numeric( lmCoefficients[grep( "dTIME", rownames( lmCoefficients ) ), "t value"] )

    anovaValuesData[[p]]$Values[anovaValuesData[[p]]$ThicknessRegions == roiName] <- 
      anova( lm.bl, lm.bl2 )$Pr[2]

    setTxtProgressBar( pb, which( name == thicknessNames ) )
    }
  cat( "\n" )

  titleString <- corticalThicknessPipelineNames[p]
  pheatmap( log( pValuesData[[p]] ), cluster_rows = FALSE, cluster_cols = FALSE, main = titleString,
    filename = paste0( manuscriptDirectory, "../Figures/", titleString, "_logpvaluesPACC.pdf" ), 
    width = 4, height = 8 )
  pheatmap( tValuesData[[p]], cluster_rows = FALSE, cluster_cols = FALSE, main = titleString,
    filename = paste0( manuscriptDirectory, "../Figures/", titleString, "_tvaluesPACC.pdf" ), 
    width = 4, height = 8 )
  }  

##
#
# Make violin plots
#
#

gg_color_hue <- function(n) {
  hues = seq( 15, 375, length = n + 1 )
  hcl( h = hues, l = 65, c = 100 )[1:n]
}

ttestComparisons <- list()

for( dx in c( "CN", "LMCI", "AD" ) )
  {
  significanceCount <- rep( 0, length( pValuesData ) ) 
  logQValues <- list() 
  for( i in seq_len( length( pValuesData ) ) )
    {
    logQValues[[i]] <- p.adjust( pValuesData[[i]][, dx], 'holm' )
    logQValues[[i]][which( logQValues[[i]] > 0.1 )] <- NA
    logQValues[[i]] <- log10( logQValues[[i]] )
    significanceCount[i] <- sum( !is.na( logQValues[[i]] ) )
    }

  violinDataFrame <- data.frame( 
    Pipeline = c( 
      rep( corticalThicknessPipelineNames[1], numberOfRegions ),
      rep( corticalThicknessPipelineNames[2], numberOfRegions ),
      rep( corticalThicknessPipelineNames[3], numberOfRegions ),
      rep( corticalThicknessPipelineNames[4], numberOfRegions ),
      rep( corticalThicknessPipelineNames[5], numberOfRegions )
      ), 
    logQValues = c( 
      logQValues[[1]], logQValues[[2]], logQValues[[3]], logQValues[[4]], logQValues[[5]]
      )
    )
  dxPlot <- ggplot( violinDataFrame, 
      aes( x = Pipeline, y = logQValues, color = Pipeline, fill = Pipeline ) ) + 
    geom_violin() +
    geom_dotplot( binaxis = 'y', stackdir = 'center', position = position_dodge(1), 
      fill = 'white', dotsize = 1.0 ) +
    theme( legend.position = "none" ) + 
    scale_y_continuous( name = "Log-scaled adjusted p-values" ) +
    scale_fill_manual( values = c( gg_color_hue( 5 ) ) ) +
   theme( axis.text.x = element_text( face = "bold", size = 10, angle = 45, hjust = 1 ) ) +
    ggtitle( paste0( 'Significance map by pipeline: ', dx ) )
  ggsave( paste0( manuscriptDirectory, "../Figures/PACC_", dx, ".pdf" ), plot = dxPlot,
    width = 12, height = 6, units = 'in' )


  ttestComparisons <- matrix( NA, nrow = 5, ncol = 5 )
  for( i in seq_len( nrow( ttestComparisons ) ) )
    {
    for( j in seq_len( ncol( ttestComparisons ) ) )
      {
      ttestComparisons[i, j] <- t.test( tValuesData[[i]][, dx], 
        tValuesData[[j]][, dx], alternative = 'less', 
        paired = TRUE )$p.value
      }  
    }
  ttestComparisons[which( ttestComparisons > 0.1 )] <- NA  

  titleString <- paste0( "Change in PACC prediction in ", dx, " cohort" )
  pheatmap( log10( ttestComparisons ), cluster_rows = FALSE, cluster_cols = FALSE, main = titleString,
    filename = paste0( manuscriptDirectory, "../Figures/PACC", "_", dx, "_ttest.pdf" ), 
    width = 6.25, height = 6, breaks = seq( -20, 0, by = 1 ),
    labels_row = corticalThicknessPipelineNames, labels_col = corticalThicknessPipelineNames )
  }  

