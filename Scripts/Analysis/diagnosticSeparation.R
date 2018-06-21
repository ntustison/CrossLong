library( ggplot2 )
library( dplyr )
library( nlme )
library( lubridate )
library( knitr )
library( kableExtra )
library( ADNIMERGE )

# baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )
manuscriptDirectory <- paste0( baseDirectory, 'Manuscript/' )

corticalThicknessPipelineNames <- c(  'FSCross', 'FSLong', 'ANTsCross', 'ANTsNative', 'ANTsSST' )
numberOfRegions <- 62

diagnosticLevels <- c( "CN", "LMCI", "AD" )

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

pvalueTable <- matrix( data = NA, nrow = numberOfRegions, 
  ncol = length( corticalThicknessPipelineNames ) * length( diagnosticLevels ) )
confidenceIntervalTable <- matrix( data = NA, nrow = numberOfRegions, 
  ncol = length( corticalThicknessPipelineNames ) * length( diagnosticLevels ) )

for( i in 1:length( corticalThicknessData ) )
  {
  subjects <- unique( corticalThicknessData[[i]]$ID )    

  AGE.bl <- c() 
  DX.bl <- c() 
  ICV.bl <- c()
  APOE4 <- c()
  GENDER <- c()

  for( j in 1:length( subjects ) )
    {
    subjectIndices <- which( corticalThicknessData[[i]]$ID == subjects[j] )  
    subjectData <- corticalThicknessData[[i]][subjectIndices,]

    adnimergeIndices <- which( adnimerge$PTID == subjects[j] )
    adnimergeData <- adnimerge[adnimergeIndices,]

    AGE.bl <- append( AGE.bl, rep( subjectData$AGE[1], nrow( subjectData ) ) )
    DX.bl <- append( DX.bl, rep( subjectData$DIAGNOSIS[1], nrow( subjectData ) ) )
    ICV.bl <- append( ICV.bl, rep( adnimergeData$ICV.bl[1], nrow( subjectData ) ) )
    GENDER <- append( GENDER, rep( adnimergeData$PTGENDER[1], nrow( subjectData ) ) )
    APOE4 <- append( APOE4, rep( adnimergeData$APOE4[1], nrow( subjectData ) ) )

    for( k in 2:nrow( subjectData ) )
      {
      span <- interval( ymd( subjectData$EXAM_DATE[1] ), ymd( subjectData$EXAM_DATE[k] ) )
      subjectData$VISIT[k] <- as.numeric( as.period( span ), "years" )
      }
    subjectData$VISIT[1] <- 0.0  
    corticalThicknessData[[i]]$VISIT[subjectIndices] <- subjectData$VISIT
    }

  thicknessColumns <- grep( "thickness", colnames( corticalThicknessData[[i]] ) )
  Y.bl <- matrix( ncol = numberOfRegions, nrow = nrow( corticalThicknessData[[i]] ) )

  for( j in 1:length( subjects ) )
    {
    subjectIndices <- which( corticalThicknessData[[i]]$ID == subjects[j] )  
    subjectData <- corticalThicknessData[[i]][subjectIndices,]
  
    blIndices = which( corticalThicknessData[[i]]$ID == subjects[j] & corticalThicknessData[[i]]$VISIT == 0 )
    for ( k in subjectIndices )
      {
      Y.bl[k,] <- as.numeric( corticalThicknessData[[i]][blIndices, thicknessColumns ] )
      }
    }

  dY = data.matrix( corticalThicknessData[[i]][,thicknessColumns] ) - Y.bl
  pb <- txtProgressBar( min = 0, max = numberOfRegions, style = 3 )
   
  for( j in 1:length( thicknessColumns ) )
    {
    corticalThicknessDataFrame <- data.frame( dY = dY[,j],
                                              Y.bl = Y.bl[,j],
                                              VISIT = corticalThicknessData[[i]]$VISIT/12,
                                              ID = corticalThicknessData[[i]]$ID,
                                              DX.bl = factor( DX.bl, levels = diagnosticLevels ),
                                              AGE.bl = AGE.bl,
                                              GENDER = factor( GENDER ),
                                              ICV.bl = ICV.bl,
                                              APOE4 = APOE4
                                            )
    # lmeModel <- lme( dY ~ VISIT * DX.bl + AGE.bl + APOE4 + GENDER + ICV.bl, 
    #   random = ~ 1 | ID, data = corticalThicknessDataFrame ) 
    lmeModel <- lme( dY ~ VISIT * DX.bl + AGE.bl, 
      random = ~ 1 | ID, data = corticalThicknessDataFrame ) 

    coefficientsDF <- as.data.frame( coef( summary( lmeModel ) ) )
    

    ## Do controls
    row <- which( rownames( coefficientsDF ) == 'VISIT' )

    value <- coefficientsDF$Value[row]
    std.Error <- coefficientsDF$Std.Error[row]
    pvalue <- coefficientsDF$`p-value`[row]
    lowerInterval <- value - 1.96 * std.Error
    upperInterval <- value + 1.96 * std.Error

    index <- ( 1 - 1 ) * length( corticalThicknessPipelineNames ) + i
    pvalueTable[j, index] <- log10( pvalue + 1e-10 )
    confidenceIntervalTable[j, index] <- paste0( as.character( 
      round( lowerInterval, 3 ) ), ",", as.character( 
      round( upperInterval, 3 ) ) )

    ## Do LMCI
    row <- which( rownames( coefficientsDF ) == 'VISIT:DX.blLMCI' )

    value <- coefficientsDF$Value[row]
    std.Error <- coefficientsDF$Std.Error[row]
    pvalue <- coefficientsDF$`p-value`[row]
    lowerInterval <- value - 1.96 * std.Error
    upperInterval <- value + 1.96 * std.Error

    index <- ( 2 - 1 ) * length( corticalThicknessPipelineNames ) + i
    pvalueTable[j, index] <- log10( pvalue + 1e-10 )
    confidenceIntervalTable[j, index] <- paste0( as.character( 
      round( lowerInterval, 3 ) ), ",", as.character( 
      round( upperInterval, 3 ) ) )

    ## Do AD
    row <- which( rownames( coefficientsDF ) == 'VISIT:DX.blAD' )

    value <- coefficientsDF$Value[row]
    std.Error <- coefficientsDF$Std.Error[row]
    pvalue <- coefficientsDF$`p-value`[row]
    lowerInterval <- value - 1.96 * std.Error
    upperInterval <- value + 1.96 * std.Error

    index <- ( 3 - 1 ) * length( corticalThicknessPipelineNames ) + i
    pvalueTable[j, index] <- log10( pvalue + 1e-10 )
    confidenceIntervalTable[j, index] <- paste0( as.character( 
      round( lowerInterval, 3 ) ), ",", as.character( 
      round( upperInterval, 3 ) ) )

    setTxtProgressBar( pb, j )
    }
  cat( "\n" )
  }
  
##
# Create the .tex tables
##

pvalueTableDF <- data.frame( pvalueTable )
pvalueTableDF <- cbind( dktBrainGraphRegions, pvalueTableDF )

leftFile <- paste0( manuscriptDirectory, "leftAovTable.tex" )
pvalueTableDF[1:31,] %>% 
  # mutate_if( is.numeric, funs( round( ., 2 ) ) ) %>%
  mutate_if( is.numeric, function( x ) {
    cell_spec( x, "latex", bold = F, color = "black", 
    background = spec_color( x, begin = 0.65, end = 1.0, option = "B", 
      alpha = 0.9, na_color = "#FFFFFF", scale_from = c( -10.0, -1.0 ), direction = 1 ) )
    } ) %>%
  kable( format = "latex", escape = F, 
    col.names = c( "DKT", rep( corticalThicknessPipelineNames, 3 ) ), linesep = "", 
    align = "c", booktabs = T, caption = 
    paste0( "95\\% confidence intervals for the difference in slope values for the ", 
            "three diagnoses (CN, LMCI, AD) of the ADNI-1 data set for each DKT region ",
            "of the left hemisphere.  Each cell is color-coded based on the adjusted log-scaled $p$-value ",
            "significance from dark orange ($p$ < 1e-10) to yellow ($p$ = 0.1). ",
            "Absence of color denotes nonsignificance." ) ) %>%
  column_spec( 1, bold = T ) %>%
  row_spec( 0, angle = 45, bold = F ) %>%
  kable_styling( position = "center", latex_options = c( "scale_down" ) ) %>%
  add_header_above( c( " ", "CN" = 5, "LMCI" = 5, "AD" = 5 ), bold = T ) %>%
  cat( file = leftFile, sep = "\n" )

rightFile <- paste0( manuscriptDirectory, "rightAovTable.tex" )
pvalueTableDF[32:62,] %>% 
  mutate_if( is.numeric, function( x ) {
    cell_spec( x, "latex", bold = F, color = "black", 
    background = spec_color( x, begin = 0.65, end = 1.0, option = "B", 
      alpha = 0.9, na_color = "#FFFFFF", scale_from = c( -10.0, -1.0 ), direction = 1 ) )
    } ) %>%
  kable( format = "latex", escape = F, 
    col.names = c( "DKT", rep( corticalThicknessPipelineNames, 3 ) ), linesep = "", 
    align = "c", booktabs = T, caption = 
    paste0( "95\\% confidence intervals for the difference in slope values for the ", 
            "three diagnoses (CN, LMCI, AD) of the ADNI-1 data set for each DKT region ",
            "of the right hemisphere.  Each cell is color-coded based on the adjusted log-scaled $p$-value ",
            "significance from dark orange ($p$ < 1e-10) to yellow ($p$ = 0.1). ",
            "Absence of color denotes nonsignificance." ) ) %>%
  column_spec( 1, bold = T ) %>%
  row_spec( 0, angle = 45, bold = F ) %>%
  kable_styling( position = "center", latex_options = c( "scale_down" ) ) %>%
  add_header_above( c( " ", "CN" = 5, "LMCI" = 5, "AD" = 5 ), bold = T ) %>%
  cat( file = rightFile, sep = "\n" )

## Now replace the adjusted p-values with the actual confidence
## intervals

leftFile2 <- paste0( manuscriptDirectory, "leftAovTable2.tex" )
rightFile2 <- paste0( manuscriptDirectory, "rightAovTable2.tex" )

inputFiles <- c( leftFile, rightFile )
outputFiles <- c( leftFile2, rightFile2 )

resultsPvalues <- list( pvalueTableDF[1:31,], pvalueTableDF[32:62,] )
resultsCI <- list( confidenceIntervalTable[1:31,], confidenceIntervalTable[32:62,] )

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
        tokens[j] <- gsub( resultsPvalues[[i]][currentRow, j], 
          resultsCI[[i]][currentRow, j-1], tokens[j], fixed = TRUE )  
        }
      currentRow <- currentRow + 1  
      line <- paste( tokens, collapse = " & ")
      }
    writeLines( line, file2Id )
    }
  close( fileId )
  close( file2Id )
  }


