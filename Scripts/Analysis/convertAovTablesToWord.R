library( flextable )
library( officer )

baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
manuscriptDirectory <- paste0( baseDirectory, 'Manuscript/JAD/' )
dataDirectory <- paste0( baseDirectory, 'Data/' )

numberOfRegions <- 62

corticalThicknessPipelineNames <- c( 'FSCross', 'FSLong', 'ANTsCross', 'ANTsNative', 'ANTsSST' )
diagnosticLevels <- c( "CN", "LMCI", "AD" )

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions )

leftFile2 <- paste0( manuscriptDirectory, "leftAovTable2.tex" )
rightFile2 <- paste0( manuscriptDirectory, "rightAovTable2.tex" )

leftFileDoc <- paste0( manuscriptDirectory, "leftAovTable2.docx" )
rightFileDoc <- paste0( manuscriptDirectory, "rightAovTable2.docx" )

inputFiles <- c( leftFile2, rightFile2 )

# numberOfCells <- 5 * 3 * 0.5 * numberOfRegions
# testData <- as.data.frame( matrix( runif( numberOfCells ), nrow = 0.5 * numberOfRegions ) )
# testData <- cbind( dktBrainGraphRegions[1:(0.5*numberOfRegions)], testData )
# colnames( testData ) <- colKeys

regionalData <- list()
bgColorsData <- list()

for( n in seq_len( length( inputFiles ) ) )
  {
  regionalData[[n]] <- data.frame( matrix( ncol = 5 * 3 + 1, nrow = 0.5 * numberOfRegions ), stringsAsFactors = FALSE )
  bgColorsData[[n]] <- data.frame( matrix( ncol = 5 * 3 + 1, nrow = 0.5 * numberOfRegions ), stringsAsFactors = FALSE )

  fileId <- file( inputFiles[n], "r" )

  line <- readLines( fileId, n = 10 )

  for( i in seq_len( nrow( bgColorsData[[n]] ) ) )
    {
    line <- readLines( fileId, n = 1 )
    line <- gsub( ' ', '', line )
    line <- gsub('\\', '', line, fixed = TRUE )
    line <- gsub("\\[|\\]", "", line )
    line <- gsub( 'cellcolor\\[HTML\\]', '', line )
    line <- gsub( 'textcolor\\{black\\}', '', line )
    line <- gsub( 'cellcolorHTML', '', line )

    tokens <- unlist( strsplit( line, '&' ) )

    for( j in seq_len( ncol( bgColorsData[[n]] ) ) )
      {
      if( j == 1 )
        {
        bgColorsData[[n]][i, j] <- "#FFFFFF"
        regionalData[[n]][i, j] <- tokens[1]
        } else {
        cellElement <- tokens[j]

        cellTokens <- unlist( strsplit( cellElement, '\\}\\{\\{' ) )
        cellTokens <- gsub("\\{", "", cellTokens )
        cellTokens <- gsub("\\}", "", cellTokens )

        bgColorsData[[n]][i, j] <- paste0( "#", cellTokens[1] )
        regionalData[[n]][i, j] <- cellTokens[2]
        }
      }
    }
  close( fileId )
  }


outputFiles <- c( leftFileDoc, rightFileDoc )

colKeys <- c( 'DKT', paste0( corticalThicknessPipelineNames, 1 ),
                     paste0( corticalThicknessPipelineNames, 2 ),
                     paste0( corticalThicknessPipelineNames, 3 ))

tableMapping <- data.frame( col_keys = colKeys,
                            colB = c( "", rep( "LMCI-CN", 5 ), rep( "AD-LMCI", 5 ), rep( "AD-CN", 5 ) ),
                            colA = c( 'DKT', rep( corticalThicknessPipelineNames, 3 ) ) )

for( n in 1:2 )
  {
  testData <- regionalData[[n]]
  colnames( testData ) <- colKeys

  ft <- flextable( testData, col_keys = colKeys )
  ft <- set_header_df(ft, mapping = tableMapping, key = "col_keys" )
  ft <- merge_h(ft, part = "header")
  ft <- fontsize( ft, size = 10, part = "all" )

  ft <- hline_top( ft, border = fp_border(width = 2 ), j = 1:16, part = 'body' )
  ft <- hline_top( ft, border = fp_border(width = 2 ), j = 1:16, part = 'header' )
  ft <- hline( ft, border = fp_border(width = 2 ), j = 2:16, part = 'header' )
  ft <- align( ft, align = "center", part = 'header' )
  ft <- bold( ft, bold = TRUE, part = "header" )
  ft <- bold( ft, j = 1, bold = TRUE, part = "body" )

  ft <- autofit(ft)

  for( i in seq_len( nrow( bgColorsData[[n]] ) ) )
    {
    for( j in seq_len( ncol( bgColorsData[[n]] ) ) )
      {
      ft <- bg( ft, i = i, j = j, bg = bgColorsData[[n]][i, j], part = "body" )
      }
    }

  doc <- read_docx()
  doc <- body_add_flextable( doc, value = ft )

  print( doc, target = outputFiles[n] )
  }
