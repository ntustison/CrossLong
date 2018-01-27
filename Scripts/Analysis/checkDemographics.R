library( ADNIMERGE )
library( XML )

xmlDirectory <- "/Users/ntustison/Desktop/XmlFiles/"

baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )

corticalThicknessPipelineNames <- c( 'ANTsCross', 'ANTsNative', 'ANTsSST', 'FSCross', 'FSLong' )
numberOfRegions <- 62

dktRegions <- read.csv( paste0( dataDirectory, 'dkt.csv' ) )
dktBrainGraphRegions <- dktRegions$brainGraph[( nrow( dktRegions ) - numberOfRegions + 1 ):nrow( dktRegions )]
dktBrainGraphRegions <- gsub( " ", "", dktBrainGraphRegions ) 

dataList <- list()
for( i in 1:length( corticalThicknessPipelineNames ) )
  {
  csvFile <- paste0( dataDirectory, 'reconciled_', corticalThicknessPipelineNames[i], '.csv' )
  cat( "Reading ", csvFile, "\n" )
  dataList[[i]] <- read.csv( csvFile )
  }
thicknessColumns <- grep( "thickness", colnames( dataList[[1]] ) )


dxDataFrame <- data.frame( SubjectID = character(), 
                           ImageID = integer(), 
                           VisitCode = character(), 
                           csvDx = character(), 
                           xmlDx = character(), 
                           adnimergeDxbl = character() )

pb <- txtProgressBar( min = 0, max = nrow( dataList[[1]] ), style = 3 )
for( i in 1:nrow( dataList[[1]]) )
  {
  xmlFiles <- list.files( path = xmlDirectory, 
    pattern = paste0( "*I", dataList[[1]]$IMAGE_ID[i], ".xml" ),
    full.names = TRUE )  
  if( length( xmlFiles ) != 1 )  
    {
    stop( paste0( "Error:  ADNI_", dataList[[1]]$ID[i], "*I", dataList[[1]]$IMAGE_ID[i], ".xml" ) )
    }
  xmlfile <- xmlParse( xmlFiles[1] )  

  # Get the Dx (xml)
  nodes <- getNodeSet( xmlfile, "//subjectInfo" )
  nodeAttrs <- sapply( nodes, xmlAttrs )
  nodeValues <- sapply( nodes, xmlValue )
  xmlDx <- nodeValues[which( nodeAttrs == "DX Group" )]
  
  # Get the visit to match up with adnimerge
  nodes <- getNodeSet( xmlfile, "//visitIdentifier" )
  nodeValues <- sapply( nodes, xmlValue )
  visit <- nodeValues[1]

  visitCode <- "bl"
  if( grepl( "Month 6", visit ) ) {
    visitCode <- "m06"
    } else if( grepl( "Month 12", visit ) ) {
    visitCode <- "m12"
    } else if( grepl( "Month 18", visit ) ) {
    visitCode <- "m18"
    } else if( grepl( "Month 24", visit ) ) {
    visitCode <- "m24"
    } else if( grepl( "Month 36", visit ) ) {
    visitCode <- "m36"
    }

  index <- which( adnimerge$PTID == dataList[[1]]$ID[i] & adnimerge$VISCODE == visitCode )

  adniMergeDx <- 'NA'
  if( length( index ) > 0 )
    {
    adniMergeDx <- adnimerge$DX.bl[index]
    }

  dxSingle <- data.frame( SubjectId = dataList[[1]]$ID[i], 
                          ImageID = dataList[[1]]$IMAGE_ID[i],
                          VisitCode = visitCode,
                          csvDx = dataList[[1]]$DIAGNOSIS[i],
                          xmlDx = xmlDx,
                          adnimergeDxbl = adniMergeDx
                        )

  if( i == 1 )
    {
    dxDataFrame <- dxSingle
    } else {
    dxDataFrame <- rbind( dxDataFrame, dxSingle )  
    }

  setTxtProgressBar( pb, i )  
  }

write.csv( dxDataFrame, file = "~/Desktop/dxDataFrame.csv", quote = FALSE, row.names = FALSE )  