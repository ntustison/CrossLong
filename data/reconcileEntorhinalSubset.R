
referenceAnts <- read.csv( 'adniCrossSectionalAntsMergeSubset_WithScr.csv' )
demoColumns <- 1:10

entorhinalData <- read.csv( 'adniEntorhinalAnts.csv')

commonImageIds <- unique( intersect( referenceAnts$IMAGE_ID, entorhinalData$IMAGE_ID ) )
cat( "Length of common image IDs = ", length( commonImageIds ), "\n" )

entorhinalData <- entorhinalData[which( entorhinalData$IMAGE_ID %in% commonImageIds ),]
if( any( duplicated( entorhinalData$IMAGE_ID ) ) )
  {
  entorhinalData <- entorhinalData[-which( duplicated( entorhinal$IMAGE_ID ) ),]
  }
entorhinalData <- entorhinalData[with( entorhinalData, order( ID ) ),]

if( ! identical( entorhinalData$IMAGE_ID, referenceAnts$IMAGE_ID ) )
  {
  cat( "IMAGE_IDs are not identical.\n" )
  return;
  } else {
  cat( "IMAGE_IDs are identical.\n" )
  }
entorhinalData$ID <- droplevels( entorhinalData$ID )

thickness2.left.entorhinal <- entorhinalData$volume.left.entorhinal / entorhinalData$surface.area.left.entorhinal
thickness2.right.entorhinal <- entorhinalData$volume.right.entorhinal / entorhinalData$surface.area.right.entorhinal

entorhinalData <- cbind( referenceAnts[, demoColumns],
                         entorhinalData[, 3:4],
                         thickness2.left.entorhinal,
                         thickness2.right.entorhinal,
                         entorhinalData[, 5:ncol( entorhinalData )] )

write.csv( entorhinalData, file = 'adniEntorhinalAntsMergeSubset_WithScr.csv', quote = FALSE, row.names = FALSE )
