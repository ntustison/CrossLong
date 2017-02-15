
referenceAnts <- read.csv( 'adniCrossSectionalAntsMergeSubset_WithScr.csv' )
entorhinalData <- read.csv( 'adniEntorhinalAnts.csv')

commonImageIds <- unique( intersect( referenceAnts$IMAGE_ID, entorhinalData$IMAGE_ID ) )
cat( "Length of common image IDs = ", length( commonImageIds ), "\n" )


entorhinalData <- entorhinalData[which( entorhinalData$IMAGE_ID %in% commonImageIds ),]
if( any( duplicated( entorhinalData$IMAGE_ID ) ) )
  {
  entorhinalData <- entorhinalData[-which( duplicated( entorhinal$IMAGE_ID ) ),]
  }
# entorhinalData <- entorhinalData[with( entorhinalData, order( ID ) ),]

write.csv( entorhinalData, file = 'adniEntorhinalAntsMergeSubset_WithScr.csv', quote = FALSE, row.names = FALSE )
