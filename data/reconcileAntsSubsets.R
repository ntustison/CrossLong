crossAnts <- read.csv( "thicknessResultsSubjectSpace.csv" )
sstAnts <- read.csv( "adniLongitudinalAnts.csv" )
nativeAnts <- read.csv( "adniLongitudinalNativeSpaceAnts.csv" )
crossFS <- read.csv( "freesurferCrossSectional.csv" )
longFS <- read.csv( "freesurferLongitudinal.csv" )

thicknessData <- list()
thicknessData[[1]] <- crossAnts
thicknessData[[2]] <- sstAnts
thicknessData[[3]] <- nativeAnts
thicknessData[[4]] <- crossFS
thicknessData[[5]] <- longFS

colsToKeep <- list()
colsToKeep[[1]] <- c( 1:4, 6, 7, 15:18, 27:88 )
colsToKeep[[2]] <- c( 1:4, 6, 7, 10, 15:17, 29:90 )
colsToKeep[[3]] <- c( 1:4, 6, 7, 10, 15:17, 22:83 )
colsToKeep[[4]] <- c( 1:ncol( thicknessData[[4]] ) )
colsToKeep[[5]] <- c( 1:ncol( thicknessData[[5]] ) )

sharedColnames <- colnames( thicknessData[[2]] )[colsToKeep[[2]]]

for( i in 1:length( thicknessData ) )
  {
  if( any( duplicated( thicknessData[[i]]$IMAGE_ID ) ) )
    {
    thicknessData[[i]] <- thicknessData[[i]][-which( duplicated( thicknessData[[i]]$IMAGE_ID ) ),]
    }
  thicknessData[[i]] <- thicknessData[[i]][,colsToKeep[[i]]]
  }

commonImageIds <- unique( intersect( thicknessData[[1]]$IMAGE_ID, thicknessData[[2]]$IMAGE_ID ) )
for( i in 3:length( thicknessData ) )
  {
  commonImageIds <- unique( intersect( commonImageIds, thicknessData[[i]]$IMAGE_ID ) )
  }

cat( "Length of common image IDs = ", length( commonImageIds ), "\n" )

for( i in 1:length( thicknessData ) )
  {
  thicknessData[[i]] <- thicknessData[[i]][which( thicknessData[[i]]$IMAGE_ID %in% commonImageIds ),]
  if( any( duplicated( thicknessData[[i]]$IMAGE_ID ) ) )
    {
    thicknessData[[i]] <- thicknessData[[i]][-which( duplicated( thicknessData[[i]]$IMAGE_ID ) ),]
    }
  thicknessData[[i]] <- thicknessData[[i]][with( thicknessData[[i]], order( ID, IMAGE_ID ) ),]
  if( i >= 4 )
    {
    thicknessData[[i]] <- cbind( thicknessData[[1]][,1:10], thicknessData[[i]][,3:64] )
    }
  }

for( i in 1:length( thicknessData ) )
  {
  thicknessData[[i]] <- thicknessData[[i]][with( thicknessData[[i]], order( ID, AGE ) ),]
  colnames( thicknessData[[i]] ) <- sharedColnames
  }

# > levels( sstSubset$VISIT )
# [1] "ADNI1 Baseline"    "ADNI1 Screening"   "ADNI1/GO Month 12" "ADNI1/GO Month 18"
# [5] "ADNI1/GO Month 24" "ADNI1/GO Month 36" "ADNI1/GO Month 6"  "No Visit Defined"
# [9] "Unscheduled"

oldLevelNames <- levels( thicknessData[[2]]$VISIT )[1:7]
newLevelNames <- c( "bl", "scr", "m12", "m18", "m24", "m36", "m06" )

for( i in 1:length( thicknessData ) )
  {
  thicknessData[[i]] <- thicknessData[[i]][-which( thicknessData[[i]]$VISIT == "No Visit Defined" | thicknessData[[i]]$VISIT == "Unscheduled" ),]
  for( j in 1:length( oldLevelNames ) )
    {
    levels( thicknessData[[i]]$VISIT )[match( oldLevelNames[j], levels( thicknessData[[i]]$VISIT ) )] <- newLevelNames[j]
    }
#   thicknessData[[i]] <- thicknessData[[i]][-which( thicknessData[[i]]$VISIT == "scr" ),]
  }

# Remove image ids which duplicate visits (whichever is the later data assessed by AGE)

duplicateImageIds <- c()
copyThicknessData <- thicknessData[[1]]

timePointsString <- c( 'scr', 'bl', 'm06', 'm12', 'm18', 'm24', 'm36' )
timePointsNumeric <- c( 0, 0, 6, 12, 18, 24, 36 )

numericVisit <- rep( 'NA', length( copyThicknessData$VISIT ) )
for( m in 1:length( timePointsString ) )
  {
  numericVisit[copyThicknessData$VISIT == timePointsString[m]] <- timePointsNumeric[m]
  }
copyThicknessData$VISIT <- as.numeric( numericVisit )

subjectIds <- levels( copyThicknessData$ID )
for( j in 1:length( subjectIds ) )
  {
  subjectData <- copyThicknessData[which( copyThicknessData$ID == subjectIds[j] ),]
  subjectData <- subjectData[with( subjectData, order( AGE ) ),]
  if( anyDuplicated( subjectData$VISIT ) > 0 )
    {
    duplicateImageIds <- append( duplicateImageIds, subjectData$IMAGE_ID[which( duplicated( subjectData$VISIT ) )] )
    }
  }
for( i in 1:length( thicknessData ) )
  {
  thicknessData[[i]] <- thicknessData[[i]][-which( thicknessData[[i]]$IMAGE_ID %in% duplicateImageIds ),]
  }

filenames <- c( 'adniCrossSectionalAntsMergeSubset_WithScr.csv',
                'adniLongitudinalAntsMergeSubset_WithScr.csv',
                'adniLongitudinalNativeSpaceAntsMergeSubset_WithScr.csv',
                'adniCrossSectionalFreeSurferMergeSubset_WithScr.csv',
                'adniLongitudinalFreeSurferMergeSubset_WithScr.csv'
                 )
for( i in 1:length( thicknessData ) )
  {
  write.csv( thicknessData[[i]], file = filenames[i], quote = FALSE, row.names = FALSE )
  }
