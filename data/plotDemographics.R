library( ggplot2 )

# Questions of interest:
#  *  Age ranges, male and females  (https://rpubs.com/walkerke/pyramids_ggplot2)
#  *  Number of Normal, MCI, LMCI, and AD
#  *  How many time points?  Missing time points?


adniData <- read.csv( 'adniCrossSectionalAntsMergeSubset.csv' )
demoAdniDataFrame <- data.frame( ID = adniData$ID,
                                 DIAGNOSIS = adniData$DIAGNOSIS,
                                 AGE = adniData$AGE,
                                 SEX = adniData$SEX,
                                 VISIT = adniData$VISIT )

# Did any of the subjects change diagnosis during imaging?
# Also get any subjects that have a single time point.

singleTimePointIndices <- c()

idLevels <- levels( demoAdniDataFrame$ID )
for( i in 1:length( idLevels ) )
  {
  subjectDataFrame <- demoAdniDataFrame[which( demoAdniDataFrame$ID == idLevels[i] ),]
  if( length( subjectDataFrame$DIAGNOSIS ) > 1 )
    {
    for( j in 2:length( subjectDataFrame$DIAGNOSIS ) )
      {
      if( subjectDataFrame$DIAGNOSIS[j] != subjectDataFrame$DIAGNOSIS[1] )
        {
        cat( "Subject ", subjectDataFrame$ID[1], " changes: ", subjectDataFrame$DIAGNOSIS, "\n" )
        next;
        }
      }
    } else {
    cat( "Subject", as.character( subjectDataFrame$ID[1] ), "only has a single time point.\n" )
    index <- which( demoAdniDataFrame$ID == idLevels[i] )
    singleTimePointIndices <- append( singleTimePointIndices, index )
    }
  }

demoAdniDataFrame <- demoAdniDataFrame[-singleTimePointIndices,]
demoAdniDataFrame <- as.data.frame( lapply( demoAdniDataFrame, function (x) if ( is.factor(x) ) factor(x) else x ) )

demoAdniDataFrame$DIAGNOSIS <- factor( demoAdniDataFrame$DIAGNOSIS, levels = c( "Normal", "MCI", "LMCI", "AD" ) )

demoPlot <- ggplot( data = demoAdniDataFrame, aes( VISIT ) ) +
            geom_bar( aes( fill = SEX ), position = "dodge" ) +
            facet_wrap( ~ DIAGNOSIS ) +
            labs( y = "Count", x = "Visit" ) +
            scale_fill_manual( values = alpha( c( "navyblue", "darkred" ), 1.0 ) ) +
            guides( fill = guide_legend( title = "Gender" ) )
ggsave( "demoPlot.pdf", plot = demoPlot, width = 8, height = 4, units = 'in', dpi = 300 )



