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
                                 VISIT = adniData$VISIT,
                                 MMSCORE = adniData$MMSCORE )

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
#             scale_fill_manual( values = alpha( c( "navyblue", "darkred" ), 1.0 ) ) +
            guides( fill = guide_legend( title = "Gender" ) )
ggsave( "demoPlot.png", plot = demoPlot, width = 8, height = 4, units = 'in', dpi = 300 )

# Create 2D histograms of AGE vs. MMSE at m12 (since it has the highest count)

demoAdniDataFrame12 <- demoAdniDataFrame[which( demoAdniDataFrame$VISIT == "m12" ),]


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

demoPlot2 <- ggplot( data = demoAdniDataFrame12 ) +
            stat_bin2d( aes( x = AGE, y = MMSCORE ), binwidth = c( 1.00, 1.75 ) ) +
            facet_wrap( ~ DIAGNOSIS ) +
            labs( y = "Mini-Mental State Examination", x = "Age" ) +
#             scale_fill_gradientn( limits = c( 1, 15 ), colours = gg_color_hue( 10 ) ) +
            guides( fill = guide_legend( title = "Count" ) )
ggsave( "demoPlot2.png", plot = demoPlot2, width = 8, height = 4, units = 'in', dpi = 300 )
