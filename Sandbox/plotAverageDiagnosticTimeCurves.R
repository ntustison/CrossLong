library( reshape2 )
library( ggplot2 )
library(splines)
library(MASS)

crossData <- read.csv( "adniCrossSectionalAntsMergeSubset.csv" )
long1Data <- read.csv( "adniLongitudinalAntsMergeSubset.csv" )
long2Data <- read.csv( "adniLongitudinalNativeSpaceAntsMergeSubset.csv" )


# thickness.right.entorhinal

thicknessDataFrame <- data.frame( ID = crossData$ID,
                           VISIT = crossData$VISIT,
                           DIAGNOSIS = crossData$DIAGNOSIS,
                           CROSSDATA = crossData$thickness.right.entorhinal,
                           LONG1DATA = long1Data$thickness.right.entorhinal,
                           LONG2DATA = long2Data$thickness.right.entorhinal
                         )

timePointsString <- c( 'bl', 'm06', 'm12', 'm18', 'm24', 'm36' )
timePointsNumeric <- c( 0, 6, 12, 18, 24, 36 )

numericVisit <- rep( 'NA', length( thicknessDataFrame$VISIT ) )
for( m in 1:length( timePointsString ) )
  {
  numericVisit[thicknessDataFrame$VISIT == timePointsString[m]] <- timePointsNumeric[m]
  }
thicknessDataFrame$VISIT <- as.numeric( numericVisit )

# normalize all subjects using simple protocol

thicknessMelt <- melt( thicknessDataFrame,
                       c( 'ID', 'VISIT', 'DIAGNOSIS' ),
                       c( 'CROSSDATA', 'LONG1DATA', 'LONG2DATA' ),
                       variable.name = 'METHOD',
                       value.name = 'thickness'
                     )
thicknessPlot <- ggplot( thicknessMelt, aes( x = VISIT, y = thickness, colour = METHOD ) ) +
                 stat_smooth( method = "lm", size = 0.5 ) +
                 geom_point( alpha = 0.1 ) +
                 scale_y_continuous( limits = c( 3, 4 ) ) +
                 facet_wrap( ~ DIAGNOSIS )
ggsave( filename = "~/Desktop/thicknessPlot.pdf", plot = thicknessPlot, width = 8, height = 4, units = "in" )
