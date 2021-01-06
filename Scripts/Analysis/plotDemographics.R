library( ggplot2 )

# Questions of interest:
#  *  Age ranges, male and females  (https://rpubs.com/walkerke/pyramids_ggplot2)
#  *  Number of CN, LMCI, and AD
#  *  How many time points?  Missing time points?

# baseDirectory <- '/Users/ntustison/Data/Public/CrossLong/'
baseDirectory <- '/Users/ntustison/Documents/Academic/InProgress/CrossLong/'
dataDirectory <- paste0( baseDirectory, 'Data/' )

adniData <- read.csv( paste0( dataDirectory, 'reconciled_ANTsCross.csv' ) )
adniData$DIAGNOSIS <- factor( adniData$DIAGNOSIS, levels = c( "CN", "LMCI", "AD" ) )

demoPlot <- ggplot( data = adniData, aes( VISIT ) ) +
            geom_bar( aes( fill = SEX ), position = "dodge" ) +
            facet_wrap( ~ DIAGNOSIS ) +
            labs( y = "Count", x = "Visit" ) +
#             scale_fill_manual( values = alpha( c( "navyblue", "darkred" ), 1.0 ) ) +
            guides( fill = guide_legend( title = "Gender" ) )
ggsave( "demoPlot.png", plot = demoPlot, width = 8, height = 2.5, units = 'in', dpi = 300 )

# Create 2D histograms of AGE vs. MMSE at m12 (since it has the highest count)

adniDataFrame12 <- adniData[which( adniData$VISIT == 12 ),]


gg_color_hue <- function( n ) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

demoPlot2 <- ggplot( data = adniDataFrame12 ) +
            stat_bin2d( aes( x = AGE, y = MMSCORE ), binwidth = c( 1.00, 1.75 ) ) +
            facet_wrap( ~ DIAGNOSIS ) +
            labs( y = "Mini-Mental State Examination", x = "Age" ) +
#             scale_fill_gradientn( limits = c( 1, 15 ), colours = gg_color_hue( 10 ) ) +
            guides( fill = guide_legend( title = "Count" ) )
ggsave( "demoPlot2.png", plot = demoPlot2, width = 8, height = 2.5, units = 'in', dpi = 300 )
