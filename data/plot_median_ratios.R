library( ggplot2 )

modelResults <- read.csv( "model_results.csv" )
ratiosCross <- modelResults$X50.[1:62]


ratiosPlot <- ggplot( data = ratios ) +
              geom_point( aes( y = Ratios, x = Regions, colour = Pipeline, shape = Pipeline ), size = 3 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
              scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical Region', y = 'Ratio Value' ) +
              theme( legend.position = "right" )
ggsave( "~/Desktop/ratios.pdf", ratiosPlot, width = 10, height = 3 )
