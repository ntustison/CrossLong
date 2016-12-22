library( ggplot2 )
library( brainGraph )

data( "dkt" )

results <- read.csv( "model_results.csv" )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "cross", "Cross-sectional", x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "long1", "Long-SST", x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "long2", "Long-native", x) else x ) )

results$method <- factor( results$method, levels = c( "Cross-sectional", "Long-SST", "Long-native" ) )


sigmaResults <- results[ grep( "sigma", results$region_var ),]
sigmaResults$Regions <- factor( dkt$name, levels = dkt$name )
sigmaResults$Measurement <- rep( 'Within-subject variability', length( sigmaResults$Regions ) )

tauResults <- results[ grep( "tau_0", results$region_var ),]
tauResults$Regions <- factor( dkt$name, levels = dkt$name )
tauResults$Measurement <- rep( 'Between-subject variability', length( tauResults$Regions ) )

ratioResults <- results[ grep( "var_ratio", results$region_var ),]
ratioResults$Regions <- factor( dkt$name, levels = dkt$name )
ratioResults$Measurement <- rep( 'Variance ratio', length( ratioResults$Regions ) )


sigmaPlot <- ggplot( data = sigmaResults, aes( y = X50., x = Regions, colour = method, shape = method ) ) +
              geom_errorbar( aes( ymin = X50. - 1.97 * sd, ymax = X50. + 1.97 * sd ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
#               scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical region', y = 'Within-subject variability', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "../Figures/sigma.png", sigmaPlot, width = 10, height = 3 )

tauPlot <- ggplot( data = tauResults, aes( y = X50., x = Regions, colour = method, shape = method ) ) +
              geom_errorbar( aes( ymin = X50. - 1.97 * sd, ymax = X50. + 1.97 * sd ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
#               scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical region', y = 'Between-subject variability', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "../Figures/tau.png", tauPlot, width = 10, height = 3 )


ratioPlot <- ggplot( data = ratioResults, aes( y = X50., x = Regions, colour = method, shape = method ) ) +
              geom_point( size = 2 ) +
              geom_errorbar( aes( ymin = X50. - 1.97 * sd, ymax = X50. + 1.97 * sd ), width = 0.5 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
#               scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical region', y = 'Variance ratio', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "../Figures/ratio.png", ratioPlot, width = 10, height = 3 )


allDataResults <- rbind( sigmaResults, tauResults, ratioResults )
allDataResults$Measurement <- factor( x = allDataResults$Measurement,
  levels = c( 'Within-subject variability', 'Between-subject variability', 'Variance ratio' ) )


boxPlot <- ggplot( data = allDataResults, aes( x = method, y = X50., fill = method ) ) +
              geom_boxplot( notch = TRUE ) +
#               scale_fill_manual( "", values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              facet_wrap( ~Measurement, scales = 'free', ncol = 3 ) +
              theme( legend.position='none' ) +
              labs( x = '', y = 'Variance ratio' )
ggsave( "../Figures/allData.png", boxPlot, width = 10, height = 3 )


#
