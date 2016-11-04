library( ggplot2 )

results <- read.csv( "model_results.csv" )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "cross", "Cross.", x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "long1", "Long. 1", x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "long2", "Long. 2", x) else x ) )

ratios <- read.csv( "median_ratios.csv" )

sigmaResults <- results[ grep( "sigma", results$region_var ),]
sigmaResults$Regions <- ratios$Regions
sigmaResults$Measurement <- rep( 'Within-Subject Variance', length( sigmaResults$Regions ) )

tauResults <- results[ grep( "tau_0", results$region_var ),]
tauResults$Regions <- ratios$Regions
tauResults$Measurement <- rep( 'Between-Subject Variance', length( tauResults$Regions ) )

ratioResults <- results[ grep( "var_ratio", results$region_var ),]
ratioResults$Regions <- ratios$Regions
ratioResults$X50. <- ratioResults$X50. / 10
ratioResults$Measurement <- rep( 'Variance Ratio', length( ratios$Regions ) )

sigmaPlot <- ggplot( data = sigmaResults ) +
              geom_point( aes( y = X50., x = Regions, colour = method, shape = method ), size = 3 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
              scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical Region', y = 'Within-Subject Variance', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "~/Desktop/sigma.pdf", sigmaPlot, width = 10, height = 3 )


tauPlot <- ggplot( data = tauResults ) +
              geom_point( aes( y = X50., x = Regions, colour = method, shape = method ), size = 3 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
              scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical Region', y = 'Between-Subject Variance', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "~/Desktop/tau.pdf", tauPlot, width = 10, height = 3 )


ratioPlot <- ggplot( data = ratioResults ) +
              geom_point( aes( y = X50., x = Regions, colour = method, shape = method ), size = 3 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
              scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical Region', y = 'Variance Ratio', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "~/Desktop/ratio.pdf", ratioPlot, width = 10, height = 3 )


allDataResults <- rbind( sigmaResults, tauResults, ratioResults )
allDataResults$Measurement <- factor( x = allDataResults$Measurement,
  levels = c( 'Within-Subject Variance', 'Between-Subject Variance', 'Variance Ratio' ) )


boxPlot <- ggplot( data = allDataResults, aes( x = method, y = X50., fill = method ) ) +
              geom_boxplot( notch = TRUE ) +
              scale_fill_manual( "", values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              facet_wrap( ~Measurement, scales = 'free', ncol = 3 ) +
              theme( legend.position='none' ) +
              labs( x = '', y = 'Value' )
ggsave( "~/Desktop/allData.pdf", boxPlot, width = 10, height = 3 )





