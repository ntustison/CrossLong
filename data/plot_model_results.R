library( ggplot2 )
library( brainGraph )

data( "dkt" )

results <- read.csv( "model_results_FINAL.csv" )

pipeline <- c()
for( i in 1:nrow( results ) )
  {
  pipeline[i] <- paste0( as.character( results$software[i] ), "_",  as.character( results$method[i] ) )
  }
results$pipeline <- pipeline
pipelineLevels <- c( "ANTs Cross", "ANTs SST", "ANTs Native", "FS Cross", "FS Long" )


results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "ANTs_cross", pipelineLevels[1], x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "ANTs_long1", pipelineLevels[2], x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "ANTs_long2", pipelineLevels[3], x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "FS_cross", pipelineLevels[4], x) else x ) )
results <- as.data.frame( lapply( results, function(x) if( is.character( x ) | is.factor( x ) ) gsub( "FS_long1", pipelineLevels[5], x) else x ) )
results$pipeline <- factor( results$pipeline, levels = c( "FS Cross", "FS Long", "ANTs Cross", "ANTs SST", "ANTs Native" ) )

sigmaResults <- results[ grep( "sigma", results$param ),]
sigmaResults$Regions <- factor( dkt$name, levels = dkt$name )
sigmaResults$Measurement <- rep( 'Within-subject variability', length( sigmaResults$Regions ) )

tauResults <- results[ grep( "tau_0", results$param ),]
tauResults$Regions <- factor( dkt$name, levels = dkt$name )
tauResults$Measurement <- rep( 'Between-subject variability', length( tauResults$Regions ) )

ratioResults <- results[ grep( "var_ratio", results$param),]
ratioResults$Regions <- factor( dkt$name, levels = dkt$name )
ratioResults$Measurement <- rep( 'Variance ratio', length( ratioResults$Regions ) )


sigmaPlot <- ggplot( data = sigmaResults, aes( y = X50., x = Regions, colour = pipeline, shape = pipeline ) ) +
              geom_errorbar( aes( ymin = X50. - 1.97 * sd, ymax = X50. + 1.97 * sd ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
#               scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical region', y = 'Within-subject variability', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "../Figures/sigma_FINAL.png", sigmaPlot, width = 10, height = 3 )

tauPlot <- ggplot( data = tauResults, aes( y = X50., x = Regions, colour = pipeline, shape = pipeline ) ) +
              geom_errorbar( aes( ymin = X50. - 1.97 * sd, ymax = X50. + 1.97 * sd ), width = 0.5 ) +
              geom_point( size = 2 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
#               scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical region', y = 'Between-subject variability', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "../Figures/tau_FINAL.png", tauPlot, width = 10, height = 3 )


ratioPlot <- ggplot( data = ratioResults, aes( y = X50., x = Regions, colour = pipeline, shape = pipeline ) ) +
              geom_point( size = 2 ) +
              geom_errorbar( aes( ymin = X50. - 1.97 * sd, ymax = X50. + 1.97 * sd ), width = 0.5 ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
#               scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical region', y = 'Variance ratio', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "../Figures/ratio_FINAL.png", ratioPlot, width = 10, height = 3 )

regionalRatioDifferences <- ratioResults$X50.[which( ratioResults$pipeline == "ANTs Native" )] - ratioResults$X50.[which( ratioResults$pipeline == "FS Long" )]
ratioResults$Regions <- factor( ratioResults$Regions, levels( ratioResults$Regions )[order( regionalRatioDifferences )])
ratioResults$RegionalDifferences <- regionalRatioDifferences

ratioPlot <- ggplot( data = ratioResults, aes( y = X50., x = Regions, shape = pipeline ) ) +
              geom_line( size = 0.3, linetype = "dashed", aes( y = RegionalDifferences, group = 1 ) ) +
              geom_errorbar( aes( ymin = X50. - 1.97 * sd, ymax = X50. + 1.97 * sd, colour = pipeline ), width = 0.5 ) +
              geom_point( size = 2, aes( colour = pipeline ) ) +
              theme( axis.text.x = element_text( face="bold", size = 8, angle = 60, hjust = 1 ) ) +
#               scale_color_manual( values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              labs( x = 'Cortical region', y = 'Variance ratio', colour = "", shape = "" ) +
              theme( legend.position = "right" )
ggsave( "../Figures/ratio_FINAL_ordered.png", ratioPlot, width = 10, height = 3 )




regionalRatioDifference <- ratioResults

allDataResults <- rbind( sigmaResults, tauResults, ratioResults )
allDataResults$Measurement <- factor( x = allDataResults$Measurement,
  levels = c( 'Within-subject variability', 'Between-subject variability', 'Variance ratio' ) )


boxPlot <- ggplot( data = allDataResults, aes( x = pipeline, y = X50., fill = pipeline ) ) +
              geom_boxplot( notch = FALSE ) +
#               scale_fill_manual( "", values = colorRampPalette( c( "navyblue", "darkred" ) )(3) ) +
              facet_wrap( ~Measurement, scales = 'free', ncol = 3 ) +
              theme( legend.position='none' ) +
              theme( axis.text.x = element_text( face="bold", size = 10, angle = 45, hjust = 1 ) ) +
              labs( x = '', y = 'Variance ratio' )
ggsave( "../Figures/allData_FINAL.png", boxPlot, width = 10, height = 4 )


#
