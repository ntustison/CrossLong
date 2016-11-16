library( ggplot2 )
library( reshape2 )

oasis_values <- read.csv( 'oasis-trt-20-volume.csv' )
oasis_values_rigid <- read.csv( 'oasis-trt-20-volume-rigid.csv' )

oasis_diff_values <- ( oasis_values_rigid[,] - oasis_values[,] ) / oasis_values[,]
oasis_diff_values$ID <- oasis_values$ID
oasis_diff_values[,2:ncol( oasis_diff_values )] <- abs( oasis_diff_values[,2:ncol( oasis_diff_values )] )
# oasis_diff_values <- oasis_diff_values[,-which( names( oasis_diff_values ) %in% c( "ID" ) )]

oasisMelt <- melt( oasis_diff_values )

numberOfSubjects <- nrow( oasis_diff_values )

regions <- colnames( oasis_values )[2:length( ncol( oasis_values ) )]

results <- read.csv( "model_results.csv" )

sigmaResults <- results[ grep( "sigma", results$region_var ),]
sigmaResults$Regions <- regions
sigmaResults$Measurement <- rep( 'Within-Subject Variance', length( sigmaResults$Regions ) )

tauResults <- results[ grep( "tau_0", results$region_var ),]
tauResults$Regions <- regions
tauResults$Measurement <- rep( 'Between-Subject Variance', length( tauResults$Regions ) )

ratioResults <- results[ grep( "var_ratio", results$region_var ),]
ratioResults$Regions <- regions
ratioResults$Measurement <- rep( 'Variance Ratio', length( ratioResults$Regions ) )

crossIndices <- which( ratioResults$method == "cross" )
long1Indices <- which( ratioResults$method == "long1" )
long2Indices <- which( ratioResults$method == "long2" )


tauMatrix <- matrix( rep( tauResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisMelt$Tau <- as.vector( tauMatrix )
sigmaMatrix <- matrix( rep( sigmaResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisMelt$Sigma <- as.vector( sigmaMatrix )
ratioMatrix <- matrix( rep( ratioResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisMelt$VarianceRatio <- as.vector( ratioMatrix )

ggplot( data = oasisMelt, aes( x = VarianceRatio, y = value ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Variance Ratio' ) +
  ylab( '% Absolute Volumetric Difference')
ggsave( '~/Desktop/correlationVolumeVarianceRatio.pdf', width = 8, height = 4 )

summary( lm( value ~ VarianceRatio, data = oasisMelt ) )

ggplot( data = oasisMelt, aes( x = Sigma, y = value ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Within-Subject Variance' ) +
  ylab( '% Absolute Volumetric Difference')
ggsave( '~/Desktop/correlationVolumeWithinSubjectVariance.pdf', width = 8, height = 4 )

summary( lm( value ~ Sigma, data = oasisMelt ) )

ggplot( data = oasisMelt, aes( x = Tau, y = value ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Between-Subject Variance' ) +
  ylab( '% Absolute Volumetric Difference')
ggsave( '~/Desktop/correlationVolumeBetweenSubjectVariance.pdf', width = 8, height = 4 )

summary( lm( value ~ Tau, data = oasisMelt ) )


# oasis_values <- read.csv( 'oasis-trt-20-volume.csv' )
# oasis_values_rigid <- read.csv( 'oasis-trt-20-volume-rigid.csv' )
# var_ratios <- read.csv( 'diff_med_ratios_1.csv' )
#
# mean_oasis <- colMeans( oasis_values[,2:ncol( oasis_values )] )
#
# myDf <- data.frame( Oasis = mean_oasis, NormedVarRatios = var_ratios$normed )
#
# ggplot( data = myDf, aes( y = Oasis, x = NormedVarRatios ) ) +
#   geom_point() +
#   geom_smooth( method = lm, se = TRUE ) +
#   xlab( 'Var ratios (normed)' ) +
#   ylab( 'Oasis')
# ggsave( '~/Desktop/correlationVolume.pdf', width = 5, height = 5)
#
# fit = lm( myDf$Oasis ~ myDf$NormedVarRatios )
# cat( "r^2 = ", summary( fit )$r.squared, ", p-value = ", anova(fit)$'Pr(>F)'[1], "\n" )
#
# mean_oasis <- colMeans( oasis_values_rigid[,2:ncol( oasis_values )] -
#                         oasis_values[,2:ncol( oasis_values_rigid )]
#                       )
#
# myDf <- data.frame( Oasis = mean_oasis, NormedVarRatios = var_ratios$normed )
#
# ggplot( data = myDf, aes( y = Oasis, x = NormedVarRatios ) ) +
#   geom_point() +
#   geom_smooth( method = lm, se = TRUE ) +
#   xlab( 'Var ratios (normed)' ) +
#   ylab( 'R(Volume) - Volume')
# ggsave( '~/Desktop/correlationVolumeDifference.pdf', width = 5, height = 2 )
#
# fit = lm( myDf$Oasis ~ myDf$NormedVarRatios )
# cat( "r^2 = ", summary( fit )$r.squared, ", p-value = ", anova(fit)$'Pr(>F)'[1], "\n" )


