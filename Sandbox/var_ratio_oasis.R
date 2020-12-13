library( ggplot2 )
library( reshape2 )

######################################################################
#
# Look at change in regional volume with rotation
#
######################################################################

oasis_volume_values <- read.csv( 'oasis-trt-20-volume.csv' )
oasis_volume_values_rigid <- read.csv( 'oasis-trt-20-volume-rigid.csv' )

oasis_volume_diff_values <- ( oasis_volume_values_rigid[,] - oasis_volume_values[,] ) / oasis_volume_values[,]
oasis_volume_diff_values$ID <- oasis_volume_values$ID
oasis_volume_diff_values[,2:ncol( oasis_volume_diff_values )] <- abs( oasis_volume_diff_values[,2:ncol( oasis_volume_diff_values )] )
# oasis_volume_diff_values <- oasis_volume_diff_values[,-which( names( oasis_volume_diff_values ) %in% c( "ID" ) )]

oasisVolumeMelt <- melt( oasis_volume_diff_values )

numberOfSubjects <- nrow( oasis_volume_diff_values )
regions <- colnames( oasis_volume_values )[2:length( ncol( oasis_volume_values ) )]

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
oasisVolumeMelt$Tau <- as.vector( tauMatrix )
sigmaMatrix <- matrix( rep( sigmaResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisVolumeMelt$Sigma <- as.vector( sigmaMatrix )
ratioMatrix <- matrix( rep( ratioResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisVolumeMelt$VarianceRatio <- as.vector( ratioMatrix )

# ggplot( data = oasisVolumeMelt, aes( x = VarianceRatio, y = value ) ) +
#   geom_point() +
#   geom_smooth( method = lm, se = TRUE ) +
#   xlab( 'Variance Ratio' ) +
#   ylab( '% Absolute Volumetric Difference')
# ggsave( '../Figures/correlationVolumeVarianceRatio.pdf', width = 8, height = 4 )
#
# summary( lm( value ~ VarianceRatio, data = oasisVolumeMelt ) )

ggplot( data = oasisVolumeMelt, aes( x = Sigma, y = value ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Within-Subject Variability' ) +
  ylab( '% Absolute Volumetric Difference')
ggsave( '../Figures/correlationVolumeWithinSubjectVariance.pdf', width = 8, height = 4 )

summary( lm( value ~ Sigma, data = oasisVolumeMelt ) )

ggplot( data = oasisVolumeMelt, aes( x = Tau, y = value ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Between-Subject Variability' ) +
  ylab( '% Absolute Volumetric Difference')
ggsave( '../Figures/correlationVolumeBetweenSubjectVariance.pdf', width = 8, height = 4 )

summary( lm( value ~ Tau, data = oasisVolumeMelt ) )

######################################################################
#
# Look at change in regional surface area with rotation
#
######################################################################

oasis_surfaceArea_values <- read.csv( 'oasis-trt-20-surfaceArea.csv' )
oasis_surfaceArea_values_rigid <- read.csv( 'oasis-trt-20-surfaceArea-rigid.csv' )

oasis_surfaceArea_diff_values <- ( oasis_surfaceArea_values_rigid[,] - oasis_surfaceArea_values[,] ) / oasis_surfaceArea_values[,]
oasis_surfaceArea_diff_values$ID <- oasis_surfaceArea_values$ID
oasis_surfaceArea_diff_values[,2:ncol( oasis_surfaceArea_diff_values )] <- abs( oasis_surfaceArea_diff_values[,2:ncol( oasis_surfaceArea_diff_values )] )
# oasis_surfaceArea_diff_values <- oasis_surfaceArea_diff_values[,-which( names( oasis_surfaceArea_diff_values ) %in% c( "ID" ) )]

oasisSurfaceAreaMelt <- melt( oasis_surfaceArea_diff_values )

tauMatrix <- matrix( rep( tauResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisSurfaceAreaMelt$Tau <- as.vector( tauMatrix )
sigmaMatrix <- matrix( rep( sigmaResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisSurfaceAreaMelt$Sigma <- as.vector( sigmaMatrix )
ratioMatrix <- matrix( rep( ratioResults$X50.[long1Indices], numberOfSubjects ), nrow = numberOfSubjects, byrow = TRUE )
oasisSurfaceAreaMelt$VarianceRatio <- as.vector( ratioMatrix )

# ggplot( data = oasisSurfaceAreaMelt, aes( x = VarianceRatio, y = value ) ) +
#   geom_point() +
#   geom_smooth( method = lm, se = TRUE ) +
#   xlab( 'Variance Ratio' ) +
#   ylab( '% Absolute Surface Area Difference')
# ggsave( '../Figures/correlationSurfaceAreaVarianceRatio.pdf', width = 8, height = 4 )
#
# summary( lm( value ~ VarianceRatio, data = oasisSurfaceAreaMelt ) )

ggplot( data = oasisSurfaceAreaMelt, aes( x = Sigma, y = value ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Within-Subject Variability' ) +
  ylab( '% Absolute Surface Area Difference')
ggsave( '../Figures/correlationSurfaceAreaWithinSubjectVariance.pdf', width = 8, height = 4 )

summary( lm( value ~ Sigma, data = oasisSurfaceAreaMelt ) )

ggplot( data = oasisSurfaceAreaMelt, aes( x = Tau, y = value ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Between-Subject Variability' ) +
  ylab( '% Absolute Surface Area Difference')
ggsave( '../Figures/correlationSurfaceAreaBetweenSubjectVariance.pdf', width = 8, height = 4 )

summary( lm( value ~ Tau, data = oasisSurfaceAreaMelt ) )



