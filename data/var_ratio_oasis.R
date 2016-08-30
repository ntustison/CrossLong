library(ggplot2)

oasis_values <- read.csv( 'oasis-trt-20-volume.csv' )
oasis_values_rigid <- read.csv( 'oasis-trt-20-volume-rigid.csv' )
var_ratios <- read.csv( 'var_ratios_2.csv' )

mean_oasis <- colMeans( oasis_values[,2:ncol( oasis_values )] )

myDf <- data.frame( Oasis = mean_oasis, NormedVarRatios = var_ratios$normed )

ggplot( data = myDf, aes( y = Oasis, x = NormedVarRatios ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Var ratios (normed)' ) +
  ylab( 'Oasis')
ggsave( '~/Desktop/correlationVolume.pdf', width = 5, height = 5)

fit = lm( myDf$Oasis ~ myDf$NormedVarRatios )
cat( "r^2 = ", summary( fit )$r.squared, ", p-value = ", anova(fit)$'Pr(>F)'[1], "\n" )

mean_oasis <- colMeans( oasis_values_rigid[,2:ncol( oasis_values )] -
                        oasis_values[,2:ncol( oasis_values_rigid )]
                      )

myDf <- data.frame( Oasis = mean_oasis, NormedVarRatios = var_ratios$normed )

ggplot( data = myDf, aes( y = Oasis, x = NormedVarRatios ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Var ratios (normed)' ) +
  ylab( 'R(Volume) - Volume')
ggsave( '~/Desktop/correlationVolumeDifference.pdf', width = 5, height = 2 )

fit = lm( myDf$Oasis ~ myDf$NormedVarRatios )
cat( "r^2 = ", summary( fit )$r.squared, ", p-value = ", anova(fit)$'Pr(>F)'[1], "\n" )


