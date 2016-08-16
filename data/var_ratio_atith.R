library(ggplot2)

oasis_atith_values <- read.csv( 'oasis-trt-20-atith.csv' )
oasis_atith_values_rigid <- read.csv( 'oasis-trt-20-atith-rigid.csv' )
var_ratios <- read.csv( 'var_ratios_2.csv' )

# ATITH in native space

mean_atith <- colMeans( oasis_atith_values[,2:ncol( oasis_atith_values )] )

myDf <- data.frame( Atith = mean_atith, NormedVarRatios = var_ratios$normed )

ggplot( data = myDf, aes( y = Atith, x = NormedVarRatios ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Var ratios (normed)' ) +
  ylab( 'Atith') + coord_fixed()
ggsave( '~/Desktop/correlationAtith.pdf', width = 5, height = 5)

fit = lm( myDf$Atith ~ myDf$NormedVarRatios )
cat( "r^2 = ", summary( fit )$r.squared, ", p-value = ", anova(fit)$'Pr(>F)'[1], "\n" )

# The difference between ATITH in native space and ATITH after rigid alignment to template

mean_atith <- colMeans( oasis_atith_values_rigid[,2:ncol( oasis_atith_values )] -
                        oasis_atith_values[,2:ncol( oasis_atith_values_rigid )]
                      )

myDf <- data.frame( Atith = mean_atith, NormedVarRatios = var_ratios$normed )

ggplot( data = myDf, aes( y = Atith, x = NormedVarRatios ) ) +
  geom_point() +
  geom_smooth( method = lm, se = TRUE ) +
  xlab( 'Var ratios (normed)' ) +
  ylab( 'R(Atith) - Atith') + coord_fixed()
ggsave( '~/Desktop/correlationAtithDifference.pdf', width = 5, height = 2 )

fit = lm( myDf$Atith ~ myDf$NormedVarRatios )
cat( "r^2 = ", summary( fit )$r.squared, ", p-value = ", anova(fit)$'Pr(>F)'[1], "\n" )


