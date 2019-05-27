#!/usr/bin/env Rscript

library( ggplot2 )
library( rmarkdown )

srcDir <- "./"
buildrmd <- 'smi.Rmd'

########   Morning    ##########

# Retrospective

rawrmds <- c( "format.Rmd",
              "smiContent.Rmd"
               )

for ( x in 1:length( rawrmds ) ) {
  if ( x == 1 )
    {
    cmd<-paste( "cat ", rawrmds[x], " > ", buildrmd )
    } else {
      cmd<-paste( "cat ", rawrmds[x], " >> ", buildrmd )
    }
  system( cmd )
  }

render( buildrmd, clean = TRUE )
