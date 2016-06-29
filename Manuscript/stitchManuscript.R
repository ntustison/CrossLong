library( rmarkdown )
library( ggplot2 )

stitchedFile <- "stitched.Rmd";

formatFile <- "format.Rmd"

rmdFiles <- c( formatFile,
               "abstract.Rmd",
               "intro.Rmd",
               "methods.Rmd",
               "results.Rmd",
               "discussion.Rmd"
   )

for( i in 1:length( rmdFiles ) )
  {
  if( i == 1 )
    {
    cmd <- paste( "cat", rmdFiles[i], ">", stitchedFile )
    } else {
    cmd <- paste( "cat", rmdFiles[i], ">>", stitchedFile )
    }
  system( cmd )
  }

cat( '\n Pandoc rendering', stitchedFile, '\n' )
render( stitchedFile, output_format = "all" )

