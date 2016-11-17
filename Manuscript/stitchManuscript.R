library( rmarkdown )
library( ggplot2 )

stitchedFile <- "stitched.Rmd";

formatFile <- "format.Rmd"

rmdFiles <- c( formatFile,
               "titlePage.Rmd",
#               "notes.Rmd",
               "abstract.Rmd",
               "intro.Rmd",
               "imagingMethods.Rmd",
               "processingMethods.Rmd",
               "statisticalMethods.Rmd",
               "results.Rmd",
               "discussion.Rmd"
   )


rmdFiles <- c( formatFile,
   "titlePage.Rmd",
  #               "notes.Rmd",
   "abstract.Rmd",
   "intro.Rmd",
   "imagingMethods.Rmd",
   "processingMethods.Rmd",
   "statisticalMethods.Rmd",
   "results.Rmd",
   "discussion.Rmd",
   "acknowledgments.Rmd"
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
render( stitchedFile, output_format = "pdf_document" )
