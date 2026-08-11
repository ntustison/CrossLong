# Longitudinal mapping of cortical thickness measurements: an ADNI-based evaluation study

<!--
# CrossLong

## Notes on compilation:

1.  Make sure you have the ``rmarkdown`` package.

2. After cloning the repo., navigate to the Manuscript subdirectory.

2.  Compile the source .Rmd files by typing ``Rscript stitchManuscript.R``.

3.  html, .docx, .tex, and .pdf files will be produced (e.g., ``stitched.pdf``).
-->

This ``CrossLong`` repository contains the raw manuscript files, data, scripts
and other material associated with our work describing [the ANTs longitudinal 
cortical thickness pipeline](https://pubmed.ncbi.nlm.nih.gov/31356207/)

> Nicholas J. Tustison, Andrew J. Holbrook, Brian B. Avants, Jared M. Roberts, 
> Philip A. Cook, Zachariah M. Reagh, Jeffrey T. Duda, James R. Stone, 
> Daniel L. Gillen, and Michael A. Yassa for the Alzheimer’s Disease Neuroimaging 
> Initiative. Longitudinal Mapping of Cortical Thickness Measurements: An Alzheimer's
> Disease Neuroimaging Initiative-Based Evaluation Study, J Alzheimers Dis 2019;71(1):165-183.

________________________________

## Directory organization

* __Abstracts/__:  Rmd files for submitted abstracts.
* __Data/__: .csv files used for different analysis components.  
    * __RegionalPredictionPlots/__:  Prediction plots for MMSE for each of the regions (see Scripts/Analysis/doPrediction.R).
    * __RegionalThicknessSlopeDistributions/__:  Various plots showing the thickness slope distributions for each pipeline in each of the 62 DKT regions (see Scripts/Analysis/plotSlopeDistributions.R).
    * __RegionalThicknessSpaghettiPlots/__:  Spaghetti plots of the thickness values for each pipeline in each of the 62 DKT regions (see Scripts/Analysis/plotSpaghetti.R).
    * __XmlFiles/__:  Original ADNI Xml files providing the demographics of the cohort used.
* __Figures/__:  .png and .pdf files used in the manuscript.
* __Manuscript/__:  Rmd files to create the manuscript submission.  Also contains reviewer responses and cover letters to the editor.
    * __Neuroimage/__:  Neuroimage submission (including reviews and our responses).
    * __JAD/__:  Journal of Alzheimer's Disease submission (including reviews and our responses).
* __Presentations/__:  Rmd files to create boomer presentations.
* __Sandbox/__:  Outdated R scripts and .csv data files.
* __Scripts/__
    * __Analysis/__:  R scripts to analyze the results.
    * __RunStudy/__:  Perl scripts to run the FreeSurfer and ANTs pipelines, run DKT joint fusion, and parse files.
* __Template/__:  ADNI template and auxiliary images used for the ANTs pipelines.  Also, contains related .png files of the template with superimposed ratio values.

