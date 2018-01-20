#! /usr/bin/perl -w

use File::Basename;
use File::Path;
use File::Spec;

# https://github.com/binarybottle/mindboggle/blob/master/LABELS

my @roiLabels = ( 16, 24, 14, 15, 72, 85, 4, 5, 6, 7, 10, 11, 12, 13, 17, 18,
                  25, 26, 28, 30, 91, 43, 44, 45, 46, 49, 50, 51, 52, 53, 54,
                  57, 58, 60, 62, 92, 630, 631, 632,
                  1002, 1003, 1005, 1006, 1007, 1008, 1009, 1010, 1011,
                  1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020,
                  1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029,
                  1030, 1031, 1034, 1035,
                  2002, 2003, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                  2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
                  2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029,
                  2030, 2031, 2034, 2035
                 );

my @roiNames = ( "brain stem",
                 "csf",
                 "3rd ventricle",
                 "4th ventricle",
                 "5th ventricle",
                 "optic chiasm",
                 "left lateral ventricle",
                 "left inferior lateral ventricle",
                 "left cerebellum exterior",
                 "left cerebellum white matter",
                 "left thalamus proper",
                 "left caudate",
                 "left putamen",
                 "left pallidum",
                 "left hippocampus",
                 "left amygdala",
                 "left lesion",
                 "left accumbens area",
                 "left ventral DC",
                 "left vessel",
                 "left basal forebrain",
                 "right lateral ventricle",
                 "right inferior lateral ventricle",
                 "right cerebellum exterior",
                 "right cerebellum white matter",
                 "right thalamus proper",
                 "right caudate",
                 "right putamen",
                 "right pallidum",
                 "right hippocampus",
                 "right amygdala",
                 "right lesion",
                 "right accumbens area",
                 "right ventral DC",
                 "right vessel",
                 "right basal forebrain",
                 "cerebellar vermal lobules I-V",
                 "cerebellar vermal lobules VI-VII",
                 "cerebellar vermal lobules VIII-X",
                 "left caudal anterior cingulate",        # 1002
                 "left caudal middle frontal",
                 "left cuneus",
                 "left entorhinal",
                 "left fusiform",
                 "left inferior parietal",
                 "left inferior temporal",
                 "left isthmus cingulate",
                 "left lateral occipital",
                 "left lateral orbitofrontal",
                 "left lingual",
                 "left medial orbitofrontal",
                 "left middle temporal",
                 "left parahippocampal",
                 "left paracentral",
                 "left pars opercularis",
                 "left pars orbitalis",
                 "left pars triangularis",
                 "left pericalcarine",
                 "left postcentral",
                 "left posterior cingulate",
                 "left precentral",
                 "left precuneus",
                 "left rostral anterior cingulate",
                 "left rostral middle frontal",
                 "left superior frontal",
                 "left superior parietal",
                 "left superior temporal",
                 "left supramarginal",
                 "left transverse temporal",
                 "left insula",
                 "right caudal anterior cingulate",      # 2002
                 "right caudal middle frontal",
                 "right cuneus",
                 "right entorhinal",
                 "right fusiform",
                 "right inferior parietal",
                 "right inferior temporal",
                 "right isthmus cingulate",
                 "right lateral occipital",
                 "right lateral orbitofrontal",
                 "right lingual",
                 "right medial orbitofrontal",
                 "right middle temporal",
                 "right parahippocampal",
                 "right paracentral",
                 "right pars opercularis",
                 "right pars orbitalis",
                 "right pars triangularis",
                 "right pericalcarine",
                 "right postcentral",
                 "right posterior cingulate",
                 "right precentral",
                 "right precuneus",
                 "right rostral anterior cingulate",
                 "right rostral middle frontal",
                 "right superior frontal",
                 "right superior parietal",
                 "right superior temporal",
                 "right supramarginal",
                 "right transverse temporal",
                 "right insula"
                 );


my @fileTypes = ( "BrainExtractionMask.nii.gz",
                           "BrainNormalizedToTemplate.nii.gz",
                           "BrainSegmentation.nii.gz",
                           "BrainSegmentation0N4.nii.gz",
                           "BrainSegmentationConvergence.txt",
                           "BrainSegmentationPosteriors1.nii.gz",
                           "BrainSegmentationPosteriors2.nii.gz",
                           "BrainSegmentationPosteriors3.nii.gz",
                           "BrainSegmentationPosteriors4.nii.gz",
                           "BrainSegmentationPosteriors5.nii.gz",
                           "BrainSegmentationPosteriors6.nii.gz",
                           "BrainSegmentationTiledMosaic.png",
                           "CorticalThickness.nii.gz",
                           "CorticalThicknessNormalizedToTemplate.nii.gz",
                           "CorticalThicknessTiledMosaic.png",
                           "MalfLabels.nii.gz",
                           "SubjectToTemplate0GenericAffine.mat",
                           "SubjectToTemplate1Warp.nii.gz",
                           "SubjectToTemplateLogJacobian.nii.gz",
                           "TemplateToSubject0Warp.nii.gz",
                           "TemplateToSubject1GenericAffine.mat",
                           "brainvols.csv"
                           );



for( my $i = 0; $i < @roiLabels; $i++ )
  {
  print "$roiLabels[$i] -> $roiNames[$i]\n";
  }

my $baseDir = '/Volumes/G-RAID/Data/ADNI';
my $resultsFile = "${baseDir}/adniLongitudinalNativeSpaceAntsMergeSubset.csv";
my $longitudinalFile = "${baseDir}/adniLongitudinalAntsMergeSubset.csv";
my $crossSectionalFile = "${baseDir}/adniCrossSectionalAntsMergeSubset.csv";

my $templateDir = "${baseDir}/Templates/Normal/antsDktMalfLabels.nii.gz";
my $longitudinalDir = "${baseDir}/LongitudinalThicknessANTs/";
my $longitudinalNativeSpaceDir = "${baseDir}/LongitudinalThicknessANTsNativeSpace/";
my $crossSectionalDir = "${baseDir}/SingleTimePointAnalysisOld/ThicknessAnts/";

my $atlasLabels = "${templateDir}/antsDktMalfLabels.nii.gz";

open( INFILE, $crossSectionalFile );
my @crossSectionalData = <INFILE>;
close( INFILE );

my $numberOfThicknessLabels = 62;

open( FILE, ">${resultsFile}" );
print FILE "${crossSectionalData[0]}";

for( my $i = 1; $i < @crossSectionalData; $i++ )
  {
  my @crossSectionalSingleSubject = split( ',', $crossSectionalData[$i] );
  my $subjectId = $crossSectionalSingleSubject[0];
  my $imageId = $crossSectionalSingleSubject[9];

  print "$subjectId -> $imageId\n";

  my @tmp = <${crossSectionalDir}/${subjectId}/*/*//*/*_I${imageId}_RefinedDktMalf.nii.gz>;
  if( @tmp != 1 )
    {
    print "${crossSectionalDir}/${subjectId}/*/*//*/*_I${imageId}_RefinedDktMalf.nii.gz\n";
    die "Error:  dkt malf\n";
    }
  my $dktLabels = $tmp[0];
  my @comps = split( '_', $dktLabels );
  my $otherId = $comps[-3];

  @tmp = <${longitudinalNativeSpaceDir}/${subjectId}/ADNI*_${otherId}_I${imageId}*//ADNI*_${otherId}_I${imageId}CorticalThickness.nii.gz>;
  if( @tmp != 1 )
    {
    print "${longitudinalNativeSpaceDir}/${subjectId}/ADNI*_${otherId}_I${imageId}*//ADNI*_${otherId}_I${imageId}CorticalThickness.nii.gz\n";
    die "Error:  thickness\n";
    }
  my $thickness = $tmp[0];

  print "$thickness\n";
  print "$dktLabels\n";

  # get thickness values

  my $tmpLabels = "${baseDir}/tmpThicknessMask.nii.gz";
  `ThresholdImage 3 $thickness $tmpLabels 0 0 0 1`;

  # get mean thickness
  my @out3 = `LabelIntensityStatistics 3 $thickness $tmpLabels`;
  chomp( $out3[1] );
  my @labelstats = split( ' ', $out3[1] );
  my $totalMeanThickness = $labelstats[1];
  `BinaryOperateImages 3 $tmpLabels x $dktLabels $tmpLabels`;

  my @subjectRoiThicknessMeans = ( 'NA' ) x $numberOfThicknessLabels;

  @out3 = `LabelIntensityStatistics 3 $thickness $tmpLabels`;

  for( my $j = 1; $j < @out3; $j++ )
    {
    chomp( $out3[$j] );
    my @labelstats = split( ' ', $out3[$j] );
    my $roiIndex = -1;
    for( my $k = 0; $k < @roiLabels; $k++ )
      {
      if( $labelstats[0] == $roiLabels[$k] && $labelstats[0] >= 1000 )
        {
        $roiIndex = $k;
        last;
        }
      }
    print "$labelstats[0] -> $roiIndex\n";
    if( $roiIndex == -1 )
      {
      next;
      }

    $roiIndex -= 39;
    $subjectRoiThicknessMeans[$roiIndex] = $labelstats[1];
    }
  unshift( @subjectRoiThicknessMeans, $totalMeanThickness );
  my @longitudinalNativeSpaceData = @crossSectionalSingleSubject;
  splice( @longitudinalNativeSpaceData, 27, $numberOfThicknessLabels+1, @subjectRoiThicknessMeans );

  my $roiString = join( ',',  @longitudinalNativeSpaceData );

  print FILE "$roiString";
  }
close( FILE );
