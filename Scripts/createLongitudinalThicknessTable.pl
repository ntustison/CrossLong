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
                 "left caudal middle frontal",            # 1003
                 "left cuneus",                           # 1005
                 "left entorhinal",                       # 1006
                 "left fusiform",                         # 1007
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

my @singleSubjectTemplateFileTypes = ( "T_template0.nii.gz",
                                       "T_templateBrainExtractionMask.nii.gz",
                                       "T_templateBrainExtractionMaskPrior.nii.gz",
                                       "T_templateBrainExtractionRegistrationMask.nii.gz",
                                       "T_templateBrainNormalizedToTemplate.nii.gz",
                                       "T_templateBrainSegmentation.nii.gz",
                                       "T_templateBrainSegmentation0N4.nii.gz",
                                       "T_templateBrainSegmentationConvergence.txt",   #unimportant file
                                       "T_templateBrainSegmentationPosteriors1.nii.gz",
                                       "T_templateBrainSegmentationPosteriors2.nii.gz",
                                       "T_templateBrainSegmentationPosteriors3.nii.gz",
                                       "T_templateBrainSegmentationPosteriors4.nii.gz",
                                       "T_templateBrainSegmentationPosteriors5.nii.gz",
                                       "T_templateBrainSegmentationPosteriors6.nii.gz",
                                       "T_templateBrainSegmentationTiledMosaic.png",
                                       "T_templateCorticalThickness.nii.gz",
                                       "T_templateCorticalThicknessNormalizedToTemplate.nii.gz",
                                       "T_templateCorticalThicknessTiledMosaic.png",
                                       "T_templateExtractedBrain0N4.nii.gz",
                                       "T_templateMalfLabels.nii.gz",
                                       "T_templatePriors1.nii.gz",
                                       "T_templatePriors2.nii.gz",
                                       "T_templatePriors3.nii.gz",
                                       "T_templatePriors4.nii.gz",
                                       "T_templatePriors5.nii.gz",
                                       "T_templatePriors6.nii.gz",
                                       "T_templateRegistrationTemplateBrainMask.nii.gz",
                                       "T_templateSubjectToTemplate0GenericAffine.mat",
                                       "T_templateSubjectToTemplate1Warp.nii.gz",
                                       "T_templateSubjectToTemplateLogJacobian.nii.gz",
                                       "T_templateTemplateToSubject0Warp.nii.gz",
                                       "T_templateTemplateToSubject1GenericAffine.mat",
                                       "T_templatebrainvols.csv" );

my @timePointFileTypes = ( "BrainExtractionMask.nii.gz",
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
                           "GroupTemplateToSubjectWarp.nii.gz",
                           "MalfLabels.nii.gz",
                           "RegistrationTemplateBrainMask.nii.gz",
                           "RigidToSST0GenericAffine.mat",
                           "RigidToSSTWarped.nii.gz",
                           "SubjectToGroupTemplateWarp.nii.gz",
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

my $basedir = '/Volumes/G-RAID/Data/ADNI/';
my $thicknessdir = "${basedir}/LongitudinalThicknessAnts/";
# my @subjectDirs = <${basedir}/Nifti/???_S_????>;
my @subjectDirs = ( "${basedir}/Nifti/023_S_0331", "${basedir}/Nifti/023_S_0376" );

my $csvfile = "${basedir}/adniDemographicsX.csv";
open( FILE, "<${csvfile}" );
my @contents = <FILE>;
close( FILE );

my $resultsfile =  "${basedir}/adniLongitudinalAntsDifferentWeightXXX.csv";

my $atlasLabels = "${basedir}/Templates/Normal/DktMALF/antsDktMalfLabels.nii.gz";

open( FILE, ">${resultsfile}" );
chomp( ${contents[0]} );
print FILE "$contents[0],brain volume,csf volume,gray matter volume,white matter volume,deep gray matter volume,";
print FILE "brain stem volume,cerebellar volume,total mean thickness,";

# cortical thickness regions
my $numberOfThicknessLabels = 0;
for( my $i = 0; $i < @roiLabels; $i++ )
  {
  if( $roiLabels[$i] >= 1000 )
    {
    print FILE "thickness ${roiNames[$i]},";
    $numberOfThicknessLabels++;
    }
  }

# surface area regions
for( my $i = 0; $i < @roiLabels; $i++ )
  {
  print FILE "area ${roiNames[$i]},";
  }
# volume regions
for( my $i = 0; $i < @roiLabels; $i++ )
  {
  print FILE "volume ${roiNames[$i]},";
  }

# jacobian regions
for( my $i = 0; $i < @roiLabels - 1; $i++ )
  {
  if( $roiLabels[$i] >= 1000 )
    {
    print FILE "jacobian ${roiNames[$i]},";
    }
  }
print FILE "jacobian ${roiNames[@roiLabels-1]}\n";


for( my $i = 0; $i < @subjectDirs; $i++ )
  {
  #   print "${subjectDirs[$i]}\n";

  my $whichSubDirectory = '';
  my $maxNumberOfTimePoints = 0;

  my @subSubjectDirs = <${subjectDirs[$i]}/M*>;
  for( my $j = 0; $j < @subSubjectDirs; $j++ )
    {
    my @timePointDirs = <${subSubjectDirs[$j]}/*>;
    if( @timePointDirs > $maxNumberOfTimePoints )
      {
      $whichSubDirectory = ${subSubjectDirs[$j]};
      $maxNumberOfTimePoints = scalar( @timePointDirs );
      }
    }

  if( $maxNumberOfTimePoints <= 1 )
    {
    $singleCount++;
    next;
    }

  ( my $outputPrefix = $whichSubDirectory ) =~ s/Nifti/LongitudinalThicknessANTs/;
  ( my $outputPrefix2 = $whichSubDirectory ) =~ s/Nifti/LongitudinalThicknessANTsDifferentWeight/;

#   print "$outputPrefix\n";

  my $commandFile = "${outputPrefix}/antsLongitudinalThicknessCommand.sh";
  my $singleSubjectTemplateDirectory = "${outputPrefix}SingleSubjectTemplate";
  my $singleSubjectTemplateDirectory2 = "${outputPrefix2}SingleSubjectTemplate";

  #################################################################################
  #
  # Go through and check the individual time points.
  #
  #################################################################################

  my @timePointImages = <${whichSubDirectory}/*/*/ADNI*.nii.gz>;

  if( @timePointImages > 0 )
    {
    for( my $j = 0; $j < @timePointImages; $j++ )
      {
      my @comps = split( '/', $timePointImages[$j] );
      ( my $prefix = ${comps[-1]} ) =~ s/\.nii\.gz//;

      my ( $filenameX, $directoryX, $suffixX ) = fileparse( $timePointImages[$j], ".nii.gz" );

      my @subjectXmlFiles = <${directoryX}/*.xml>;
      if( @subjectXmlFiles == 0 )
        {
        next;
        }
      open( XMLFILE, "<${subjectXmlFiles[0]}" );
      my @xmlContents = <XMLFILE>;
      close( XMLFILE );

      my $xmlId = '';
      for( my $ii = 0; $ii < @xmlContents; $ii++ )
        {
        if( $xmlContents[$ii] =~ m/image\ uid/ )
          {
          my @tokens = split( '\"', $xmlContents[$ii] );
          ( $xmlId = $tokens[1] ) =~ s/^I//;
          last;
          }
        }
      if( $xmlId eq '' )
        {
        next;
        }

#       if( $xmlId !~ m/160811/ && $xmlId !~ m/107742/ && $xmlId !~ m/112246/ && $xmlId !~ m/33109/ && $xmlId !~ m/33114/ &&  $xmlId !~ m/106190/ )
#         {
#         next;
#         }
#       else
#         {
#         print "Doing $xmlId.\n";
#         }

      my $subjectDemographics = '';
      for( my $jj = 1; $jj < @contents; $jj++ )
        {
        my @tokens = split( ',', $contents[$jj] );
        my $imageId = $tokens[9];

        if( $imageId !~ m/$xmlId/ )
          {
          next;
          }

        $subjectDemographics = $contents[$jj];
        chomp( $subjectDemographics );
        }

      if( $subjectDemographics eq '' )
        {
        next;
        }

      my $thickness = "${singleSubjectTemplateDirectory2}/../${prefix}_${j}/${prefix}CorticalThickness.nii.gz";
      my $dktLabels = "${outputPrefix}/../${prefix}_${j}/${prefix}MalfLabels.nii.gz";
      my $brainMask = "${outputPrefix}/../${prefix}_${j}/${prefix}BrainExtractionMask.nii.gz";
      my $segmentation = "${singleSubjectTemplateDirectory2}/../${prefix}_${j}/${prefix}BrainSegmentation.nii.gz";
      my $jacobian = "${outputPrefix}/../${prefix}_${j}/${prefix}SubjectToGroupTemplateLogJacobian.nii.gz";
      my $jacobianWarp = "${outputPrefix}/../${prefix}_${j}/${prefix}SubjectToGroupTemplateWarp.nii.gz";

      if( ! -e $brainMask || ! -e $segmentation || ! -e $dktLabels || ! -e $jacobianWarp )
        {
        print "xxxxxx ${thickness}\n";
        print "xxxxxx ${brainMask}\n";
        print "xxxxxx ${segmentation}\n";
        print "xxxxxx ${dktLabels}\n";
        print "xxxxxx ${jacobianWarp}\n";
        next;
        }

      if( ! -e $jacobian )
        {
        `CreateJacobianDeterminantImage 3 $jacobianWarp $jacobian 1 1`;
        }

      my $localAtlasDktMalf = "${outputPrefix}/../${prefix}_${j}/${prefix}SubjectToGroupTemplateDktMalfLabels.nii.gz";
      if( ! -e $localAtlasDktMalf )
        {
        `ChangeImageInformation 3 $atlasLabels $localAtlasDktMalf 4 $jacobian`;
        }

      print "* *********************************************\n";
      print "Processing subject ${outputPrefix}/../${prefix}_${j}/\n";
      print "* *********************************************\n";

      my $string = `GetImageInformation 3 $dktLabels 1`;
      my @spacing = split( 'x', $string );
      my $voxelvolume = $spacing[0] * $spacing[1] * $spacing[2];

      my @out = `CalculateVolumeFromBinaryImage 3 $brainMask 1`;
      chomp( $out[1] );
      my $brainVolume = $out[1];

      @out = `CalculateVolumeFromBinaryImage 3 $segmentation 1`;
      chomp( $out[1] );
      my $csfVolume = $out[1];

      @out = `CalculateVolumeFromBinaryImage 3 $segmentation 2`;
      chomp( $out[1] );
      my $grayMatterVolume = $out[1];

      @out = `CalculateVolumeFromBinaryImage 3 $segmentation 3`;
      chomp( $out[1] );
      my $whiteMatterVolume = $out[1];

      @out = `CalculateVolumeFromBinaryImage 3 $segmentation 4`;
      chomp( $out[1] );
      my $deepGrayMatterVolume = $out[1];

      @out = `CalculateVolumeFromBinaryImage 3 $segmentation 5`;
      chomp( $out[1] );
      my $brainStemVolume = $out[1];

      @out = `CalculateVolumeFromBinaryImage 3 $segmentation 6`;
      chomp( $out[1] );
      my $cerebellarVolume = $out[1];

      # get thickness values

      my $tmpLabels = "${basedir}/tmpThicknessMask.nii.gz";
      `ThresholdImage 3 $thickness $tmpLabels 0 0 0 1`;
      my $tmpGM = "${basedir}/tmpGM.nii.gz";
      my $tmpDkt = "${basedir}/tmpDkt.nii.gz";
      `ThresholdImage 3 $segmentation $tmpGM 2 2 1 0`;
      `BinaryOperateImages 3 $tmpGM x $tmpLabels $tmpLabels`;
      `BinaryOperateImages 3 $tmpGM x $dktLabels $tmpDkt`;

      # get mean thickness
      my @out3 = `LabelIntensityStatistics 3 $thickness $tmpLabels`;
      chomp( $out3[1] );
      my @labelstats = split( ' ', $out3[1] );
      my $totalMeanThickness = $labelstats[1];
      `BinaryOperateImages 3 $tmpLabels x $dktLabels $tmpLabels`;

      print FILE "$subjectDemographics,$brainVolume,$csfVolume,$grayMatterVolume,$whiteMatterVolume,$deepGrayMatterVolume,$brainStemVolume,$cerebellarVolume,$totalMeanThickness,";

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
      my $roiString = join( ',', @subjectRoiThicknessMeans );
      print FILE "${roiString},";

      # get surface area and volume values

      print "   Getting areas and volumes\n";

      my @subjectRoiSurfaceAreas = ( 'NA' ) x scalar( @roiLabels );
      my @subjectRoiVolumes = ( 'NA' ) x scalar( @roiLabels );

      @out3 = `LabelGeometryMeasures 3 $tmpDkt`;

      for( my $j = 1; $j < @out3; $j++ )
        {
        chomp( $out3[$j] );
        my @labelstats = split( ' ', $out3[$j] );
        my $roiIndex = -1;
        for( my $k = 0; $k < @roiLabels; $k++ )
          {
          if( $labelstats[0] == $roiLabels[$k] )
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
        $subjectRoiSurfaceAreas[$roiIndex] = $labelstats[2];
        $subjectRoiVolumes[$roiIndex] = $labelstats[1] * $voxelvolume;
        }
      my $roiString2 = join( ',', @subjectRoiSurfaceAreas );
      my $roiString3 = join( ',', @subjectRoiVolumes );
      print FILE "${roiString2},${roiString3},";

      # get jacobians

      print "   Getting jacobians\n";

      my @subjectRoiJacobianMeans = ( 'NA' ) x $numberOfThicknessLabels;

      @out3 = `LabelIntensityStatistics 3 $jacobian $localAtlasDktMalf`;

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
        $subjectRoiJacobianMeans[$roiIndex] = $labelstats[1];
        }
      my $roiString4 = join( ',', @subjectRoiJacobianMeans );
      print FILE "${roiString4}\n";

      unlink( $tmpLabels );
      unlink( $tmpGM );
      unlink( $tmpDkt );
      }
    }
  }
close( FILE );
