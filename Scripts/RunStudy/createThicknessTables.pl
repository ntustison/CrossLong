#! /usr/bin/perl -w

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use File::Find;
use FindBin qw($Bin);

my @roiLabels = ( 1002, 1003, 1005, 1006, 1007, 1008, 1009, 1010, 1011,
                  1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020,
                  1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029,
                  1030, 1031, 1034, 1035,
                  2002, 2003, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                  2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
                  2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029,
                  2030, 2031, 2034, 2035
                 );
my @roiNames = ( "left caudal anterior cingulate",
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
                 "right caudal anterior cingulate",
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
my $roiNamesString = join( ',', @roiNames );

my $basedir = '/Volumes/G-RAID/Data/ADNI/';

my $dktLabels = "${basedir}/Templates/Normal/antsDktMalfLabels.nii.gz";

my $csvfile =  "${basedir}/adniDemographics.csv";
open( FILE, "<${csvfile}" );
my @contents = <FILE>;
close( FILE );

my $resultsfile =  "${basedir}/thicknessResults.csv";
open( FILE, ">${resultsfile}" );
print FILE "ID,AGE,GENDER,DIAGNOSIS,WEIGHT,STUDY_ID,VISIT,SITE,FIELD_STRENGTH,IMAGE_ID,BRAIN_VOLUME,CSF_VOLUME,GM_VOLUME,WM_VOLUME,DEEP_GM_VOLUME,$roiNamesString\n";

my $count = 0;
find( \&wanted, "${basedir}/Nifti/" );

close( FILE );

sub wanted
  {
  if( $File::Find::name !~ m/\.nii\.gz$/ )
    {
    return;
    }

  $count++;

  my ( $filename, $directory, $suffix ) = fileparse( $File::Find::name, ".nii.gz" );

  ( my $subjectThicknessDir = $directory ) =~ s/Nifti/ThicknessAnts/;

#   my $thicknessMap = "${subjectThicknessDir}/${filename}_CorticalThicknessNormalizedToTemplate.nii.gz";
  my $thicknessMap = "${subjectThicknessDir}/${filename}_CorticalThickness.nii.gz";
  if( ! -e $thicknessMap )
    {
    return;
    }
  my $extractionMask = "${subjectThicknessDir}/${filename}_BrainExtractionMask.nii.gz";
  my $segmentation = "${subjectThicknessDir}/${filename}_BrainSegmentation.nii.gz";

  my $warp = "${subjectThicknessDir}/${filename}_TemplateToSubject0Warp.nii.gz";
  my $affine = "${subjectThicknessDir}/${filename}_TemplateToSubject1GenericAffine.mat";

  print "* *********************************************\n";
  print "Processing subject $count:  ${filename}\n";
  print "* *********************************************\n";

  my $refinedDktMalf = "${subjectThicknessDir}/${filename}_RefinedDktMalf.nii.gz";
  my $thicknessMask = "${subjectThicknessDir}/${filename}_ThicknessMask.nii.gz";

  my @xfrmArgs = ( 'antsApplyTransforms', '-d', 3,
                   '-i', $dktLabels,
                   '-r', $thicknessMap,
                   '-o', $refinedDktMalf,
                   '-n', 'NearestNeighbor',
                   '-t', $affine,
                   '-t', $warp );
  system( @xfrmArgs ) == 0 || die "Error (xfrm).\n";

  `ThresholdImage 3 $thicknessMap $thicknessMask 0 0 0 1`;
  `BinaryOperateImages 3 $refinedDktMalf x $thicknessMask $refinedDktMalf`;

  unlink( $thicknessMask );

  my @subjectXmlFiles = <${directory}/*.xml>;
  if( @subjectXmlFiles == 0 )
    {
    return;
    }
  open( XMLFILE, "<${subjectXmlFiles[0]}" );
  my @xmlContents = <XMLFILE>;
  close( XMLFILE );

  my $xmlId = '';
  for( my $i = 0; $i < @xmlContents; $i++ )
    {
    if( $xmlContents[$i] =~ m/image\ uid/ )
      {
      my @tokens = split( '\"', $xmlContents[$i] );
      ( $xmlId = $tokens[1] ) =~ s/^I//;
      last;
      }
    }
  if( $xmlId eq '' )
    {
    return;
    }

  my $subjectDemographics = '';
  for( my $j = 1; $j < @contents; $j++ )
    {
    my @tokens = split( ',', $contents[$j] );
    my $imageId = $tokens[-1];

    if( $imageId !~ m/$xmlId/ )
      {
      next;
      }

    $subjectDemographics = $contents[$j];
    chomp( $subjectDemographics );
    }

  if( $subjectDemographics eq '' )
    {
    return;
    }

  my @out = `CalculateVolumeFromBinaryImage 3 $extractionMask 1`;
  chomp( $out[1] );
  my $brainVolume = $out[1];

  @out = `CalculateVolumeFromBinaryImage 3 $segmentation 1`;
  chomp( $out[1] );
  my $csfVolume = $out[1];
  @out = `CalculateVolumeFromBinaryImage 3 $segmentation 2`;
  chomp( $out[1] );
  my $gmVolume = $out[1];
  @out = `CalculateVolumeFromBinaryImage 3 $segmentation 3`;
  chomp( $out[1] );
  my $wmVolume = $out[1];
  @out = `CalculateVolumeFromBinaryImage 3 $segmentation 4`;
  chomp( $out[1] );
  my $deepgmVolume = $out[1];

  @out = `LabelIntensityStatistics 3 $thicknessMap $refinedDktMalf`;
  my @out3 = `LabelGeometryMeasures 3 $refinedDktMalf`;

  my @subjectRoiThicknessMeans = ( 0 ) x @roiLabels;
  for( my $j = 1; $j < @out3; $j++ )
    {
    chomp( $out[$j] );
    my @labelstats = split( ' ', $out[$j] );

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
    $subjectRoiThicknessMeans[$roiIndex] = $labelstats[1];
    }

  my $roiString = join( ',', @subjectRoiThicknessMeans );
  print FILE "${subjectDemographics},${brainVolume},${csfVolume},${gmVolume},${wmVolume},${deepgmVolume},${roiString}\n";

  }

