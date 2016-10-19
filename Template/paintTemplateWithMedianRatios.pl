#! /usr/bin/perl -w

use strict;

use File::Basename;
use File::Path;
use File::Spec;

my $templateDkt = 'T_template0DktLabels.nii.gz';
my $templateDktMask = 'T_template0DktLabelsMask.nii.gz';
my $templateDktSmoothMask = 'T_template0DktLabelsSmoothMask.nii.gz';
my $ratioCsvFile = '../data/median_ratios.csv';
my $templateRatios = 'T_template0MedianRatios.nii.gz';
my $templateRatiosRgb = 'T_template0MedianRatios.mha';
my $templateRatiosDilated = 'T_template0MedianRatiosDilated.nii.gz';
my $templateRatiosDilatedRgb = 'T_template0MedianRatiosDilated.mha';

my $lookupTable = 'lookupTable.vtk';

`cp $templateDkt $templateRatios`;
`ThresholdImage 3 $templateDkt $templateDktMask 0 0 0 1`;
`SmoothImage 3 $templateDktMask 1.0 $templateDktSmoothMask 1`;
`ThresholdImage 3 $templateDktSmoothMask $templateDktSmoothMask 0.1 10 1 0`;

open( FILE, "<${ratioCsvFile}" );
my @csvContents = <FILE>;
close( FILE );

# values are column 4

my @ratios = ();
for( my $i = 1; $i < @csvContents; $i++ )
  {
  chomp( $csvContents[$i] );
  my @stats = split( ',', $csvContents[$i] );

  push( @ratios, $stats[3] );
  }

my @dktLabels = ();
my @out = `LabelGeometryMeasures 3 $templateDkt`;
for( my $i = 1; $i < @out; $i++ )
  {
  my @stats = split( ' ', $out[$i] );
  my $label = $stats[0];

  push( @dktLabels, $label );
  }

my @pipelines = ( 'cross', 'long1', 'long2' );

for( my $p = 0; $p < @pipelines; $p++ )
  {
  `cp $templateDkt $templateRatios`;

  for( my $i = 0; $i < @dktLabels; $i++ )
    {

    my $index = $i + scalar( @dktLabels ) * $p;

    print "Replacing $dktLabels[$i] -> ($index) ${ratios[$index]} \n";
    `ImageMath 3 $templateRatios ReplaceVoxelValue $templateRatios $dktLabels[$i] $dktLabels[$i] $ratios[$index]`;
    }

  `ImageMath 3 $templateRatiosDilated GD $templateRatios 3`;

  `ConvertScalarImageToRGB 3 $templateRatios $templateRatiosRgb $templateDktMask jet none 2.0 7.0 0 255 $lookupTable`;
  `ConvertScalarImageToRGB 3 $templateRatiosDilated $templateRatiosDilatedRgb none jet none 2.0 7.0 0 255 $lookupTable`;


  my @args = ( 'antsSurf', '-d', 3,
                           '-s', "[${templateDktMask},255x255x255]",
                           '-f', "[${templateRatiosDilatedRgb},${templateDktSmoothMask},0.75]",
                           '-i', 25,
                           '-a', 0.03,
                           '-d', '[0x0x0,255x255x255]',
                           '-b', $lookupTable
                     );
#   system( @args );


  `antsVol -i sum.nii.gz -d ${pipelines[$p]}_median_posterior3d_90x90x0.png[1.5,90x90x0,255x255x255] -f $templateRatiosRgb`;
  `antsVol -i sum.nii.gz -d ${pipelines[$p]}_median_superior3d_90x180x0.png[1.5,90x180x0,255x255x255] -f $templateRatiosRgb`;
  `antsVol -i sum.nii.gz -d ${pipelines[$p]}_median_right3d_180x0x90.png[1.5,180x0x90,255x255x255] -f $templateRatiosRgb`;
  `antsVol -i sum.nii.gz -d ${pipelines[$p]}_median_left3d_0x0x270.png[1.5,0x0x270,255x255x255] -f $templateRatiosRgb`;
  `antsVol -i sum.nii.gz -d ${pipelines[$p]}_median_frontal3d_90x270x0.png[1.5,90x270x0,255x255x255] -f $templateRatiosRgb`;


  @args = ( 'CreateTiledMosaic',
                  '-d', '2',
                  '-i', 'T_template0.nii.gz',
                  '-r', 'T_template0MedianRatios.mha',
                  '-x', 'T_template0DktLabelsMask.nii.gz',
                  '-a', 0.5,
                  '-o', "${pipelines[$p]}_tiledMosaic.png",
                  '-s', '[2,mask-2,mask+2]',
                  '-p', 'mask+5',
                  '-g', 1 );
  system( @args );
  }

