#! /usr/bin/perl -w

use strict;

use File::Basename;
use File::Path;
use File::Spec;

my $templateDkt = 'T_template0DktLabels.nii.gz';
my $templateDktMask = 'T_template0DktLabelsMask.nii.gz';
my $templateDktSmoothMask = 'T_template0DktLabelsSmoothMask.nii.gz';
my $ratioCsvFile = '../data/var_ratios_2.csv';
my $templateNormedRatios = 'T_template0NormedRatios.nii.gz';
my $templateUnnormedRatios = 'T_template0UnnormedRatios.nii.gz';
my $templateNormedRatiosRgb = 'T_template0NormedRatios.mha';
my $templateUnnormedRatiosRgb = 'T_template0UnnormedRatios.mha';
my $templateNormedRatiosDilated = 'T_template0NormedRatiosDilated.nii.gz';
my $templateUnnormedRatiosDilated = 'T_template0UnnormedRatiosDilated.nii.gz';
my $templateNormedRatiosDilatedRgb = 'T_template0NormedRatiosDilated.mha';
my $templateUnnormedRatiosDilatedRgb = 'T_template0UnnormedRatiosDilated.mha';

my $normedLookupTable = 'normedLookupTable.vtk';
my $unnormedLookupTable = 'unnormedLookupTable.vtk';

`cp $templateDkt $templateNormedRatios`;
`cp $templateDkt $templateUnnormedRatios`;
`ThresholdImage 3 $templateDkt $templateDktMask 0 0 0 1`;
`SmoothImage 3 $templateDktMask 1.0 $templateDktSmoothMask 1`;
`ThresholdImage 3 $templateDktSmoothMask $templateDktSmoothMask 0.1 10 1 0`;

open( FILE, "<${ratioCsvFile}" );
my @csvContents = <FILE>;
close( FILE );

# unnormed values are column 3
# normed values are column 4

my @normedRatios = ();
my @unnormedRatios = ();
for( my $i = 1; $i < @csvContents; $i++ )
  {
  chomp( $csvContents[$i] );
  my @stats = split( ',', $csvContents[$i] );

  push( @unnormedRatios, $stats[2] );
  push( @normedRatios, $stats[3] );
  }

my @dktLabels = ();
my @out = `LabelGeometryMeasures 3 $templateDkt`;

for( my $i = 1; $i < @out; $i++ )
  {
  my @stats = split( ' ', $out[$i] );
  my $label = $stats[0];

  push( @dktLabels, $label );
  }

for( my $i = 0; $i < @dktLabels; $i++ )
  {
  print "Replacing $dktLabels[$i]\n";
  `ImageMath 3 $templateNormedRatios ReplaceVoxelValue $templateNormedRatios $dktLabels[$i] $dktLabels[$i] $normedRatios[$i]`;
  `ImageMath 3 $templateUnnormedRatios ReplaceVoxelValue $templateUnnormedRatios $dktLabels[$i] $dktLabels[$i] $unnormedRatios[$i]`;
  }

`ImageMath 3 $templateNormedRatiosDilated GD $templateNormedRatios 3`;
`ImageMath 3 $templateUnnormedRatiosDilated GD $templateUnnormedRatios 3`;

`ConvertScalarImageToRGB 3 $templateNormedRatios $templateNormedRatiosRgb $templateDktMask jet none -0.2 0.2 0 255 $normedLookupTable`;
`ConvertScalarImageToRGB 3 $templateUnnormedRatios $templateUnnormedRatiosRgb $templateDktMask jet none -1.5 1.5 0 255 $unnormedLookupTable`;

`ConvertScalarImageToRGB 3 $templateNormedRatiosDilated $templateNormedRatiosDilatedRgb none jet none -0.2 0.2 0 255 $normedLookupTable`;
`ConvertScalarImageToRGB 3 $templateUnnormedRatiosDilated $templateUnnormedRatiosDilatedRgb none jet none -1.5 1.5 0 255 $unnormedLookupTable`;


my @normedArgs = ( 'antsSurf', '-d', 3,
                                 '-s', "[${templateDktMask},255x255x255]",
                                 '-f', "[${templateNormedRatiosDilatedRgb},${templateDktSmoothMask},0.75]",
                                 '-i', 25,
                                 '-a', 0.03,
                                 '-d', '[0x0x0,255x255x255]',
                                 '-b', $normedLookupTable
                   );
system( @normedArgs );

my @unnormedArgs = ( 'antsSurf', '-d', 3,
                                 '-s', "[${templateDktMask},255x255x255]",
                                 '-f', "[${templateUnnormedRatiosDilatedRgb},${templateDktSmoothMask},0.75]",
                                 '-i', 25,
                                 '-a', 0.03,
                                 '-d', '[0x0x0,255x255x255]',
                                 '-b', $unnormedLookupTable
                   );
system( @unnormedArgs );

system( "sh createPngs.sh" );

