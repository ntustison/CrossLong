#! /usr/bin/perl -w

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use File::Find;
use FindBin qw($Bin);

my @roiLabels = ( 5, 6, 7, 8 );
my @roiNames = ( "left aLEC",
                 "right aLEC",
                 "left pMEC",
                 "right pMEC",
                 "left both",
                 "right both"
                 );
my $roiNamesString = join( ',', @roiNames );

my $csvfile =  "/Users/ntustison/Data/Public/CrossLong/data/adniLongitudinalAntsMergeSubset_WithScr.csv";
open( FILE, "<${csvfile}" );
my @contents = <FILE>;
close( FILE );

chomp( $contents[0] );
my @tokens = split( ',', $contents[0] );
my @demographics = @tokens[0..9];
my $demographicsString = join( ',', @demographics );

my $resultsfile =  "/Users/ntustison/Desktop//adniEntorhinalMergeSubset_withScr.csv";
open( FILE, ">${resultsfile}" );
print FILE "$demographicsString,$roiNamesString\n";

for( my $i = 1; $i < @contents; $i++ )
  {
  chomp( $contents[$i] );
  my @tokens = split( ',', $contents[$i] );
  my $id = $tokens[0];
  my $imageId = $tokens[6];
  my @demographics = @tokens[0..9];
  my $demographicsString = join( ',', @demographics );

  my @meanThicknesses = ( 'NA' ) x ( scalar( @roiLabels ) + 2 );

  print "${id} --> ${imageId}\n";

  my @tmp = </Users/ntustison/Desktop/EntorhinalResults/ADNI_${id}_*_I${imageId}EntorhinalThicknessResults.txt>;
  if( @tmp == 1 )
    {
    open( FILE2, "<${tmp[0]}" );
    my @localContents = <FILE2>;
    close( FILE2 );

    my @sums = ( 0 ) x scalar( @roiLabels );

    for( my $j = 1; $j < @localContents; $j++ )
      {
      chomp( $localContents[$j] );
      my @localTokens = split( ' ', $localContents[$j] );

      my $label = $localTokens[0];
      my $meanThickness = $localTokens[1];
      my $sum = $localTokens[6];

      my ( $index ) = grep { $roiLabels[$_] eq $label } ( 0 .. @roiLabels-1 );
      $meanThicknesses[$index] = $meanThickness;
      $sums[$index] = $sum;
      }
    if( $sums[0] > 0 && $sums[2] > 0 )
      {
      $meanThicknesses[4] = ( $sums[0] + $sums[2] ) / ( $sums[0] / $meanThicknesses[0] + $sums[2] / $meanThicknesses[2] );
      }
    if( $sums[1] > 0 && $sums[3] > 0 )
      {
      $meanThicknesses[5] = ( $sums[1] + $sums[3] ) / ( $sums[1] / $meanThicknesses[1] + $sums[3] / $meanThicknesses[3] );
      }
    }
  my $meanThicknessesString = join( ',', @meanThicknesses );
  print FILE "$demographicsString,$meanThicknessesString\n";
  }
close( FILE );
