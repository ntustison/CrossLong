#/usr/bin/perl -w

use strict;

use Cwd 'realpath';
use Switch;
use File::Find;
use File::Basename;
use File::Path;
use File::Spec;
use FindBin qw($Bin);

my $usage = qq{
  Usage: runLongitudinalThicknessStudyFreeSurfer.pl
 };

my $baseDirectory = '/bio/ntustiso/Data/Public/ADNI/';

my $fsCrossSectionalFile = "${baseDirectory}/freesurferCrossSectionalSubset.csv";
my $fsLongitudinalFile = "${baseDirectory}/freesurferLongitudinalSubset.csv";

open( FILE_FS_X, ">${fsCrossSectionalFile}" );
open( FILE_FS_L, ">${fsLongitudinalFile}" );

my $csvFile = "${baseDirectory}/Scripts/adniCrossSectionalAntsMergeSubset.csv";
open( FILE, "<${csvFile}" );
my @contents = <FILE>;
close( FILE );

my $headerString = ${contents[0]};
chomp( $headerString );
my @colNamesLong = split( ',', $headerString );
my @colNames = ( 'ID', 'IMAGE_ID', splice( @colNamesLong, 28, 62 ) );
my $colNamesString = join( ',', @colNames );

print FILE_FS_X "$colNamesString\n";
print FILE_FS_L "$colNamesString\n";

my @studyIDs = ();
my @studyImageIDs = ();
for( my $i = 1; $i < @contents; $i++ )
  {
  my @elements = split( ',', $contents[$i] );
  push( @studyIDs, $elements[0] );
  push( @studyImageIDs, $elements[9] );
  }
my @uniqueStudyIDs = do { my %seen; grep { !$seen{$_}++ } @studyIDs };

for( my $i = 0; $i < @uniqueStudyIDs; $i++ )
  {
  my $subjectDir = "${baseDirectory}/Nifti/${uniqueStudyIDs[$i]}/";
  ( my $outputPrefix = $subjectDir ) =~ s/Nifti/LongitudinalThicknessFreeSurfer/;

  my @comps = split( '/', $outputPrefix );
  my $archiveName = "${comps[-1]}.tar.gz";

  if( -e "${outputPrefix}/${archiveName}" && ! -d "${outputPrefix}/${comps[-1]}/" )
    {
    `tar -xvzf ${outputPrefix}/${archiveName} -C ${outputPrefix}/`;
    }

  my @subjectImageIDs = ();
  for( my $j = 0; $j < @studyIDs; $j++ )
    {
    if( ${studyIDs[$j]} =~ m/${uniqueStudyIDs[$i]}/ )
      {
      push( @subjectImageIDs, ${studyImageIDs[$j]} );
      }
    }

  my @timePointImages = ();
  my @timePointBaseNames = ();
  for( my $j = 0; $j < @subjectImageIDs; $j++ )
    {
    my @tmp = <${subjectDir}/*/*/*/ADNI*I${subjectImageIDs[$j]}.nii.gz>;
    if( scalar( @tmp ) != 1 )
      {
      die "No match:  ${subjectDir}/*/*/*/ADNI*I${subjectImageIDs[$j]}.nii.gz\n";
      }
    push( @timePointImages, $tmp[0] );

    my ( $filename, $path, $suffix ) = fileparse( $timePointImages[$j], '.nii.gz' );
    push( @timePointBaseNames, $filename );
    }

#   my $commandFile = "${outputPrefix}/freesurferLongitudinalThicknessCommand.sh";

  # Parse the stats file for each time point

  # get cross-sectional numbers

  for( my $j = 0; $j < @timePointBaseNames; $j++ )
    {
    my $leftStatsFile = "${outputPrefix}/${uniqueStudyIDs[$i]}/${timePointBaseNames[$j]}/stats/lh.aparc.DKTatlas40.stats";
    open( FILE_S, "<${leftStatsFile}" );
    my @contentsS = <FILE_S>;
    close( FILE_S );
    my @leftThicknessMeasurements = ();
    for( my $k = 53; $k <= 83; $k++ )
      {
      chomp( $contentsS[$k] );
      my @stats = split( ' ', $contentsS[$k] );
      push( @leftThicknessMeasurements, $stats[4] );
      }
    my $leftThicknessString = join( ',', @leftThicknessMeasurements );

    my $rightStatsFile = "${outputPrefix}/${uniqueStudyIDs[$i]}/${timePointBaseNames[$j]}/stats/rh.aparc.DKTatlas40.stats";
    open( FILE_S, "<${rightStatsFile}" );
    @contentsS = <FILE_S>;
    close( FILE_S );
    my @rightThicknessMeasurements = ();
    for( my $k = 53; $k <= 83; $k++ )
      {
      chomp( $contentsS[$k] );
      my @stats = split( ' ', $contentsS[$k] );
      push( @rightThicknessMeasurements, $stats[4] );
      }
    my $rightThicknessString = join( ',', @rightThicknessMeasurements );

    print FILE_FS_X "${uniqueStudyIDs[$i]},${subjectImageIDs[$j]},${leftThicknessString},${rightThicknessString}\n"
    }

  # get longitudinal numbers

  for( my $j = 0; $j < @timePointBaseNames; $j++ )
    {
    my $leftStatsFile = "${outputPrefix}/${uniqueStudyIDs[$i]}/${timePointBaseNames[$j]}.long.Template/stats/lh.aparc.DKTatlas40.stats";
    open( FILE_S, "<${leftStatsFile}" );
    my @contentsS = <FILE_S>;
    close( FILE_S );
    my @leftThicknessMeasurements = ();
    for( my $k = 53; $k <= 83; $k++ )
      {
      chomp( $contentsS[$k] );
      my @stats = split( ' ', $contentsS[$k] );
      push( @leftThicknessMeasurements, $stats[4] );
      }
    my $leftThicknessString = join( ',', @leftThicknessMeasurements );

    my $rightStatsFile = "${outputPrefix}/${uniqueStudyIDs[$i]}/${timePointBaseNames[$j]}.long.Template/stats/rh.aparc.DKTatlas40.stats";
    open( FILE_S, "<${rightStatsFile}" );
    @contentsS = <FILE_S>;
    close( FILE_S );
    my @rightThicknessMeasurements = ();
    for( my $k = 53; $k <= 83; $k++ )
      {
      chomp( $contentsS[$k] );
      my @stats = split( ' ', $contentsS[$k] );
      push( @rightThicknessMeasurements, $stats[4] );
      }
    my $rightThicknessString = join( ',', @rightThicknessMeasurements );

    print FILE_FS_L "${uniqueStudyIDs[$i]},${subjectImageIDs[$j]},${leftThicknessString},${rightThicknessString}\n"
    }
  }

close( FILE_FS_X );
close( FILE_FS_L );
