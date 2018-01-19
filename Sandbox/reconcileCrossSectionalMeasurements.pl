#! /usr/bin/perl -w

use File::Path;
use File::Basename;
use File::Find;
use File::Spec;
use Cwd;
use Cwd 'realpath';


my $badCsvFile = "adniCrossSectionalAntsMergeSubset_BadMeasurements.csv";
# my $goodCsvFile = "CrossSectional_CommonTimepoints.csv";
my $goodCsvFile = "thicknessResultsSubjectSpace.csv";
my $newCsvFile = "adniCrossSectionalAntsMergeSubset.csv";

open( FILE1, "<${badCsvFile}" );
my @badContents = <FILE1>;
close( FILE1 );

my @badImageIds = ();
for( my $i = 1; $i < @badContents; $i++ )
  {
  chomp( $badContents[$i] );
  my @tokens = split( ',', $badContents[$i] );
  push( @badImageIds, $tokens[9] );
  }

open( FILE2, "<${goodCsvFile}" );
my @goodContents = <FILE2>;
close( FILE2 );

my @goodImageIds = ();
for( my $i = 1; $i < @goodContents; $i++ )
  {
  chomp( $goodContents[$i] );
  my @tokens = split( ',', $goodContents[$i] );
  push( @goodImageIds, $tokens[14] );
  }


chomp( $badContents[0] );
my @tokens = split( ',', $badContents[0] );
my @colNames = splice( @tokens, 0, 90 );
my $colNamesString = join( ',', @colNames );

open( FILE3, ">${newCsvFile}" );
print FILE3 "$colNamesString\n";
for( my $i = 0; $i < @badImageIds; $i++ )
  {
  my $badImageId = $badImageIds[$i];
  print "$badImageId ($i out of $#badImageIds)\n";

  my $goodIndex = -1;
  for( my $j = 0; $j < @goodImageIds; $j++ )
    {
    if( $goodImageIds[$j] == $badImageId )
      {
      $goodIndex = $j + 1;
      last;
      }
    }
  if( $goodIndex == -1 )
    {
    print "$badImageId ImageId not found.\n";
    }

  my @badTokens = split( ",", $badContents[$i+1] );
  my @goodTokens = split( ",", $goodContents[$goodIndex] );

  my @newTokens = ( @goodTokens[0..5],
                    $badTokens[6],
                    @goodTokens[7..8],
                    $badImageId,
                    @goodTokens[9..12],
                    @goodTokens[15..25],
                    'NA', 'NA', 'NA',
                    @goodTokens[26..87]
                  );

  my $newString = join( ',', @newTokens );
  print FILE3 "$newString\n";
  }
close( FILE3 );