#/usr/bin/perl -w

use strict;

use Cwd 'realpath';
use Switch;
use File::Find;
use File::Basename;
use File::Path;
use File::Spec;
use FindBin qw($Bin);
use File::Copy::Recursive qw(fcopy rcopy dircopy fmove rmove dirmove);

 my $baseDirectory = "/Volumes/BlueBook/ADNI/Nifti/";

my @suffixList = ( ".nii.gz" );
my @subjectDirs = <${baseDirectory}/???_S_????>;


my $count = 0;
my $singleCount = 0;
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
  ( my $redoDirectory = $whichSubDirectory ) =~ s/Nifti/LongitudinalThicknessANTsRedo/;

  my $commandFile = "${outputPrefix}/antsLongitudinalThicknessCommand.sh";

  my $singleSubjectTemplateDirectory = "${outputPrefix}SingleSubjectTemplate";

  # check if template prior 6 exists since that is one of the last things we do
  # dealing with the template

  if( ! -e "${singleSubjectTemplateDirectory}/T_templatePriors6.nii.gz" )
    {
    # missing 109 files.
    $count++;
    local $File::Copy::Recursive::SkipFlop = 1;
    dircopy( $outputPrefix, $redoDirectory );
    }
  else
    {
    my @timePointImages = <${whichSubDirectory}/*/*/ADNI*.nii.gz>;

    if( @timePointImages > 0 )
      {
      for( my $j = 0; $j < @timePointImages; $j++ )
        {
        my @comps = split( '/', $timePointImages[$j] );
        ( my $prefix = ${comps[-1]} ) =~ s/\.nii\.gz//;
        my $timePointCorticalThickness = "${outputPrefix}/../${prefix}_${j}/${prefix}CorticalThicknessNormalizedToTemplate.nii.gz";
        if( ! -e $timePointCorticalThickness )
          {
          $count++;
          local $File::Copy::Recursive::SkipFlop = 1;
          dircopy( $outputPrefix, $redoDirectory );
          last;
          }
        }
      }
    }

  }

my $total = scalar( @subjectDirs );

print "Missing $count files (out of $total).\n";
print "Singles = $singleCount.\n";
