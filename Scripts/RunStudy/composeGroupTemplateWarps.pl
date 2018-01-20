#/usr/bin/perl -w

use strict;

use Cwd 'realpath';
use Switch;
use File::Find;
use File::Basename;
use File::Path;
use File::stat;
use Time::Piece;
use File::Spec;
use FindBin qw($Bin);
use File::Copy::Recursive qw(fcopy rcopy dircopy fmove rmove dirmove);

 my $baseDirectory = "/Volumes/BlueBook/ADNI/Nifti/";

my @suffixList = ( ".nii.gz" );
my @subjectDirs = <${baseDirectory}/???_S_????>;


my $groupTemplate = "${baseDirectory}/../Templates/Normal/T_template0.nii.gz";

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

  my $commandFile = "${outputPrefix}/antsLongitudinalThicknessCommand.sh";

  my $singleSubjectTemplateDirectory = "${outputPrefix}SingleSubjectTemplate";

  my @timePointImages = <${whichSubDirectory}/*/*/ADNI*.nii.gz>;

  if( @timePointImages > 0 )
    {
    for( my $j = 0; $j < @timePointImages; $j++ )
      {
      my @comps = split( '/', $timePointImages[$j] );
      ( my $prefix = ${comps[-1]} ) =~ s/\.nii\.gz//;

      my $timePointGroupWarp = "${outputPrefix}/../${prefix}_${j}/${prefix}GroupTemplateToSubjectWarp.nii.gz";
      my $timePointGroupInverseWarp = "${outputPrefix}/../${prefix}_${j}/${prefix}SubjectToGroupTemplateWarp.nii.gz";

      my $timePointN4 = "${outputPrefix}/../${prefix}_${j}/${prefix}BrainSegmentation0N4.nii.gz";

      if( ! -e $timePointN4 )
        {
        next;
        }

      my $doTimePointGroupWarp = 0;
      if( ! -e $timePointGroupWarp )
        {
        $doTimePointGroupWarp = 1;
        }
      else
        {
        my $modtime = localtime( stat( $timePointGroupWarp )->mtime )->ymd('');
        if( $modtime > 20141218 )
          {
          $doTimePointGroupWarp = 1;
          }
        }

      if( $doTimePointGroupWarp )
        {
        print "1: ${outputPrefix}/../${prefix}_${j}\n";

        if( -e "${outputPrefix}/../${prefix}_${j}/${prefix}TemplateToSubject0Warp.nii.gz" )
          {
          my @args = ( 'antsApplyTransforms',
                         '-d', 3, '--float', 1,
                         '-r', $timePointN4,
                         '-o', "[$timePointGroupWarp,1]",
                         '-t', "${outputPrefix}/../${prefix}_${j}/${prefix}TemplateToSubject1GenericAffine.mat",
                         '-t', "${outputPrefix}/../${prefix}_${j}/${prefix}TemplateToSubject0Warp.nii.gz",
                         '-t', <${singleSubjectTemplateDirectory}/*TemplateToSubject1GenericAffine.mat>,
                         '-t', <${singleSubjectTemplateDirectory}/*TemplateToSubject0Warp.nii.gz>
                     );
  #         system( @args ) == 0 || die "Error: xfrm1\n";
          system( @args );
          }
        }

      my $doTimePointGroupInverseWarp = 0;
      if( ! -e $timePointGroupInverseWarp )
        {
        $doTimePointGroupInverseWarp = 1;
        }
      else
        {
        my $modtime = localtime( stat( $timePointGroupInverseWarp )->mtime )->ymd('');
        if( $modtime > 20141218 )
          {
          $doTimePointGroupInverseWarp = 1;
          }
        }

      if( $doTimePointGroupInverseWarp )
        {
        print "2: ${outputPrefix}/../${prefix}_${j}\n";
        if( -e "${outputPrefix}/../${prefix}_${j}/${prefix}SubjectToTemplate1Warp.nii.gz" )
          {
          my @args = ( 'antsApplyTransforms',
                         '-d', 3, '--float', 1,
                         '-r', $groupTemplate,
                         '-o', "[$timePointGroupInverseWarp,1]",
                         '-t', <${singleSubjectTemplateDirectory}/*SubjectToTemplate1Warp.nii.gz>,
                         '-t', <${singleSubjectTemplateDirectory}/*SubjectToTemplate0GenericAffine.mat>,
                         '-t', "${outputPrefix}/../${prefix}_${j}/${prefix}SubjectToTemplate1Warp.nii.gz",
                         '-t', "${outputPrefix}/../${prefix}_${j}/${prefix}SubjectToTemplate0GenericAffine.mat"
                     );

  #         system( @args ) == 0 || die "Error: xfrm2\n";
          system( @args );
          }
        }
      }
    }
  }
