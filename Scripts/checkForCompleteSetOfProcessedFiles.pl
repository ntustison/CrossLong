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
#                                        "T_templateExtractedBrain0N4.nii.gz",
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
                           "SubjectToGroupTemplateDktMalfLabels.nii.gz",
                           "SubjectToGroupTemplateLogJacobian.nii.gz",
                           "SubjectToGroupTemplateWarp.nii.gz",
                           "SubjectToTemplate0GenericAffine.mat",
                           "SubjectToTemplate1Warp.nii.gz",
                           "SubjectToTemplateLogJacobian.nii.gz",
                           "TemplateToSubject0Warp.nii.gz",
                           "TemplateToSubject1GenericAffine.mat",
                           "brainvols.csv"
                           );


my @singleSubjectTemplateExtraFileTypes = ();
my @singleSubjectTemplateMissingFileTypes = ();
my @singleSubjectTemplatesWithMissingFiles = ();

my @timePointExtraFileTypes = ();
my @timePointMissingFileTypes = ();
my @timePointsWithMissingFiles = ();

my $numberOfSubjectsWithASingleTimePoint = 0;
my $numberOfSubjectsWithMultipleTimePoints = 0;

my $count = 0;
my $singleCount = 0;
for( my $i = 0; $i < @subjectDirs; $i++ )
  {

  if( ${subjectDirs[$i]} =~ m/133_S_0525/ )
    {
    next;
    }

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

  print "$outputPrefix\n";

  my $commandFile = "${outputPrefix}/antsLongitudinalThicknessCommand.sh";
  my $singleSubjectTemplateDirectory = "${outputPrefix}SingleSubjectTemplate";

  my @singleSubjectTemplateContents = <${singleSubjectTemplateDirectory}/*.*>;

  my @singleSubjectTemplateFiles = ();
  for( my $j = 0; $j < @singleSubjectTemplateContents; $j++ )
    {
    my ( $filename, $directories, $suffix ) = fileparse( $singleSubjectTemplateContents[$j] );
    push( @singleSubjectTemplateFiles, $filename );
    }

  # check if we have any missing files in SST

  my $isFileMissing = 0;
  for( my $j = 0; $j < @singleSubjectTemplateFileTypes; $j++ )
    {
    my ( $filename, $directories, $suffix ) = fileparse( $singleSubjectTemplateFileTypes[$j] );

    if( ! grep {$_ eq $filename} @singleSubjectTemplateFiles )
      {
      $isFileMissing = 1;
      print "   Missing SST file:  $filename\n";
      if( ! grep {$_ eq $filename} @singleSubjectTemplateMissingFileTypes )
        {
        push( @singleSubjectTemplateMissingFileTypes, $filename );
        }
      }
    }
  if( $isFileMissing )
    {
    push( @singleSubjectTemplatesWithMissingFiles, "${outputPrefix}SingleSubjectTemplate" );
    }

  # check if we have any extra files in SST

  for( my $j = 0; $j < @singleSubjectTemplateContents; $j++ )
    {
    my ( $filename, $directories, $suffix ) = fileparse( $singleSubjectTemplateContents[$j] );

    if( ! grep {$_ eq $filename} @singleSubjectTemplateFileTypes )
      {
      print "   Extra SST file:  $filename\n";
#       system( "rm -f $singleSubjectTemplateContents[$j]" );

      if( ! grep {$_ eq $filename} @singleSubjectTemplateExtraFileTypes )
        {
        push( @singleSubjectTemplateExtraFileTypes, $filename );
        }
      }
    }

#################################################################################
#
# Now go through and check the individual time points.
#
#################################################################################

  my @timePointImages = <${whichSubDirectory}/*/*/ADNI*.nii.gz>;

  if( @timePointImages == 1 )
    {
    $numberOfSubjectsWithASingleTimePoint++;
    }
  elsif( @timePointImages > 1 )
    {
    $numberOfSubjectsWithMultipleTimePoints++;
    }

  if( @timePointImages > 0 )
    {
    for( my $j = 0; $j < @timePointImages; $j++ )
      {
      my @comps = split( '/', $timePointImages[$j] );
      ( my $prefix = ${comps[-1]} ) =~ s/\.nii\.gz//;
      my $timePointCorticalThickness = "${outputPrefix}/../${prefix}_${j}/${prefix}CorticalThicknessNormalizedToTemplate.nii.gz";

      my @timePointContents = <${outputPrefix}/../${prefix}_${j}/*.*>;

      my @timePointFiles = ();
      for( my $j = 0; $j < @timePointContents; $j++ )
        {
        my ( $filename, $directories, $suffix ) = fileparse( $timePointContents[$j] );
        push( @timePointFiles, $filename );
        }

      # check if we have any missing files

      my $isFileMissing = 0;
      for( my $k = 0; $k < @timePointFileTypes; $k++ )
        {
        my $filename = "${prefix}${timePointFileTypes[$k]}";

        if( ! grep {$_ eq $filename} @timePointFiles )
          {
          $isFileMissing = 1;
          print "   Missing time point file ($j):  $filename\n";
          if( ! grep {$_ eq ${timePointFileTypes[$k]} } @timePointMissingFileTypes )
            {
            push( @timePointMissingFileTypes, ${timePointFileTypes[$k]} );
            }
          }
        }
      if( $isFileMissing )
        {
        push( @timePointsWithMissingFiles, "${outputPrefix}/../${prefix}_${j}/" );
        }

      # check if we have any extra files

      for( my $k = 0; $k < @timePointContents; $k++ )
        {
        my ( $filename, $directories, $suffix ) = fileparse( $timePointContents[$k] );
        $filename =~ s/${prefix}//;

        if( ! grep {$_ eq $filename} @timePointFileTypes )
          {
          print "   Extra time point file:  $filename\n";
#           print "rm -f $timePointContents[$k]\n";

          if( ! grep {$_ eq $filename} @timePointExtraFileTypes )
            {
            push( @timePointExtraFileTypes, $filename );
            }
          }
        }
      }
    }
  }

print "\n******************************\n\n";

print "Number of subjects with a single time point = $numberOfSubjectsWithASingleTimePoint\n";
print "Number of subjects with multiple time points = $numberOfSubjectsWithMultipleTimePoints\n";
print "\n******************************\n\n";

print "Extra file types:\n";
for( my $i = 0; $i < @timePointExtraFileTypes; $i++ )
  {
  print "  $timePointExtraFileTypes[$i]\n";
  }

print "\n\n";

my $numberOfTimePointsWithMissingFiles = scalar( @timePointsWithMissingFiles );
print "Number of time points with missing files = $numberOfTimePointsWithMissingFiles\n";

print "Missing file types:\n";
for( my $i = 0; $i < @timePointMissingFileTypes; $i++ )
  {
  print "  $timePointMissingFileTypes[$i]\n";
  }











print "\n******************************\n\n";
print "Extra file types:\n";
for( my $i = 0; $i < @singleSubjectTemplateExtraFileTypes; $i++ )
  {
  print "  $singleSubjectTemplateExtraFileTypes[$i]\n";
  }

print "\n\n";

my $numberOfSingleSubjectTemplatesWithMissingFiles = scalar( @singleSubjectTemplatesWithMissingFiles );
print "Number of single subject templates with missing files = $numberOfSingleSubjectTemplatesWithMissingFiles\n";

print "Missing file types:\n";
for( my $i = 0; $i < @singleSubjectTemplateMissingFileTypes; $i++ )
  {
  print "  $singleSubjectTemplateMissingFileTypes[$i]\n";
  }

# ##################################################
# #
# # Current output-- so what we want to do now is
# #   do the registrations locally
# #
# ##################################################
# #
# # Extra file types:
# #
# #
# # Number of single subject templates with missing files = 44
# # Missing file types:
# #   T_templateSubjectToTemplate0GenericAffine.mat
# #   T_templateSubjectToTemplate1Warp.nii.gz
# #   T_templateTemplateToSubject0Warp.nii.gz
# #   T_templateTemplateToSubject1GenericAffine.mat
# #
#
# print "Incomplete single subject template directories:\n";
#
# my $adniTemplate = '/Volumes/BlueBook/ADNI/Templates/Normal/T_template0_BrainCerebellum.nii.gz';
# for( my $i = 0; $i < @singleSubjectTemplatesWithMissingFiles; $i++ )
#   {
#   print "${singleSubjectTemplatesWithMissingFiles[$i]}\n";
#   my $localTemplate = "${singleSubjectTemplatesWithMissingFiles[$i]}/T_templateExtractedBrain0N4.nii.gz";
#   my $localOutputPrefix = "${singleSubjectTemplatesWithMissingFiles[$i]}/T_templateSubjectToTemplate";
#
#   my $localRegistrationCommandFile = "${singleSubjectTemplatesWithMissingFiles[$i]}/T_templateRegistrationToAdniTemplateCommand.sh";
#
#   open( FILE, ">${localRegistrationCommandFile}" );
#
#   print FILE "antsRegistrationSyNQuick.sh -d 3 -f $adniTemplate -m $localTemplate -n 4 -o ${localOutputPrefix}\n\n";
#   print FILE "mv ${localOutputPrefix}1InverseWarp.nii.gz ${singleSubjectTemplatesWithMissingFiles[$i]}/T_templateTemplateToSubject0Warp.nii.gz\n\n";
#   print FILE "antsApplyTransforms -d 3 -o Linear[${singleSubjectTemplatesWithMissingFiles[$i]}/T_templateTemplateToSubject1GenericAffine.mat,1] -t ${localOutputPrefix}0GenericAffine.mat\n";
#
#   print FILE "rm -f ${singleSubjectTemplatesWithMissingFiles[$i]}/T_templateSubjectToTemplateInverseWarped.nii.gz\n";
#   print FILE "rm -f ${singleSubjectTemplatesWithMissingFiles[$i]}/T_templateSubjectToTemplateWarped.nii.gz\n";
#   close( FILE );
#   }











