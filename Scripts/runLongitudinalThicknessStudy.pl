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
  Usage: runLongitudinalThicknessStudy.pl
 };

my $baseDirectory = "/home/njt4n/share2/Data/Public/ADNI/Nifti/";
# my $baseDirectory = "//Volumes/G-RAID/Data/ADNI/Nifti/";

my $templateDirectory = "${baseDirectory}/";
my $ANTSPATH = "/home/njt4n/share2/Pkg/ANTs/bin/bin/";
my $UTILSPATH = "/home/njt4n/share2/Pkg/Utilities/bin/";

my $brainExtractionTemplate = '/home/njt4n/share2/Data/Public/ADNI/Templates/Normal/T_template0.nii.gz';
my $brainTemplateSkullStripped = '/home/njt4n/share2/Data/Public/ADNI/Templates/Normal/T_template0_BrainCerebellum.nii.gz';
my $brainExtractionProbabilityMask = '/home/njt4n/share2/Data/Public/ADNI/Templates/Normal/T_template0ProbabilityMask.nii.gz';
my $brainExtractionMask = '/home/njt4n/share2/Data/Public/ADNI/Templates/Normal/T_template0ExtractionMask.nii.gz';
my $brainParcellationProbabilityMask = '/home/njt4n/share2/Data/Public/ADNI/Templates/Normal/Priors/priors%d.nii.gz';

my $malfDataDir = '/home/njt4n/share2/Data/Public/MICCAI-2012-Multi-Atlas-Challenge-Data/';
my $cookAtlasPriorsString = '';
my @malfBrainImages = <${malfDataDir}/training-images/????_3_BrainCerebellum.nii.gz>;
for( my $i = 0; $i < @malfBrainImages; $i++ )
  {
  ( my $labels = $malfBrainImages[$i] ) =~ s/training-images/training-labels/;
  $labels =~ s/BrainCerebellum/glm_6labels/;
  $cookAtlasPriorsString .= " -a $malfBrainImages[$i] -l $labels";
  }

my @suffixList = ( ".nii.gz" );

my @subjectDirs = <${baseDirectory}/???_S_????>;

my $count = 0;
# for( my $i = 0; $i < @subjectDirs; $i++ )
# for( my $i = 0; $i < 400; $i++ )
# for( my $i = 400; $i < 700; $i++ )
for( my $i = 700; $i < @subjectDirs; $i++ )
  {
  print "${subjectDirs[$i]}\n";

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
    next;
    }

  $count++;

  ( my $outputPrefix = $whichSubDirectory ) =~ s/Nifti/LongitudinalThicknessANTs2/;

  if( ! -e $outputPrefix )
    {
    mkpath( $outputPrefix );
    }

  my @timePointImages = <${whichSubDirectory}/*/*/ADNI*.nii.gz>;

  my $commandFile = "${outputPrefix}/antsLongitudinalThicknessCommand.sh";
  print "$commandFile\n";

  open( FILE, ">${commandFile}" );
  print FILE "#!/bin/sh\n";
  print FILE "export ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1\n";
  print FILE "\n";

  my @antsCommands = ();
  $antsCommands[0] = "${ANTSPATH}/antsLongitudinalCorticalThickness.sh \\";
  $antsCommands[1] = "   -d 3 \\";
  $antsCommands[2] = "   -o ${outputPrefix} \\";
  $antsCommands[3] = "   -c 0 \\";
  $antsCommands[4] = "   -t $brainTemplateSkullStripped \\";
  $antsCommands[5] = "   -e $brainExtractionTemplate \\";
  $antsCommands[6] = "   -m $brainExtractionProbabilityMask \\";
  $antsCommands[7] = "   -f $brainExtractionMask \\";
  $antsCommands[8] = "   -p $brainParcellationProbabilityMask \\";
  $antsCommands[9] = "   -r 1 \\";
  $antsCommands[10]= "   $cookAtlasPriorsString \\";
  $antsCommands[11]= "   @timePointImages";

  for( my $k = 0; $k < @antsCommands; $k++ )
    {
    if( $k < @antsCommands )
      {
      print FILE "$antsCommands[$k]\n";
      }
    }
  print FILE "\n";
  close( FILE );


#   if( ! -e "${outputPrefix}CorticalThickness.nii.gz" )
#     {
     print "** adni thickness ${outputPrefix}\n";
    if( $count % 3 == 0 )
      {
      system( "qsub -N ${i}_adni -v ANTSPATH=$ANTSPATH -q nopreempt -l nodes=1:ppn=1 -l mem=12gb -l walltime=100:00:00 $commandFile" );
      }
    else
      {
      system( "qsub -N ${i}_adni -v ANTSPATH=$ANTSPATH -q standard -l nodes=1:ppn=1 -l mem=12gb -l walltime=100:00:00 $commandFile" );
      }
     $count++;
#     }
#   else
#     {
#     print " adni thickness ${filename}\n";
#     }
#    sleep 1;
  }

print "$count files.\n";
