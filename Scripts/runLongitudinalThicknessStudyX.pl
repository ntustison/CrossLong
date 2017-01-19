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

my $ANTSPATH = '/data/users/ntustiso/Pkg/ANTs/bin/bin/';

my $baseDirectory = '/fast-scratch/ntustiso/Data/Public/ADNI/';
my $templateDirectory = '/bio/ntustiso/Data/Public/ADNI/Template/';
my $outputDirectory = "${baseDirectory}LongitudinalThicknessANTs/";




my $brainExtractionTemplate = "${templateDirectory}/T_template0.nii.gz";
my $brainTemplateSkullStripped = "${templateDirectory}/T_template0_BrainCerebellum.nii.gz";
my $brainExtractionProbabilityMask = "${templateDirectory}/T_template0_BrainCerebellumProbabilityMask.nii.gz";
my $brainExtractionMask = "${templateDirectory}/T_template0_BrainCerebellumExtractionMask.nii.gz";
my $brainParcellationProbabilityMask = "${templateDirectory}/Priors/priors%d.nii.gz";

my $malfDataDir = "/bio/ntustiso/Data/Public/MICCAI-2012-Multi-Atlas-Challenge-Data/";
my $cookAtlasPriorsString = '';
my @malfBrainImages = <${malfDataDir}/training-images/????_3_BrainCerebellum.nii.gz>;
for( my $i = 0; $i < @malfBrainImages; $i++ )
  {
  ( my $labels = $malfBrainImages[$i] ) =~ s/training-images/training-labels/;
  $labels =~ s/BrainCerebellum/glm_6labels/;
  $cookAtlasPriorsString .= " -a $malfBrainImages[$i] -l $labels";
  }

my @suffixList = ( ".nii.gz" );

my $count = 0;

my @subjectDirs = <${outputDirectory}/???_S_????/*/>;

for( my $i = 0; $i < @subjectDirs; $i++ )
  {
  my $outputPrefix = ${subjectDirs[$i]};

  my $commandFile = "${outputPrefix}/antsLongitudinalThicknessCommand.sh";

  if( ! -e $commandFile )
    {
    next;
    }

  open( FILE, "<${commandFile}" );
  my @contents = <FILE>;
  close( FILE );

  my $commandFileX = "${outputPrefix}/antsLongitudinalThicknessCommandX.sh";
  open( FILEX, ">${commandFileX}" );

  for( my $c = 0; $c < @contents; $c++ )
    {
    $contents[$c] =~ s/LongitudinalThicknessANTs2/LongitudinalThicknessANTs/g;


    if( $c == 2 )
      {
      print FILEX "export ANTSPATH=${ANTSPATH}\n";
      }
    elsif( $c < 4 )
      {
      ( my $newPath = $contents[$c] ) =~ s/\/home\/njt4n\/share2/\/data\/users\/ntustiso/g;

      print FILEX $newPath;
      }
    elsif( $contents[$c] =~ m/-r/ )
      {
      print FILEX "   -r 0 -q 3 \\\n";
      }
    else
      {
      ( my $newPath = $contents[$c] ) =~ s/\/home\/njt4n\/share2/\/bio\/ntustiso/g;

      print FILEX $newPath;
      }
    }
  close( FILEX );

  print "** adni thickness ${outputPrefix}\n";
  system( "qsub -N adniX_${i} -q yassalab,abio,asom,bio,som,pub64,pub8i,free64 -ckpt restart $commandFileX" );
 
  $count++;
  }
print "$count files.\n";
