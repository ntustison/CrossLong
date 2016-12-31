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
  Usage: createDktMalfLabels.pl <outputDir>
 };

my ( $outputDirectory ) = @ARGV;

my $dktDirectory = '/home/njt4n/share/Data/Public/OASIS-TRT-20/';

#  ======== need to change per cohort =======================

my $thicknessDirectory = '/home/njt4n/share/Data/Public/ADNI/ThicknessAnts/';

#  ==========================================================

my $baseDirectory = "/home/njt4n/share/Data/Public/ADNI/";

my $csvfile =  "${baseDirectory}/nki.csv";
open( FILE, "<${csvfile}" );
my @contents = <FILE>;
close( FILE );

my $ANTSPATH = "/home/njt4n/share/Pkg/ANTs/bin/bin/";
my $UTILSPATH = "/home/njt4n/share/Pkg/Utilities/bin/";

if( ! -d $outputDirectory )
  {
  mkpath( $outputDirectory, {verbose => 0, mode => 0755} ) or
    die "Can't create output directory $outputDirectory\n\t";
  }

my @suffixList = ( ".nii.gz" );

my @t1ImagesAD = <${baseDirectory}/AD*/*/*/*/*//ADNI*.nii.gz>;
my @t1ImagesMCI = <${baseDirectory}/MCI*/*/*/*/*//ADNI*.nii.gz>;
my @t1ImagesNormal = <${baseDirectory}/Normal*/*/*//*/*/ADNI*.nii.gz>;

my @t1Images = ( @t1ImagesAD, @t1ImagesMCI, @t1ImagesNormal );

my $count = 0;
for( my $i = 0; $i < @t1Images; $i++ )
  {
  print "$t1Images[$i]\n";

  my ( $filename, $directories, $suffix ) = fileparse( $t1Images[$i], @suffixList );

  my @comps = split( '/', $directories );

  my $thicknessPath = "${thicknessDirectory}/${comps[-7]}/${comps[-6]}/${comps[-5]}/${comps[-4]}/${comps[-3]}/${comps[-2]}/";
  if( ! -e $thicknessPath )
    {
    next;
    }
  my $thicknessPrefix = "${thicknessPath}/${filename}_";

  my $t1Image = $t1Images[$i];

  my $n4 = "${thicknessPrefix}BrainSegmentation0N4.nii.gz";
  my $mask = "${thicknessPrefix}BrainExtractionMask.nii.gz";
  my $skullStripped = "${thicknessPrefix}SkullStripped.nii.gz";
  my $malfImage = "${thicknessPrefix}DktMalf.nii.gz";

  if( -e $malfImage )
    {
    next;
    }

  my $commandFile = "${outputPrefix}command.sh";

  open( FILE, ">${commandFile}" );
  print FILE "#!/bin/sh\n";
  print FILE "export ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1\n";
  print FILE "\n";

  my $dktXfrmPrefix = "${dktDirectory}/SyNToAdniNormalTemplate/T_template_BrainCerebellumx";
  my $template = "/home/njt4n/share/Data/Public/ADNI/Templates/Normal/T_template0_BrainCerebellum.nii.gz";

  my @dktInverseWarps = <${dktXfrmPrefix}*InverseWarp.nii.gz>;

  print FILE "${UTILSPATH}/BinaryOperateImages 3 $n4 x $mask $skullStripped\n";

  if( ! -e "${thicknessPath}/T_template0xN41Warp.nii.gz" )
    {
    print FILE "${ANTSPATH}/antsRegistrationSyN.sh -d 3 -f $template -m $skullStripped -o ${thicknessPath}/T_template0xN4\n";
    }

  # register the images

  my @labelImages = ();
  my @warpedImages = ();
  for( my $j = 1; $j <= @dktInverseWarps; $j++ )
    {
    my $fileId = "OASIS-TRT-20-${j}";

    my $dktVolume = "${dktDirectory}/Volumes/${fileId}_brain.nii.gz";
    my $dktLabels = "${dktDirectory}/Labels/${fileId}_DKT31_CMA_labels.nii.gz";

    my $outputPrefix = "${thicknessPrefix}x${fileId}";

    my @args = ( "${ANTSPATH}/antsApplyTransforms",
                   '-d', 3,
                   '-i', $dktLabels,
                   '-r', $n4,
                   '-o', "${outputPrefix}LabelsWarped.nii.gz",
                   '-n', 'NearestNeighbor',
                   '-t', "[${thicknessPath}/T_template0xN40GenericAffine.mat,1]",
                   '-t', "${thicknessPath}/T_template0xN41InverseWarp.nii.gz",
                   '-t', "${dktXfrmPrefix}${fileId}1Warp.nii.gz",
                   '-t', "${dktXfrmPrefix}${fileId}0GenericAffine.mat"
                 );
    print FILE "@{args}\n";

    my @args = ( "${ANTSPATH}/antsApplyTransforms",
                   '-d', 3,
                   '-i', $dktVolume,
                   '-r', $n4,
                   '-o', "${outputPrefix}Warped.nii.gz",
                   '-n', 'Linear',
                   '-t', "[${thicknessPath}/T_template0xN40GenericAffine.mat,1]",
                   '-t', "${thicknessPath}/T_template0xN41InverseWarp.nii.gz",
                   '-t', "${dktXfrmPrefix}${fileId}1Warp.nii.gz",
                   '-t', "${dktXfrmPrefix}${fileId}0GenericAffine.mat"
                 );
    print FILE "@{args}\n";

    push( @labelImages, "${outputPrefix}LabelsWarped.nii.gz" );
    push( @warpedImages, "${outputPrefix}Warped.nii.gz" );
    }

  print FILE "${ANTSPATH}/jointfusion 3 1 -g @warpedImages -l @labelImages -m Joint[0.1,2] -tg $n4 $malfImage\n";

  print FILE "rm ${thicknessPath}/T_template0xN41InverseWarp.nii.gz ${thicknessPath}/T_template0xN41Warp.nii.gz ${thicknessPath}/T_template0xN40GenericAffine.mat";
  print FILE "rm @labelImages @warpedImages\n";

  print FILE "\n";
  close( FILE );

  print "** labels ${filename}\n";
  $count++;
  system( "qsub -N ${count}dkt -v ANTSPATH=$ANTSPATH -q standard -l nodes=1:ppn=1 -l mem=10gb -l walltime=50:00:00 $commandFile" );
  }
