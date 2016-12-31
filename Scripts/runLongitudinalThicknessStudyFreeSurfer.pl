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

my @freeSurferTrashSubdirectoryNames = ( 'bem', 'label', 'mri', 'scripts', 'src', 'surf', 'tmp', 'touch', 'trash' );

my $csvFile = "${baseDirectory}/Scripts/adniCrossSectionalAntsMergeSubset.csv";
open( FILE, "<${csvFile}" );
my @contents = <FILE>;
close( FILE );

my @studyIDs = ();
my @studyImageIDs = ();
for( my $i = 1; $i < @contents; $i++ )
  {
  my @elements = split( ',', $contents[$i] );
  push( @studyIDs, $elements[0] );
  push( @studyImageIDs, $elements[9] );
  }
my @uniqueStudyIDs = do { my %seen; grep { !$seen{$_}++ } @studyIDs };

my @subjectDirs = <${baseDirectory}/Nifti/???_S_????//*>;

my $count = 0;

for( my $i = 0; $i < @uniqueStudyIDs; $i++ )
  {
  my $subjectDir = "${baseDirectory}/Nifti/${uniqueStudyIDs[$i]}/";
  ( my $outputPrefix = $subjectDir ) =~ s/Nifti/LongitudinalThicknessFreeSurfer/;
  ( my $outputPrefixScratch = $outputPrefix ) =~ s/bio/scratch/;

  my @comps = split( '/', $outputPrefixScratch );
  my $archiveName = "${comps[-1]}.tar.gz";

  if( -e "${outputPrefix}/${archiveName}" )
    {
    next;
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
    my @tmp = <${subjectDir}/*/*/ADNI*I${subjectImageIDs}.nii.gz>;
    if( scalar( @tmp ) != 1 )
      {
      die "No match:  ${subjectDir}/*/*/ADNI*I${subjectImageIDs}.nii.gz\n";
      }
    push( @timePointImages, $tmp[0] );

    my ( $filename, $path, $suffix ) = fileparse( $timePointImages[$j], '.nii.gz' );
    push( @timePointBaseNames, $filename );
    }

  # Call longitudinal FreeSurfer stream

  my $commandFile = "${outputPrefix}/freesurferLongitudinalThicknessCommand.sh";

  $count++;

  if( ! -e $outputPrefix )
    {
    mkpath( $outputPrefix );
    }

  print "$commandFile\n";

  open( FILE, ">${commandFile}" );
  print FILE "#!/bin/bash\n\n";
  print FILE "\n";

  print FILE "\n";
  print FILE "# Step 1 --- do cross-sectional first\n";
  print FILE "\n";
  for( my $j = 0; $j < @timePointImages; $j++ )
    {
    print FILE "recon-all -i $timePointImages[$j] -s ${timePointBaseNames[$j]} -sd ${outputPrefixScratch} -all\n";
    }
  print FILE "\n";
  print FILE "# Step 2 --- build template\n";
  print FILE "\n";
  my $commandString = "recon-all -sd ${outputPrefixScratch} -base Template";
  for( my $j = 0; $j < @timePointImages; $j++ )
    {
    $commandString .= " -tp ${timePointBaseNames[$j]}";
    }
  $commandString .= " -all";
  print FILE "${commandString}\n";
  print FILE "\n";
  print FILE "# Step 3 --- do longitudinal refinement\n";
  print FILE "\n";
  for( my $j = 0; $j < @timePointImages; $j++ )
    {
    print FILE "recon-all -i $timePointImages[$j] -sd ${outputPrefixScratch} -long ${timePointBaseNames[$j]} Template -all\n";
    }
  print FILE "\n";
  print FILE "# Step 4 --- clean-up:  remove everything except the tp stats dirs\n";
  print FILE "\n";
  print FILE "rm -rf ${outputPrefixScratch}/Template/";
  print FILE "rm -rf ${outputPrefixScratch}/fsaverage";
  print FILE "rm -rf ${outputPrefixScratch}/lh.EC_average/";
  print FILE "rm -rf ${outputPrefixScratch}/rh.EC_average/";
  for( my $j = 0; $j < @timePointImages; $j++ )
    {
    for( my $k = 0; $k < @freeSurferTrashSubdirectoryNames; $k++ )
      {
      print FILE "rm -rf ${outputPrefixScratch}/${timePointBaseNames[$j]}/${freeSurferTrashSubdirectoryNames[$k]}/\n";
      print FILE "rm -rf ${outputPrefixScratch}/${timePointBaseNames[$j]}.long.Template/${freeSurferTrashSubdirectoryNames[$k]}/\n";
      }
    }
  print FILE "\n";


  print FILE "tar -cvzf $archiveName -C ${outputPrefixScratch}/../ ${comps[-1]}\n";
  print FILE "mv $archiveName ${outputPrefix}\n";
  print FILE "rm -rf ${outputPrefixScratch}\n";
  close( FILE );

  print "** fs thickness ${outputPrefixScratch}\n";
  system( "qsub -N adni2_${i} -q yassalab,abio,asom,bio,som,pub64,pub8i,free64 -ckpt restart $commandFile" );

  sleep 1;
  }

print "$count files.\n";
