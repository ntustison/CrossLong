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


my @tmpQstat = `qstat -u ntustiso | grep adniTem`;
my @jobs = ();
my @job_names = ();
for( my $i = 0; $i < @tmpQstat; $i++ )
  {
  chomp( $tmpQstat[$i] );
  my @jobStats = split( ' ', $tmpQstat[$i] );
  push( @jobs, $jobStats[0] );

  my @tmpQstatJob = `qstat -j $jobStats[0] | grep job_name`;
  chomp( $tmpQstatJob[0] );
  my @jobNameStats = split( ' ', $tmpQstatJob[0] );
  push( @job_names, $jobNameStats[1] );
  }

for( my $i = 0; $i < @job_names; $i++ )
  {
  print "$jobs[$i] ---> $job_names[$i]\n";
  }


my $count = 0;

for( my $i = 0; $i < @uniqueStudyIDs; $i++ )
  {
  my $subjectDir = "${baseDirectory}/Nifti/${uniqueStudyIDs[$i]}/";
  ( my $outputPrefix = $subjectDir ) =~ s/Nifti/LongitudinalThicknessFreeSurfer/;
  ( my $outputPrefixScratch = $outputPrefix ) =~ s/bio/scratch/;

  my @comps = split( '/', $outputPrefixScratch );

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
    my @tmp = <${subjectDir}/*/*/*//ADNI*I${subjectImageIDs[$j]}.nii.gz>;
    if( scalar( @tmp ) != 1 )
      {
      die "No match:  ${subjectDir}/*/*/*/ADNI*I${subjectImageIDs[$j]}.nii.gz\n";
      }
    push( @timePointImages, $tmp[0] );

    my ( $filename, $path, $suffix ) = fileparse( $timePointImages[$j], '.nii.gz' );
    push( @timePointBaseNames, $filename );
    }


  if( ! -e $outputPrefix )
    {
    mkpath( $outputPrefix );
    }

  my @allSubjectImages = ( @timePointImages );
  my @allSubjectBaseNames = ( @timePointBaseNames );

  # Run non-included images
  my ( $filenameX, $pathX, $suffixX ) = fileparse( $timePointImages[0] );
  my @allImages = <${pathX}/../../*/*/*.nii.gz>;
  for( my $j = 0; $j < @allImages; $j++ )
    {
    my ( $filenameY, $pathY, $suffixY ) = fileparse( $allImages[$j], ".nii.gz" );
    my $isOther = 1;
    for( my $k = 0; $k < @timePointImages; $k++ )
      {
      my ( $filenameZ, $pathZ, $suffixZ ) = fileparse( $timePointImages[$k], ".nii.gz" );
      if( $filenameZ =~ m/$filenameY/ )
        {
        $isOther = 0;
        last;
        }
      }
    if( $isOther == 1 )
      {
      push( @allSubjectImages, $allImages[$j] );
      push( @allSubjectBaseNames, $filenameY );

#       if( ! -e "${outputPrefix}/${filenameY}/stats/wmparc.stats" )
#         {
#         $count++;
#         my $commandFile = "${outputPrefix}/${filenameY}Command.sh";
#         print "$commandFile\n";
#         open( FILE, ">${commandFile}" );
#         print FILE "#!/bin/bash\n\n";
#
#         print FILE "export FREESURFER_HOME=/data/users/ntustiso/Pkg/freesurfer\n";
#         print FILE "source \$FREESURFER_HOME/SetUpFreeSurfer.sh\n";
#
#         print FILE "rm -rf ${outputPrefixScratch}/${filenameY}/\n";
#         print FILE "mkdir -p ${outputPrefixScratch}/${filenameY}/\n\n";
#
#         print FILE "\n";
#         print FILE "# Step 1 --- do cross-sectional first\n";
#         print FILE "\n";
#
#         print FILE "echo \"Nick Tustison comment:  Step 1\"\n";
#         print FILE "recon-all -i $allImages[$j] -s ${filenameY} -sd ${outputPrefixScratch}/${filenameY} -cw256 -all\n\n";
#
#         print FILE "tar -cvzf ${filenameY}.tar.gz -C ${outputPrefixScratch}/${filenameY}/../ ${filenameY}\n";
#         print FILE "mv ${filenameY}.tar.gz ${outputPrefix}\n";
#         print FILE "rm -rf ${outputPrefixScratch}/${filenameY}/\n";
#         close( FILE );
#         system( "qsub -N adniXfs_${uniqueStudyIDs[$i]}_${j} -q yassalab,abio,asom,bio,som,pub64,pub8i,free64 -ckpt restart $commandFile" );
#         sleep 2;
#         }
      }
    }


  # Call longitudinal FreeSurfer stream


#   for( my $j = 0; $j < @timePointImages; $j++ )
#     {
# #    print FILE "\n\necho \"${outputPrefix}/${timePointBaseNames[$j]}\" | mail -s \"ADNI FreeSurfer\" \"ntustiso\@uci.edu\"\n";
#     if( -e "${outputPrefix}/${timePointBaseNames[$j]}.tar.gz" )
#       {
#       $count++;
# #      print "${outputPrefix}/${timePointBaseNames[$j]}.tar.gz\n";
#       `rm -f ${outputPrefix}/${timePointBaseNames[$j]}.tar.gz`;
#
#       my $commandFile = "${outputPrefix}/${timePointBaseNames[$j]}Command.sh";
#       print "$commandFile\n";
#       open( FILE, ">${commandFile}" );
#       print FILE "#!/bin/bash\n\n";
#
#       print FILE "export FREESURFER_HOME=/data/users/ntustiso/Pkg/freesurfer\n";
#       print FILE "source \$FREESURFER_HOME/SetUpFreeSurfer.sh\n";
#
#       print FILE "rm -rf ${outputPrefixScratch}/${timePointBaseNames[$j]}/\n";
#       print FILE "mkdir -p ${outputPrefixScratch}/${timePointBaseNames[$j]}/\n\n";
#
#       print FILE "\n";
#       print FILE "# Step 1 --- do cross-sectional first\n";
#       print FILE "\n";
#
#       print FILE "echo \"Nick Tustison comment:  Step 1\"\n";
#       print FILE "recon-all -i $timePointImages[$j] -s ${timePointBaseNames[$j]} -sd ${outputPrefixScratch}/${timePointBaseNames[$j]} -all\n\n";
#
#       print FILE "tar -cvzf ${timePointBaseNames[$j]}.tar.gz -C ${outputPrefixScratch}/${timePointBaseNames[$j]}/../ ${timePointBaseNames[$j]}\n";
#       print FILE "mv ${timePointBaseNames[$j]}.tar.gz ${outputPrefix}\n";
#       print FILE "rm -rf ${outputPrefixScratch}/${timePointBaseNames[$j]}/\n";
#       close( FILE );
#
#       system( "qsub -N adnifs_${uniqueStudyIDs[$i]}_${j} -q yassalab,abio,asom,bio,som,pub64,pub8i,free64 -ckpt restart $commandFile" );
#       sleep 1;
#       }
#     else
#       {
#       next;
#       }
#     }


  # Determine if the template file exist
  my $templateStatsFile = "${outputPrefix}/Template/stats/wmparc.stats";
  if( ! -e $templateStatsFile )
    {
    next;
    }

 # Check to see that all the cross-sectional results are complete

  my $isComplete = 1;
  for( my $j = 0; $j < @allSbujectBaseNames; $j++ )
    {
    my $statsFile = "${outputPrefix}/${allSubjectBaseNames[$j]}/stats/wmparc.stats";
    if( ! -e $statsFile )
      {
      $isComplete = 0;
      last;
      }
    }

  if( $isComplete == 1 )
    {
    next;
    }

 # Check to see if the job is still running

  my $isRunning = 0;
  for( my $k = 0; $k < @job_names; $k++ )
    {
    if( $job_names[$k] =~ m/adniTem_${uniqueStudyIDs[$i]}/ )
      {
      $isRunning = 1;
      last;
      }
    }
  if( $isRunning == 1 )
    {
    next;
    }

  $count++;

  my $commandFile = "${outputPrefix}/makeTemplateStep2Command.sh";
  print "$commandFile\n";
  open( FILE, ">${commandFile}" );
  print FILE "#!/bin/bash\n\n";

  print FILE "export FREESURFER_HOME=/data/users/ntustiso/Pkg/freesurfer\n";
  print FILE "source \$FREESURFER_HOME/SetUpFreeSurfer.sh\n";

  print FILE "\n";
  print FILE "# Step 2 --- build template\n";
  print FILE "\n";

  print FILE "echo \"Nick Tustison comment:  Step 2\"\n";

  my $commandString = "recon-all -sd ${outputPrefix} -base Template";
  for( my $j = 0; $j < @allSubjectImages; $j++ )
    {
    $commandString .= " -tp ${allSubjectBaseNames[$j]}";
    }
  $commandString .= " -all";
  print FILE "${commandString}\n";
  print FILE "\n";
  close( FILE );

  system( "qsub -N adniTem_${uniqueStudyIDs[$i]} -q yassalab,abio,asom,bio,som,pub64,pub8i,free64 -ckpt restart $commandFile" );
  sleep 5;

#   my $commandFile = "${outputPrefix}/refineStep3Command.sh";
#   print "$commandFile\n";
#   open( FILE, ">${commandFile}" );
#   print FILE "# Step 3 --- do longitudinal refinement\n";
#   print FILE "#!/bin/bash\n\n";
#
#   print FILE "export FREESURFER_HOME=/data/users/ntustiso/Pkg/freesurfer\n";
#   print FILE "source \$FREESURFER_HOME/SetUpFreeSurfer.sh\n";
#   print FILE "\n";
#
#   print FILE "echo \"Nick Tustison comment:  Step 3\"\n";
#
#   for( my $j = 0; $j < @timePointImages; $j++ )
#     {
#     print FILE "recon-all -sd ${outputPrefix} -long ${timePointBaseNames[$j]} Template -all\n";
#     }
#   print FILE "\n";
#   print FILE "# Step 4 --- clean-up:  remove everything except the tp stats dirs\n";
#   print FILE "\n";
#   print FILE "rm -rf ${outputPrefixScratch}/Template/\n";
#   print FILE "rm -rf ${outputPrefixScratch}/fsaverage/\n";
#   print FILE "rm -rf ${outputPrefixScratch}/lh.EC_average/\n";
#   print FILE "rm -rf ${outputPrefixScratch}/rh.EC_average/\n";
#   for( my $j = 0; $j < @timePointImages; $j++ )
#     {
#     for( my $k = 0; $k < @freeSurferTrashSubdirectoryNames; $k++ )
#       {
#       print FILE "rm -rf ${outputPrefix}/${timePointBaseNames[$j]}/${freeSurferTrashSubdirectoryNames[$k]}/\n";
#       print FILE "rm -rf ${outputPrefix}/${timePointBaseNames[$j]}.long.Template/${freeSurferTrashSubdirectoryNames[$k]}/\n";
#       }
#     }
#   print FILE "\n";
#   close( FILE );
#
#   print "** fs thickness ${outputPrefix}\n";
#   system( "qsub -N adniRef_${i} -q yassalab,abio,asom,bio,som,pub64,pub8i,free64 -ckpt restart $commandFile" );
#
#   sleep 10;
  }

print "$count files.\n";
