#! /usr/bin/perl -w
# Olivier Julien 2016
#
use Parallel::ForkManager;
use IO::Zlib;
##############
# USER INPUT #
##############
$indices = "./RTK4_library_reference/indices.txt";
$sequencing_files = "./sequencing_runs";
$process_number_max = "3";
$output_dir = "./counts_set9_RTK4/";
##############

# Create output directory
system("\\rm -r $output_dir");
system("mkdir $output_dir");

# Get all NGS files
@fastq_files = glob ("$sequencing_files/*.fastq.gz");

# Parallel processing (enter the max number of proccess here
my $pm = Parallel::ForkManager->new($process_number_max);
DATA_LOOP:

# Go though each .fastq.gz file
foreach (@fastq_files)
{
print "Processing $_ in progress...\n";

my $pid = $pm->start and next DATA_LOOP;

$filename = $_;
$filename =~ s/$sequencing_files//g;

open (file1, "$indices") || die "Could not open input file";
open (file2, "gunzip -c $_ |") || die "Could not open input file";
open (out1, ">$output_dir/$filename.counts") || die "Could not open input file";

my %id2seq = ();
my $id = '';

while (<file1>)
        {
        chomp;
        if($_ =~ /^>(.+)/)
		{
		$id = $1;
		}
	if ($_ =~ m/^[ATGC]+$/i)
		{
            	$id2seq{$id} .= $_;
		}
        }

# Print hash of indices
#print "$_, $id2seq{$_}\n" for (keys %id2seq);

#################
# Read NGS data #
#################
my %NGS_seq;
while (<file2>)
	{
	if ($_ =~ m/^[NATGC]+$/i)
		{
		$read = $_;
		$read = substr($read, 0, 35);
   		$NGS_seq{$read}++;
		}
	}

######################################
# Count number of matching sequences #
######################################

for my $value (values %id2seq) {
    $NGS_seq{$value}++;
}

keys %id2seq;
for my $gene (sort keys %id2seq)
	{
	my $value = $id2seq{$gene};
	my $count = $NGS_seq{$value}-1;

#	print "$gene\t$count\n";
	print out1 "$gene\t$count\n";
	}

###
print "Processing $filename completed!\n";

close (file1);
close (file2);
close (out1);

$pm->finish; # Terminates the child process
}

$pm->wait_all_children;
print "Done.\n";

