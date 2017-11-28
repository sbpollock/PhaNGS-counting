#! /usr/bin/perl -w
# Olivier Julien 2016
# This script will format indices for NGS analysis using a text file in this format:
# NTKR2.01	TCTTCTTACGCTGCTTTG
# NTKR2.02	CATTCTGGTTGGTGGCCGTACTACTCTGCTGGTTTT
# ...
#
# Insert post H3 seqeunce here:
my $postH3;
$postH3 = 'GACTACTGGGGTCAAGGAACCCTGGTCAAGATCGGAAGAGCACACGTCTG';
my $index_file = 'set7_library.txt';
#
# 
#

open (file1, "$index_file") || die "Could not open input file";
open (out1, ">indices.txt") || die "Could not open input file";

while (<file1>)
        {
	push(@list1, $_);
        }

$i = 0;

while ($list1[$i])
        {
	@line = split(/\t/, $list1[$i]);

	$name = $line[0];
	$H3 = $line[1];

	$H3 =~ s/\r\n//g;

	$read = "$H3$postH3";
	$read = substr($read, 0, 35); # trimming read to 35 bp

	if ($H3 =~ m/[A-Z]/)
		{
		print ">$name\n$read\n";
		print out1 ">$name\n$read\n";
		}
	$i++;
	}

close (file1);
close (out1);
