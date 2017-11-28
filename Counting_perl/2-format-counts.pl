#! /usr/bin/perl -w
# Olivier Julien 2016
#

# open new counts_table.txt file
system("rm -f counts_table.txt");
open (out1, ">counts_table.txt") || die "Could not open input file";

# Grab all of the .counts files
@counts_files = glob ("*.counts");
# Create a genes.txt file containing the gene names
system("awk '{print \$1}' $counts_files[0] > genes.txt");

# For each .counts file, create a .counts.1 file that contains only the counts
foreach (@counts_files)
	{
	system("awk '{print \$2}' $_ > $_.1");
	}

# Create a list of all .counts.1 files
@counts_1_files = glob ("*.counts.1");
$scal_files = join(" ", @counts_1_files);
print "$scal_files\n";

# Format and print header to match wells
print "Gene\t";
print out1 "Gene\t";
foreach (@counts_files)
        {
	chomp;
	$filename = $_;
	$well = $filename;
	$well =~ s/SP05-//g;
	$well =~ s/_S[\d]+_L006_R1_001_indices.txt.counts//g;
	$well =~ s/501/A/g;
	$well =~ s/502/B/g;
	$well =~ s/503/C/g;
	$well =~ s/504/D/g;
	$well =~ s/505/E/g;
	$well =~ s/506/F/g;
	$well =~ s/507/G/g;
	$well =~ s/508/H/g;
	$well =~ s/_7//g;
	
	print "$well\t";
	print out1 "$well\t";
        }
print "\n";
print out1 "\n";

# Paste the gene list and the data
system("paste genes.txt $scal_files");
system("paste genes.txt $scal_files >> counts_table.txt");

close (out1);


