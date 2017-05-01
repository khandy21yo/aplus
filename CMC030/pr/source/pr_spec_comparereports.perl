#!/usr/bin/perl

# pr_spec_comparereports.perl
#
#	Compare two reports for differences in state taxes
#
# Description
#
#	This is a special that is used to compare the state tax
#	columns in two different reports to find out why they
#	have a different grand total
#
# Bugs
#
#	File names are hard coded into the program
#
# History
#
#	04/13/2001 - Kevin Handy
#		Original version
#

#
# Open up and read in the payroll state tax audit report
#
open(ONE, "temp1.tmp") || die "temp1.tmp";
while(<ONE>)
{
	if (/^[0-9]/)
	{
#		chomp;
		$empid = substr($_, 0, 9);
		$stax = substr($_, 61, 9);
#		print STDERR $empid, "'", $stax, "'\n";
		$one{$empid} += $stax;
	}
}
close ONE;

open(TWO, "temp2.tmp") || die "temp2.tmp";
while(<TWO>)
{
	if (/^[0-9]/)
	{
		$empid = substr($_, 7, 9);
		$stax = substr($_, 104, 9);
#		print STDERR $empid, "'", $stax, "'\n";
		$two{$empid} += $stax;
		$one{$empid} += 0;
	}
}
close TWO;

open(TWO, "temp3.tmp") || die "temp3.tmp";
while(<TWO>)
{
	if (/^[0-9]/)
	{
		$empid = substr($_, 7, 9);
		$stax = substr($_, 104, 9);
#		print STDERR $empid, "'", $stax, "'\n";
		$two{$empid} += $stax;
		$one{$empid} += 0;
	}
}
close TWO;

#
# Now compare the two sets of numbers
#

for $key (sort keys %one)
{
#	if ($one{$key} != $two{$key})
	if (abs($one{$key} - $two{$key}) > 0.001)
	{
		printf "%-10.10s %12.2f %12.2f\n", $key, $one{$key}, $two{$key};
	}
}
