## Nathan Ahlgren, Clark University
## Script to parse out COVID-19 case data for cities/towns surrounding Worcester based
## on data published on Worcester city announements: http://www.worcesterma.gov/announcements
## Including towns: Shrewsbury, Holden, Grafton, Leceister, Millbury
## Uses regex to pull case data based on the text format used in the announcements
## To use announcement text are copies and concatenated into the file all_Worcester_COVID_announcements.txt
## Usage: perl parse_city_data.pl  all_Worcester_COVID_announcements.txt

use strict;

my $in = $ARGV[0];
my $l;
my $d;
my %grafton;
my %shrew;
my %leic;
my %holden;
my %millbury;
my $out;
my %deaths;
my $death;

open (IN,$in) or die "Couldn't open infile $in:$!\n";

while ($l = <IN>) {
	chomp $l;
	## if line has date, record that
	if ($l =~ m/The City of Worcester has released the following COVID-19 update for (\w+) (\d+):/) {
		$d = "2020-" . $1 . "-" . sprintf("%02d", $2);
		$d=~s/June/06/;
		$d=~s/May/05/;
		$d=~s/April/04/;
		$d=~s/March/03/;
#		print $l . "\n\t$d\n";
		}
	else {
		if ($l =~m/Shrewsbury:/) {
		
			$l=~m/Shrewsbury: (\d+)/;
			$shrew{$d} = $1;
			$l=~m/Grafton: (\d+)/;
			$grafton{$d} = $1;
			$l=~m/Holden: (\d+)/;
			$holden{$d} = $1;
			$l=~m/Shrewsbury: (\d+)/;
			$shrew{$d} = $1;
			$l=~m/Leicester: (\d+)/;
			$leic{$d} = $1;
			if ($l=~m/Millbury/) {
				if ($l=~m/Millbury: (\d+) as of /) {
					}
				else {
					$l=~m/Millbury: (\d+)/;
					$millbury{$d} = $1;
					}
				} # if Millbury		
			} # end if Shew .*
		} # end else

	if ($l =~m/ospital.* (\d+) total COVID-related deaths/) {
		$death = $1;
#		print $d . "\t" . $death . "\n";
#		print $l . "\n";
		$deaths{$d} = $death;
		}
	
	if ($l =~m/(\d+) \(increase of \d+ from \w+\) total COVID-related deaths/) {
		$death = $1;
#		print $d . "\t" . $death . "\n";
#		print $l . "\n";
		$deaths{$d} = $death;
		}

	
	

	
	} # end while

$out = "Shrewsbury_city.tsv";
open(OUT,">$out");
print OUT "Date\tcases\n";
foreach $d (sort keys %shrew) {
#	print  "$d\t$shrew{$d}\n";
	print OUT "$d\t$shrew{$d}\n";
	}
close OUT;

$out = "Grafton_city.tsv";
open(OUT,">$out");
print OUT "Date\tcases\n";

foreach $d (sort keys %shrew) {
	print OUT "$d\t$grafton{$d}\n";
	}
close OUT;

$out = "Holden_city.tsv";
open(OUT,">$out");
print OUT "Date\tcases\n";
foreach $d (sort keys %shrew) {
	print OUT "$d\t$holden{$d}\n";
	}
close OUT;

$out = "Leceister_city.tsv";
open(OUT,">$out");
print OUT "Date\tcases\n";
foreach $d (sort keys %shrew) {
	print OUT "$d\t$leic{$d}\n";
	}
close OUT;

$out = "Millbury_city.tsv";
open(OUT,">$out");
print OUT "Date\tcases\n";
foreach $d (sort keys %millbury) {
	print OUT "$d\t$millbury{$d}\n";
	}
close OUT;

$out = "Worcester_deaths.tsv";
open(OUT,">$out");
print OUT "Date\tDeaths\n";
foreach $d (sort keys %deaths) {
	print OUT "$d\t$deaths{$d}\n";
	}
close OUT;