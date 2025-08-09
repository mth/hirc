use strict;

my ($who, $nick) = @ARGV;
my $DAT_FILE = "seen.dat";

$| = 1;

unless (open SEEN, "<$DAT_FILE") {
	print STDERR "$DAT_FILE: $!\n";
	print "Pole siin kedagi.\n";
	exit 1
}

our %seen;

while (<SEEN>) {
	my @r = split /\t/s;
	unless ($#r) {
		$_->[1] =~ s/^\+//s for values %seen;
		next
	}
	our $k = lc $r[0];
	if (length $r[2] <= 1 and exists $seen{$k}) {
		$_ = $seen{$k}->[2];
		$r[2] = $_ if $_;
	}
	$seen{$k} = \@r;
}

# write compacted seen.dat
# this is because the main bot just appends to the seen.dat
open TMP, ">$DAT_FILE.tmp" or die "Cannot create temp";
print TMP join("\t", @{$_}) for values %seen;
close TMP;

rename "$DAT_FILE.tmp", $DAT_FILE or print STDERR "$DAT_FILE: $!\n";
close SEEN;

if (lc $who eq lc $nick) {
	print "$nick: Paraku oled sa ikka veel siin";
	exit
}

my $r = $seen{lc $nick};
if ($r) {
	$r->[1] =~ /^(\+?)(.*)/s;
	my $alive = $1;
	if ($alive) {
		print "$r->[0] on praegu kanalil"
	} else {
		my $t = time - $2;
		my $s = $t % 60;
		my $t = int($t / 60);
		my $m = $t % 60;
		my $t = int($t / 60);
		my $h = $t % 24;
		my $t = int($t / 24);
		print ("$r->[0] oli siin ${t}d ${h}h ${m}m ${s}s tagasi" .
				($r->[2] =~ /\S/ ? " ja ütles: $r->[2]" : "\n"))
	}
} else {
	print "Ei tea midagi ${nick}'st\n"
}
