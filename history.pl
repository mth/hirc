#!/usr/bin/perl

die "illegal arguments\n" unless $#ARGV == 1;
($fn, $key) = @ARGV;
die "$fn: $!\n" unless open F, '<', $fn;
die "illegal definition key\n" if $key =~ /[\x00-\x1f]/;
$key = lc $key . "\t";
$len = length $key;
my $exists;
while (<F>) {
	if ($key eq lc (substr $_, 0, $len)) {
		split /\t/;
		print $_[2] if $_[2] =~ /\S/;
		$exists = 1
	}
}
print "Mis $ARGV[1]?\n" unless $exists
