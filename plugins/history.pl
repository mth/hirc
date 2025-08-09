#!/usr/bin/perl

die "illegal arguments\n" unless $#ARGV == 1;
($fn, $key) = @ARGV;
die "$fn: $!\n" unless open F, '<', $fn;
die "illegal definition key\n" if $key =~ /[\x00-\x1f]/;
$key = lc $key . "\t";
$len = length $key;
while (<F>) {
	if ($key eq lc (substr $_, 0, $len)) {
		my @x = split /\t/;
		next unless $x[2] =~ /\S++$/;
		print $prev if $& ne $who;
		$who = $&;
		$prev = $x[2];
	}
}
print $prev // "Mis $ARGV[1]?\n";
