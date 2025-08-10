#!/usr/bin/perl

while (<STDIN>) {
	last if $_ eq "*******\n"
}
while (<STDIN>) {
	chop;
	next unless $_;
	last if /^1 About/;
	if (/^\s+(\S.*)/) {
		print "$t\t$1\n"
	} elsif (/^(\w.*)/) {
		$t = $1
	}
}
