#!/usr/bin/perl

sub simplify {
	my $x = lc $_[0];
	$x =~ s/\s*(?:\[.*\]|\(.*\))*$//;
	$x =~ s/first/1st/g;
	$x =~ s/second/2nd/g;
	$x =~ s/third/3rd/g;
	$x =~ s/[-\/+,. ]| and | or //g;
	$x
}

foreach (@ARGV) {
	open F, "<$_" or die "$_: $!";
DEF:	while (<F>) {
		/^([^\t]+)\t\s*(.*?\S)\s*$/ or next;
		my $v = $2;
		my $a = $acr{$1};
		$acr{$1} = $a = [] unless defined $a;
		my $x = simplify($v);
		for (@{$a}) {
			my $y = simplify($_);
			if (index($x, $y) >= 0 or index($y, $x) >= 0) {
				next DEF
			}
		}
		push @{$a}, $v;
	}
}

for $k (sort (keys %acr)) {
	print "$k\t$_\n" for sort @{$acr{$k}};
}
