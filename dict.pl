use strict;

my $MAX_NEW = 30;
my $DICT = "base.dict";
my %dict;
my @new_;
my $newPos = 0;

$| = 1;

if (open F, "<$DICT") {
	while (<F>) {
		next unless /^([^\t]+)\t([^\t\n]*)/s;
		my $key = lc $1;
		if ($2) {
			$dict{$key} = [$1, $2];
			if (lc $new_[$newPos - 1] ne $key) {
				$new_[$newPos] = $1;
				$newPos = ($newPos + 1) % $MAX_NEW;
			}
		} else {
			delete $dict{$key}
		}
	}
	close F
} else {
	print STDERR "$DICT: $!"
}

my @new = ();
for (my $i = $newPos + $MAX_NEW; --$i >= $newPos;) {
	my $v = $new_[$i % $MAX_NEW];
	push @new, $v if $v;
}
@new_ = ();

sub def_key {
	my ($msg, $name, $val, $loser) = @_;
	$name =~ tr/\t\n/  /;
	$val =~ tr/\t\n/  /;
	my ($s, $m, $h, $D, $M, $Y) = localtime(time);
	unless (open F, ">>$DICT") {
		print "Baas on kinni: $!\n";
		return
	}
	printf F "%s\t%s\t@ %04d-%02d-%02d %02d:%02d:%02d by %s\n",
		$name, $val, $Y + 1900, $M + 1, $D, $h, $m, $s, $loser;
	close F;
	if ($val) {
		my $key = lc $name;
		$dict{$key} = [$name, $val];
		if (lc $new[0] ne $key) {
			pop @new if @new >= $MAX_NEW;
			unshift @new, $name;
		}
	} else {
		delete $dict{lc $name}
	}
	print $msg, "\n"
}

my %modify = (
	'!learn' => sub {
		if ($_[3]) {
			print "$_[0] on juba tuntud\n"
		} else {
			def_key("Jätsin meelde: $_[0]", @_)
		}
	},
	'!brainwash' => sub {
		if ($_[3]) {
			def_key("Uuendasin $_[0]", @_)
		} else {
			print "Ma ei mäleta enam, mis $_[0] on.\n"
		}
	},
	'!add' => sub {
		my ($key, $val, $loser, $old) = @_;
		if ($old) {
			def_key("Uuendasin $_[0]", $key, "$old | $val", $loser);
		} else {
			print "Ma ei mäleta enam, mis $_[0] on.\n"
		}
	}
);

while (<STDIN>) {
	next unless s/^(\S+)\s(\S+)\s+//;
	my $loser = $1;
	my $cmd = $2;
	s/\s+$//s;
	if ($cmd eq '?' || $cmd eq '??') {
		if (my $def = $dict{lc $_}) {
			my %cycle;
			my ($key, $val) = @{$def};
			while (not defined $cycle{$key} and
			       $val =~ /^\?\??\s+(.*?)\s*$/ and ($def = $dict{lc $1})) {
				($key, $val) = @{$def};
				$cycle{$key} = 1;
			}
			print "$key = $val\n";
		} else {
			print "$_ ei eksisteeri\n"
		}
	} elsif ($cmd eq '!?') {
		my $s = lc $_;
		my @r;
		while ($#r < 30 and (my ($k, $def) = each %dict)) {
			my ($name, $val) = @{$def};
			if (index($k, $s) >= 0 or index(lc $val, $s) >= 0) {
				push @r, $name
			}
		}
		if (@r) {
			print join(", ", @r), "\n"
		} else {
			print "Mis $_?\n"
		}
	} elsif ($modify{$cmd}) {
		if (/^((?:[\w+ -]|[^\x00-\x80])+?)\s*=\s*(\S.*)$/) {
			my $val;
			if (my $def = $dict{lc $1}) {
				(my $name, $val) = @{$def};
			}
			$modify{$cmd}->($1, $2, $loser, $val);
		} else {
			print "Su väljendusoskus jätab soovida.\n";
		}
	} elsif ($cmd eq '!bite') {
		if (my $def = $dict{lc $_}) {
			my ($key, $val) = @{$def};
			if ($val =~ s/\s+\|[^|]*$//s) {
				def_key("$_ hammustatud.", $key, $val, $loser);
			} else {
				print "$_ küljest pole midagi hammustada\n"
			}
		} else {
			print "$_ on misasi?\n"
		}
	} elsif ($cmd eq '!forget') {
		if ($dict{lc $_}) {
			def_key("Unustasin $_ tähenduse", $_, '', $loser)
		} else {
			print "Mida ma pidin unustama?\n"
		}
	} elsif ($cmd eq '!new') {
		print join(', ', @new), "\n" if @new;
	}
}
