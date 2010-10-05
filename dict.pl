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
	close F;
} else {
	print STDERR "$DICT: $!"
}

my @new = ();
for (my $i = $newPos + $MAX_NEW; --$i >= $newPos;) {
	my $v = $new_[$i % $MAX_NEW];
	push @new, $v if $v and $dict{lc $v};
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
		$name = lc $name;
		delete $dict{$name};
		@new = grep {lc $_ eq $name} @new
	}
	print $msg, "\n"
}

sub find {
	my $s = lc $_[0];
	my %rr;
	while ((my ($k, $def) = each %dict)) {
		my ($name, $val) = @{$def};
		if (index($k, $s) >= 0 or index(lc $val, $s) >= 0) {
			next if length($name) <= 1 or index($name, ', ') >= 0;
			my $c = 0;
			if ($k eq $s) {
				$c = 10
			} elsif ($k =~ /(?:^|\W)\Q$s\E(?:\W|$)/) {
				$c = 8
			} elsif ($k =~ /\Q$s\E/sg) {
				$c = 2
			}
			$c += 4 if $val =~ /(?:^|[^\w.?\/&=])\Q$s\E(?:[^\w?\/&=]|$)/is;
			++$c while $val =~ /\Q$s\E/sig;
			$rr{$name} = $c;
		}
	}
	my @r = sort {$rr{$b} <=> $rr{$a}} (keys %rr);
	@r = @r[0..29] if @r > 30;
	print join(", ", @r), "\n" if @r;
	@r
}

my %modify = (
	'!learn' => sub {
		if ($_[3]) {
			print "$_[0] on juba tuntud\n"
		} elsif ($_[0] =~ /, /) {
			print "Koma on paha\n"
		} elsif ($_[0] =~ / \| /) {
			print "Toru on halb\n"
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
		my $at;
		$at = $1 if s/ +\[(\d+)\]//s;
		if (my $def = $dict{lc $_}) {
			my %cycle;
			my ($key, $val) = @{$def};
			while (not defined $cycle{$key} and
			       $val =~ /^\?\??\s+(.*?)\s*$/ and ($def = $dict{lc $1})) {
				($key, $val) = @{$def};
				$cycle{$key} = 1;
			}
			if ($at) {
				my @parts = split(/\s+\|\s+/, $val);
				$val = $parts[$at - 1];
			}
			print "$key - $val\n";
		} elsif (!$at and !find($_)) {
			print "$_ ei eksisteeri\n"
		}
	} elsif ($cmd eq '!?') {
		find($_) or print "Mis $_?\n"
	} elsif ($modify{$cmd}) {
		if (/^((?:[\w+ .-]|[^\x00-\x80])+?)\s*=\s*(\S.*)$/ or
		    /^((?:[\w+ .-]|[^\x00-\x80])+?)\s+(\S.*)$/) {
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
			if ($val =~ s/\s+\|\s+([^|]*)$//s) {
				def_key("$_ küljest hammustatud `$1'.",
					 $key, $val, $loser);
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
