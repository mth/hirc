#my $UA = 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:54.0) Gecko/20100101 Firefox/54.0';
my $UA = 'Mozilla/5.0 (Windows NT 10.0; rv:109.0) Gecko/20100101 Firefox/115.0';

sub http {
	`curl -sf -A '$UA' --connect-timeout '$_[0]' '$_[1]'`
}

$last_update = 0;
#$emhi_url = 'http://213.184.50.180/ilma_andmed/xml/observations.php';
$emhi_url = 'https://www.ilmateenistus.ee/ilma_andmed/xml/observations.php';

# https://ilmateenistus2022.envir.ee/ilma_andmed/xml/observations.php

sub update {
	print STDERR "ilmauuendus $dt\n";
	my @parnu;
	my %temp;
	my $name;
	for (split /\n/s, http(40, $emhi_url)) {
		if (/<name>(.*)<\/name>/) {
			$name = $1;
			$name =~ s/ *(?:\([^)]*\))?//;
		} elsif (/<airtemperature>(.*)<\/airtemperature>/) {
			$temp{$name} = $1;
			$temp{Tartu} = $temp{$name} if $name =~ /^Tartu-Kvissental/;
			push @parnu, $temp{$name}
				 if $name_ =~ /P..?rnu/ && $temp{$name} =~ /\d/;
		}
	}
	my @Tallinn = grep {$_} map {$temp{$_}} qw(Pirita Tallinn-Harku);
        push @Tallinn, $temp{Rohuneeme} unless @Tallinn;
        $temp{Tallinn} += $_ for @Tallinn;
        $temp{Tallinn} /= @Tallinn;
	if (@parnu) {
		my $parnu = 0;
		$parnu += $_ for @parnu;
		$temp{'Pärnu'} = $parnu / @parnu;
	}
	$_ = http(10, 'http://meteo.physic.ut.ee/et/frontmain.php?m=2');
	s/<[^>]*>/ /g;
	if (/ Temperatuur +(-?\d+(\.\d+)?) /) {
		my $tartu = $1;
		my $tt = $temp{Tartu};
		#if (abs($tt - $tartu) < 4 or $tt !~ /\d/) {
		if ($tt !~ /\d/) {
			$temp{Tartu} = $tartu;
#		} elsif (abs($tt - $tartu) < 4) {
		} elsif (abs($tt - $tartu) <= 6) {
			$temp{Tartu} = sprintf("%.1f", ($tt + $tartu) / 2);
#			$temp{Tartu} = sprintf("%.1f", ($tt*2 + $tartu) / 3);
		}
	}
	#uus url: http://tgftp.nws.noaa.gov/data/observations/metar/decoded/EETN.TXT
	#@tallinn = `/usr/bin/lynx -source http://weather.noaa.gov/pub/data/observations/metar/stations/EETN.TXT`;
	#if ($tallinn[1] =~ / (M?)(\d\d)\/M?\d\d /) {
	#	$tallinn = $1 ? -$2 : $2;
	#}
	#@tallinn_ = split(/[ ']/, `/bin/nc 193.40.240.131 6340`);
	#if (@tallinn_ > 2) {
#	my $tallinn = http(10, 'http://www.ilm.ee/~data/tallinn/temp');
#	chomp $tallinn;
#	if ($tallinn =~ /-?[\d.]+/ and
#			($tallinn ne '0.0' or $temp{Tallinn} !~ /\d/)) {
#		$tallinn = $&; # $tallinn_[2];
#		#$tallinn =~ tr/,/./;
#		my $tt = $temp{Tallinn};
#		if ($tt =~ /\d/) {
#			if (abs($tt - $tallinn) <= 6) {
#				$tallinn += $tt * 2;
#				$tallinn = sprintf("%.1f", $tallinn / 3);
#			} else { # valetab, raisk.
#				$tallinn = $tt;
#			}
#		}
#		$temp{Tallinn} = $tallinn;
#		#$tartu = http(10, 'http://www.ilm.ee/~data/tartu/temp');
#		#$temp{Tartu} = $tartu if $tartu =~ /\d/;
#	}
	#($s,$m,$h) = localtime($last_update = time);
	#$m = sprintf "%02d", $m;
	#$ilm = "kell $h:$m";
	my @ilm;
	for (qw(Tallinn Tartu Viljandi Keila Pärnu Võru)) {
		push @ilm, "$_: $temp{$_}°C" if defined $temp{$_}
	}
	$ilm = join '; ', @ilm;
	$ilm =~ s/([0-9])\.([0-9])/\1,\2/sg;
	$ilm =~ s/\n//sg;
	$last_update = time;
}

$| = 1;

while (<STDIN>) {
	$dt = time - $last_update;
	&update if $dt > 60 * 10;
	print "$ilm\n" if $ilm;
}
