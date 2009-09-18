$last_update = 0;

sub update {
	print STDERR "ilmauuendus $dt\n";
	$lynx = '/usr/bin/lynx';
	open F, '/usr/bin/wget -q -O - http://www.emhi.ee/ilma_andmed/xml/observations.php|';
	while (<F>) {
		if (/<name>(.*)<\/name>/) {
			$name = $1;
			$name =~ s/ *\([^)]*\)//;
		} elsif (/<airtemperature>(.*)<\/airtemperature>/) {
			$temp{$name} = $1
		}
	}
	close F;
	$temp{Tallinn}=`$lynx -source http://www.ilm.ee/~data/tallinn/temp`;
	$temp{Tartu}=`$lynx -dump http://meteo.physic.ut.ee/et/frontmain.php?m=2 |grep Temp|tr -s " " " "|cut -d " " -f3`;
	($s,$m,$h) = localtime($last_update = time);
	$m = sprintf "%02d", $m;
	$ilm = "kell $h:$m";
	for (qw(Tallinn Tartu Viljandi Pärnu)) {
		$ilm = "$ilm; $_: $temp{$_}°C"
	}
	$ilm =~ s/([0-9])\.([0-9])/\1,\2/sg;
	$ilm =~ s/\n//sg;
}

$| = 1;

while (<STDIN>) {
	$dt = time - $last_update;
	&update if $dt > 60 * 20;
	print "$ilm\n";
}
