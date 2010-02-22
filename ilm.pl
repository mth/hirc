$last_update = 0;

sub update {
	print STDERR "ilmauuendus $dt\n";
	$lynx = '/usr/bin/lynx';
	# www.emhi.ee - 213.184.50.180
	open F, '/usr/bin/wget -q -O - http://213.184.50.180/ilma_andmed/xml/observations.php|';
	while (<F>) {
		if (/<name>(.*)<\/name>/) {
			$name = $1;
			$name =~ s/ *\([^)]*\)//;
		} elsif (/<airtemperature>(.*)<\/airtemperature>/) {
			$temp{$name} = $1
		}
	}
	close F;
	#$temp{Tallinn}=`$lynx -source http://www.ilm.ee/~data/tallinn/temp`;
	#$temp{Keila}=`$lynx -source http://www.ilm.ee/~data/keila/temp`;
	# meteo.physic.ut.ee - 193.40.11.172
	$temp{Tartu}=`$lynx -dump http://193.40.11.172/et/frontmain.php?m=2 |grep Temp|tr -s " " " "|cut -d " " -f3`;
	($s,$m,$h) = localtime($last_update = time);
	$m = sprintf "%02d", $m;
	$ilm = "kell $h:$m";
	for (qw(Tallinn Tartu Viljandi Keila Pärnu Võru)) {
		$ilm = "$ilm; $_: $temp{$_}°C" if $temp{$_}
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
