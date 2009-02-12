$last_update = 0;

sub update {
	print STDERR "ilmauuendus $dt\n";
	$lynx = '/usr/bin/lynx';
	$tallinn=`$lynx -source http://www.ilm.ee/~data/tallinn/temp`;
	$tartu=`$lynx -dump http://meteo.physic.ut.ee/et/frontmain.php?m=2 |grep Temp|tr -s " " " "|cut -d " " -f3`;
#	$viljandi=`$lynx -source http://www.ilm.ee/~data/viljandi/temp`;
	$viljandi=`$lynx -source http://vana.nadal.viljandi.ee/nilm/temp`;
	$parnu=`$lynx -dump http://ilm.transcom.ee/mobile.aspx`;
	$parnu =~ /\n +..?hk ([-,0-9]+)/s;
	$parnu = $1;
	($s,$m,$h) = localtime($last_update = time);
	$m = sprintf "%02d", $m;
	$ilm = "kell $h:$m; Tallinn: $tallinn°C; Tartu: $tartu°C; Viljandi: $viljandi°C; Pärnu: $parnu°C";
	#$ilm = "kell $h:$m; Tallinn: $tallinn°C; Tartu: $tartu°C; Pärnu: $parnu°C";
	$ilm =~ s/([0-9])\.([0-9])/\1,\2/sg;
	$ilm =~ s/\n//sg;
}

$| = 1;

while (<STDIN>) {
	$dt = time - $last_update;
	&update if $dt > 60 * 20;
	print "$ilm\n";
}
