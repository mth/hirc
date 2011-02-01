use Encode;
use LWP;

$user_agent = LWP::UserAgent->new;
$user_agent->agent('Lynx/2.8.6rel.4 libwww-FM/2.14');

sub http {
	my ($timeout, $url) = @_;
	my $req = HTTP::Request->new(GET => $url);
	$user_agent->timeout($timeout);
	my $res = $user_agent->request($req);
	if ($res->is_success) {
		return $res->content
	}
}

$last_update = 0;
$emhi_url = 'http://213.184.50.180/ilma_andmed/xml/observations.php';

sub update {
	print STDERR "ilmauuendus $dt\n";
	for (split /\n/s, http(40, $emhi_url)) {
		if (/<name>(.*)<\/name>/) {
			$name = encode("UTF-8", $1);
			$name =~ s/ *\([^)]*\)//;
		} elsif (/<airtemperature>(.*)<\/airtemperature>/) {
			$temp{$name} = $1
		}
	}
	$_ = http(10, 'http://193.40.11.172/et/frontmain.php?m=2');
	s/<[^>]*>/ /g;
	$temp{Tartu} = $1 if / Temperatuur +(-?\d+(\.\d+)?) /;
	#@tallinn = `/usr/bin/lynx -source http://weather.noaa.gov/pub/data/observations/metar/stations/EETN.TXT`;
	#if ($tallinn[1] =~ / (M?)(\d\d)\/M?\d\d /) {
	#	$tallinn = $1 ? -$2 : $2;
	#}
	#@tallinn_ = split(/[ ']/, `/bin/nc 193.40.240.131 6340`);
	#if (@tallinn_ > 2) {
	$tallinn = http(10, 'http://www.ilm.ee/~data/tallinn/temp');
	chomp $tallinn;
	if ($tallinn =~ /-?[\d.]+/ and
			($tallinn ne '0.0' or $temp{Tallinn} !~ /\d/)) {
		$tallinn = $&; # $tallinn_[2];
		#$tallinn =~ tr/,/./;
		if ($temp{Tallinn} =~ /\d/) {
			$tallinn += $temp{Tallinn};
			$tallinn = sprintf("%.1f", $tallinn / 2);
		}
		$temp{Tallinn} = $tallinn;
		#$tartu = http(10, 'http://www.ilm.ee/~data/tartu/temp');
		#$temp{Tartu} = $tartu if $tartu =~ /\d/;
	}
	($s,$m,$h) = localtime($last_update = time);
	$m = sprintf "%02d", $m;
	$ilm = "kell $h:$m";
	for (qw(Tallinn Tartu Viljandi Keila Pärnu Võru)) {
		$ilm = "$ilm; $_: $temp{$_}°C" if defined $temp{$_}
	}
	$ilm =~ s/([0-9])\.([0-9])/\1,\2/sg;
	$ilm =~ s/\n//sg;
}

$| = 1;

while (<STDIN>) {
	$dt = time - $last_update;
	&update if $dt > 60 * 10;
	print "$ilm\n";
}
