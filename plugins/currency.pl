use strict;

my $SRC = 'http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml';

sub parse {
	return () unless open F, "<$_[0]";
	my $data = join('', <F>);
	close F;
	my %currencies;
	while ($data =~ /<Cube\s+time='([\d-]*)'> |
			<Cube\s+currency='(\w+)'\s+rate='([\d.]+)'\/>/sgx) {
		if ($1) {
			$currencies{date} = $1
		} else {
			$currencies{$2} = $3
		}
	}
	%currencies
}

sub init {
	my $DATA = 'eurofxref-daily.xml';
	my $DATA_TMP = "$DATA.tmp";

	my ($sec, $min, $hour, $mday, $month, $year) = gmtime(time - 86400);
	my $yesterday = sprintf "%04d-%02d-%02d", $year + 1900, $month + 1, $mday;

	my %cur_data = parse($DATA);
	my $no_data = keys %cur_data <= 1;

	if ($no_data or $cur_data{date} lt $yesterday) {
		unlink $DATA_TMP;
		system '/usr/bin/wget', '-q', '-O', $DATA_TMP, $SRC;
		my %new_data = parse($DATA_TMP);
		if ($no_data or keys %new_data > 1) {
			rename $DATA_TMP, $DATA;
			%cur_data = %new_data
		}
	}

	%cur_data
}

my $sum;
my $cur;

if ($ARGV[0] =~ /^\s*(\d+(?:\.\d+)?)\s+(\w+)\s*$/) {
	$sum = $1;
	$cur = $2;
} elsif ($ARGV[0] =~ /^\s*(\w+)\s*$/) {
	$sum = undef;
	$cur = $1;
} elsif ($ARGV[0] =~ /\S/) {
	print "Mine arenema\n";
	exit 0
}

$cur = uc $cur;
my %cur_data = &init;
$cur_data{EEK} = 15.6466;

unless ($cur) {
	print join(', ', sort grep {$_ ne 'date'} keys %cur_data) . "\n";
	exit 0
}

my $rate = $cur_data{$cur};
unless ($rate) {
	print "Ah keri pekki\n";
	exit 0
}

if ($sum) {
	my $eur = sprintf "%.4f", $sum / $rate;
	if ($cur eq 'EEK') {
		print "$sum $cur on $eur €\n"
	} else {
		my $eek = sprintf "%.2f", $sum / $rate * 15.6466;
		print "$sum $cur on $eur € ($eek kr)\n"
	}
} else {
	print "1 € on $rate $cur\n"
}
