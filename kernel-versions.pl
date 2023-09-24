my $URL = 'https://kernel.org/';

sub kernel_list {
	my @result;
	if (open my $inf, '-|', '/usr/bin/curl', '-s', $URL) {
		while (<$inf>) {
			while (/<strong>(\d+.\d+.\d+)<\/strong>/g) {
				push @result, $1
			}
		}
		return join(' | ', @result)
	}
	print STDERR "curl $URL: $!";
	exit 1;
}

my $topic = $ARGV[0];
$topic =~ s/^(?: *+[\d.]++ *+(?:\| *+|$))*+//s;
$topic = " | $topic" if $topic;
print kernel_list(), "$topic\n";
