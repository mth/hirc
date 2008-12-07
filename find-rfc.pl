use strict;

my $INDEX = '1rfc_index.txt';
my $find = $ARGV[0];
my $txt;

if ($find =~ /\D/) {
	print "Imelik number\n";
	exit
}

open RFC, $INDEX or die "$INDEX: $!";
while (<RFC>) {
	if (/^~{79}/) {
		while (<RFC>) {
			if (/^(\d+) / and $1 == $find) {
				s/^\d+ //;
				chomp;
				$txt = $_;
				while (<RFC>) {
					chomp;
					s/\s+//s;
					last unless $_;
					$txt .= " $_"
				}
				last
			}
		}
		last
	}
}

print $txt || "Pole sellist.", "\n";
