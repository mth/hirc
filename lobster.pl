#!/usr/bin/perl

use strict;
use Sys::Syslog;

my $lobster_url = 'https://lobste.rs/t/linux,unix,networking,hardware,release';
my $log_file = $ARGV[0] // 'lobster.log';

sub fetch_lobster {
	open my $out, '-|', '/usr/bin/curl', '-sS', $lobster_url or die "curl $lobster_url: $!";
	local $/;
	my $result = <$out>;
	return $result if close $out;
	syslog(3, $result) if $result;
	exit 1;
}

sub unescape {
	local $_ = $_[0];
	s/&\#(\d+?);/chr $1/seg;
	s/&lt;/</sg;
	s/&gt;/>/sg;
	s/&quot;/"/sg;
	s/&amp;/\&/sg;
	$_
}

our %lobster_log;
if (open my $log, '<', $log_file) {
	for (<$log>) {
		chomp;
		$lobster_log{$_} = 1
	}
} else {
	print STDERR "Reading $log_file: $!\n";
}

my $html = &fetch_lobster;
while ($html =~ /<div\ [^>]*?[" ]h-entry">.*?
	         <div\ [^>]*?[" ]score">[^>\d]*?(\d+)[^>]*?<\/div>.*?
		 <a(?=\ [^>]*?class="u-url")[^>]*?\ href="([^">]+)"[^>]*?>([^<]+?)<\/a>.*?
		 <\/div>\s*<\/div>/sgx) {
	next unless $1 >= 50;
	my $url = unescape($2);
	my $description = unescape($3);
	next if $lobster_log{$url} or $url !~ /^https?:\/\// or $url =~ /^https:\/\/lobste.rs\//;
	print "$url  $description\n";
	open my $log, '>>', $log_file or die "Log $log_file error: $!\n";
	print $log "$url\n"
}
