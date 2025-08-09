#!/usr/bin/perl

for (`/usr/bin/finger \@finger.kernel.org`) {
	push @r, $1 if / latest stable [^:]*:\s*([0-9.]+)/
}
print "The latest stable Linux kernel is: " . join(" | ", @r) . "\n"
