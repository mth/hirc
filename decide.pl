#!/usr/bin/perl

@a = split /\s+/, $ARGV[0];
print "hmm... ", $a[rand @a], "\n";
