#!/usr/bin/perl

$_ = shift;
@a = /\s+(?:või|ja|or)\s+|,/ ? split /\s+(?:või|or)\s+|,\s*/ : split /\s+/;
print "hmm... ", $a[rand @a], "\n";
