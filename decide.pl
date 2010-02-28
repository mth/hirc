#!/usr/bin/perl

$_ = shift;
/\s+(?:või|ja)\s+|,/ ? split /\s+või\s+|,\s*/ : split /\s+/;
print "hmm... ", $_[rand @_], "\n";
