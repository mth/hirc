#!/usr/bin/perl

$_ = shift;
/\s+(?:v�i|ja)\s+|,/ ? split /\s+v�i\s+|,\s*/ : split /\s+/;
print "hmm... ", $_[rand @_], "\n";
