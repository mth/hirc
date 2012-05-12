#!/usr/bin/perl

$_ = shift;
/\s+(?:või|ja|or)\s+|,/ ? split /\s+(?:või|or)\s+|,\s*/ : split /\s+/;
print "hmm... ", $_[rand @_], "\n";
