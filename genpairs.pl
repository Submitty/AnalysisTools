#!/usr/bin/env perl

use strict;
use warnings;

if (scalar(@ARGV) != 2) {
	print "Invalid arguments\n";
	exit 1;
}

my @files;

for my $arg (@ARGV) {
	opendir(DIR, $arg) or die $!;
	my @listing;
	while (readdir(DIR)) {
		next if m/^\./;
		push @listing, $_;
	}
	closedir(DIR);
	push @files, [ @listing ];
}

for my $first (@{$files[0]}) {
	for my $second (@{$files[1]}) {
		print "$ARGV[0]/$first\n$ARGV[1]/$second\n";
	}
}
