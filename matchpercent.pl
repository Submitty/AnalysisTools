#!/usr/bin/env perl

use strict;
use warnings;

sub compare_fingerprints {
	my ($firstref, $secondref) = @_;
	my (@smaller, @larger);
	if (scalar(@$firstref) < scalar(@$secondref)) {
		@smaller = @$firstref;
		@larger = @$secondref
	} else {
		@smaller = @$secondref;
		@larger = @$firstref
	}
	my $total = 0;
	for my $el (@larger) {
		if (grep(/^$el$/, @smaller)) { $total += 1; }
	}
	return $total / @larger;
}

while (defined(my $first = <>) && defined(my $second = <>)) {
	$first =~ s/^\s+|\s+$//g;
	$second =~ s/^\s+|\s+$//g;
	my $first_type = filetype($first);
	my $second_type = filetype($second);
	print "$first:$second:", compare_fingerprints(\@first_fingerprint, \@second_fingerprint), "\n";
}
