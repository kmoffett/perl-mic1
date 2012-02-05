#!/usr/bin/perl
###
## mac1.pl  -  MAC-1 virtual machine
## Copyright (C) 2004-2012 Kyle Moffett <kyle@moffetthome.net>
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License, version 2, as published
## by the Free Software Foundation.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program (in the file "LICENSE.GPLv2".  If not, you may obtain
## it from the following URL:
##   http://www.gnu.org/licenses/gpl-2.0.txt
###

use warnings;
use strict;

use lib './perlmod';

use Mic1;
use Mac1;

my $mac = Mac1->new;

my $term = !@ARGV;

$| = 1 if $term;

my @bin;
print "> " if $term;
CHUNK:
while (<>) {
	last CHUNK if /^\s*(STOP|QUIT)\s*$/;
	if (/^\s*PARSE\s*$/) {
		$mac->code_binary(\@bin);
	} elsif (/^\s*CLEAR\s*$/) {
		@bin = ();
	} elsif (/^\s*RUN\s*$/) {
		STEP:
		until ($mac->{mic1}{halt}) {
			$mac->{mic1}->single_step(1);
			print "Press <ENTER> to continue...";
			scalar <> or last STEP;
		}
		$mac->reset;
	} elsif (/^\s*PRINT\s*$/) {
		for (@bin) {
			printf "0x\%04x\n", $_;
		}
	} else {
		chomp;
		push @bin, hex($_);
	}
	print "> " if $term;
}

print "\n";

