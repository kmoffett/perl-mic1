#!/usr/bin/perl
###
## mic1.pl  -  MIC-1 virtual machine
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

my $mic = Mic1->new;

my $term = !@ARGV;

my @text;
print "> " if $term;
CHUNK:
while (<>) {
	last CHUNK if /^\s*(STOP|QUIT)\s*$/;
	if (/^\s*PARSE\s*$/) {
		$mic->code_text(\@text);
	} elsif (/^\s*CLEAR\s*$/) {
		@text = ();
	} elsif (/^\s*RUN\s*$/) {
		STEP:
		until ($mic->{halt}) {
			$mic->single_step(1);
			print "Press <ENTER> to continue...";
			scalar <> or last STEP;
		}
	} elsif (/^\s*PRINT\s*$/) {
		print "            A  C                                    \n";
		print "            M  O  A     M  M        E               \n";
		print "            U  N  L  S  B  A  R  W  N               \n";
		print "HEX         X  D  U  H  R  R  D  R  C  C  B  A  ADDR\n";
		print "                                                    \n";
		for (@{$mic->{code}}) {
			my $b = $_;
			printf "0x\%08x  ", $b;
			printf "\%01i  ", ($b & Mic1::D_AMUX)>>31;
			printf "\%01i  ", ($b & Mic1::D_COND)>>29;
			printf "\%01i  ", ($b & Mic1::D_ALU)>>27;
			printf "\%01i  ", ($b & Mic1::D_SH)>>25;
			printf "\%01i  ", ($b & Mic1::D_MBR)>>24;
			printf "\%01i  ", ($b & Mic1::D_MAR)>>23;
			printf "\%01i  ", ($b & Mic1::D_RD)>>22;
			printf "\%01i  ", ($b & Mic1::D_WR)>>21;
			printf "\%01i  ", ($b & Mic1::D_ENC)>>20;
			printf "\%02i ", ($b & Mic1::D_C)>>16;
			printf "\%02i ", ($b & Mic1::D_B)>>12;
			printf "\%02i ", ($b & Mic1::D_A)>>8;
			printf "0x\%02x\n", ($b & Mic1::D_ADDR)>>0;
		}
	} else {
		push @text, $_;
	}
	print "> " if $term;
}

print "\n";

