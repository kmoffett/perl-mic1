###
## Mac1  -  MAC-1 virtual machine
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
package Mac1;

use warnings;
use strict;

use Mic1;

# Use this space to preload and parse the default MIC1 code for our interpreter
# NOTE: The code is in the data section following this file
our @mac1_default_text;
our @mac1_default_code;
INIT { @mac1_default_text = <Mac1::DATA>; close(Mac1::DATA); }

sub global_code	( $	);

sub new		( $	);
sub code_binary	( $\@	);
sub code_text	( $\@	);
sub reset	( $	);
#sub DESTROY	( $	);

sub global_code	( $	) {
	unless (@mac1_default_code) {
		# Whoops, we haven't compiled it yet
		my $compiler = Mic1->new;
		$compiler->code_text([@mac1_default_text]);
		@mac1_default_code = @{$compiler->{code}};
	}
	return [@mac1_default_code];
}

sub new		( $ ) {
	my($proto) = (@_);
	my $class = ref $proto || $proto;
	
	my $self = {};
	bless $self,$class;
	
	if (ref($proto)) {
		# Copy the MIC1
		$self->{mic1} = Mic1->new;
	} else {
		# Create a new MIC1 and use the default code
		$self->{mic1} = Mic1->new;
		$self->{mic1}->code_binary($self->global_code);
	}
	
	return $self;
}

sub code_binary	( $\@	) {
	my($self,$code) = @_;
	$self->{mic1}->reset;
	$self->{mic1}{mem} = $code;
}

sub code_text	( $\@	) {
	my($self,$code) = @_;
	return 0;
}

sub reset	( $	) {
	my($self) = @_;
	my $code = $self->{mic1}{mem};
	$self->{mic1}->reset;
	$self->{mic1}{mem} = $code;
}

sub run		( $$	) {
	my($self,$verbose) = @_;
	return $self->{mic1}->run($verbose);
}

1;

__DATA__

# MAC1 interpreter in MIC1
main:	mar := pc; rd;					# Main loop
	pc := pc + 1; rd;				# Increment program counter
	ir := mbr; if n then goto b1xxx;		# Save and decode the instruction
	tir := lshift(ir + ir); if n then goto b01xx;	
	tir := lshift(tir); if n then goto b001x;	
	alu := tir; if n then goto STOD;			
LODD:	mar := ir; rd;					# Load Direct		(0b0000)
fin_rd:	rd;						
	ac := mbr; goto main;				
STOD:	mar := ir; mbr := ac; wr;			# Store Direct		(0b0001)
fin_wr:	wr; goto main;					# Finish the write
b001x:	alu := tir; if n then goto SUBD;			
ADDD:	mar := ir; rd;					# Add Direct		(0b0010)
fn_rad:	rd;						
	ac := mbr + ac; goto main;			
SUBD:	mar := ir; rd;					# Subtract Direct	(0b0011)
fn_rsb:	ac := ac + 1; rd;				
	a := inv(mbr);					
	ac := ac + a; goto main;			
b01xx:	tir := lshift(tir); if n then goto b011x;	
b010x:	alu := tir; if n then goto JZER;			
JPOS:	alu := ac; if n then goto main;			# Jump if Positive	(0b0100)
do_jmp:	pc := band(ir,amask); goto main;		
JZER:	alu := ac; if z then goto do_jmp;		# Jump if Zero		(0b0101)
	goto main;						
b011x:	alu := tir; if n then goto LOCO;			
JUMP:	pc := band(ir,amask); goto main;		# Jump Always		(0b0110)
LOCO:	ac := band(ir,amask); goto main;		# Load Constant		(0b0111)
b1xxx:	tir := lshift(ir + ir); if n then goto b11xx;	
b10xx:	tir := lshift(tir); if n then goto b101x;	
b100x:	alu := tir; if n then goto STOL;		
LODL:	a := ir + sp;					# Load Stack Local	(0b1000)
	mar := a; rd; goto fin_rd;			
STOL:	a := ir + sp;					# Store Stack Local	(0b1001)
	mar := a; mbr := ac; wr; goto fin_wr;		
b101x:	alu := tir; if n then goto SUBL;		
ADDL:	a := ir + sp;					# Add Stack Local	(0b1010)
	a := band(a,amask);				# BUGFIX, show to Mr. Strong
	mar := a; rd; goto fn_rad;				
SUBL:	a := ir + sp;					# Subtract Stack Local	(0b1011)
	a := band(a,amask);				# BUGFIX, show to Mr. Strong
	mar := a; rd; goto fn_rsb;			
b11xx:	tir := lshift(tir); if n then goto b111x;	
b110x:	alu := tir; if n then goto JNZE;		
JNEG:	alu := ac; if n then goto do_jmp;		# Jump if Negative	(0b1100)
	goto main;					
JNZE:	alu := ac; if z then goto main;			# Jump if Nonzero	(0b1101)
	pc := band(ir,amask); goto main;		
b111x:	tir := lshift(tir); if n then goto bFxxx;	
CALL:	sp := sp + (-1);				# Function Call		(0b1110)
	mar := sp; mbr := pc; wr;			
	pc := band(ir,amask); wr; goto main;		
bFxxx:	tir := lshift(tir); if n then goto bF1xx;
bF0xx:	tir := lshift(tir); if n then goto bF01x;
bF00x:	alu := tir; if n then goto POPI;
PSHI:	mar := ac; rd;
	sp := sp + (-1); rd;
	mar := sp; wr; goto fin_wr;
POPI:	mar := sp; sp := sp + 1; rd;
	rd;
	mar := ac; wr; goto fin_wr;
bF01x:	alu := tir; if n then goto POP;
PUSH:	sp := sp + (-1);
	mar := sp; mbr := ac; wr; goto fin_wr;
POP:	mar := sp; sp := sp + 1; rd;
	rd;
	ac := mbr; goto main;
bF1xx:	tir := lshift(tir); if n then goto bF11x;
bF10x:	alu := tir; if n then goto SWAP;
RETN:	mar := sp; sp := sp + 1; rd;
	rd;
	pc := mbr; goto main;
SWAP:	a := ac;
	ac := sp;
	sp := a; goto main;
bF11x:	alu := tir; if n then goto fin_sp;
INSP:	a := band(ir,smask);
fin_sp:	sp := sp + a; goto main;
DESP:	a := band(ir,smask);
	a := inv(a);
	a := a + 1; goto fin_sp;

