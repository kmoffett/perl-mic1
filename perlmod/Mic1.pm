###
## Mic1  -  MIC-1 virtual machine
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
package Mic1;

use warnings;
use strict;

use constant {
	D_AMUX	=> 0x80000000,
	D_COND	=> 0x60000000,
	D_ALU	=> 0x18000000,
	D_SH	=> 0x06000000,
	D_MBR	=> 0x01000000,
	D_MAR	=> 0x00800000,
	D_RD	=> 0x00400000,
	D_WR	=> 0x00200000,
	D_ENC	=> 0x00100000,
	D_C	=> 0x000F0000,
	D_B	=> 0x0000F000,
	D_A	=> 0x00000F00,
	D_ADDR	=> 0x000000FF,
};

our @reg_lst = qw( PC AC SP IR TIR 0 1 -1 AMASK SMASK A B C D E F );
our %reg_map;
@reg_map{$reg_lst[$_],lc $reg_lst[$_]} = ($_,$_) for (0 .. $#reg_lst);

sub new		( $   );
sub code_binary	( $\@ );
sub code_asm	( $\@ );
sub _conv_asm	( $$  );
sub reset	( $   );
sub single_step	( $;$ );
sub run		( $   );
#sub DESTROY	( $   );

sub new		( $   ) {
	my($proto) = (@_);
	my $class = ref $proto || $proto;
	
	my $self = {};
	bless $self,$class;
	
	if (ref($proto)) {
		# Deep copy the registers, data memory, and code memory
		$self->{regs}	= [ @{$proto->{regs}}	];
		$self->{mem}	= [ @{$proto->{mem}}	];
		$self->{code}	= [ @{$proto->{code}}	];

		# Other CPU fields
		$self->{addr}	= $proto->{addr};
		$self->{mar}	= $proto->{mar};
		$self->{mbr}	= $proto->{mbr};
		$self->{load}	= $proto->{load};
		$self->{store}	= $proto->{store};
		$self->{halt}	= $proto->{halt};
	} else {
		# A CPU reset does most of what we need
		$self->reset;

		# Just initialize anything else
		$self->{code}	= [];
	}
	
	return $self;
}

sub code_binary	( $\@ ) {
	my($self,$code) = (@_);
	$self->{code} = $code;
}

sub _cwarn	( $@  ) {
	my $lineno = shift @_;
	print STDERR "warning: line $lineno: ", @_, "\n";
	return undef;
};
sub _cerr	( $@  ) {
	my $lineno = shift @_;
	print STDERR "error: line $lineno: ", @_, "\n";
	return undef;
};

# Small sub to return a register number given a string
sub _getreg	( $  ) {
	my $str = shift;
	$str = lc $str;
	$str =~ s/^\s*(.*?)\s*$/$1/;			# Remove extra whitespace
	$str =~ s/^(1|\+\s*1|\(\s*\+\s*1\s*\))$/1/;	# Fix the +1 register
	$str =~ s/^(-\s*1|\(\s*-\s*1\s*\))$/-1/;	# Fix the -1 register
	$str =~ /^(pc|ac|sp|ir|tir|0|1|-1|amask|smask|a|b|c|d|e|f)$/ or return undef;
	return $reg_map{$str};
}

sub code_text	( $\@ ) {
	my($self,$asm) = (@_);
	my @lines = @$asm;
	my %labels;
	my($lineno,$byteno);
	
	# Pass 1: Remove comments, extra whitespace
	my $multicomment = 0;
	$lineno = 0;
	LINE:
	for my $line (@lines) {
		$lineno++;
		if ($multicomment) {
			if ($line =~ s{^.*\*/}{}) {
				$multicomment = 0;
			} else {
				$line = "";
			}
		}
		$line =~ s{\#.*$}{};
		$line =~ s{//.*$}{};
		$multicomment = $lineno if $line =~ s{/\*.*$}{};
		$line =~ s{^\s+$}{};
	}
	return _cerr $multicomment, "Unterminated multi-line comment" if $multicomment;
	
	# Pass 2: Split the labels
	$lineno = 0;
	$byteno = 0;
	LINE:
	for my $line (@lines) {
		$lineno++;
		
		next LINE if $line eq "";
		
		# Grab out labels
		while ($line =~ s{^\s*([\w\d_]+)\:\s+}{}) {
			my $label = $1;
			if ($label =~ /^\d+$/) {
				if ($label+0 != $byteno) {
					_cwarn $lineno, "Byte label does not match current byte";
				}
			} else {
				$labels{$label} = $byteno;
			}
		}
		
		$line =~ s/^[\s\n]*(.*?)[\s\n]*$/$1/;
		
		if ($line ne "") {
			$labels{$byteno} = $byteno;
			$byteno++;
		}
	}
	
	# Pass 2: Parse the file
	$lineno = 0;
	$byteno = 0;
	my @code;
	LINE:
	for my $line (@lines) {
		$lineno++;
		
		# Ignore empty lines
		next LINE if $line eq "";
		
		# Chomp instructions
		my %curinst;
		while ($line =~ s{^\s*(.*?)\s*;\s*}{}) {
			my $instr = $1;
			
			# TODO - Add order checking
			if ($instr eq "rd") {							# Read instruction
				_cwarn $lineno, "Read operation already specified"				if $curinst{rd};
				return _cerr $lineno, "Read operation specified simultaneously with a write"	if $curinst{wr};
				_cwarn $lineno, "Subinstructions not listed in execution order"			if $curinst{cond};
				$curinst{rd} = 1;
				
			} elsif ($instr eq "wr") {						# Write instruction
				_cwarn $lineno, "Write operation already specified"				if $curinst{wr};
				return _cerr $lineno, "Write operation specified simultaneously with a read"	if $curinst{rd};
				_cwarn $lineno, "Subinstructions not listed in execution order"		if $curinst{cond};
				$curinst{wr} = 1;
				
			} elsif ($instr eq "halt") {						# Halt instruction
				return _cerr $lineno, "Multiple jumps from a single instruction are invalid"	if $curinst{cond};
				$curinst{cond} = 3;
				#print STDERR "Parsed 'halt': byteno=$byteno\n";
				$curinst{jump} = $byteno;
				
			} elsif ($instr =~ /^goto\s+([\w\d_]+)$/) {				# Unconditional jump
				return _cerr $lineno, "Multiple jumps from a single instruction are invalid"	if $curinst{cond};
				$curinst{cond} = 3;
				my $label = $1;
				return _cerr $lineno, "No such label '$label'" unless exists $labels{$label};
				#print STDERR "Parsed 'goto': label=$label byteno=".$labels{$label}."\n";
				$curinst{jump} = $labels{$label};
				
			} elsif ($instr =~ /^if\s+(n|z)\s+then\s+goto\s+([\w\d_]+)$/) {		# Conditional jump
				return _cerr $lineno, "Multiple jumps from a single instruction are invalid"	if $curinst{cond};
				$curinst{cond} = (($1 eq 'n')?1:2);
				my $label = $2;
				return _cerr $lineno, "No such label '$label'" unless exists $labels{$label};
				#print STDERR "Parsed 'ifgoto': cond=$1 label=$label byteno=".$labels{$label}."\n";
				$curinst{jump} = $labels{$label};
				
			} elsif ($instr =~ s/^(\w+)\s*:=\s*(.*?)\s*$/$2/) {					# Assignment
				my $dst = $1;
				
				# Check the destination
				if ($dst eq "mar") {
					my $src = _getreg $instr;
					return _cerr $lineno, "When storing to MAR, source must be a register" unless defined $src;
					return _cerr $lineno, "Multiple attempted stores to MAR" if $curinst{mar};
					_cwarn $lineno, "Subinstructions not listed in execution order"
						if $curinst{cond} or $curinst{rd} or $curinst{wr} or $curinst{alu};
					if (!$curinst{bset}) {
						# The B bus is unused, just use it
						$curinst{bset} = 2;
						$curinst{mar} = 1;
						$curinst{bsrc} = $src;
					} elsif ($curinst{bsrc} == $src) {
						# Ok, the existing use for the B bus matches up
						$curinst{bset} = 2;
						$curinst{mar}  = 1;
					} elsif ($curinst{bset} == 1) {
						# There is an existing use for the B bus, but A and B might be swappable
						if ($curinst{aset} == 0) {
							# There is nothing on the A bus, put whatever was on the B bus there
							$curinst{aset} = 2;
							$curinst{asrc} = $curinst{bsrc};
							$curinst{amux} = 0;
							$curinst{mar} = 1;
						} elsif ($curinst{aset} == 1 and $curinst{asrc} == $src and !$curinst{amux}) {
							# Wohoo, the A bus it switchable and matches
							$curinst{aset} = 2;
							$curinst{asrc} = $curinst{bsrc};
							$curinst{amux} = 0;
							$curinst{bset} = 2;
							$curinst{bsrc} = $src;
							$curinst{mar} = 1;
						} else {
							# Crud, the B bus can't be moved
							return _cerr $lineno, "Unable to schedule bus usage";
						}
					} else {
						return _cerr $lineno, "Unable to schedule bus usage";
					}
				} elsif ($dst eq "mbr" or $dst eq "alu" or defined _getreg $dst) { # It's a store through the ALU, so we have more parsing to do
					my $sh = 0;
					my($op,$abus,$bbus);
					
					_cwarn $lineno, "Subinstructions not listed in execution order"
						if $curinst{cond} or $curinst{rd} or $curinst{wr};
					_cwarn $lineno, "Multiple ALU instructions are specified and may be incompatible"
						if exists $curinst{alu};
					
					# Check for a shift
					$sh = (($1 eq 'r')?1:2) if $instr =~ s/^(r|l)shift\(\s*(.*?)\s*\)$/$2/;
					return _cerr $lineno, "Multiple shifter settings specified" if exists $curinst{sh} and $sh != $curinst{sh};
					$curinst{sh} = $sh;
					
					# Determine the operation
					if ($instr =~ /^(\S+)\s*(\+|-)\s*(\S+)$/) { # Addition
						$op = 0;
						my ($aval,$oval,$bval) = ($1,$2,$3);
						$abus = (($aval eq "mbr")?"mbr":_getreg $aval);
						$bbus = (($bval eq "mbr")?"mbr":_getreg $bval);
						if ($oval eq '-') {
							return _cerr $lineno, "Cannot subtract anything other than 1, 0 and -1" if $bbus eq "mbr";
							if	($bbus == 5) { $bbus = 5; } # Zero
							elsif	($bbus == 6) { $bbus = 7; } # +1 -> -1
							elsif	($bbus == 7) { $bbus = 6; } # -1 -> +1
							else { return _cerr $lineno, "Cannot subtract anything other than 1, 0, and -1"; }
						}
					} elsif ($instr =~ /^band\s*\(\s*(\S+)\s*,\s*(\S+)\s*\)$/) { # Binary and
						$op = 1;
						my ($aval,$bval) = ($1,$2);
						$abus = (($aval eq "mbr")?"mbr":_getreg $aval);
						$bbus = (($bval eq "mbr")?"mbr":_getreg $bval);
						return _cerr $lineno, "Invalid argument(s) for band()" unless defined($abus) and defined($bbus);
					} elsif ($instr =~ /^inv\s*\(\s*(\S+)\s*\)$/) { # Binary not
						$op = 3;
						$abus = (($1 eq "mbr")?"mbr":_getreg $1);
						return _cerr $lineno, "Invalid argument for inv()" unless defined($abus);
					} else {
						$op = 2;
						$abus = (($instr eq "mbr")?"mbr":_getreg $instr);
						return _cerr $lineno, "Invalid source for assignment: $1" unless defined($abus);
					}
					return _cerr $lineno, "Multiple ALU settings specified" if exists $curinst{alu} and $op != $curinst{alu};
					$curinst{alu} = $op;
					
					# First set up the input bus requirements:
					# 	Can only put abus on the B bus if there are 2 args
					#	If MBR is used as a source it must go on the A busi
					if ($abus eq "mbr") {
						return _cerr $lineno, "Cannot have both inputs read from the MBR" if defined $bbus and $bbus eq "mbr";
						if ($curinst{amux}) {
							# It's already there, so use it
							$curinst{aset} = 2;
						} elsif (not $curinst{aset}) {
							# The spot is open, so use it
							$curinst{aset} = 2;
							$curinst{amux} = 1;
						} elsif ($curinst{aset} == 1 and not $curinst{bset}) {
							# The B bus is open, and the A bus is relocatable
							$curinst{bset} = 2;
							$curinst{bsrc} = $curinst{asrc};
							$curinst{amux} = 1;
							$curinst{aset} = 2;
						} else {
							# Nothing is free enough
							return _cerr $lineno, "Unable to schedule bus usage";
						}
						
						# Now for the B bus
						if (defined $bbus) {
							if ($curinst{bset} and $curinst{bsrc} == $bbus) {
								# That's there too
								$curinst{bset} = 2;
							} elsif (not $curinst{bset}) {
								# It's free for use
								$curinst{bsrc} = $bbus;
								$curinst{bset} = 2;
							} else {
								return _cerr $lineno, "Unable to schedule bus usage";
							}
						}
					} elsif (defined $bbus and $bbus eq "mbr") {
						if ($curinst{amux}) {
							# It's already there, so use it
							$curinst{aset} = 2;
						} elsif (not $curinst{aset}) {
							# The spot is open, so use it
							$curinst{aset} = 2;
							$curinst{amux} = 1;
						} elsif ($curinst{aset} == 1 and not $curinst{bset}) {
							# The B bus is open, and the A bus is relocatable
							$curinst{bset} = 2;
							$curinst{bsrc} = $curinst{asrc};
							$curinst{amux} = 1;
							$curinst{aset} = 2;
						} else {
							# Nothing is free enough
							return _cerr $lineno, "Unable to schedule bus usage";
						}
						
						# Now for the B bus
						if ($curinst{bsrc} == $abus) {
							# That's there too
							$curinst{bset} = 2;
						} elsif ($curinst{bset} == 0) {
							# It's free for use
							$curinst{bsrc} = $abus;
							$curinst{bset} = 2;
						} else {
							return _cerr $lineno, "Unable to schedule bus usage";
						}
					} elsif ($curinst{aset} and !$curinst{amux} and $curinst{asrc} == $abus) {
						# Hey, it's already there, so use it
						$curinst{aset} = 2 unless defined $bbus;
						
						# Now check the B bus
						if (defined $bbus) {
							if ($curinst{bsrc} == $bbus and $curinst{bset}) {
								# Hey, that one's there too!
								$curinst{bset} = $curinst{aset} if $curinst{aset} > $curinst{bset};
							} elsif ($curinst{bset} == 0) {
								# Ok, it's not there, but that spot is empty
								$curinst{bset} = $curinst{aset};
								$curinst{bsrc} = $bbus;
							} else {
								# Crud, this won't work, it's already used
								return _cerr $lineno, "Unable to schedule bus usage";
							}
						}
					} elsif (defined $bbus and $curinst{bset} and $curinst{bsrc} == $abus) {
						# Ok, we have 2 arguements, and abus matches the B bus

						# Now check the A bus
						if ($curinst{aset} and !$curinst{amux} and $curinst{asrc} == $abus) {
							# Hey that's there too
							$curinst{aset} = $curinst{bset} if $curinst{bset} > $curinst{aset};
						} elsif (not $curinst{aset}) {
							# Ok, it's not there, but we can use that spot
							$curinst{aset} = $curinst{bset};
							$curinst{asrc} = $bbus;
						} else {
							# Crud, this won't work, it's already used
							return _cerr $lineno, "Unable to schedule bus usage";
						}
					} elsif (defined $bbus and $curinst{aset} and !$curinst{amux} and $curinst{asrc} == $bbus) {
						# Ok, we have 2 arguements, and bbus matches the A bus
						
						# Now check the B bus (We already know it's not the same)
						if (not $curinst{bset}) {
							# Ok, we can use that spot
							$curinst{bset} = $curinst{aset};
							$curinst{bsrc} = $abus;
						} else {
							# Crud, this won't work, it's already used
							return _cerr $lineno, "Unable to schedule bus usage";
						}
					} elsif (defined $bbus and $curinst{bset} and $curinst{bsrc} == $bbus) {
						# Ok, we have 2 arguements, and bbus matches the B bus
						
						# Now check the A bus (We already know it's not the same)
						if (not $curinst{aset}) {
							# Ok, we can use that spot
							$curinst{aset} = $curinst{bset};
							$curinst{asrc} = $abus;
						} else {
							# Crud, this won't work, it's already used
							return _cerr $lineno, "Unable to schedule bus usage";
						}
					} elsif (not $curinst{aset}) {
						# Nothing matches, but this spot is open, so try it
						$curinst{aset} = 1;
						$curinst{asrc} = $abus;
						
						# Now check the B bus (Doesn't match)
						if (defined $bbus) {
							if (not $curinst{bset}) {
								# Ok, that spot is empty
								$curinst{bset} = 1;
								$curinst{bsrc} = $bbus;
							} else {
								# Crud, this won't work, it's already used
								return _cerr $lineno, "Unable to schedule bus usage";
							}
						}
					} elsif (defined $bbus and not $curinst{bset}) {
						# If we get here, we're doomed anyway because the a spot is taken, but
						# it doesn't match anything, and this is a 2 arguemnt ALU function
						return _cerr $lineno, "Unable to schedule bus usage";
					} else {
						return _cerr $lineno, "Unable to schedule bus usage";
					}
					
					# AGH, finally done with scheduling the input bus, now check the output bus
					if ($dst eq "mbr") {
						_cwarn $lineno, "Duplicate store to MBR" if $curinst{mbr};
						$curinst{mbr} = 1;
					} elsif ($dst eq "alu") {
						# TSILB (Don't need to do anything if it's not stored anywhere)
					} else {
						if ($curinst{enc}) {
							return _cerr $lineno, "Cannot store to multiple registers" unless defined $dst and $curinst{cdst} == _getreg $dst;
							_cwarn $lineno, "Duplicate store to register";
						} else {
							$curinst{cdst} = _getreg $dst;
							$curinst{enc} = 1;
						}
					}
				} else {
					return _cerr $lineno, "Invalid destination for assignment: $dst";
				}
			} else {
				return _cerr $lineno, "Invalid statement: $instr";
			}
		}

		# Now actually enter the instruction data in the code block
		if (%curinst) {
			my $b = ((($curinst{amux}||0)		) && D_AMUX)	|
				((($curinst{cond}||0)<<29	) &  D_COND)	|
				((($curinst{alu}||0)<<27	) &  D_ALU)	|
				((($curinst{sh}||0)<<25		) &  D_SH)	|
				((($curinst{mbr}||0)		) && D_MBR)	|
				((($curinst{mar}||0)		) && D_MAR)	|
				((($curinst{rd}||0)		) && D_RD)	|
				((($curinst{wr}||0)		) && D_WR)	|
				((($curinst{enc}||0)		) && D_ENC)	|
				((($curinst{cdst}||0)<<16	) &  D_C)	|
				((($curinst{bsrc}||0)<<12	) &  D_B)	|
				((($curinst{asrc}||0)<<8	) &  D_A)	|
				((($curinst{jump}||0)<<0	) &  D_ADDR);
			$code[$byteno++] = $b;
		}

		if ($line ne "") {
			print STDERR "error: line $lineno: Parse error, couldn't parse $line\n";
			return undef;
		}
	}
	
	$self->code_binary(\@code);
}

sub reset	( $   ) {
	my($self) = (@_);
	
	# Destroy and recreate regs
	$self->{regs}	= [];
	$self->{regs}[$_] = 0 for (0 .. $#reg_lst);
	$self->{regs}[$reg_map{-1}]	= 0xFFFF;
	$self->{regs}[$reg_map{1}]	= 0x0001;
	$self->{regs}[$reg_map{0}]	= 0x0000;
	$self->{regs}[$reg_map{AMASK}]	= 0x0FFF;
	$self->{regs}[$reg_map{SMASK}]	= 0x00FF;
	
	# Destroy and recreate mem
	$self->{mem}	= [];
	$self->{mem}[$_] = 0 for (0 .. (2^12)-1);
	
	# Other data
	$self->{addr}	= 0;
	$self->{mar}	= 0;
	$self->{mbr}	= 0;
	$self->{load}	= 0;
	$self->{store}	= 0;
	$self->{halt}	= 0;
	
	return $self;
}

sub single_step	( $;$ ) {
	my($self,$verbose) = (@_);
	
	# Check for halt state
	if ($self->{halt}) {
		print "CPU halted, not single stepping\n" if $verbose;
		return $self;
	}
	
	# Load the new instruction from the control store
	my $instr = $self->{code}[$self->{addr}];
	printf "INSTR	= CONTROL_STORE[0x\%x] = 0x\%x\n",$self->{addr},$instr if $verbose;
	
	# Increment the address in the control store
	$self->{addr}++;
	
	# Load the A and B latches from the appropriate registers
	my $a_latch = $self->{regs}[($instr & D_A)>>8];
	printf "A_LATCH	= REG[\%d] = 0x\%x\n",($instr & D_A)>>8,$a_latch if $verbose;
	my $b_latch = $self->{regs}[($instr & D_B)>>12];
	printf "B_LATCH	= REG[\%d] = 0x\%x\n",($instr & D_B)>>12,$b_latch if $verbose;
	
	# Optionally load the MAR from the 'B' latch
	if ($instr & D_MAR) {
		$self->{mar} = $b_latch;
		printf "MAR	= B_LATCH\n" if $verbose;
	}
	
	# Determine whether the 'A' ALU input comes from MBR or the 'A' latch
	my $a_mux;
	if ($instr & D_AMUX) {
		$a_mux = $self->{mbr};
		printf "A_MUX	= MBR = 0x\%x\n", $a_mux if $verbose;
	} else {
		$a_mux = $a_latch;
		printf "A_MUX	= A_LATCH\n" if $verbose;
	}
	
	# Cause the ALU to compute a result
	my $alu = $instr & D_ALU;
	if (($alu>>27) == 0) {
		$alu = $a_mux + $b_latch;
		printf "ALU	= A_MUX + B_LATCH = 0x\%x\n",$alu if $verbose;
	} elsif (($alu>>27) == 1) {
		$alu = $a_mux & $b_latch;
		printf "ALU	= A_MUX & B_LATCH = \%x\n",$alu if $verbose;
	} elsif (($alu>>27) == 2) {
		$alu = $a_mux;
		printf "ALU	= A_MUX = 0x\%x\n",$alu if $verbose;
	} else { # ($alu>>27) == 3
		$alu = ~$a_mux;
		printf "ALU	= ~A_MUX = 0x\%x\n",$alu if $verbose;
	}
	$alu &= 0xFFFF;
	
	# Determine the N and Z flags
	my($n_flag,$z_flag) = ($alu & 0x8000, !$alu);
	printf "N	= \%d\nZ	= \%d\n",($n_flag?1:0),($z_flag?1:0) if $verbose;
	
	# Shift the result of the ALU
	my $shifter;
	if ($instr & D_SH) {
		if ((($instr & D_SH)>>25) == 1) {
			$shifter = 0xFFFF & ($alu>>1);
			printf "SHIFTER	= ALU >> 1 = 0x\%x\n",$shifter if $verbose;
		} else {
			$shifter = 0xFFFF & ($alu<<1);
			printf "SHIFTER = ALU << 1 = 0x\%x\n",$shifter if $verbose;
		}
	} else {
		$shifter = $alu;
		printf "SHIFTER = ALU\n" if $verbose;
	}
	
	# Store to MBR and/or registers if desired
	if ($instr & D_MBR) {
		$self->{mbr} = $shifter;
		printf "MBR = SHIFTER\n" if $verbose;
	}
	if ($instr & D_ENC) {
		$self->{regs}[ ($instr & D_C)>>16 ] = $shifter;
		printf "REG[\%d] = SHIFTER\n",($instr & D_C)>>16 if $verbose;
	}
	
	# Complete a memory load or store if desired
	if ($instr & D_RD && $self->{load}) {
		$self->{mbr} = 0x0FFF & $self->{mem}[$self->{mar}];
		$self->{load} = 0;
		$instr &= ~D_RD;
		printf "LOAD DONE: MBR = MEM[0x\%x] = 0x\%x\n",$self->{mar},$self->{mbr} if $verbose;
	}
	if ($instr & D_WR && $self->{store}) {
		$self->{mem}[$self->{mar}] = $self->{mbr};
		$self->{store} = 0;
		$instr &= ~D_WR;
		printf "STORE DONE: MEM[0x\%x] = MBR = 0x\%x\n",$self->{mar},$self->{mbr} if $verbose;
	}
	
	# Warn if there's an aborted load or store
	if ($self->{load}) {
		printf "ABORTED LOAD: MBR = MEM[0x\%x] = 0x\%x\n",$self->{mar},$self->{mbr} if $verbose;
		$self->{load} = 0;
	}
	if ($self->{store}) {
		printf "ABORTED STORE: MEM[0x\%x] = MBR = 0x\%x\n",$self->{mar},$self->{mbr} if $verbose;
		$self->{store} = 0;
	}
	
	# Begin a memory load or store if desired
	if ($instr & D_RD) {
		$self->{load}	= 1;
		printf "LOAD START: MBR = MEM[0x\%x] = 0x\%x\n",$self->{mar},$self->{mbr} if $verbose;
	}
	if ($instr & D_WR) {
		$self->{store}	= 1;
		printf "STORE START: MEM[0x\%x] = MBR = 0x\%x\n",$self->{mar},$self->{mbr} if $verbose;
	}
	
	# Perform a jump instruction if wanted
	# EXT: Unconditional jumping to the current instruction constitutes a CPU halt
	my $oldaddr = $self->{addr}-1;
	if ((($instr & D_COND)>>29) == 1 && $n_flag) {
		$self->{addr} = $instr & D_ADDR;
		printf "JUMPING TO 0x\%x\n",$self->{addr} if $verbose;
	}
	if ((($instr & D_COND)>>29) == 2 && $z_flag) {
		$self->{addr} = $instr & D_ADDR;
		printf "JUMPING TO 0x\%x\n",$self->{addr} if $verbose;
	}
	if ((($instr & D_COND)>>29) == 3) {
		$self->{addr} = $instr & D_ADDR;
		if ($self->{addr} == $oldaddr) {
			$self->{halt} = 1;
			printf "HALTING!!!\n";
		} else {
			printf "JUMPING TO 0x\%x\n",$self->{addr} if $verbose;
		}
	}
	
	return $self;
}

sub run		( $   ) {
	my ($self) = (@_);
	$self->single_step until $self->{halt};
}

1;

