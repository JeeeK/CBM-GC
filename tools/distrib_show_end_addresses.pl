#!/usr/bin/perl
# 2020-10-28 J. E. Klasek j klasek at
#
# Generate address table with start and end for
# use in distrib/supergc.txt
#

use Data::Dumper;

my %rewritenames = ( 
	'jk-gc-basic.l:' => 'supergc-basic',
	'jk-gc-irqa.l:' => 'supergc-a000',
	'jk-gc-irqe.l:' => 'supergc-e000',
);

#print Dumper(\%rewritenames)."\n";

# how many bytes after SAVE location are used ...
$save_bytes = 6;


open(F,"grep SAVE ../jk-gc-*.l|") || die("List files missing!\n");

while(<F>) {
	m|^[./]*(\S+)\s+SAVE\s*=\s*\$([\da-f]{4})|i && do {
		my $name = $1;
		my $addr = uc($2);
		#print "DEBUG: name=$name, addr=$addr\n";
		$name =~ s|(.*)|$rewritenames{$name}|e;
		my $daddr = hex($addr);
		my $dend = $daddr + $save_bytes;
		my $end = sprintf("%04X",$dend);

		printf("        %-16s\$%-7s%-8s\$%-7s%-8s\n",$name,$addr,$daddr,$end,$dend);
	};
}
close F;


