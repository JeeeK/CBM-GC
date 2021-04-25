#!/usr/bin/perl
# 2021-04-25 J. E. Klasek j klasek at
#
# Generate address table with start and end for
# use in distrib/blodgc.txt
#

use Data::Dumper;

my %rewritenames = ( 
	'blod-gc.o' => 'blodgc',
);

# how many additional bytes are used ...
$save_bytes = 0;


$DIR = '..';
if ( -f '../blod-gc/blod-gc.o' ) {
	$DIR = '../blod-gc';
}


for my $F ("$DIR/blod-gc.o") {
	#my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
	#	$atime,$mtime,$ctime,$blksize,$blocks)
	#	= stat($F);
	open(F,$F);
	read(F,$prg,2);
	my $len  = (stat(F))[7] - 2;
	close F;
	@bytes = unpack("CC",$prg);
	my $daddr = $bytes[1]<<8+$bytes[0];
	my $name = $F;
	$name =~ s{.*/([^/]+)$}{$1};
	$name =~ s|(.*)|$rewritenames{$name}|e;
	$addr = sprintf("%04X",$daddr);
	#print "DEBUG: name=$name, addr=$addr\n";
	my $dend = $daddr + $len - 1 + $save_bytes;
	my $end = sprintf("%04X",$dend);
	my $hlen = sprintf("%04X",$len);

	printf("%-16s\$%-7s%-8s\$%-7s%-8s\$%-7s%-8s\n",
		$name,$addr,$daddr,$end,$dend,$hlen,$len);
}


