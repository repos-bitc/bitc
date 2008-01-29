#!/usr/bin/perl

open(F, "TypeInfer.cxx");

$mode = 0;

while(!eof(F)) {    
    $ool = $ol;
    $ol = $l;
    $l = <F>;

    if($l =~ m/\/\*--------/) {
	print $ool;
	$mode = 1;
    }
    elsif ($l =~ m/--------\*\//) {
	$mode = 0;
	print "\n";
    } 
    elsif($mode) {
	print $l;
    }
}

close(F);
