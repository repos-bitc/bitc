#!/usr/bin/perl

open(F, "TypeInfer.cxx");

$mode = 0;

while(!eof(F)) {    
    $l = <F>;

    if($l =~ m/\/\*--------/) {
	$mode = 1;
    }
    elsif ($l =~ m/--------\*\//) {
	$mode = 0;
	print "\n\n";
    } 
    elsif($mode) {
	print $l;
    }
}

close(F);
