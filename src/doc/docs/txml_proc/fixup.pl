#!/usr/bin/perl
use re 'eval';

open(F, $ARGV[0]);
$BR = qr{ \[ ( (?: (?> [^\[\]]+ )| (??{ $BR }) )* ) \] }x;
$br = qr{ \{  (?: (?> [^{}]+ )	| (??{ $br }) )* \} }x;

$sups="\\\\raise.5ex\\\\hbox\{\\\\small($br)\}";
$subs="\\\\lower.5ex\\\\hbox\{\\\\small($br)\}";
$em="\\\\ensuremath";
$EM="\\ensuremath";
$b="\\\\textbf";
$B="\\textbf";
$MIT="\\mathit";
$emem="$em($br)$em($br)";
$dummyem="$em\\{\\_\\{\\}\\}";
$derive="$em\\{\\\\vdash\\}";
$models="$em\\{\\\\models\\}";
$deriveSp="($derive)$em\\{\\_($br)\\}";
#$deriveSup="($derive)$em\\{\\^($br)\\}";
#$derive1="($derive)$em\\{\\^($br)\\_($br)\\}";
#$derive2="($derive)$em\\{\\_($br)\\^($br)\\}";
#$derive3="($derive)$em\\{\\^($br)\\}$em\\{\\_($br)\\}";
#$derive4="($derive)$em\\{\\_($br)\\}$em\\{\\^($br)\\}";

$modelSp="($models)$em\\{\\_($br)\\}";
$EqSp1="(=)$em\\{\\_(\\{$em\\{\\\\blacktriangledown\\}\\})\\}";
$EqSp2="(=)$em\\{\\_(\\{$em\\{\\\\bigtriangledown\\}\\})\\}";
$EqSp3="(=)$em\\{\\_(\\{$em\\{\\\\circ\\}\\})\\}";
$pcst="$em\\{\\\\hookrightarrow\\}";
$pcstSp="($pcst)$em\\{\\^($br)\\}";
$bigcup="\\\\textbf\\{$em\\{\\\\cup\\}\\}";
$bigcap="\\\\textbf\\{$em\\{\\\\cap\\}\\}";
$exclcup="\\\\emph\\{$em\\{\\\\cup\\}\\}";
$spSquare="\\\\emph\\{$em\\{\\\\Box\\}\\}";
$sp2Square="\\\\textbf\\{$em\\{\\\\Box\\}\\}";
$sp2Dtri="\\\\textbf\\{$em\\{\\\\bigtriangledown\\}\\}";
$emset="$em($br)";
$plural1="$emset$em\\{(\\_$br)?\\^\\{\\*\\}(\\_$br)?\\}";
$plural2="$em\\{($br)\\{(\\_$br)?\\^\\{\\*\\}(\\_$br)?\\}\\}";
$plural3="\\{($BR)\\}$em\\{\\^\\{\\*\\}\\}";
$pluralbad="$em\\{\\^\\{\\*\\}\\}";
$transR="\\{$em\\{\\\\Rightarrow\\}\\}\\*";

while(!eof(F)) {    
    $l = <F>;
    
    do {
	$ol = $l;

	#Subscript / Superscript handling
	$l =~ s/$sups$subs/$EM\{^$1_$2\}/g;
	$l =~ s/$subs$sups/$EM\{_$1^$2\}/g;
	$l =~ s/$sups/$EM\{^$1\}/g;
	$l =~ s/$subs/$EM\{_$1\}/g;
	$l =~ s/$dummyem//g;
	
	#Bold-Ops - Big-Ops
	$l =~ s/$bigcup/$EM\{\\bigcup\}/g;
	$l =~ s/$bigcap/$EM\{\\bigcap\}/g;

	#Disjoint union: Dotted union
	$l =~ s/$exclcup/$EM\{\\mathaccent\\cdot\\cup\}/g;

	#Special Box/Triangle: Dotted box/Triangle
	$l =~ s/$spSquare/$EM\{\\mathaccent\\cdot\\Box\}/g;
	$l =~ s/$sp2Square/$EM\{\\mathaccent\\circ\\Box\}/g;
	$l =~ s/$sp2Dtri/$EM\{\\mathaccent\\circ\\bigtriangledown\}/g;
	
        #{| to {\!| and |} to |\!}
	# Note that this is *not* paranthesizing replacement
	$l =~ s/\{\\\{\}\{\\textbar\}/$EM\{\\\{\\!\|\}/g;
	$l =~ s/\{\\textbar\}\{\\\}\}/$EM\{\|\\!\\\}\}/g;
	
	# \textbf{*}letter\textbf{*} to \mathbb{letter}
	$l =~ s/$b\{\*\}([A-Za-z])\\textbf\{\*\}/$EM\{\\mathbb{$1}\}/g;
	
	# \emph{*}letter\emph{*} to \mathcal{letter}
	$l =~ s/\\emph\{\*\}([A-Za-z])\\emph\{\*\}/$EM\{\\mathcal{$1}\}/g;

	# \textbf{\emph{*}}letter\textbf{\emph{*}} to \mathfrak{letter}
	$l =~ s/\\textbf\{\\emph\{\*\}\}([A-Za-z])\\textbf\{\\emph\{\*\}\}/$EM\{\\mathfrak{$1}\}/g;

	# \textbf{\emph{*}}letter\textbf{\emph{*}} to \mathfrak{letter}
	$l =~ s/\\textbf\{\\emph\{\*\}\}([A-Za-z])\\textbf\{\\emph\{\*\}\}/$EM\{\\mathfrak{$1}\}/g;

	# \textbf{\emph{\?}}text\textbf{\emph{\?}} to \ensuremath{\mathrm{text}}
	$l =~ s/\\textbf\{\\emph\{\?\}\}([A-Za-z0-9\(\)]+)\\textbf\{\\emph\{\?\}\}/$EM\{\\mathrm\{$1\}\}/g;
	# \textbf{\emph{\_}}text\textbf{\emph{\_}} to \ensuremath{\mathit{text}}
	$l =~ s/\\textbf\{\\emph\{\{\\_\}\}\}([A-Za-z0-9]+)\\textbf\{\\emph\{\{\\_\}\}\}/$EM\{\\mathit\{$1\}\}/g;

	#OK??
	#Derivation and Modelling
	$l =~ s/$deriveSp/\\mbox\{$1\\hskip -0.9ex\\raise0.25ex\\hbox\{$EM\{\_\{\_$MIT\{$2\}\}\}\}\}/g;
	$l =~ s/$modelSp/\\mbox\{$1\\hskip -1.4ex$EM\{\_\{_$MIT\{$2\}\}\}\}/g;
	#$l =~ s/$modelSp/$EM\{\\stackrel\{\\models\}\{\_\{$2\}\}\}/g;
	
	#Transitive Right execution
	$l =~ s/$transR/$EM\{\\stackrel\{\*\}\{\\Rightarrow\}\}/g;

	#Equality under another operator
	$l =~ s/$EqSp1/$EM\{\\stackrel\{\\blacktriangledown\}\{=\}\}/g;
	$l =~ s/$EqSp2/$EM\{\\stackrel\{\\triangledown\}\{=\}\}/g;
	$l =~ s/$EqSp3/$EM\{\\stackrel\{\\circ\}\{=\}\}/g;
	#$l =~ s/$EqSp1/\\mbox\{=\\hskip -1.2ex\\lower 0.1ex\\hbox\{$EM\{\_\{\_\{\\blacktriangledown\}\}\}\}\}/g;
	#$l =~ s/$EqSp2/\\mbox\{=\\hskip -1.2ex\\lower 0.3ex\\hbox\{$EM\{\_\{\{\\triangledown\}\}\}\}\}/g;
	#$l =~ s/$EqSp1/$EM\{\{=\}\\atop\{\\blacktriangledown\}\}/g;
	
	#Polymorphic constraint
	$l =~ s/$pcstSp/\\mbox\{$1\\hskip -2ex$EM\{\^\{$MIT\{$2\}\}\}\}/g;

	# Reduce the spacing after figures
	#$l =~ s/\\caption($br)\\end/\\caption$1\\vspace\{-4ex\}\\end/g;

	#Plural handling
	#FIX: Deal with parentheses
	$l =~ s/$plural1/$EM\{\\overline\{$1$2$3\}\}/g;
	$l =~ s/$plural2/$EM\{\\overline\{$1$2$3\}\}/g;
	$l =~ s/$plural3/$EM\{\\overline\{\{$1\}\}\}/g;

	# \ensuremath{} redundancy elimination
	$l =~ s/$emem/\\ensuremath{$1$2}/g;
	
	# Worst hack to fix \\ in the beginning of a line to \vspace(4pt}
	# The correct thing to do is to add a @latex.ptsz arrtibute to 
	# OSDOC's <br>
	$l =~ s/^\\\\/\\vspace\{4pt\}/g;

	# hack in some vspaces
	$l =~ s/\.\.([\-0-9a-z]+)\\\\/\\vspace\{$1\}/g;
	
	#\emph{?}Y\emph{?} -> \checkmark
	$l =~ s/\\emph\{\?\}Y\\emph\{\?\}/$EM\{\\checkmark\}/g;
	
	#Remove unnecessary copyright
	$l =~ s/\\typesetcopyright\{\}//g;
	
	#FIX until osdoc section emission is fixed for LLNCS
	$l =~ s/\\newcommand\{\\mySection\}\[1\]\{\\section\*\{#1\}\}/\\newcommand\{\\mySection\}\[1\]\{\\section\{#1\}\}/g;
	$l =~ s/\\newcommand\{\\mySubsection\}\[1\]\{\\subsection\*\{#1\}\}/\\newcommand\{\\mySubsection\}\[1\]\{\\subsection\{#1\}\}/g;
	$l =~ s/\\newcommand\{\\mySubsubsection\}\[1\]\{\\subsubsection\*\{#1\}\}/\\newcommand\{\\mySubsubsection\}\[1\]\{\\subsubsection\{#1\}\}/g;
    } while($ol ne $l);
    
    #This pass may not really be desirable
    do {
	$ol = $l;
	
	# I am not checking \ensuremath{}, careful.
	$l =~ s/_\{([a-z]+)([0-9]+)\}/\_\{$1\_\{$2\}\}/g;
	$l =~ s/\^\{([a-z]+)([0-9]+)\}/\^\{$1\_\{$2\}\}/g;
    } while($ol ne $l);

    #This pass may not really be necessary    
    do {
	$ol = $l;
	$l =~ s/$emem/\\ensuremath\{$1$2\}/g;
    } while($ol ne $l);

    if($l =~ /($pluralbad)/) {
        print STDERR "Bad plural usage: $1::$l";
    }  
    
    print $l;
}    
close(F);

