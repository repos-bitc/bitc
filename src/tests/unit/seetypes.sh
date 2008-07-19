#!/bin/sh
for i in `ls $@` ; do 
#  echo  "- - - - - - $i - - - - - -";
#  cat $i; 
#  echo "";
  echo $i | cut -f1 -d'.' | ../../compiler/frontend/BUILD/bitcc -I. -I../../libbitc/ --showtypes `cat -` -o .typ.o  $i ; 
  echo "";
  rm -f .typ.o
done

