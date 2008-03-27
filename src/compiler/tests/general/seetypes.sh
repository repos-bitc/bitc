#!/bin/sh
for i in `ls $@` ; do 
  echo  "- - - - - - $i - - - - - -";
  cat $i; 
  echo "";
  echo $i | cut -f1 -d'.' | ../../frontend/BUILD/bitcc -I. -I../../runtime/ --showtypes `cat -` -o .typ.o  $i ; 
  echo "";
  rm -f .typ.o
done

