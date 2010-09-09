FGBACKCOLOR=lightgrey
FGTEXTCOLOR=black
BGBACKCOLOR=darkgrey
BGTEXTCOLOR=white

PNGSIZE=84x20
PTSIZE=14

GENPNG="convert -gravity center -size ${PNGSIZE} -font Helvetica -pointsize ${PTSIZE} -antialias"

for i in "$@"
do
  filename=`echo $i | tr '[A-Z ]' '[a-z_]'`
  fgfile="${filename}.fg.png"
  bgfile="${filename}.bg.png"
  ${GENPNG} xc:${FGBACKCOLOR} -fill ${FGTEXTCOLOR} -draw "text 0,0 '${i}'" ${fgfile}
  #convert -bordercolor blue -border 2x2 ${fgfile} ${fgfile}
  ${GENPNG} xc:${BGBACKCOLOR} -fill ${BGTEXTCOLOR} -draw "text 0,0 '${i}'" ${bgfile}
  #convert -bordercolor white -border 2x2 ${bgfile} ${bgfile}
done
