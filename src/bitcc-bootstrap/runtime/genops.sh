SPC=""
BITCMANGLE_U=""
BITCMANGLE_S=""
BITCMANGLE_W="_4word"
BITCMANGLE_F="_5float"
BITCMANGLE_D="_6double"

function bitcmangle_u()
{
  local bits=$1
  BITCMANGLE_U=""
  if [ ${bits} -eq 8 ]
  then
    SPC=" "
    BITCMANGLE_U="_5uint8"
    return
  elif [ ${bits} -eq 16 ]
  then
    SPC=""
    BITCMANGLE_U="_6uint16"
    return
  elif [ ${bits} -eq 32 ]
  then
    BITCMANGLE_U="_6uint32"
    return
  elif [ ${bits} -eq 64 ]
  then
    BITCMANGLE_U="_6uint64"
    return
  fi
}

function bitcmangle_s()
{
  local bits=$1
  BITCMANGLE_S=""
  if [ ${bits} -eq 8 ]
  then
    SPC=" "
    BITCMANGLE_S="_4int8"
    return
  elif [ ${bits} -eq 16 ]
  then
    SPC=""
    BITCMANGLE_S="_5int16"
    return
  elif [ ${bits} -eq 32 ]
  then
    BITCMANGLE_S="_5int32"
    return
  elif [ ${bits} -eq 64 ]
  then
    BITCMANGLE_S="_5int64"
    return
  fi
}

for i in 8 16 32 64
do
  bitcmangle_u $i
  imangle=${BITCMANGLE_U}
  ispace=${SPC}
  for k in 8 16 32 64
  do
    bitcmangle_u $k
    kmangle=${BITCMANGLE_U}
    kspace=${SPC}
    if [ $i -lt $k ]
    then
      echo "DEFUNSIGNEDOPS(bitc_uns${i}_t,${ispace}    ${imangle},${ispace}  bitc_uns${k}_t,${kspace} ${kmangle});"
    fi
    for j in 1 2 3 4 5 6 7 8 \
             9 10 11 12 13 14 15 16 \
             17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 \
             33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 \
             49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
    do
      if [ $j -lt 10 ]
      then
        jspace=" "
      else
        jspace=""
      fi
      if [ $j -lt $i -a $i -le $k ]
      then        
        echo "DEFUNSIGNEDOPS(bitc_uns${i}_t,${ispace} BF${j}${imangle},${jspace}${ispace} bitc_uns${k}_t,${kspace} ${kmangle});"
      fi
    done
  done
done

for i in 8 16 32 64
do
  bitcmangle_s $i
  imangle=${BITCMANGLE_S}
  ispace=${SPC}
  for k in 8 16 32 64
  do
    bitcmangle_s $k
    kmangle=${BITCMANGLE_S}
    kspace=${SPC}
    if [ $i -lt $k ]
    then
      echo "DEFSIGNEDOPS(bitc_int${i}_t,${ispace}    ${imangle},${ispace}  bitc_int${k}_t,${kspace} ${kmangle});"
    fi
    for j in 1 2 3 4 5 6 7 8 \
             9 10 11 12 13 14 15 16 \
             17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 \
             33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 \
             49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
    do
      if [ $j -lt 10 ]
      then
        jspace=" "
      else
        jspace=""
      fi
      if [ $j -lt $i -a $i -le $k ]
      then        
        echo "DEFSIGNEDOPS(bitc_int${i}_t,${ispace} BF${j}${imangle},${jspace}${ispace} bitc_int${k}_t,${kspace} ${kmangle});"
      fi
    done
  done
done

for i in 8 16 32 64
do
  bitcmangle_s $i
  bitcmangle_u $i
  echo "DEFRESIGN(bitc_uns${i}_t,${SPC} ${BITCMANGLE_U},${SPC} bitc_int${i}_t,${SPC} ${BITCMANGLE_S});"
done

for i in 8 16 32 64
do
  bitcmangle_s $i
  bitcmangle_u $i
  echo "DEFCAST(bitc_uns${i}_t,${SPC} ${BITCMANGLE_U},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
  echo "DEFCAST(bitc_int${i}_t,${SPC} ${BITCMANGLE_S},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
  echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_uns${i}_t, ${SPC} ${BITCMANGLE_U});"
  echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_int${i}_t,${SPC} ${BITCMANGLE_S});"
done
 
echo "DEFCAST(bitc_float_t,${SPC} ${BITCMANGLE_F},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
echo "DEFCAST(bitc_double_t,${SPC} ${BITCMANGLE_D},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_float_t, ${SPC} ${BITCMANGLE_F});"
echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_double_t,${SPC} ${BITCMANGLE_D});"

echo "/* Conversion across mutability, -- try to avoid */"
echo "/* From mutable to immutable */"
for i in 8 16 32 64
do
  bitcmangle_s $i
  bitcmangle_u $i
  echo "DEFCAST(bitc_uns${i}_t,${SPC} M${BITCMANGLE_U},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
  echo "DEFCAST(bitc_int${i}_t,${SPC} M${BITCMANGLE_S},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
  echo "DEFCAST(bitc_word_t, ${SPC} M${BITCMANGLE_W}, ${SPC} bitc_uns${i}_t, ${SPC} ${BITCMANGLE_U});"
  echo "DEFCAST(bitc_word_t, ${SPC} M${BITCMANGLE_W}, ${SPC} bitc_int${i}_t,${SPC} ${BITCMANGLE_S});"
done
 
echo "DEFCAST(bitc_float_t,${SPC} M${BITCMANGLE_F},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
echo "DEFCAST(bitc_double_t,${SPC} M${BITCMANGLE_D},${SPC} bitc_word_t, ${SPC} ${BITCMANGLE_W});"
echo "DEFCAST(bitc_word_t, ${SPC} M${BITCMANGLE_W}, ${SPC} bitc_float_t, ${SPC} ${BITCMANGLE_F});"
echo "DEFCAST(bitc_word_t, ${SPC} M${BITCMANGLE_W}, ${SPC} bitc_double_t,${SPC} ${BITCMANGLE_D});"

echo "/* From immutable to mutable -- try to avoid */"
for i in 8 16 32 64
do
  bitcmangle_s $i
  bitcmangle_u $i
  echo "DEFCAST(bitc_uns${i}_t,${SPC} ${BITCMANGLE_U},${SPC} bitc_word_t, ${SPC} M${BITCMANGLE_W});"
  echo "DEFCAST(bitc_int${i}_t,${SPC} ${BITCMANGLE_S},${SPC} bitc_word_t, ${SPC} M${BITCMANGLE_W});"
  echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_uns${i}_t, ${SPC} M${BITCMANGLE_U});"
  echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_int${i}_t,${SPC} M${BITCMANGLE_S});"
done
 
echo "DEFCAST(bitc_float_t,${SPC} ${BITCMANGLE_F},${SPC} bitc_word_t, ${SPC} M${BITCMANGLE_W});"
echo "DEFCAST(bitc_double_t,${SPC} ${BITCMANGLE_D},${SPC} bitc_word_t, ${SPC} M${BITCMANGLE_W});"
echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_float_t, ${SPC} M${BITCMANGLE_F});"
echo "DEFCAST(bitc_word_t, ${SPC} ${BITCMANGLE_W}, ${SPC} bitc_double_t,${SPC} M${BITCMANGLE_D});"
