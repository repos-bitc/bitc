#!/bin/sh

MYNAME=$0

BLACKLIST="/dev/tty|/proc/meminfo"

while true
do
  case $1 in
  -x) shift 1; "$@"; exit $?;;
  -t) target=$2; shift 2;;
  -d) depfile=$2; shift 2;;
  -b) BLACKLIST="$BLACKLIST|$2"; shift 2;;
  *) break;;
  esac
done

# If you comment out this line, xcache will operate:
exec "$@"

cache=`echo $target | tr '/' '.'`

#echo "##Target is:   ${target}" >&2
#echo "##Dep File is: ${depfile}" >&2
#echo "##Command is:  $@" >&2

# I am relying here on the fact that md5sum will exit with
# an error code if the depgen file does not exist.
if [ -f .xcache.sums.${cache} ]
then
  md5sum --status -c .xcache.sums.${cache}
  if [ $? -eq 0 ]
  then
    #echo "##cache wins if present" >&2
    if [ -f .xcache.out.${cache} ]
    then
      echo xcache: restoring...
      cp -p .xcache.out.${cache} ${target}
      exit 0
    fi
  else
    #echo "##cache loses" >&2
    # Nuke the cache ASAP:
    rm -f .xcache.out.${cache} .xcache.sums.${cache} .xcache.files.${cache}
    echo xcache: bad sums...
  fi
else
  echo xcache: no sums...
fi

# Bother. We actually need to run the command.
# Use strace and sed to collect the list of files that the command
# actually touches as it executes.
#
# The extra invocation of depgen is to work around a limitation in
# strace: strace does not report the error code. We therefore invoke
# ourselves recursively in order to run the real command in a subshell.
# Having done that, we extract the exit code from the trace on the
# subordinate depgen, then extract the PID of the subordinate depgen,
# then filter those results out.
strace -o /tmp/$$.trc -f -F -q -eopen,waitpid ${MYNAME} -x "$@"
cp /tmp/$$.trc .xcache.trc.${cache}

# The trace file now holds most of what we need, but we had to do
# a nested shell hack to get the subject exit code reported, and
# we need to filter out the files that were used by the intermediate
# process. Fortunately this isn't so bad.
exitline=`grep waitpid /tmp/$$.trc| grep WEXITSTATUS | tail -1`
boguspid=`echo $exitline|sed 's/ .*$//'`
exitcode=`echo $exitline|sed 's/^.*== //' | sed 's/}\].*$//'`

if [ $exitcode -ne 0 ]
then
  echo Xcache: exit code was $exitcode
  exit $exitcode
fi

#echo boguspid: ${boguspid}
#echo exitcode: ${exitcode}

files=`grep -v "^${boguspid} " /tmp/$$.trc | grep open |sed '/) *= -1/d'| sed 's/^.*open("//' | sed 's/",.*//' | sort | uniq | egrep -v "(${target}|${BLACKLIST})"`

rm /tmp/$$.trc

echo "$files" > .xcache.files.${cache}

for f in $files
do
  if [ ! -f $f ]
  then
    # This dependency is something that md5sum cannot check.
    # Abandon here without caching the result.
    echo Xcache: bad dependency on $f
    exit 0
  fi
done

# All of the dependencies were cacheable, and we have generated
# output. Cache the result.

md5sum $files > .xcache.sums.${cache}
#cat .xcache.sums.${target}

echo xcache: cacheing...
cp ${target} .xcache.out.${cache}

#for f in ${alsocache}
#do
#  if [ -f ${f} ]
#  then
#    nm=`echo $f | tr '/' '.'`
#    cp ${f} .xcache.out.${nm}
#  fi
#done

exit 0
