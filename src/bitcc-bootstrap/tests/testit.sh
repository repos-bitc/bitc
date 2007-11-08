#!/bin/sh
#
# Copyright (C) 2006, Jonathan S. Shapiro.
#
# This file is part of the Coyotos Operating System.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2,
# or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#

# Test harness.
usage() {
    echo "General usage: testit [options] command"
    echo "Recognized options: "
    echo "  -s <int>   # expect non-zero status code <int>"
    echo "  -o <file>  # match output against content of <file>"
    echo "  -i <file>  # use <file> as input"
    echo "  -v         # verbose: show stdout/stderr on miscompare"
    echo "  -q         # quiet: issue output only on bad result"
    exit 0
}

quiet=0
verbose=0
expect_status=0
expect_out=""
use_input=""

if [ -n "${expect_out}" ]
then
    if [ ! -r $expect_out ]
    then
	echo "Cannot open reference output ${expect_out}"
    fi
fi

# Cannot use getopt, because options are positionally sensitive.
while true
do
	case $1 in
	-s)     expect_status=$2; shift; shift;;
	-o)     expect_out=$2; shift; shift;;
	-q)     quiet=1; shift;;
	-v)     verbose=1; shift;;
	-i)     use_input=$2; shift; shift;;
	--help)	usage; break;;
	-*)	usage; break;;
	*)      break;;
	esac
done

if [ $# -eq 0 ]
then
    usage
fi

if [ -n "${use_input}" ]
then
    if [ ! -r $use_input ]
    then
	echo "Cannot open reference input ${use_input}"
    fi
fi

trap "{ rm /tmp/$$.*; exit; }" EXIT
trap "{ exit; }" SEGV

cmd=$1
shift

# This is HORRIBLE. Bash insists on printing out extraneous output even
# if an offending signal has a trap handler! The following sleazy
# bullshit is needed to suppress this misbehavior.
if [ -n "${use_input}" ]
then
    (${cmd} $* > /tmp/$$.out 2>/tmp/$$.err < ${use_input}; exit $?) 2>/dev/null
    sub_status=$?
else
    (${cmd} $* > /tmp/$$.out 2>/tmp/$$.err; exit $?) 2>/dev/null
    sub_status=$?
fi

good="yes"


if [ ${sub_status} -ne ${expect_status} ]
then
    echo "Bug: ${cmd} exited with status ${sub_status}, expected ${expect_status}"
    good="no"
fi

if [ -n "${expect_out}" ]
then
    cmp -s /tmp/$$.out ${expect_out}
    if [ $? -ne 0 ]
    then
	echo "Bug: ${cmd} output does not match expectations."
        good="no"
    fi
fi

if [ ${good} = "no" ]
then
    if [ ${verbose} -eq 1 ]
    then
        echo "=== EXPECTED OUTPUT ==="
        cat $expect_out
        echo "=== GOT OUTPUT ==="
        cat /tmp/$$.out
        echo "=== GOT ERROR ===="
        cat /tmp/$$.out
        echo "=================="
    fi
fi

if [ ${good} = "no" ]
then
    echo -n "[BAD] "
    echo "${cmd} $* "
elif [ ${quiet} -eq 0 ]
then
    echo -n "[OK]  "
    echo "${cmd} $* "
fi

exit 0
