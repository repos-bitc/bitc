#!/bin/sh
#
# Copyright (C) 2006, Jonathan S. Shapiro.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or
# without modification, are permitted provided that the following
# conditions are met:
#
#   - Redistributions of source code must contain the above 
#     copyright notice, this list of conditions, and the following
#     disclaimer. 
#
#   - Redistributions in binary form must reproduce the above
#     copyright notice, this list of conditions, and the following
#     disclaimer in the documentation and/or other materials 
#     provided with the distribution.
#
#   - Neither the names of the copyright holders nor the names of any
#     of any contributors may be used to endorse or promote products
#     derived from this software without specific prior written
#     permission. 
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Test harness.
usage() {
    echo "General usage: testit [options] command"
    echo "Recognized options: "
    echo "  -s <int>   # expect non-zero status code <int>"
    echo "  -o <file>  # match output against content of <file>"
    echo "  -e <file>  # match error output against content of <file>"
    echo "  -i <file>  # use <file> as input"
    echo "  -m <mode>  # one of \"compile\" or \"exec\""
    echo "  -v         # verbose: show stdout/stderr on miscompare"
    echo "  -q         # quiet: issue output only on bad result"
    exit 0
}

quiet=0
verbose=0
expect_status=0
expect_out=/dev/null
expect_err=/dev/null
use_input=/dev/null

# Cannot use getopt, because options are positionally sensitive.
while true
do
	case $1 in
	-s)     expect_status=$2; shift; shift;;
	-o)     expect_out=$2; shift; shift;;
	-e)     expect_err=$2; shift; shift;;
	-m)     test_mode=$2; shift; shift;;
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

if [ -n "${expect_out}" ]
then
    if [ ! -r $expect_out ]
    then
	echo "Cannot open reference output ${expect_out}"
    fi
fi

if [ -n "${expect_err}" ]
then
    if [ ! -r $expect_err ]
    then
	echo "Cannot open reference error output ${expect_err}"
    fi
fi

if [ "${test_mode}" = "compile" ]
then
    # If we were only supposed to compile, then execution 
    # presumptively succeeded:
    exit 0
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

if [ ${good} = "yes" ]
then
    cmp -s /tmp/$$.out ${expect_out}
    if [ $? -ne 0 ]
    then
	echo "Bug: ${cmd} output does not match expectations."
        good="no"
    fi
fi

if [ ${good} = "yes" ]
then
    cmp -s /tmp/$$.err ${expect_err}
    if [ $? -ne 0 ]
    then
	echo "Bug: ${cmd} error output does not match expectations."
        good="no"
    fi
fi

if [ ${good} = "no" ]
then
    if [ ${verbose} -eq 1 ]
    then
        echo "=== EXPECTED OUTPUT ==="
        cat $expect_out
        echo "=== EXPECTED ERROR OUTPUT ==="
        cat ${expect_err}
        echo "=== GOT OUTPUT ==="
        cat /tmp/$$.out
        echo "=== GOT ERROR ===="
        cat /tmp/$$.err
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
