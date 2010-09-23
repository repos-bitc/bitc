##########################################################################
#
# Copyright (C) 2010, Jonathan S. Shapiro
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
#
##########################################################################

clconv1.test: EXPECT_STATUS=1
closure.test: EXPECT_STATUS=3
AppLoc.test: EXPECT_STATUS=3
newException.test: EXPECT_STATUS=1
newException.test: EXPECT_OUTPUT=newException.stdout
VecBoundsError.test: EXPECT_STATUS=1
VecBoundsError.test: EXPECT_OUTPUT=VecBoundsError.stdout
UncaughtException.test: EXPECT_STATUS=1
UncaughtException.test: EXPECT_OUTPUT=UncaughtException.stdout
wchar.test: EXPECT_OUTPUT=wchar.stdout
odd_even_letrec.test: EXPECT_STATUS=139
Echo_Args.test: EXPECT_OUTPUT=Echo_Args.stdout
echo-char.test: USE_INPUT=echo-char.stdin
echo-char.test: EXPECT_OUTPUT=echo-char.stdout
hello.test: EXPECT_OUTPUT=hello.stdout
Fact-Rec.test: EXPECT_STATUS=120
Fact-Loop.test: EXPECT_STATUS=120
Fact-Mut.test: EXPECT_STATUS=120
intout.test: EXPECT_OUTPUT=intout.stdout
io-time.test: ARGS=file.input
io-time.test: EXPECT_OUTPUT=io-time.stdout
sha1-file.test: ARGS="file.input 128028"
sha1-file.test: EXPECT_OUTPUT=sha1-file.stdout
sha1-test.test: EXPECT_OUTPUT=sha1-test.stdout 
twomodule.test: EXPECT_OUTPUT=twomodule.stdout 
sizeof.test: EXPECT_STATUS=4
bitsizeof.test: EXPECT_STATUS=1
bitset.test: EXPECT_OUTPUT=bitset.stdout
array-byref.test: EXPECT_STATUS=6
vec-badly-typed.test: TEST_MODE=compile
BUILD/vec-badly-typed: TEST_MODE=compile
BUILD/vec-badly-typed: COMPILE_ERROUTPUT=vec-badly-typed.errout
BUILD/vec-badly-typed: EXPECT_STATUS=1

badsyntax.test: TEST_MODE=compile
BUILD/badsyntax: TEST_MODE=compile
BUILD/badsyntax: COMPILE_ERROUTPUT=badsyntax.errout
BUILD/badsyntax: EXPECT_STATUS=1
BUILD/cyclic: COMPILE_ERROUTPUT=cyclic.errout

string-lex.test: TEST_MODE=compile
BUILD/string-lex: EXPECT_STATUS=1
BUILD/string-lex: COMPILE_ERROUTPUT=string-lex.errout

cyclic.test: TEST_MODE=compile
