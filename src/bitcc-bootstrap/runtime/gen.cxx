/**************************************************************************
 *
 * Copyright (C) 2006, Johns Hopkins University.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 *   - Redistributions of source code must contain the above 
 *     copyright notice, this list of conditions, and the following
 *     disclaimer. 
 *
 *   - Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions, and the following
 *     disclaimer in the documentation and/or other materials 
 *     provided with the distribution.
 *
 *   - Neither the names of the copyright holders nor the names of any
 *     of any contributors may be used to endorse or promote products
 *     derived from this software without specific prior written
 *     permission. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 **************************************************************************/

#include <iostream>

using namespace std;
 
int main()
{
   size_t sizes[] = {8, 16, 32, 64};
   size_t nSizes = sizeof(sizes)/sizeof(size_t);
   
   for(size_t i=0; i < nSizes; i++) {
     size_t theSize = sizes[i];
     cout << "(definstance (Integral  int" << theSize
	  << ") preludeID)" 
	  << endl;     
     cout << "(definstance (Integral uint" << theSize
	  << ") preludeID)" 
	  << endl;     
   }
   cout << endl;

   for(size_t i=0; i < nSizes; i++) {
     size_t theSize = sizes[i];
     cout << "(definstance (Integral (mutable  int" << theSize
	  << ")) preludeID)" 
	  << endl;     
     cout << "(definstance (Integral (mutable uint" << theSize
	  << ")) preludeID)" 
	  << endl;          
   }
   cout << endl;

   for(size_t i=0; i < nSizes; i++) {
     size_t theSize = sizes[i];
     for(size_t j=1; j <= theSize; j++) {
       cout << "(definstance (Integral (bitfield  int" << theSize
	    << " " << j << ")) preludeID)"
	    << endl;
       cout << "(definstance (Integral (bitfield uint" << theSize
	    << " " << j << ")) preludeID)"
	    << endl;
     }
     cout << endl;

   }
   cout << endl;

   for(size_t i=0; i < nSizes; i++) {
     size_t theSize = sizes[i];
     for(size_t j=1; j <= theSize; j++) {
       cout << "(definstance (Integral (mutable (bitfield  int" << theSize
	    << " " << j << "))) preludeID)"
	    << endl;
       cout << "(definstance (Integral (mutable (bitfield uint" << theSize
	    << " " << j << "))) preludeID)"
	    << endl;
     }
     cout << endl;
   }   
   cout << endl;
}
