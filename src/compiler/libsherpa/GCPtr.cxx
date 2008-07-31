/*
 * Copyright (c) 2008, The EROS Group, LLC and Johns Hopkins
 * University. All rights reserved.
 * 
 * This software was developed to support the EROS secure operating
 * system project (http://www.eros-os.org). The latest version of
 * the OpenCM software can be found at http://www.opencm.org.
 * 
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials
 *    provided with the distribution.
 * 
 * 3. Neither the name of the The EROS Group, LLC nor the name of
 *    Johns Hopkins University, nor the names of its contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <map>
#include <iostream>
#include <libsherpa/GCPtr.hxx>

using namespace std;

namespace sherpa {
#ifdef GCPTR_SUPPORT_RAW
  typedef map<const void *, GCRefCounter *> ObMap;
  static ObMap obMap;

  GC_Null_tag GC_NULL;

  GCRefCounter *
  GCRefCounter::Find(const void *ob)
  {
    if (ob == NULL)
      return &GCRefCounter::NullPtrCounter;

    // check if already registered here
    ObMap::iterator itr = obMap.find(ob);
    if (itr != obMap.end()) {
      assert(itr->second);
      return itr->second;
    }

    return NULL;
  }

  void
  GCRefCounter::Register() 
  {
#ifdef GCPTR_DEBUG
    void *obEnd = (void *) (((uintptr_t)pObject) + obSize());
    std::cerr << "Register " << hex << pObject << dec
	      << ".." << hex << obEnd << dec
	      << " (" << obSize() << ")" << std::endl;
#endif

    ObMap::iterator itr = obMap.find(pObject);
    assert (itr == obMap.end());

    assert(this);
    obMap[pObject] = this;
  }

  void 
  GCRefCounter::Deregister()
  {
#ifdef GCPTR_DEBUG
    void *obEnd = (void *) (((uintptr_t)pObject) + obSize());
    std::cerr << "Deregister " << hex << pObject << dec
	      << ".." << hex << obEnd << dec
	      << " (" << obSize() << ")" << std::endl;
#endif

    ObMap::iterator itr = obMap.find(pObject);
    obMap.erase(itr);
  }
#endif

  // Static counter object for null pointers. Note that since this
  // starts with a non-zero count, it should never get deleted.
  GCRefCounter GCRefCounter::NullPtrCounter(NULL, 1, 0);
}

// Local Variables:
// mode:c++
// End:
