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

#include "AST.hxx"
#include "Options.hxx"
#include "UocInfo.hxx"
#include "Type.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "Trail.hxx"
#include "inter-pass.hxx"
#include "TypeInferCommon.hxx"

//#define VERBOSE


bool
typeInfer(std::ostream& errStream, GCPtr<AST> ast, 
	  GCPtr<Environment<TypeScheme> > gamma,
	  GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	  GCPtr< CVector<GCPtr<Type> > >impTypes,
	  bool isVP, 
	  GCPtr<TCConstraints> tcc,
	  unsigned long uflags,
	  GCPtr<Trail> trail,
	  int mode,
	  unsigned flags);

bool
typeEqInfer(std::ostream& errStream, GCPtr<AST> ast, 
	    GCPtr<Environment<TypeScheme> > gamma,
	    GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	    GCPtr< CVector<GCPtr<Type> > >impTypes,
	    bool isVP, 
	    GCPtr<TCConstraints> tcc,
	    unsigned long uflags,
	    GCPtr<Trail> trail,
	    int mode,
	    unsigned flags);

/**************************************************************/
/*              Interface to the outside world                */
/**************************************************************/

bool
UocInfo::fe_typeCheck(std::ostream& errStream,
		      bool init, unsigned long flags)
{
  // Careful: the input flags are interface flags `uflags,'
  // and not the internal flags `flags.' 

#ifdef VERBOSE  
    errStream << "Now Processing " << ifName;
    errStream << " ast = " << ast->astTypeName();
    errStream << std::endl;
#endif
  
  GCPtr<CVector<GCPtr<Type> > > impTypes = new CVector<GCPtr<Type> >;
  GCPtr<Trail> trail = new Trail;
  bool errFree = true;

  // FIX: TEMPORARY
  if(Options::inferenceAlgorithm == inf_eq)
    flags |= TYP_NO_PRELUDE;

  if(init) {
    
    if(flags & INF_REINIT) {
      assert(gamma);      
      assert(gamma->parent);
      gamma = gamma->parent->newDefScope();

      assert(instEnv);      
      assert(instEnv->parent);
      instEnv = instEnv->parent->newDefScope();      
    }
    else {
      gamma = new Environment<TypeScheme>(this->uocName);
      instEnv = new Environment< CVector<GCPtr<Instance> > >(this->uocName);
    }
    if((flags & TYP_NO_PRELUDE) == 0)
      CHKERR(errFree, initGamma(std::cerr, gamma, instEnv, ast, flags));
    
    if(!errFree)
      return false;
  }

  switch(Options::inferenceAlgorithm) {
  case inf_hm:
    CHKERR(errFree, typeInfer(errStream, ast, gamma, instEnv, 
			      impTypes, false, 
			      new TCConstraints, flags, trail, 
			      USE_MODE, TI_NONE));
    break;
  case inf_eq:
    CHKERR(errFree, typeEqInfer(errStream, ast, gamma, instEnv, 
				impTypes, false, 
				new TCConstraints, flags, trail, 
				USE_MODE, TI_NONE));
    break;
  }
  
  CHKERR(errFree, checkImpreciseTypes(errStream, gamma, impTypes));
  
#ifdef VERBOSE  
  errStream << "________________________________________" << std::endl;
#endif

  return errFree;
}

