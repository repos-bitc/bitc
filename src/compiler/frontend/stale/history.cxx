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

#if 0
/////////////////////////////////////////////////
//     Type system history                     //
/////////////////////////////////////////////////
// -------------- !!! DO  NOT DELETE !!!   ----------------- //
// This is an older version of Type Specialize. 
// It HAS IMPORTANT COMMENTS that no longer apply, but need to be 
// reviewed whenever a change is necessary.
// DO NOT DELETE.
// // The following function has the distinction of being the most
// // debugged (and most vulnerale) function.
GCPtr<Type>
Type::TypeSpecialize(CVector<GCPtr <Type> >& ftvs,
		     CVector<GCPtr <Type> >& nftvs)
{
  Type *t = getType();
  Type *theType = new Type(t->kind, t->ast);
  theType->arrlen = t->arrlen;
  theType->defAst = t->defAst;
  theType->myContainer = t->myContainer;
  GCPtr<Type> retType = theType;
  
  //std::cout << "To Specialize " << '`' << ast->s << '\''
  //	    << this->asString()  << std::endl;  
#if 0  
  // NO ARBITRARY RECURSION FOR NOW
  if((t->kind != ty_fix) && (t->mark & MARK)) {
    Type *tv = new Type(ty_tvar, t->ast);
    tv->link = t;
    GCPtr<Type> fix = new Type(ty_fix, t->ast);
    fix->components.append(new comp(tv));
    retType = fix;
    return retType;
  }  
  ...    
  t->mark |= MARK;
  ...
#endif

  if(t->sp != NULL)
    retType = t->sp;
  else {    
    t->sp = retType;

    switch(t->kind) {
    case ty_fix:
      {
	Type *tv = new Type(ty_tvar, t->ast);
	Type *sp = t->components[0]->typ->getType();
	tv->link = sp;
	retType = tv;
	break;
      }
 
    case ty_tvar:
      {
	size_t i=0;
	for(i=0; i<ftvs.size(); i++) {
	  Type *ftv = ftvs[i]->getType();
	  //assert(ftv->kind == ty_tvar);
	  if(ftv->kind == ty_tvar && t->uniqueID == ftv->uniqueID) {
	    theType->link = nftvs[i]; // ***
	    break;
	  }
	}

	// Instead of ***, was:
	// 	if(nftvs[i]->link == NULL)
	// 	  nftvs[i]->link = theType;     
	// 	else
	// 	  retType = nftvs[i]->link;


	// If the variable was NOT in ftv list, then 
	// we should link it to the original, in order to honor
	// variable capture
	if(i == ftvs.size())
	  theType->link = t;
   
	// THE FOLLOWING IS WRONG !!! 
	// This will not honor variable capture
	// if(theType->link == NULL) {
	//   GCPtr<Type> mytype = theType;
	//   ftvs.append(t); 
	//   nftvs.append(mytype);       
	// }
	break;
      }

    default:
      {      
	/* Deal with Type-args */
	for(size_t i=0; i<t->typeArgs.size(); i++) {
	  Type *arg = t->typeArgs[i]->getType();
	  Type *newArg = new Type(ty_tvar, arg->ast);

	  if(arg->kind != ty_tvar) {
	    newArg->link = arg;
	  }
	  else {
	    for(size_t j=0; j<ftvs.size(); j++) {
	      Type *ftv = ftvs[j]->getType();
	      //assert(ftv->kind == ty_tvar);
	      if(ftv->kind == ty_tvar  && arg->uniqueID == ftv->uniqueID) {
		newArg->link = nftvs[j];    
		break;
	      }
	    }
 
	    // 	std::cout << "arg = " << arg->asString() 
	    // 		  << " ftvs = ";
	    // 	for(size_t j=0; j<ftvs.size(); j++) 
	    // 	  std::cout << " " << ftvs[j]->asString();	  
	    // 	std::cout << std::endl;
	    // The HISTORICAL assert.
	    // assert(newArg->link != NULL);
	    // This has been removed because, if I have:
	    // (define c12  (lambda (clArg:(list 'd)) clArg))
	    // when time comes to specialize 
	    // 	To Specialize `clArg'(list 'a101)
	    // 	arg = 'a101 ftvs = 
	    // clArg is in the environment, but has NOT been 
	    // generalized YET.
	    // So, in order to honor variable capture, we must do:
	    if(newArg->link == NULL)
	      newArg->link = arg;	  
	  } 
	  theType->typeArgs.append(newArg);
	}
   
   
	/* Deal with Components */
	for(size_t i=0; i<t->components.size(); i++) {
	  comp *nComp = 
	    new comp(t->components[i]->name,
		     t->components[i]->typ->TypeSpecialize(ftvs, nftvs));
	  theType->components.append(nComp);
	}
	break;
      }      
    }
    t->sp =  NULL;
  }
  //   std::cout << "\t Specialized " << '`' << ast->s << '\''
  //     	    << *this << " to " << *retType
  //     	    << std::endl;
 
  return retType;
}
 
Old Super constraint Handling:
    case at_super:
      {	
	for(size_t d = 0; d < tcdecl->children.size(); d++) {
	  AST *tcApp = tcdecl->children[d];
	  TYPEINFER(tcApp, gamma, instEnv, impTypes, isVP, 
		    sigma->tcc, uflags, trail, USE_MODE, 
		    TI_TYP_EXP | TI_TYP_APP | TI_TCC_SUB);
	  
	  if(!errFree)
	    break;
		  
	  AST *superIdent = tcApp->children[0];
	  AST *superAST = superIdent->symbolDef;	

	  // Make sure that the superclass declarations are
	  // acyclic (form a ADG)
	  if(!superDAG(superAST, ident)) {
	    errStream << tcdecl->loc << ": "
		      << "super-class declarations must form a DAG,"
		      << " but this declaration forms a loop"
		      << std::endl;
	    errFree = false;
	  }
	
	  if(!errFree)
	    break;

	  Typeclass *sup = superIdent->symType->getType();	
	  sup->flags |= TY_CT_SUBSUMED;
	  sigma->tcc->addPred(sup);
	}
	break;
      }

 
// VERBOSE VERSION OF COLLECTALLFTVSWRTGAMMA
static void
collectftvsWrtGamma(Type *typ,
		    CVector<Type *>& tvs,
		    const Environment<TypeScheme> *gamma,
		    std::string pad = "")
{   
  Type *t = typ->getType();

  if(t->mark & MARK2)
    return;

  t->mark |= MARK2;

  std::string myPad = pad;
  pad += "  ";
  //std::cout << myPad <<"Marked  " << t->asString() << std::endl;

  if(t->kind == ty_tvar) {
    assert(t->components.size() == 0);
    if(!boundInGamma(t, gamma) && !(tvs.contains(t))) 
      tvs.append(t);
  }
  else {
    for(size_t i=0; i < t->components.size(); i++) {
      if(!(t->mark & MARK2)) {
	std::cerr << myPad << t->asString() << ": comp = " << i 
		  << " MARK IS CLEAR" 
		  << std::endl;
	assert(false);
      }
      collectftvsWrtGamma(t->components[i]->typ, tvs, gamma, pad);
    }

    for(size_t i=0; i < t->typeArgs.size(); i++) {
      if(!(t->mark & MARK2)) {
	std::cerr << myPad << t->asString() << ": arg = " << i 
		  << " MARK IS CLEAR" 
		  << std::endl;
	assert(false);
      }
      collectftvsWrtGamma(t->typeArgs[i], tvs, gamma, pad);
    }

    if(t->fnDeps)
      for(size_t i=0; i < t->fnDeps->size(); i++) {
	if(!(t->mark & MARK2)) {
	  std::cerr << myPad << t->asString() << ": fnDep = " << i 
		    << " MARK IS CLEAR" 
		    << std::endl;
	  assert(false);
	}
	collectftvsWrtGamma(FNDEP(t)[i], tvs, gamma, pad);
      }    
  }

  //std::cout << myPad << "Cleared " << t->asString() << std::endl;
  t->mark &= ~MARK2;
}


// Old Instance checking for disjointness
  if(!mySigma->tau->equals(hisSigma->tau))
    return false;

  if(mySigma->tcc == NULL)
    return true;

  if(hisSigma->tcc == NULL)
    return true;
  
  CVector<Type *> ftvs;
  collectAllftvs(mySigma->tau, ftvs);
  
  bool uniquefound = false;
  for(size_t i=0; i < mySigma->tcc->pred.size(); i++) {
    Typeclass *myPred = mySigma->tcc->pred[i];
    
    bool found = false;
    for(size_t j=0; j < hisSigma->tcc->pred.size(); j++) {
      Typeclass *hisPred = hisSigma->tcc->pred[j];      
      if(myPred->equals(hisPred)) {
	found = true;
	break;
      }
    }

    if(!found) {
      uniquefound = true ;
      break;
    }
  }

  if(!uniquefound)
    return true;

  uniquefound = false;
  for(size_t i=0; i < hisSigma->tcc->pred.size(); i++) {
    Typeclass *hisPred = hisSigma->tcc->pred[i];
    
    bool found = false;
    for(size_t j=0; j < mySigma->tcc->pred.size(); j++) {
      Typeclass *myPred = mySigma->tcc->pred[j];      
      if(hisPred->equals(myPred)) {
	found = true;
	break;
      }
    }

    if(!found) {
      uniquefound = true;
      break;
    }
  }

  if(!uniquefound)
    return true;
  else
    return false;
  
// Old Additional FnDep checking while solving predicates.

  if(!errFree)
    return false;
  
  for(size_t i=0; i < tcc->pred.size(); i++) {
    Typeclass *pred1 = tcc->pred[i];
    
    for(size_t j=i+1; j < tcc->pred.size(); j++) {
      Typeclass *pred2 = tcc->pred[i];
    
      if(pred1->defAst != pred2->defAst)
	break;

      if((pred1->fnDeps != NULL) && (pred2->fnDeps != NULL)) {
	assert(pred1->fnDeps->size() == pred2->fnDeps->size());\

	for(size_t fd = 0; fd < pred1->fnDeps->size(); fd++) {
	  Type *fnDep1 = FNDEP(pred1)[fd];
	  Type *fnDep2 = FNDEP(pred2)[fd];
	  
	  assert(fnDep1->defAst == fnDep2->defAst);

	  Type *fnDep1domain = fnDep1->components[0]->typ->getType();
	  Type *fnDep2domain = fnDep2->components[0]->typ->getType();

	  if(fnDep1domain->equals(fnDep2domain)) {
	    sherpa::CVector<Type *> trail;
	    AST *errAst = new AST(at_Null, errLoc);
	    CHKERR(errFree, unify(errStream, trail, NULL, errAst, 
				  fnDep1, fnDep2, 0));
	    if(!errFree) {
	      errStream << errLoc << ": "
			<< "The following leads to a contradiction:\n"
			<< pred1->asString() << "\n"
			<< pred2->asString() << "\n"
			<< fnDep1->asString() << "\n"
			<< fnDep2->asString() 
			<< std::endl;
	      errFree = false;
	      break;
	    }
	  }
	}	
      }
    }    
  }


bool 
Typeclass::TCCspecialized)
{
  if(link)
    return getType()->TCCspecialized();
  
  CVector<Type *> closure;
  CVector<Type *> tvs;

  for(size_t i=0; i < typeArgs.size(); i++) {
    Type *arg = typeArgs[i]->getType();
    if(!arg->isTvar())
      closure.append(arg);
    else
      tvs.append(arg);
  }

  if(fnDeps)
    close(closure, *fnDeps);

  for(size_t i=0; i < tvs.size(); i++)
    if(!closure.contains(tvs[i]))
      return false;

  return true;
}


///////////////////////////////////////////////////////////////////////


// New Polyinstantiator: Old FQN lookup scheme
{
  AST *mod = uoc->ast->children[0];
  for(size_t c = 0; c < mod->children.size(); c++) {
    AST *form = mod->children[c];
    
    if(form->astType != at_define && form->astType != at_proclaim)
    continue;
    
    AST *ident = form->getID();
    
    if (ident->fqn.asString() == fqn) {
      if (ident->defn)
	ident = ident->defn;
      
      AST *defForm = ident->defForm;
	
      return defForm;
    }
  }
}



///////////////////// Clconv /////////////////////////
      /* Need to emit:
	 (let ((_tmp  (ALLOC-CLOSURE closureEnv Fn)))
	 (SET!-CLOSURE (clenvName fv1 ... fvn)  lamName)
	 _tmp) */
      
      

      AST *clTmp = AST::genSym(ast, "CLS");
      AST *clPat = new AST(at_identPattern, clTmp->loc, clTmp);
      AST *fnType = ast->symType->asAST(ast->loc);
      assert(fnType->children.size() == 2);
      fnType->children[0] = cl_convert_ast(fnType->children[0],
					   outAsts, shouldHoist);
      fnType->children[1] = cl_convert_ast(fnType->children[1],
					   outAsts, shouldHoist);
      
      //AST *fnType = cl_convert_ast(ast->symType->asAST(ast->loc),
      //			       outAsts, shouldHoist);
      AST *allocClosure = new AST(at_allocClosure, ast->loc,
				  fnType);
      AST *clLb = new AST(at_letbinding, clPat->loc, 
			  clPat, allocClosure);
      AST *clLbs = new AST(at_letbindings, clLb->loc, clLb);
      
      AST *envApp = new AST(at_struct_apply, ast->loc);
      envApp->addChild(clenvName->Use());
      
      if(freeVars.size() > 0)
	for (size_t fv = 0; fv < freeVars.size(); fv++)
	  envApp->addChild(freeVars[fv]->Use());	    
      else
	envApp->addChild(new AST(at_unit, envApp->loc));
      
      AST *setClosure = new AST(at_set_closure, ast->loc, 
				clTmp->Use(),
				envApp, lamName->Use());
      AST *begin = new AST(at_begin, ast->loc, setClosure,
			   clTmp->Use());
      AST *clLet = new AST(at_let, ast->loc, clLbs, begin,
			   new AST(at_constraints, ast->loc)); 
      ast = clLet;
      SHAPDEBUG ast->PrettyPrint(ast);   

      if(ast->children[0]->astType == at_identPattern &&
	 (ast->children[1]->astType == at_lambda))
	hoistChildren = HOISTALL;    

	  if (ast->symbolDef->Flags2 & ID_IS_RECBOUND) {
	    ast->Flags2 |= (ID_NEEDS_HEAPIFY|ID_IS_CLOSED);
	    ast->symbolDef->Flags2 |= (ID_NEEDS_HEAPIFY|ID_IS_CAPTURED);
	  }
      

//////////////////// Symtab.cxx ///////////////////////////
  for(size_t i=0; i < UocInfo::ifList.size(); i++) {
    if(&*((UocInfo::ifList)[i]) == this) 
      std::cout << "My IF no. is " << i << std::endl; 
  }

  for(size_t i=0; i < UocInfo::srcList.size(); i++) {
    if(&*((UocInfo::srcList)[i]) == this) 
      std::cout << "My SRC no. is " << i << std::endl; 
  }



  std::cout << "SIZE of Bindings = " 
	    << "[" << &*env << "] "
	    << env->bindings.size()
	    << std::endl;
  for (size_t i = 0; i < env->bindings.size(); i++) {
    std::cout << "Binding: " 
	      << env->bindings[i]->nm
	      << std::endl;
  }



////////////////// Unify.cxx ////////////////////////

/* To handle the special case of Literals in bitfields*/
      
if(t1->Isize == 0) {
  t1->Isize = t2->Isize;
  break;
 }
 else if(t2->Isize == 0) {
   t2->Isize = t1->Isize;
   break;
 }

////////////////// gen-c.cxx /////////////////////////
      Old Character Printing:
      std::streamsize w = out.ostrm.width();
      char fillChar = out.ostrm.fill('0');
      
      out.ostrm << right;
      out.ostrm << oct;

      out << "'\\";
      out.ostrm.width(3);
      out << ast->litValue.c;
      out << "'";

      out.ostrm << dec;
      out.ostrm << left;
      out.ostrm.width(w);
      out.ostrm.fill(fillChar);

/////////////// BitC-pp.cxx ///////////////////////////

      GCPtr<AST> bindings = ast->children[0];
      GCPtr<AST> letbinding = bindings->children[0];
      GCPtr<AST> ident = letbinding->children[0]->children[0];
      GCPtr<AST> binds = letbinding->children[1]->children[0];
      GCPtr<AST> exprs = ast->children[1]->children[1];
      GCPtr<AST> body = letbinding->children[1]->children[1];
      out << "(" << ast->atKwd() << " ";
      BitcP(out, ident, showTypes);
      out << " ";

      assert(binds->children.size() == exprs->children.size());      
      out << "(";
      for(size_t i=0; i<binds->children.size(); i++) {
	out << "(";
	BitcP(out, binds->children[i], showTypes);
	out << " ";
	BitcP(out, exprs->children[i], showTypes);
	out << ") ";
      }
      out << ")";

      BitcP(out, body, showTypes);

      out << ")";

   Old Structure Union Type Printing:

      if (ast->astType == at_defunion ||
	  ast->astType == at_declunion) 
	out << "(union ";
      else
	out << "(struct ";

      // FIX: for debugging purposes, this should be showing the 
      // post-inference type in some cases
      if(ast->children[1]->children.size() > 0) {
	out << "(";
	BitcP(out, ast->children[0], false);
	out << " ";
	BitcP(out, ast->children[1], false);
	out << ")";
      }
      else {
	BitcP(out, ast->children[0], false);
      }

      BitcP(out, ast->children[2], false);

      if (ast->children.size() > 4) {
	out << " ";
	BitcP(out, ast->children[4], false);
      }

      out << ")";
      break;

  Old exception Type Printing:
    if(ast->children.size() > 1) {
	out << "(";
	BitcP(out, ast->children[0], false);
	out << " ";

	for(size_t i=1; i<ast->children.size(); i++) {
	  out << " ";
	  BitcP(out, ast->children[i], false);
	}
	out << ")";
      }
      else {
	BitcP(out, ast->children[0], false);
      }

////////////////// inter-pass.cxx /////////////////////////

void
UocremEnv(std::ostream& errStream, AST *ast,
	  UocInfo *uoc)
{
  switch(ast->astType) {
  case at_ident: //declValue only produces idents
    uoc->env->removeBinding(ast->s);
    uoc->gamma->removeBinding(ast->s);
    break;
    
  case at_identPattern:
    uoc->env->removeBinding(ast->children[0]->s);
    uoc->gamma->removeBinding(ast->children[0]->s);
    break;

  default:
    errStream << ast->loc << ": "
	      << "Internal Compiler Error." 
	      << "Unexpected Binding Pattern type " 
	      << ast->astTypeName()
	      << " obtained by remEnv() routine."
	      << std::endl;
    break;
  }
}

#endif
