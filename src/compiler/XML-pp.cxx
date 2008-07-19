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

#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <string>
#include <iostream>
#include "AST.hxx"
#include "backend.hxx"
#include "INOstream.hxx"
#include "Options.hxx"

using namespace std;
using namespace sherpa;

static void
XMLp(std::ostream& out, GCPtr<AST> ast, std::string pad, bool showLoc)
{
  out << pad 
      << "<"
      << AST::tagName(ast->astType)
      << " ID=\""
      << ast->ID
      << "\"";
  if (ast->s.size()) {
    out << " s=\""
	<< ast->s
	<< "\"";
  }

  if (showLoc)
    out << " loc=\"" + ast->loc.asString() + "\"";

  if(ast->astType == at_ident) {
    out << " idType=\""
        << identTypeToString(ast->identType)
        << "\"";

    out << " Sym=\"";
    if(!ast->symbolDef) {
      out << "DEF"
	  << "\"";
    }
    else {
      out << "USE"
	  << "\"";
      out << " symDef=\""
	  << ast->symbolDef->ID
	  << "\"";
    }
  }
 
  if (ast->children->size()) {
    out << ">\n";
    for(unsigned i = 0; i < ast->children->size(); i++)
      XMLp(out, ast->child(i), pad + "   ", showLoc);
    out << pad
	<< "</" 
	<< AST::tagName(ast->astType)
	<< ">\n";
  }
  else {
    out << "/>\n";
  }
}

static void
XMLd(std::ostream& out, GCPtr<AST> ast, bool showLoc)
{
  out << "<"
      << AST::tagName(ast->astType)
      << " ID=\""
      << ast->ID
      << "\"";

  if (ast->s.size()) {
    out << " s=\""
	<< ast->s
	<< "\"";
  }
  
  if (showLoc)
    out << " loc=\"" + ast->loc.asString() + "\"";

  if(ast->astType == at_ident) {

    out << " idType=\""
        << identTypeToString(ast->identType)
        << "\"";

    out << " Sym=\"";
    if(!ast->symbolDef) {
      out << "DEF"
	  << "\"";
    }
    else {
      out << "USE"
	  << "\"";
      out << "symDef=\""
	  << ast->symbolDef->ID
	  << "\"";
    }
  }

  if (ast->children->size()) {
    out << ">";
    for(unsigned i = 0; i < ast->children->size(); i++)
      XMLd(out, ast->child(i), showLoc);
    out << "</" 
	<< AST::tagName(ast->astType)
	<< ">";
  }
  else {
    out << "/>";
  }
}

bool
XMLpp(std::ostream& out, std::ostream& err, GCPtr<UocInfo> uoc)
{
  XMLp(out, uoc->uocAst, "", false);
  return true;
}

bool
XMLdump(std::ostream& out, std::ostream& err, GCPtr<UocInfo> uoc)
{
  XMLd(out, uoc->uocAst, false);
  out << std::endl;
  return true;
}



std::string
xmlMangle(std::string idName)
{
  std::stringstream ss;      
  const char *s = idName.c_str();
  
  while (*s) {
    if (*s == '<') {
      ss << "&lt;";
    }
    else if (*s == '>') {
      ss << "&gt;";
    }
    /* need a fix for !--  */
    else {
      ss << *s;
    }

    s++;
  }
  
  return ss.str();
}

static void
emitXMLType(INOstream &out, std::string name, GCPtr<TypeScheme> ts,
	    bool raw=false)
{
  TvPrinter *tvP=NULL;
  if(!raw)
    tvP = new TvPrinter;
  
  out << "<tqExpr>" << endl;
  out.more();

  out << "<id name='" << xmlMangle(name) << "'/>" << endl;
  
  if(!ts)
    out <<  "<unknown/>" << endl;
  else
    ts->asXML(tvP, out);
  
  out.less();
  out << "</tqExpr>" << endl;
}

static void 
XMLtypes(INOstream &out, GCPtr<AST> ast, bool raw=false)
{
  switch(ast->astType) {
  case at_ident:
    emitXMLType(out, ast->s, ast->scheme);
    break;    

  case at_usesel:
    XMLtypes(out, ast->child(1), raw);
    break;

  case at_interface:
  case at_module:
    {
      size_t i=0;
      out << "<btypes:TYPE>" << endl;
      out.more();
      out << "<text content='";
      if(ast->astType == at_interface) {
	i=1;
	out << "*** Interface: " << ast->child(0)->s << " ***";
      }
      else {
	out << "*** Source Module ***";
      }
      out << "'/>" << endl;
      out << "<br/>" << endl;
      for(; i<ast->children->size(); i++) {
	XMLtypes(out, ast->child(i), raw);
	out << "<br/>" << endl;
      }      
      out << "<br/>" << endl;      
      out.less();  
      out << "</btypes:TYPE>" << endl;  
      break;
    }
        
  case at_proclaim:
  case at_defexception:
  case at_deftypeclass:
  case at_defunion:    
  case at_declunion:    
  case at_defstruct:
  case at_declstruct:
  case at_define:
  case at_recdef:
  case at_identPattern:
    XMLtypes(out, ast->child(0), raw);
    break;

  case at_definstance:
    emitXMLType(out, "#Instance#", ast->scheme);
    break;    
    
  case at_declare:
  case at_import:
  case at_provide:
  case at_use:
    break;
    
  default:
    assert(false);
    break;
  }
}

void 
XML_types_PP(std::ostream &out, GCPtr<AST> ast, bool raw=false)
{
  INOstream ino(out);
  XMLtypes(ino, ast, raw);
}


bool
XMLtypesPP(std::ostream& out, std::ostream& err, GCPtr<UocInfo> uoc)
{
  XML_types_PP(out, uoc->uocAst, false);
  return true;
}

