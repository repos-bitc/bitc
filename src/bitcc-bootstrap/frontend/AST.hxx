#ifndef AST_HXX
#define AST_HXX




#include <string>
#include <libsherpa/LToken.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/GCPtr.hxx>


#include <stdint.h>
#include "FQName.hxx"
#include "LitValue.hxx"
#include "Environment.hxx"
#include "debug.hxx"

 

using namespace sherpa;






struct Type; 
struct TypeScheme; 
struct spStruct;
struct UocInfo;
struct Instance;
 
enum IdentType {
  id_unresolved,
  id_type,			
  id_value,
  id_constructor,		
  id_field,			
  id_exn,
  id_typeclass,
  
  id_interface,
  id_usebinding,		
};

enum primOp {
  op_equals,
  op_plus,
  op_minus,
  op_mul,
  op_div,
  op_mod   
};

 std::string identTypeToString(IdentType id);


#define ID_IS_TVAR       0x00000001u
#define ID_IS_CTOR       0x00000002u
#define ID_IS_IMPORTER   0x00000004u
#define ID_IS_PROVIDER   0x00000008u
#define AST_IS_VALPAT    0x00000010u  
#define ID_IS_GLOBAL     0x00000020u
#define ID_IS_GENSYM     0x00000040u  
#define ID_IS_REFIZED    0x00000080u  
#define LB_IS_ART        0x00000100u  
                                      
                                      
                                      

#define STRUCT_IS_CLSTR  0x00000200u  
                                      
#define DEF_IN_PRELUDE   0x00000400u
#define DEF_DECLARED     0x00000800u  
                                      
#define ID_IS_DO         0x00001000u  
				      
				      
				      
				      
				      
				      

                                      
                                      
#define ID_IN_AH_FRAME   0x00002000u  
#define ID_IN_LH_FRAME   0x00004000u  
#define IDLIST_HAS_LF    0x00008000u  
#define ARGVEC_HAS_AH    0x00010000u  
        
#define LOOP_APP         0x00020000u
#define SELF_TAIL        0x00040000u

#define LB_IS_DUMMY      0x00080000u  
  	 		  	      
#define TVAR_IS_DEF      0x00100000u  
			 	      
			 	      
#define ID_IS_PRIVATE    0x00200000u  
			 	      
			 	      
#define LB_POSTPONED     0x00800000u  
                                      
#define ID_IS_FIELD      0x01000000u
#define DEF_IS_ENTRYPT   0x02000000u  

#define LB_REC_BIND      0x04000000u  

#define DEF_IS_EXTERNAL  0x08000000u 
#define ENUM_UN          0x10000000u
#define SINGLE_LEG_UN    0x20000000u
#define CARDELLI_UN      0x40000000u

#define ID_IS_METHOD     0x80000000u 



#define ID_FOR_SWITCH    0x00000001u  
                                      
                                      
#define AST_IS_LOCATION  0x00000002u  
#define ID_IS_CAPTURED   0x00000004u  

                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
#define ID_IS_DEF        0x00000008u  
				      
#define ID_IS_USE        0x00000010u  
                                      
#define ID_IS_CLOSED     0x00000020u  
                                      
#define ID_NEEDS_HEAPIFY 0x00000040u  
				      
#define ID_IS_RECBOUND   0x00000080u  
                                      
#define ID_IS_MUTATED    0x00000100u  
#define APP_IS_VALUE     0x00000200u  
                                      
                                      
                                      
#define APP_NATIVE_FNXN  0x00000400u  
#define ID_MUT_CLOSED    0x00000800u  
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
#define PROCLAIM_IS_INTERNAL 0x00001000u 
                                      
                                      
                                      
                                      
                                      
#define DEF_IS_TRIVIAL_INIT  0x00002000u 
#define IDENT_MANGLED        0x00004000u 
                                      
                                      
                                      
#define LOCAL_NOGEN_VAR  0x00008000u  
                                      
                                      
                                      
                                      
                                      
#define SEL_FROM_UN_VAL  0x00010000u  
                                      
                                      
                                      
                                      
                                      
                                      
#define SEL_FROM_UN_TYPE 0x00020000u  
                                      
                                      
                                      
                                      
#define LB_MUST_GO       0x00020000u  
                                      
                                      
                                      
#define DUPED_BY_CLCONV  0x00040000u  
                                      
                                      
                                      
                                      
#define LBS_PROCESSED    0x00080000u  
                                      
                                      
                                      
                                      
                                      
                                      
                                      
#define LB_INSTANTIATED  0x00100000u  
                                      
                                      
#define ID_OBSERV_DEF    0x00200000u  
                                      
#define TVAR_POLY_SPECIAL 0x00400000u 
                                      
                                      
                                      
#define UNION_IS_REPR    0x00800000u  
#define FLD_IS_DISCM     0x01000000u  
#define LAM_NEEDS_TRANS  0x02000000u  
                                      
                                      
                                      
                                      



#define MASK_FLAGS_FROM_USE    (TVAR_IS_DEF | ID_IS_CTOR |	\
				DEF_IS_ENTRYPT | ID_OBSERV_DEF)
#define MASK_FLAGS2_FROM_USE   (ID_IS_CAPTURED)

struct AST;
struct envSet : public Countable{
  GCPtr<Environment<AST> > env;
  GCPtr<Environment<TypeScheme> > gamma;
  GCPtr<Environment< sherpa::CVector<GCPtr<Instance> > > > instEnv;
  
  envSet()
  {
    env = NULL;
    gamma = NULL;
    instEnv = NULL;
  }

  envSet(GCPtr<Environment<AST> > _env, GCPtr<Environment<TypeScheme> >_gamma,
	 GCPtr<Environment< sherpa::CVector<GCPtr<Instance> > > > _instEnv)
  {
    env = _env;
    gamma = _gamma;
    instEnv = _instEnv;
  }

  envSet(envSet &envs)
  {
    env = envs.env;
    gamma = envs.gamma;
    instEnv = envs.instEnv;
  }

  void
  updateKey(const std::string from, const std::string to)
  {
    env->updateKey(from, to);
    gamma->updateKey(from, to);
  }
};


enum AstType {
    at_Null,
    at_AnyGroup,
    at_ident,
    at_ifident,
    at_usesel,
    at_boolLiteral,
    at_charLiteral,
    at_intLiteral,
    at_floatLiteral,
    at_stringLiteral,
    at_start,
    at_version,
    at_module,
    at_interface,
    at_defunion,
    at_declunion,
    at_defstruct,
    at_declstruct,
    at_defrepr,
    at_declrepr,
    at_reprctrs,
    at_reprctr,
    at_reprrepr,
    at_refCat,
    at_valCat,
    at_opaqueCat,
    at_defexception,
    at_deftypeclass,
    at_tcdecls,
    at_tyfn,
    at_tcapp,
    at_method_decls,
    at_method_decl,
    at_qualType,
    at_constraints,
    at_definstance,
    at_methods,
    at_use,
    at_use_case,
    at_proclaim,
    at_define,
    at_import,
    at_provide,
    at_declares,
    at_declare,
    at_tvlist,
    at_constructors,
    at_constructor,
    at_fields,
    at_field,
    at_fill,
    at_reserved,
    at_bitfield,
    at_refType,
    at_valType,
    at_fn,
    at_primaryType,
    at_argVec,
    at_fnargVec,
    at_arrayType,
    at_vectorType,
    at_mutableType,
    at_typeapp,
    at_exceptionType,
    at_dummyType,
    at_identPattern,
    at_tqexpr,
    at_unit,
    at_suspend,
    at_makevectorL,
    at_vector,
    at_array,
    at_begin,
    at_select,
    at_array_nth,
    at_vector_nth,
    at_array_length,
    at_vector_length,
    at_lambda,
    at_apply,
    at_struct_apply,
    at_ucon_apply,
    at_if,
    at_and,
    at_or,
    at_not,
    at_cond,
    at_cond_legs,
    at_cond_leg,
    at_otherwise,
    at_setbang,
    at_deref,
    at_dup,
    at_allocREF,
    at_copyREF,
    at_mkClosure,
    at_setClosure,
    at_switchR,
    at_sw_legs,
    at_sw_leg,
    at_tryR,
    at_throw,
    at_let,
    at_letbindings,
    at_letbinding,
    at_letrec,
    at_do,
    at_dobindings,
    at_dobinding,
    at_dotest,
    at_localFrame,
    at_frameBindings,
    at_letStar,
    at_identList,
    at_container,
    at_docString,
    at_letGather,
    agt_var,
    agt_literal,
    agt_tvar,
    agt_CompilationUnit,
    agt_definition,
    agt_type_definition,
    agt_tc_definition,
    agt_value_definition,
    agt_if_definition,
    agt_category,
    agt_fielditem,
    agt_qtype,
    agt_type,
    agt_expr,
    agt_expr_or_define,
    agt_eform,
    agt_ucon,
    agt_ow,
};

enum { at_NUM_ASTTYPE = agt_ow};
class AST : public Countable { 
  bool isOneOf(AstType);
public:
  AstType        astType;
  std::string    s;
  sherpa::LexLoc loc;
  sherpa::GCPtr< sherpa::CVector<sherpa::GCPtr<AST> > > children;


 private:  
  static unsigned long long astCount;
  
 public:
  unsigned long long ID; 

  LitValue litValue;
  unsigned long litBase;
  
  IdentType identType;
  unsigned long Flags;
  unsigned long Flags2;

  unsigned printVariant;	

  GCPtr<TypeScheme> scheme;		
  GCPtr<Type> symType;		
  GCPtr<AST> symbolDef;
  NoGCPtr <UocInfo> uoc;  

  bool isDecl;                  

  bool isGlobal() { return (Flags & ID_IS_GLOBAL); }
  bool isFnxn(); 
  size_t nBits();
  bool isTopLevelForm();
  bool leadsToTopLevelForm();
  void clearTypes(); 
                     
                     
                     
                     
                     
 
  
  
  GCPtr<AST> defn;  
  GCPtr<AST> decl;  
              

  
  
  
  
  
  
  
  
  FQName fqn;                 
  std::string externalName;	

  
  std::string ifName;

  
  
  
  
  
  
  envSet envs;

  
  GCPtr< sherpa::CVector<spStruct *> > special;
  
  
  bool polyinst; 
                 
  bool reached; 
  GCPtr<AST> defForm; 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  GCPtr<AST> defbps; 
  
  GCPtr<Type> tagType;   
  size_t field_bits; 
  
  
  
  
  size_t unin_discm; 
                     
  size_t total_fill; 
                     
                     

  GCPtr<TypeScheme> stSigma; 
                       
                       
  GCPtr<AST> stCtr;          
                       

  
  GCPtr<AST> tvarLB;         
  GCPtr<AST> parentLB;       
  
  
  

  static GCPtr<AST> makeBoolLit(const sherpa::LToken &tok);
  static GCPtr<AST> makeIntLit(const sherpa::LToken &tok);
  static GCPtr<AST> makeStringLit(const sherpa::LToken &tok);
  static GCPtr<AST> makeCharLit(const sherpa::LToken &tok);
  static GCPtr<AST> makeFloatLit(const sherpa::LToken &tok);

  
  void disown(size_t s);
  
  
  
  
  
  static GCPtr<AST> genIdent(const char *pfx = "tmp", const bool isTV = false);

  
  
  static GCPtr<AST> genSym(GCPtr<AST> lhs, 
		     const char *pfx="tmp",
		     const bool isTV = false);

  std::string atKwd() const;

  
  
  
  void getIds(std::ostream &errStream, 
	      GCPtr<sherpa::CVector<GCPtr<AST> > > ids,
	      bool getPattern = false);  
  GCPtr<Type> getType(); 
  GCPtr<const Type> getType() const;
  
  GCPtr<AST> getCtr(); 
  

  
  GCPtr<AST> Use();

  
  AST(GCPtr<AST> ast, bool shallowCopyChildren=true);

  
  GCPtr<AST> getTrueCopy();

  
  GCPtr<AST> getDCopy();

  
  void set(GCPtr<AST> ast);

  
  
  
  
  void rename(GCPtr<AST> from, std::string newName);

  std::string asString() const;
  std::string mangledString() const;

  GCPtr<AST> getID();
  bool isUnionLeg();
  bool isMethod();

  void PrettyPrint(std::ostream& out, bool decorated = false) const;

  
  void PrettyPrint(bool decorated) const;

  AST(const AstType at = at_Null);
  // for literals:
  AST(const AstType at, const sherpa::LToken& tok);
  AST(const AstType at, const sherpa::LexLoc &loc);
  AST(const AstType at, const sherpa::LexLoc &loc,
      sherpa::GCPtr<AST> child1);
  AST(const AstType at, const sherpa::LexLoc &loc,
      sherpa::GCPtr<AST> child1,
      sherpa::GCPtr<AST> child2);
  AST(const AstType at, const sherpa::LexLoc &loc,
      sherpa::GCPtr<AST> child1,
      sherpa::GCPtr<AST> child2,
      sherpa::GCPtr<AST> child3);
  AST(const AstType at, const sherpa::LexLoc &loc,
      sherpa::GCPtr<AST> child1,
      sherpa::GCPtr<AST> child2,
      sherpa::GCPtr<AST> child3,
      sherpa::GCPtr<AST> child4);
  AST(const AstType at, const sherpa::LexLoc &loc,
      sherpa::GCPtr<AST> child1,
      sherpa::GCPtr<AST> child2,
      sherpa::GCPtr<AST> child3,
      sherpa::GCPtr<AST> child4,
      sherpa::GCPtr<AST> child5);
  ~AST();

  sherpa::GCPtr<AST> & child(size_t i) const
  {
    return children->elem(i);
  }

  void addChild(sherpa::GCPtr<AST> child);
  std::string getTokenString();

  void
  addChildrenFrom(sherpa::GCPtr<AST> other)
  {
    for(size_t i = 0; i < other->children->size(); i++)
      addChild(other->child(i));
  }

  static const char *tagName(const AstType at);
  const char *astTypeName() const;
  const char *astName() const;

  bool isMemberOfType(AstType) const;
  bool isValid() const;
};


#endif /* AST_HXX */
