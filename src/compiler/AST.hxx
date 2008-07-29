#ifndef AST_HXX
#define AST_HXX




#include <string>
#include <vector>


#include <stdint.h>
#include "FQName.hxx"
#include "LitValue.hxx"
#include "Environment.hxx"
#include "debug.hxx"

#include <libsherpa/GCPtr.hxx>
#include <libsherpa/CVector.hxx>
#define AST_SMART_PTR sherpa::GCPtr
#define AST_SUPERCLASS sherpa::Countable


 






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
#define AST_IS_VALPAT    0x00000010u  
#define ID_IS_GLOBAL     0x00000020u
#define ID_IS_GENSYM     0x00000040u  
#define ID_IS_REFIZED    0x00000080u  
#define LB_IS_ART        0x00000100u  
                                      
                                      
                                      

#define STRUCT_IS_CLSTR  0x00000200u  
                                      
#define DEF_IN_PRELUDE   0x00000400u
#define DEF_DECLARED     0x00000800u  
                                      
#define ID_IS_DO         0x00001000u  
				      
				      
				      
				      
				      
				      
 
#define ID_ENV_COPY       0x00002000u 
                                      
                                      
                                      
                                      
                                      
                                      
                                      

 
#define LOOP_APP         0x00020000u
#define SELF_TAIL        0x00040000u

#define LB_IS_DUMMY      0x00080000u  
  	 		  	      
#define TVAR_IS_DEF      0x00100000u  
			 	      
			 	      
#define ID_IS_PRIVATE    0x00200000u  
			 	      
			 	      
#define ID_IS_METHOD     0x00400000u  
#define LB_POSTPONED     0x00800000u  
                                      
#define ID_IS_FIELD      0x01000000u
#define DEF_IS_ENTRYPT   0x02000000u  

#define LB_REC_BIND      0x04000000u  

#define DEF_IS_EXTERNAL  0x08000000u 
#define ENUM_UN          0x10000000u
#define SINGLE_LEG_UN    0x20000000u
#define CARDELLI_UN      0x40000000u

#define NULLABLE_UN      0x80000000u  



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
                                      
                                      
                                      
                                      
                                      
#define LB_MUST_GO       0x00010000u  
                                      
                                      
                                      
#define DUPED_BY_CLCONV  0x00020000u  
                                      
                                      
                                      
                                      
#define LBS_PROCESSED    0x00040000u  
                                      
                                      
                                      
                                      
                                      
                                      
                                      
#define LB_INSTANTIATED  0x00080000u  
                                      
                                      
#define ID_OBSERV_DEF    0x00100000u  
                                      
#define TVAR_POLY_SPECIAL 0x00200000u 
                                      
                                      
                                      
#define UNION_IS_REPR    0x00400000u  
#define FLD_IS_DISCM     0x00800000u  
#define LAM_NEEDS_TRANS  0x01000000u  
                                      
                                      
                                      
                                      
#define INNER_REF_NDX    0x02000000u  
                                      
                                      
#define ARG_BYREF        0x04000000u  



#define MASK_FLAGS_FROM_USE    (TVAR_IS_DEF | ID_IS_CTOR | ID_ENV_COPY |\
				DEF_IS_ENTRYPT | ID_OBSERV_DEF)
#define MASK_FLAGS2_FROM_USE   (ID_IS_CAPTURED)

struct AST;
struct envSet : public sherpa::Countable{
  sherpa::GCPtr<ASTEnvironment > env;
  sherpa::GCPtr<TSEnvironment > gamma;
  sherpa::GCPtr<InstEnvironment > instEnv;
  
  envSet()
  {
    env = NULL;
    gamma = NULL;
    instEnv = NULL;
  }

  envSet(sherpa::GCPtr<ASTEnvironment > _env, sherpa::GCPtr<TSEnvironment >_gamma,
	 sherpa::GCPtr<InstEnvironment > _instEnv)
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
  at_proclaim,
  at_define,
  at_recdef,
  at_importAs,
  at_provide,
  at_import,
  at_ifsel,
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
  at_byrefType,
  at_valType,
  at_fn,
  at_primaryType,
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
  at_fqCtr,
  at_sel_ctr,
  at_array_nth,
  at_vector_nth,
  at_array_length,
  at_vector_length,
  at_lambda,
  at_argVec,
  at_apply,
  at_struct_apply,
  at_ucon_apply,
  at_if,
  at_when,
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
  at_inner_ref,
  at_allocREF,
  at_copyREF,
  at_mkClosure,
  at_setClosure,
  at_switch,
  at_sw_legs,
  at_sw_leg,
  at_try,
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

enum { at_NUM_ASTTYPE = agt_ow };

#ifndef AST_SMART_PTR
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#define AST_SMART_PTR boost::shared_ptr
#endif /* AST_SMART_PTR */

#ifndef AST_LOCATION_TYPE
#include <libsherpa/LexLoc.hxx>
#define AST_LOCATION_TYPE sherpa::LexLoc
#endif /* AST_LOCATION_TYPE */

#ifndef AST_TOKEN_TYPE
#include <libsherpa/LToken.hxx>
#define AST_TOKEN_TYPE sherpa::LToken
#endif /* AST_TOKEN_TYPE */

#ifndef AST_SUPERCLASS
#define AST_SUPERCLASS boost::enable_shared_from_this<AST>
#endif /* AST_SUPERCLASS */

class AST :public AST_SUPERCLASS { 
  bool isOneOf(AstType);
public:
  AstType        astType;
  ::std::string    s;
  AST_LOCATION_TYPE loc;
  ::std::vector<AST_SMART_PTR<AST> > children;


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

  sherpa::GCPtr<TypeScheme> scheme;		
  sherpa::GCPtr<Type> symType;		
  sherpa::GCPtr<AST> symbolDef;
  sherpa::NoGCPtr<UocInfo> uoc;  

  bool isDecl;                  

  bool isGlobal() { return (Flags & ID_IS_GLOBAL); }
  bool isFnxn(); 
  size_t nBits();
  bool isLocation();
  bool isLiteral();
  bool isTopLevelForm();
  bool leadsToTopLevelForm();
  void clearTypes(); 
                     
                     
                     
                     
                     
 
  
  
  sherpa::GCPtr<AST> defn;  
  sherpa::GCPtr<AST> decl;  
              

  
  
  
  
  
  
  
  
  FQName fqn;                 
  std::string externalName;	

  
  std::string ifName;

  
  
  
  
  
  
  envSet envs;

  
  
  

  
  sherpa::GCPtr<sherpa::CVector<spStruct *> > special;
  
  
  bool polyinst; 
                 
  bool reached; 
  sherpa::GCPtr<AST> defForm; 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  sherpa::GCPtr<AST> defbps; 
  
  sherpa::GCPtr<Type> tagType;   
  size_t field_bits; 
  
  
  
  
  size_t unin_discm; 
                     
  size_t total_fill; 
                     
                     

  sherpa::GCPtr<TypeScheme> stSigma; 
                       
                       
  sherpa::GCPtr<AST> stCtr;    
                       

  
  sherpa::GCPtr<AST> tvarLB;         
  sherpa::GCPtr<AST> parentLB;       
  
  
  

  static sherpa::GCPtr<AST> makeBoolLit(const sherpa::LToken &tok);
  static sherpa::GCPtr<AST> makeIntLit(const sherpa::LToken &tok);
  static sherpa::GCPtr<AST> makeStringLit(const sherpa::LToken &tok);
  static sherpa::GCPtr<AST> makeCharLit(const sherpa::LToken &tok);
  static sherpa::GCPtr<AST> makeFloatLit(const sherpa::LToken &tok);

  
  void disown(size_t s);
  
  
  
  
  
  static sherpa::GCPtr<AST> genIdent(const char *pfx = "tmp", const bool isTV = false);

  
  
  static sherpa::GCPtr<AST> genSym(sherpa::GCPtr<AST> lhs, 
		     const char *pfx="tmp",
		     const bool isTV = false);

  std::string atKwd() const;

  
  
  
  void getIds(std::ostream &errStream, 
	      std::vector<sherpa::GCPtr<AST> >& ids,
	      bool getPattern = false);  
  sherpa::GCPtr<Type> getType(); 
  sherpa::GCPtr<const Type> getType() const;
  
  sherpa::GCPtr<AST> getCtr(); 
  

  
  sherpa::GCPtr<AST> Use();

  
  AST(sherpa::GCPtr<AST> ast, bool shallowCopyChildren=true);

  
  sherpa::GCPtr<AST> getTrueCopy();

  
  sherpa::GCPtr<AST> getDCopy();

  
  void set(sherpa::GCPtr<AST> ast);

  
  
  
  
  void rename(sherpa::GCPtr<AST> from, std::string newName);

  std::string asString() const;
  std::string mangledString() const;

  sherpa::GCPtr<AST> getID();
  bool isUnionLeg();
  bool isMethod();

  void PrettyPrint(std::ostream& out, bool decorated = false, 
		   bool endline=true) const;

  
  void PrettyPrint(bool decorated) const;

  AST(const AstType at = at_Null);
  // for literals:
  AST(const AstType at, const AST_TOKEN_TYPE& tok);
  AST(const AstType at, const AST_LOCATION_TYPE &loc);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2,
      AST_SMART_PTR<AST> child3);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2,
      AST_SMART_PTR<AST> child3,
      AST_SMART_PTR<AST> child4);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2,
      AST_SMART_PTR<AST> child3,
      AST_SMART_PTR<AST> child4,
      AST_SMART_PTR<AST> child5);
  ~AST();

  // Helper quasi-constructors
  static inline AST_SMART_PTR<AST>
  make(const AstType at = at_Null)
  {
    AST *ast = new AST(at);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_TOKEN_TYPE& tok)
  {
    AST *ast = new AST(at, tok);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc)
  {
    AST *ast = new AST(at, loc);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       AST_SMART_PTR<AST> child1)
  {
    AST *ast = new AST(at, loc, child1);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST>& child1,
       const AST_SMART_PTR<AST>& child2)
  {
    AST *ast = new AST(at, loc, child1, child2);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST>& child1,
       const AST_SMART_PTR<AST>& child2,
       const AST_SMART_PTR<AST>& child3)
  {
    AST *ast = new AST(at, loc, child1, child2,
                       child3);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST>& child1,
       const AST_SMART_PTR<AST>& child2,
       const AST_SMART_PTR<AST>& child3,
       const AST_SMART_PTR<AST>& child4)
  {
    AST *ast = new AST(at, loc, child1, child2,
                       child3, child4);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST>& child1,
       const AST_SMART_PTR<AST>& child2,
       const AST_SMART_PTR<AST>& child3,
       const AST_SMART_PTR<AST>& child4,
       const AST_SMART_PTR<AST>& child5)
  {
    AST *ast = new AST(at, loc, child1, child2,
                       child3, child4, child5);
    return AST_SMART_PTR<AST>(ast);
  }


  AST_SMART_PTR<AST>
  child(size_t i) const
  {
    return children[i];
  }

  AST_SMART_PTR<AST>&
  child(size_t i)
  {
    return children[i];
  }

  void addChild(AST_SMART_PTR<AST> child);
  ::std::string getTokenString();

  void
  addChildrenFrom(AST_SMART_PTR<AST> other)
  {
    for(size_t i = 0; i < other->children.size(); i++)
      addChild(other->child(i));
  }

  static const char *tagName(const AstType at);
  const char *astTypeName() const;
  const char *astName() const;

  bool isMemberOfType(AstType) const;
  bool isValid() const;
};


#endif /* AST_HXX */
