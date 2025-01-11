%{

  open Lexing
  open Kawa

    (* transforme la liste de listes capturée par list(var_decl) en (string,types)list *)
  let lDeL2L l = 
    (* utilisation plus optimisé de @ parceque on part de la droite *)
    List.fold_right (@) l []

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT
%token MINUS PLUS MUL DIV MOD
%token NEG EQUAL NEQ LT LE GT GE AND OR TRUE FALSE
%token ASSIGN PRINT VAR ATTRIBUTE METHOD CLASS EXTENDS NEW THIS IF ELSE
%token WHILE RETURN TINT TBOOL TVOID LBRACK RBRACK FINAL STATIC
%token EOF

%left OR
%left AND
%left EQUAL NEQ
%nonassoc LT LE GT GE
%left PLUS MINUS
%left MUL DIV MOD
%nonassoc OPP 
%left DOT

%start program
%type <Kawa.program> program

%%

program:
| globals=list(var_decl) classes=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    {let globals = lDeL2L globals in
     {classes; globals; main} 
     }
; 

class_def:
| CLASS id=IDENT pr=parent BEGIN 
    atts=list(att_decl)
    meths=list(method_def)
  END 
  {
    let atts = lDeL2L atts in
    {
      class_name = id;
      attributes = atts;
      methods = meths;
      parent = pr;
    }
  }
;

parent:
|                   { None }
|  EXTENDS id=IDENT { Some id }
;

att_decl:
|  ATTRIBUTE t=types id=IDENT l_decl=att_decl_multiple SEMI 
  {
    List.map (fun (id,init) -> (id, t, false, init))  ((id, ref None)::l_decl)
  }
|  ATTRIBUTE t=types id=IDENT ASSIGN e=expression l_decl=att_decl_multiple SEMI 
  {
    List.map (fun (id,init) -> (id, t, false, init))  ((id, ref (Some e))::l_decl)
  }
|  ATTRIBUTE FINAL t=types id=IDENT l_decl=att_decl_multiple SEMI 
  {
    List.map (fun (id,init) -> (id, t, true,init))  ((id,ref None)::l_decl)
  }
|  ATTRIBUTE FINAL t=types id=IDENT ASSIGN e=expression l_decl=att_decl_multiple SEMI 
  {
    List.map (fun (id,init) -> (id, t, true,init))  ((id,ref (Some e))::l_decl)
  }
;

att_decl_multiple:
|                                                               { [] }
| COMMA id=IDENT l_decl=att_decl_multiple                       { (id,ref None)::l_decl }
| COMMA id=IDENT ASSIGN e=expression l_decl=att_decl_multiple   { (id,ref (Some e))::l_decl }
;

method_def:
| METHOD t=types id=IDENT LPAR params=params RPAR BEGIN
  locals=list(var_decl)
  code=list(instruction)
 END 
 {
  let locals = lDeL2L locals in
  {
    method_name = id;
    code = code;
    params = params;
    locals = locals;
    return = t;
  }
 }
;


var_decl:
| VAR t=types id=IDENT l_decl=var_decl_multiple SEMI 
{
  List.map (fun (id,init) -> (id,t,init))  ((id,None)::l_decl)
}
| VAR t=types id=IDENT ASSIGN e=expression l_decl=var_decl_multiple SEMI 
{
  List.map (fun (id,init) -> (id,t,init))  ((id,Some e)::l_decl)
}
;

var_decl_multiple:
|                                          { [] }
| COMMA id=IDENT l_decl=var_decl_multiple  { (id,None)::l_decl }
| COMMA id=IDENT ASSIGN e=expression l_decl=var_decl_multiple  { (id,Some e)::l_decl }
;

params:
| t=types id=IDENT COMMA p=params { (id,t)::p }
| t=types id=IDENT                { [(id,t)] }
|                                 { [] }
;

types_prim:
|TVOID                       { TVoid }
|TINT                        { TInt }
|TBOOL                       { TBool }
|id=IDENT                    { TClass id }
;

types:
|t=types_prim                { t }
|t=types LBRACK  RBRACK      { TArray t}
;

instruction:
|PRINT LPAR e=expression RPAR SEMI                    { Print(e) }
|mem_acc=mem ASSIGN e=expression SEMI                 { Set (mem_acc, e) }
|IF LPAR e=expression RPAR BEGIN s1=seq_instr END 
                      ELSE BEGIN s2=seq_instr END     { If(e, s1, s2) }
|WHILE LPAR e=expression RPAR BEGIN s=seq_instr END   { While(e, s) }
|RETURN e=expression SEMI                             { Return(e) }
|e=expression SEMI                                    { Expr(e) }
;

seq_instr:
|i=instruction s=seq_instr  { i::s }
|                           { [] }

mem:  
|id=IDENT                                                         { Var(id) }
|e=expression DOT id=IDENT                                        { Field(e, id) }
// pour regler le conflit avec reduce|reduce avec : NEW id=IDENT, on rajoute le DOT
|e1=expression DOT LBRACK e=expression RBRACK le=list(seq_index)  { Arr (e1,e::le) }

expression:
| n=INT                                           { Int(n) }
| m=mem                                           { Get(m) }
| THIS                                            { This }
| NEW id=IDENT                                    { New(id) }
| NEW id=IDENT LPAR se=seq_expr RPAR              { NewCstr(id, se) }
| e=expression DOT id=IDENT LPAR se=seq_expr RPAR { MethCall(e, id, se)}
| FALSE                                           { Bool false }
| TRUE                                            { Bool true }
| LPAR e=expression RPAR                          { e }
| u=uop e=expression                              { Unop (u, e) } %prec OPP  
| e1=expression b=bop e2=expression               { Binop (b, e1, e2) }
// je m'inspire de java, je veut pas imposer un autre mot clé comme Array(3) 
| NEW LBRACK RBRACK t=types_prim le=list(seq_index){ ArrayNelts (t, le) } 
| LBRACK l=seq_expr RBRACK                        { ArrayList l }
;
// pour int[5]
seq_index:
LBRACK e=expression RBRACK                        { e }

seq_expr:
| e=expression COMMA se=seq_expr                  { e::se }
| e=expression                                    { [e]   }
|                                                 { []    }
;

uop:
| NEG     { Not }
| MINUS   { Opp }
;

%inline bop:
| PLUS      { Add }
| MINUS     { Sub }
| MUL       { Mul}
| DIV       { Div }
| MOD       { Rem } 
| LT        { Lt } 
| LE        { Le }
| GT        { Gt }
| GE        { Ge }
| EQUAL     { Eq }
| NEQ       { Neq }
| AND       { And }
| OR        {Or}
;