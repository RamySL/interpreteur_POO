%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT
%token MINUS PLUS MUL DIV MOD
%token NEG EQUAL NEQ LT LE GT GE AND OR TRUE FALSE
%token ASSIGN PRINT VAR ATTRIBUTE METHOD CLASS EXTENDS NEW THIS IF ELSE
%token WHILE RETURN TINT TBOOL TVOID LBRACK RBRACK
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
    { {classes; globals; main} }
;

class_def:
| CLASS id=IDENT pr=parent BEGIN 
    atts=list(att_decl)
    meths=list(method_def)
  END 
  {
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
|  ATTRIBUTE tho=types id=IDENT SEMI { id, tho }
;

method_def:
| METHOD tho=types id=IDENT LPAR params=params RPAR BEGIN
  locals=list(var_decl)
  code=list(instruction)
 END 
 {
  {
    method_name = id;
    code = code;
    params = params;
    locals = locals;
    return = tho;
  }
 }
;

var_decl:
| VAR tho=types id=IDENT SEMI     {(id, tho)}

;

params:
| t=types id=IDENT COMMA p=params { (id,t)::p }
| t=types id=IDENT                { [(id,t)] }
|                                 { [] }


types:
|TVOID                       { TVoid }
|TINT                        { TInt }
|TBOOL                       { TBool }
|t=types LBRACK  RBRACK      { TArray t}
|id=IDENT                    { TClass id }
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
|id=IDENT                                         { Var(id) }
|e=expression DOT id=IDENT                        { Field(e, id) }
|e1=expression LBRACK e2=expression RBRACK        { Arr (e1,e2) }

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
| NEW t=types LBRACK e=expression RBRACK          { ArrayNelts (t, e) }
| LBRACK l=seq_expr RBRACK                        { ArrayList l }

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