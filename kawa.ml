(**
   Kawa : un petit langage à objets inspiré de Java
 *)

(* Types déclarés pour les attributs, pour les variables, et pour les 
   paramètres et résultats des méthodes. *)
type typ =
  | TVoid
  | TInt
  | TBool
  | TArray of typ
  | TClass of string

let rec string_array t = 
  match t with 
  |TVoid -> ""                     
  |TInt  -> "array of int"
  |TBool -> "array of int"
  |TClass cn -> Printf.sprintf "array of class %s" cn  
  |TArray t' -> "array of " ^ string_array t'

let typ_to_string = function
  | TVoid    -> "void"
  | TInt     -> "int"
  | TBool    -> "bool"
  | TArray t  -> string_array t
  | TClass c -> c

type unop  = Opp | Not
type binop = Add | Sub | Mul | Div | Rem
           | Lt  | Le  | Gt | Ge | Eq  | Neq
           | And | Or  | EqS | NeqS

(* Expressions *)
type expr =
  (* Base arithmétique *)
  | Int    of int
  | Bool   of bool
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  (* Accès à une variable ou un attribut *)
  | Get      of mem_access
  (* Objet courant *)
  | This
  (* Création d'un nouvel objet *)
  | New      of string
  | NewCstr  of string * expr list
  (* Appel de méthode *)
  | MethCall of expr * string * expr list
  (*structures*)
  (* si l'utilisateur ne saisie pas de nombre d'elt pour le tableau 
  alors un tableau à taille dynamique sera créer *)
  | ArrayNelts of typ * expr list 
  | ArrayList of expr list    

(* un type qui contient si le tableau à été init, et une liste des dimension*)
(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var   of string
  | Field of expr (* objet *) * string (* nom d'un attribut *)
  | Arr of expr * expr list (*t[1]*)

(* Instructions *)
type instr =
  (* Affichage d'un entier *)
  | Print  of expr
  (* Écriture dans une variable ou un attribut *)
  | Set    of mem_access * expr
  (* Structures de contrôle usuelles *)
  | If     of expr * seq * seq
  | While  of expr * seq
  (* Fin d'une fonction *)
  | Return of expr
  (* Expression utilisée comme instruction *)
  | Expr   of expr
  

and seq = instr list

(* Définition de méthode 

   Syntaxe : method <type de retour> <nom> (<params>) { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)
type method_def = {
    method_name: string;
    code: seq;
    params: (string * typ) list;
    locals: (string * typ * expr option) list;
    return: typ;
  }
        
(* Définition de classe 

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du 
   paramètre implicite this. *)
type class_def = {
    class_name: string;
                (* id    type   final?  val init*)
    attributes: (string * typ * bool * expr option ref)list; 
    methods: method_def list;
    parent: string option;
  }

(* Programme complet : variables globales, classes, et une séquence 
   d'instructions *)
type program = {
    classes: class_def list;
    globals: (string * typ * expr option) list;
    main: seq;
  }
