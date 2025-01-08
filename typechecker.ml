open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

(* version modfiée de celle du squelette, verifie juste qu'il n'ya pas
de duplication dans les déclaration de plus*)
let add_env l tenv =
  let l' = ref l in
  List.fold_left (fun env (x, t) -> 
    
      l' := List.tl !l';
      (if(List.for_all (fun (x',_) -> x<>x') !l') then 
        Env.add x t env
      else
        raise (Error (Printf.sprintf "duplication dans la declaration de %s" x)))
      
  ) tenv l
    

let add_class_env l env = 
  List.fold_left (fun env class_def -> 
    if(Env.mem class_def.class_name env) then error "Deux classes avec le même nom"
    else
      Env.add class_def.class_name (TClass class_def.class_name) env
    ) env l
(**)
let get_class (l: class_def list) (class_name:string): class_def = 
  try 
    List.find (fun class_d -> class_d.class_name = class_name) l
  with 
    |Not_found -> error "Classe non definie"

(* Vérifie que la liste des expressions el s'évalue de manière compatible avec le type demandé des parametres*)
let for_all_params (el:expr list) (params_types: (string * typ) list) (type_expr:expr -> typ Env.t -> typ) (tenv:typ Env.t) : unit = 
  let t_exepec = ref TVoid in
  let t_act = ref TVoid in

  try
    ( if not( List.for_all2 (fun e (_,t) ->
        let t' = type_expr e tenv in
        t_act := t';
        t_exepec := t;
        t' = t
      ) el params_types ) then type_error !t_act !t_exepec)
  with 
  |Invalid_argument _-> error "Vous n'avez pas mit le bon nombre d'arguments"



let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let typ_expected_bop bop = 
    match bop with 
    |Add |Sub |Mul |Div |Rem -> TInt
    |_ -> TBool
  in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ
  
  and ckeck_and_typ_bop e1 e2 typ bop tenv = 
    let t1 = type_expr e1 tenv in
    if(t1=typ) then 
      (
        let t2 = type_expr e2 tenv in
        if(t2=typ) then typ_expected_bop bop
        else 
        type_error t2 (typ_expected_bop bop)
      )
    else
      type_error t1 (typ_expected_bop bop)
      
  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unop (uop,e) -> 
      let te = type_expr e tenv in
      (match uop with 
          Opp ->  if (te<>TInt) then type_error te TInt else TInt
          |Not -> if (te<>TBool) then type_error te TBool else TBool
      )

    |Binop (bop, e1, e2) -> 
      (match bop with 
      |Add |Sub |Mul |Div |Rem| Lt| Le| Gt| Ge -> (ckeck_and_typ_bop e1 e2 TInt bop tenv)
      
      |And|Or -> (ckeck_and_typ_bop e1 e2 TBool bop tenv)

      |Eq|Neq ->
        let t1 = type_expr e1 tenv in
        let t2 = type_expr e2 tenv in

        if (t1=t2) then TBool 
        else (type_error t2 t1)
        
      )
    | Get mem_acc -> type_mem_access mem_acc tenv
    | This ->
        (try 
          Env.find "this" tenv
        with 
        |Not_found -> error "Utilisation de this hors classe")
        
    |New cn -> 
      (try
        Env.find cn tenv
      with
        |Not_found -> error "Classe inexistante"
      )
    (* le constructeur doit exister*)
    | NewCstr (cn, el) ->
      let c = get_class p.classes cn in
      (* La premiere methode déclarée doit être le constructeur*) 
      let meth = (match c.methods with 
                  |m::_-> 
                    (if (m.method_name = "constructor") then m
                    else
                      error "Pas de constructeur (ou pas declare en premier)")
                  |[] -> error "Il faut au moins le constructeur "
                  ) in
      if(meth.return <> TVoid) then error "Le constructeur doit avoir void comme type de retour"
      else
        for_all_params el (meth.params) type_expr tenv;
        TClass cn
    
    | MethCall(e,s,el) ->
      (match(type_expr e tenv) with
        |TClass classe_name -> 
          (try 
            let meth = List.find (fun m -> m.method_name=s) (get_class p.classes classe_name).methods in
            for_all_params el (meth.params) type_expr tenv;
            meth.return
          with
          |Not_found-> error "Methode inexistante"
          )
        |_ -> error "Appel de methode sur non-objet"
      )


  and type_mem_access m tenv = match m with
    | Var s -> 
      (try
        Env.find s tenv
      with
        |Not_found -> error "variable inexistante"
      )
    |Field(e, s) ->
      (match (type_expr e tenv) with
        TClass class_name -> 
          let c = get_class p.classes class_name in
          (try 
            let (_,t) =  List.find (fun (att, typ) -> att=s ) c.attributes in
            t
          with
            |Not_found -> error "Attribut innexistant")
      
        |_ -> error "Acces attribut pour type qui n'est pas un objet"
      )
  in

  let rec check_instr i ret tenv = match i with
    | Print e -> check e TInt tenv
    | Expr(e) -> check e TVoid tenv
    | Return(e) -> 
      if (ret = TVoid) then error "Return pour methode type void"
      else
      let t = type_expr e tenv in
      if t <> ret then type_error t ret

    | Set(mem_acc, e) ->
      let te = type_expr e tenv in 
      let t = type_mem_access mem_acc tenv in

      (* maintenant on teste l'égalité stricte mais après faut voir
      si c'est un sous type avec l'héritage*)
      if te <> t then type_error te t
    | If(e, s1, s2) -> 
      check e TBool tenv;
      check_seq s1 ret  tenv;
      check_seq s2 ret  tenv
    | While(e, s) -> 
      check e TBool tenv;
      check_seq s ret  tenv
      
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in

  let rec check_class c tenv = 

    let tenv = add_env c.attributes tenv in
    let tenv = Env.add "this" (TClass c.class_name) tenv in
    List.iter (fun m -> check_mdef m tenv ) c.methods
    
  and check_mdef m tenv =
    let tenv = add_env m.params tenv in
    let tenv = add_env m.locals tenv in

    check_seq m.code m.return tenv;

    if(m.return <> TVoid && not(return_seq m.code)) then error "Manque un Return"
  
  
  (* pour les méthodes de type different de void on oblige le return *)
  and return_exist i = 
    match i with 
      | Print _ -> false
      | Set _ -> false
      | If (_ , s1 , s2 ) -> return_seq s1 && return_seq s2
      | While _ -> false
      | Return _ -> true
      | Expr _ -> false
  and return_seq seq = 
      match seq with 
        | [] -> false
        | i :: s -> return_exist i || return_seq s
  in

  let tenv = add_class_env p.classes tenv in
  List.iter (fun c_def -> check_class c_def tenv) p.classes;
  check_seq p.main TVoid tenv
