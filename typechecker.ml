open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s or subtyp, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)

type tenv = typ Env.t
                 

(* version modfiée de celle du squelette, verifie qu'il n'ya pas
de duplication dans les déclaration et verfie le type de valeur avec laquelle 
on initialise une variable*)
let rec add_env l tenv  est_sous_type type_expr =
  let l' = ref l in
  List.fold_left (fun env (x, t, val_init) -> 
    
      l' := List.tl !l';
      (if(List.for_all (fun (x',_,_) -> x<>x') !l') then 
        (match val_init with 
          |Some e -> 
            let te = type_expr e tenv in 
            if (est_sous_type te t) then Env.add x te env
            else type_error te t
          |None -> Env.add x t env
        )
      else
        raise (Error (Printf.sprintf "duplication dans la declaration de %s" x)))
      
  ) tenv l


let add_class_env l env = 
  List.fold_left (fun env class_def -> 
    if(Env.mem class_def.class_name env) then error "Deux classes avec le même nom"
    else
      Env.add class_def.class_name ((TClass class_def.class_name)) env
    ) env l
(**)
let get_class (l: class_def list) (class_name:string): class_def = 
  try 
    List.find (fun class_d -> class_d.class_name = class_name) l
  with 
    |Not_found -> error "Classe non definie"

(* Pour verifier qu'on a pas declaré un tableau de void*)
let rec est_arr_void t = 
  match t with 
  TVoid -> true
  |TArray t' -> est_arr_void t'
  |_ -> false

(* rend la dimesion du tableau déclaré 
- new[] int [][][] : rend 3
*)
let dimension_array t = 

  let rec aux t acc = 
    match t with 
    |TArray t' -> aux t' (acc+1)
    |_ ->  acc
  in

  aux t 1
(*
- POur recuperer le type recursif des tableaux multidimension
parceque dans la syntaxe on a que le type primtif avec new.

avec int [1][2] on recupere : TArray (TArray int) 
*)
let rec get_recTyp_from_ArrayDecl n prim_type = 

  if (n = 1) then 
    TArray prim_type
  else
    TArray (get_recTyp_from_ArrayDecl (n-1) prim_type)
(*
- Recupere le type de la nieme dimension dans le tableau
*)
let rec get_nth_dim_typ t n = 
  if (n=1) then 
    t
  else
    match t with 
    |TArray t' -> get_nth_dim_typ t' (n-1)
    |_ -> error "probleme tableau !!" (* pas censé arrivé *)
let typecheck_prog p =
  
    (* Vérifie que la liste des expressions el s'évalue de manière compatible avec le type demandé des parametres*)
  let rec for_all_params el params_types type_expr tenv  = 
    let t_exepec = ref TVoid in
    let t_act = ref TVoid in
    (*
    il faut que les el soit des sous type de params
    *)
    try
      ( if not( List.for_all2 (fun e (_,t) ->
          let t' = type_expr e tenv  in
          t_act := t';
          t_exepec := t;
          est_sous_type t' t
        ) el params_types ) then type_error !t_act !t_exepec)
    with 
    |Invalid_argument _-> error "Vous n'avez pas mit le bon nombre d'arguments"

  (* vérifie que t1 est sous type de t2  *)
  and est_sous_type t1 t2 = 
    if (t1=t2) then true
    else
      (match t1,t2 with

      TClass cn1, TClass cn2 ->
        let c1 = get_class p.classes cn1 in
        (match c1.parent with
        Some parent_name -> est_sous_type (TClass parent_name) t2
        |None -> false)

      |_-> false
      )
  in

  (*retourne la premiere methode rencontré lors de la 
    remonté de la hiéarchie des classes*)
  let rec get_method class_def f = 
      try 
        List.find (fun meth_d -> meth_d.method_name = f) class_def.methods
      with 
      |Not_found -> 
        (match class_def.parent with 
        Some cn -> get_method (get_class p.classes cn) f
        |None-> raise (Error "Methode inexistante"))
    in

  let rec get_attr class_def s = 
      try 
         List.find (fun (att, typ,_,_) -> att=s ) class_def.attributes 
      with 
      |Not_found -> 
        (match class_def.parent with 
        Some cn -> get_attr (get_class p.classes cn) s 
        |None-> raise (Error "Attribut inexistant"))
    in

  let typ_expected_bop bop = 
    match bop with 
    |Add |Sub |Mul |Div |Rem -> TInt
    |_ -> TBool
  in

  let rec  check e typ tenv  =
    let typ_e = type_expr e tenv  in
    if typ_e <> typ then type_error typ_e typ
  
  and ckeck_and_typ_bop e1 e2 typ bop tenv = 
    let t1 = type_expr e1 tenv  in
    if(t1=typ) then 
      (
        let t2 = type_expr e2 tenv  in
        if(t2=typ) then typ_expected_bop bop
        else 
        type_error t2 (typ_expected_bop bop)
      )
    else
      type_error t1 (typ_expected_bop bop)
      
  and type_expr e tenv  = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unop (uop,e) -> 
      let te = type_expr e tenv  in
      (match uop with 
          Opp ->  if (te<>TInt) then type_error te TInt else TInt
          |Not -> if (te<>TBool) then type_error te TBool else TBool
      )

    |Binop (bop, e1, e2) -> 
      (match bop with 
      |Add |Sub |Mul |Div |Rem| Lt| Le| Gt| Ge -> (ckeck_and_typ_bop e1 e2 TInt bop tenv )
      
      |And|Or -> (ckeck_and_typ_bop e1 e2 TBool bop tenv )

      |Eq|Neq ->
        let t1 = type_expr e1 tenv  in
        let t2 = type_expr e2 tenv  in

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
        for_all_params el (meth.params) type_expr tenv ;
        TClass cn
    
    | MethCall(e,s,el) ->
      (match(type_expr e tenv ) with
        |TClass classe_name -> 
          
          let meth = get_method (get_class p.classes classe_name) s in
          for_all_params el (meth.params) type_expr tenv ;
          meth.return

        |_ -> error "Appel de methode sur non-objet"
      )
    | ArrayNelts (t,le) -> 
      (**!!!!!!!  
      !!! traite le cas ou t'intialise par une liste il faut refuser
      ça quand on a deja fait un new*)

      if(est_arr_void t) then error "void n'est pas un type valide pour element de tableaux"
      else
        let dim_saisie = ref 0 in
        List.iter (fun e -> check e TInt tenv ; dim_saisie := (!dim_saisie)+1) le;
        get_recTyp_from_ArrayDecl !dim_saisie t
      
    | ArrayList l -> 
      let first_type = (match l with 
                        |[] -> error "init de tableau avec une liste vide d'elements"
                        |e::tl -> type_expr e tenv 
      ) in
      List.iter (fun e -> check e first_type tenv ) l;
      TArray first_type

  and type_mem_access m tenv = match m with
    | Var s -> 
      (try
        Env.find s tenv
      with
        |Not_found -> error "variable inexistante"
      )
    |Field(e, s) ->
      (match (type_expr e tenv ) with
        TClass class_name -> 
          let c = get_class p.classes class_name in
          let (_,t,_,_) =  get_attr c s in
          t
        |_ -> error "Acces attribut pour type qui n'est pas un objet"
      )

    | Arr (e1,le) -> 
      (match (type_expr e1 tenv ) with 
        TArray t ->
          let dim_saisie = ref 0 in
          List.iter (fun e -> check e TInt tenv ; dim_saisie := (!dim_saisie)+1) le;

          if(!dim_saisie > dimension_array t) then error "acces a une dimension innexistante de tableau"
          else 
            get_nth_dim_typ t !dim_saisie
        |_ -> error " e n'est pas un tableau dans e.[n]"
      )


  in

  let rec check_instr i ret tenv  = match i with
    | Print e -> check e TInt tenv 
    | Expr(e) -> check e TVoid tenv 
    | Return(e) -> 
      if (ret = TVoid) then error "Return pour methode type void"
      else
      let t = type_expr e tenv  in
      if t <> ret then type_error t ret

    | Set(mem_acc, e) ->
      let te = type_expr e tenv  in 
      (* On verifie les affectation à un attribut final deja initialisé*)
      let t = (match mem_acc with 
                Field(e',att) ->
                  (match (type_expr e' tenv) with
                    TClass class_name -> 
                      let c = get_class p.classes class_name in
                      let (_,t,final,init) =  get_attr c att in

                      if(final && (!init) = None) then(
                        init := Some e;
                        t
                      )
                      else if (final) then error "modification d'un attribut final " 
                      else  t
                    |_ -> error "Acces attribut pour type qui n'est pas un objet"
                  ) 
                |_ -> type_mem_access mem_acc tenv   
      ) in
      if not(est_sous_type te t) then type_error te t


    | If(e, s1, s2) -> 
      check e TBool tenv ;
      check_seq s1 ret  tenv ;
      check_seq s2 ret  tenv 
    | While(e, s) -> 
      check e TBool tenv ;
      check_seq s ret  tenv 
      
  and check_seq s ret tenv  =
    List.iter (fun i -> check_instr i ret tenv ) s
  in

  let rec check_class c tenv = 

    let tenv = Env.add "this" (TClass c.class_name) tenv in
    (* On vérifie que les attributs final non initilisés à la déclaration 
    sont bien initialisés dans le constructeur *)
    let first_method = List.hd c.methods in
    let finals_non_init_list = List.filter (fun (_,_,final,init)-> final && (!init = None) )  c.attributes in
    let len_finals = List.length finals_non_init_list in

    (if (len_finals > 0 && first_method.method_name <> "constructor") then
      error "Faite un constructeur pour initialise vos attributs finals 
      ou declarer votre constructeur en premier"
    else
      if(len_finals > 0) then check_constr_init_finals finals_non_init_list first_method.code);
        
    List.iter (fun m -> check_mdef m tenv  ) c.methods
    
  and check_mdef m tenv  =

    let params = List.map (fun (id,t) -> (id,t,None)) m.params in
    let tenv = add_env params tenv  est_sous_type type_expr in
    let tenv = add_env m.locals tenv  est_sous_type type_expr in

    check_seq m.code m.return tenv ;

    if(m.return <> TVoid && not(return_seq m.code)) then error "Manque un Return"
  
  and check_constr_init_finals l_finals code = 
    if not(List.for_all ( fun (id,_,_,_) ->
          List.exists ( fun instr ->
            match instr with 
              Set(Field(obj,att),e) -> obj=This && att=id
              |_ -> false
          ) code 
      ) l_finals) then error "attribut(s) final non initilise"
    
  
  
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

  let tenv = add_env p.globals Env.empty est_sous_type type_expr in
  let tenv = add_class_env p.classes tenv in
  List.iter (fun c_def -> check_class c_def tenv) p.classes;
  check_seq p.main TVoid tenv 
