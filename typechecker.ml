open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s or subtyp, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)

type tenv = typ Env.t
                 
(** renvoie si [t] est TArray de void  *)
let rec est_arr_void t = 
  match t with 
  TVoid -> true
  |TArray t' -> est_arr_void t'
  |_ -> false

(** [add_env l tenv est_sous_type type_expr] ajoute les variables de [l] à l'environnement [tenv]. 
    Lève [Error] en cas de duplication de noms et [type_error] si le type d'une valeur initiale 
    ne correspond pas au type static déclaré. *)
let rec add_env l tenv est_sous_type type_expr =
  let l' = ref l in
  List.fold_left (fun env (x, t, val_init) -> 
      if(est_arr_void t) then error "void n'est pas un type valide pour element de tableaux"
      else(
      l' := List.tl !l';
      (if(List.for_all (fun (x',_,_) -> x<>x') !l') then 
        (match val_init with 
          
          |Some e -> 
            let te = type_expr e tenv in 
            if (est_sous_type te t) then Env.add x te env
            else 
              type_error te t
          |None -> Env.add x t env
        )
      else
        raise (Error (Printf.sprintf "duplication dans la declaration de %s" x))))
      
  ) tenv l

(* Ajoute les classes avec leur nom à l'environement, 
utile pour s'assurer que pas d'auutre variable ont l'id d'une classe 
idée pour static peut être *)
let add_class_env l env = 
  List.fold_left (fun env class_def -> 
    if(Env.mem class_def.class_name env) then error "Deux classes avec le meme nom,ou variable avec nom de classe"
    else
      Env.add class_def.class_name ((TClass class_def.class_name)) env
    ) env l

let get_class (l: class_def list) (class_name:string): class_def = 
  try 
    List.find (fun class_d -> class_d.class_name = class_name) l
  with 
    |Not_found -> error "Classe non definie"



(** [dimension_array t] renvoie la dimension du tableau décrit par [t]. 
    Par exemple, [dimension_array (TArray (TArray (TArray t)))] renvoie 3. *)
let dimension_array t = 

  let rec aux t acc = 
    match t with 
    |TArray t' -> aux t' (acc+1)
    |_ ->  acc
  in

  aux t 1

(** [get_recTyp_from_ArrayDecl n prim_type] renvoie le type récursif d'un tableau 
    multidimensionnel de profondeur [n] avec le type primitiif [prim_type]. 
    Par exemple, [get_recTyp_from_ArrayDecl 2 TInt] renvoie [TArray (TArray TInt)]. *)
let rec get_recTyp_from_ArrayDecl n prim_type = 

  if (n = 1) then 
    TArray prim_type
  else
    TArray (get_recTyp_from_ArrayDecl (n-1) prim_type)

(** [get_nth_dim_typ t n] renvoie le type de la [n]-ième dimension du tableau [t], 
    Précodnition : [t] est un [TArray] de dimension au moins [n]. *)
let rec get_nth_dim_typ t n = 
  if (n=1) then 
    t
  else
    match t with 
    |TArray t' -> get_nth_dim_typ t' (n-1)
    |_ -> error "probleme tableau !!" (* pas censé arrivé *)
let typecheck_prog p =
  
  (** [for_all_params el params_types type_expr tenv] vérifie que chaque expression de [el] 
    est un sous-type du type attendu dans [params_types]. 
    Lève [type_error] en cas d'incompatibilité ou une erreur si les longueurs diffèrent. *)
  let rec for_all_params el params_types type_expr tenv  = 
    let t_exepec = ref TVoid in
    let t_act = ref TVoid in

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
      | TArray t1,TArray t2 -> est_sous_type t1 t2
      |_-> false
      )
  in

(** [get_method class_def f] retourne la première méthode nommée [f] trouvée en remontant 
    la hiérarchie des classes depuis [class_def], en vérifyant que la méthode est accessible. 
    Lève [Error] si la méthode est inexistante. *)
  let get_method class_def f = 
      (* recherche la méthode dans les classes parentes en vérifyant l'accessibilité*)
      let rec get_method_parents class_def =
        try 
          List.find (fun meth_d -> (meth_d.method_name = f) && (meth_d.acces = Protected)) class_def.methods
        with 
        |Not_found -> 
          (match class_def.parent with 
          Some cn -> get_method_parents (get_class p.classes cn) 
          |None-> raise (Error "Methode inexistante ou innaccessible"))
      in
      (* dans la classe de la méthode on vérifie pas l'accesibilité*)
      try 
        List.find (fun meth_d -> meth_d.method_name = f) class_def.methods
      with 
      |Not_found -> 
        (match class_def.parent with 
        Some cn -> get_method_parents (get_class p.classes cn) 
        |None-> raise (Error "Methode inexistante"))
    in
  
  (** [get_attr class_def s] retourne le premier attribut nommé [s] trouvé 
    en remontant la hiérarchie des classes depuis [class_def] en vérifyant que l'attribut est accessible.  
    Lève [Error] si l'attribut est inexistant. *)
  let get_attr class_def s = 
      let rec get_attr_parents class_def =
        try 
          List.find (fun (att, typ,_,_,acces) -> att=s && acces=Protected) class_def.attributes 
        with 
        |Not_found -> 
          (match class_def.parent with 
          Some cn -> get_attr_parents (get_class p.classes cn) 
          |None-> raise (Error "Attribut inexistant ou innaccessible "))
      in

      try 
         List.find (fun (att, typ,_,_,_) -> att=s ) class_def.attributes 
      with 
      |Not_found -> 
        (match class_def.parent with 
        Some cn -> get_attr_parents (get_class p.classes cn) 
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
  
(** [ckeck_and_typ_bop e1 e2 typ bop tenv] vérifie que [e1] et [e2] sont de type [typ] 
    et renvoie le type attendu pour l'opérateur binaire [bop] si c'est le cas. 
    Lève [type_error] en cas d'incompatibilité. *)
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
        
        if (est_sous_type t1 t2 || est_sous_type t2 t1) then TBool 
        else (type_error t2 t1)
      |EqS|NeqS ->
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
        let dim_saisie = ref 0 in
        List.iter (fun e -> check e TInt tenv ; dim_saisie := (!dim_saisie)+1) le;
        get_recTyp_from_ArrayDecl !dim_saisie t
      
    | ArrayList l -> 
      let first_type = (match l with 
                        |[] -> error "init de tableau avec une liste vide d'elements"
                        |e::tl -> type_expr e tenv 
      ) in
      List.iter (fun e -> let _ = est_sous_type (type_expr e tenv) first_type in ()) l;
      TArray first_type

    |Instanceof (e, t) -> 
      match (type_expr e tenv) with 
      |TClass _ -> 
        (match t with 
          TClass cn -> 
            if(Env.mem cn tenv) then TBool
            else
              error "instanceof : classe innexistante dans"
          |_ -> failwith "ne doit pas arrivé (assurré par le parser)"
          )
      |_ -> error "dans : e instanceof t. e doit etre un objet"

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
          let (_,t,_,_,_) =  get_attr c s in
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
                      let (_,t,final,init,_) =  get_attr c att in

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

    let finals_non_init_list = List.filter (fun (_,_,final,init,_)-> final && (!init = None) )  c.attributes in
    let len_finals = List.length finals_non_init_list in

    (if (len_finals > 0) then
      (try 
        (if ((List.hd c.methods).method_name <> "constructor") then 
            error "Faite un constructeur pour initialise vos attributs finals")
        with 
        _ ->  error "Faite un constructeur pour initialise vos attributs finals 
                ou declarer votre constructeur en premier")

    else
      if(len_finals > 0) then check_constr_init_finals finals_non_init_list (List.hd c.methods).code);
        
    List.iter (fun m -> check_mdef m tenv  ) c.methods
    
  and check_mdef m tenv  =

    let params = List.map (fun (id,t) -> (id,t,None)) m.params in
    let tenv = add_env params tenv  est_sous_type type_expr in
    let tenv = add_env m.locals tenv  est_sous_type type_expr in

    check_seq m.code m.return tenv ;

    if(m.return <> TVoid && not(return_seq m.code)) then error "Manque un Return"
  (*!!!!!!!!! Ecrit la spec*)
  and check_constr_init_finals l_finals code = 
    if not(List.for_all ( fun (id,_,_,_,_) ->
          List.exists ( fun instr ->
            match instr with 
              Set(Field(obj,att),e) -> obj=This && att=id
              |_ -> false
          ) code 
      ) l_finals) then error "attribut(s) final non initilise"
    
  
  
  (* Inspirée du cours *)
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

  let tenv = add_class_env p.classes Env.empty in
  let tenv = add_env p.globals tenv est_sous_type type_expr in
  List.iter (fun c_def -> check_class c_def tenv) p.classes;
  check_seq p.main TVoid tenv 
