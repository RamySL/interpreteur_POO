open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VArray of value array 
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value


(** [create_array t_prim ln] crée un tableau avec élément de la dimension n de type [t_prim] 
    avec les dimensions spécifiées dans [ln]. 
    Préconditions (vérifiées avec le typechecker) : [ln] n'est jamais vide, [t_prim] est un type valide, 
    et s'il n'y a qu'un seul élément dans [ln], alors [t_prim] est un type primitif. 
    Lève une erreur si ces préconditions ne sont pas respectées. *)
let rec create_array t_prim ln = 
  
  match ln with 
  [] -> failwith "N'est jamais censé arriver (traité au typechecker)"
  |[n] -> 
    (match t_prim with 
    TInt -> VArray (Array.make (n) (VInt 0))
    |TBool -> VArray (Array.make (n) (VBool false))
    |TClass cn -> VArray (Array.make (n) (VObj {cls=cn;fields=Hashtbl.create 0}))
    |_ -> failwith "N'est jamais censé arriver (traité au typechecker)"
    )
  |n:: tl ->
    VArray (Array.init (n) (fun _-> create_array t_prim tl))


(** [get_index li array] accede aux indices des différente dimensions un par
    un sauf le dernier indice dans la liste, et retour ce dernier indice avec le dernier accès
    Précondition : [li] n'est pas vide et [array] à aumoins List.length[li] dimensions
    Lève une erreur si cette précondition n'est pas respectée. *)
let rec get_index li array = 
  match li,array with 
  |[],_ -> failwith "N'est jamais censé arriver (traité au typechecker)"
  |[i],VArray array -> 
    if(Array.length array <= i) then (raise (Error "index out of bounds"))
    else array,i
  |i::tl, VArray array -> 
    if(Array.length array <= i) then (raise (Error "index out of bounds"))
    else
    get_index tl array.(i)
  |_-> failwith "N'est jamais censé arriver (traité au typechecker)"

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  let var_init = ref [] in
  (* Je transforme les var int = 5; à la déclaration en des instruction
  que je rajoute implicitement aux codes du main *)
  List.iter( (fun (x, _, e_option) -> 
    (match e_option with 
      None -> Hashtbl.add env x Null
      |Some e -> var_init := (Set(Var x,e))::!var_init 
      )
    )
  ) p.globals;
  
  let main_code = (List.rev !var_init) @ p.main in
  

  let get_class cn = 
    List.find (fun class_d -> class_d.class_name = cn) p.classes 
  in

(** [set_All_fields fields class_def eval] initialise tous les attributs d'un objet, 
    y compris ceux hérités, en remontant jusqu'à la classe la plus haute avant d'ajouter les attributs. 
    En cas de nom d'attributs identiques, ceux des classes les plus basses dans l'hiearchie vont 
    être prit parceque ajoutés en dernier. 
    *)
  let rec set_All_fields fields class_def eval = 
    
    let aux fields class_def = 
      
      List.iter (fun (att,_,_,init) ->
        (match !init with 
          None -> Hashtbl.add fields att Null;
          |Some e -> 
            try 
              (* ceux qui levent erreur ici vont être init
              avec les Set() ajoutés au main*)
              let ve = eval e in
              Hashtbl.add fields att ve;
            with
            _ -> Hashtbl.add fields att Null;
          )) class_def.attributes;
    in

    match class_def.parent with 
    None -> aux fields class_def
    |Some cn ->
      let c = get_class cn in
      set_All_fields fields c eval;
      aux fields class_def
  in

  
(** [get_method class_def f] retourne la première méthode nommée [f] rencontrée 
    en remontant la hiérarchie des classes depuis [class_def]. 
    Lève une erreur si aucune méthode n'est trouvée. *)
  let rec get_method class_def f = 
    try 
      List.find (fun meth_d -> meth_d.method_name = f) class_def.methods
    with 
    |Not_found -> 
      (match class_def.parent with 
      Some cn -> get_method (get_class cn) f
      |_-> failwith "definition de methode introuvable (pas normal)")
  in
  (** [eval_call f this args eval] évalue un appel à la méthode [f] de l'objet [this] 
    avec les arguments [args]. 
    *)
  let rec eval_call f this args eval =
    let c = (get_class this.cls) in
    let meth = get_method c f in

    let aux lenv = 
      List.iter (fun (var,_,init) ->
        (match init with 
          None -> Hashtbl.add lenv var Null
          |Some e -> Hashtbl.add lenv var (eval e)
          )) meth.locals;
    in

    let lenv = Hashtbl.create 16 in
    Hashtbl.add lenv "this" (VObj this);
    (*ajout des parametre dans l'espace local*)
    List.iter2 (fun (par, _) v  -> Hashtbl.add lenv par v) meth.params args;
    (*ajout des vars locals dans l'espace local*)
    aux lenv;
    exec_seq meth.code lenv;

  and exec_seq s lenv =
    (let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false

    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Unop (uop,e) -> 
        (match uop with 
            Opp ->  VInt (-evali e)
            |Not -> VBool (not(evalb e))
        )
      |Binop (bop, e1, e2) -> 
        (match bop with 
        Add  -> VInt (evali e1 + evali e2)
        |Sub -> VInt (evali e1 - evali e2)
        |Mul -> VInt (evali e1 * evali e2)
        |Div -> 
          let v2 = evali e2 in
          (if (v2=0) then raise (Error "Division par 0")
          else VInt (evali e1 / evali e2))
        |Rem ->
          let v2 = evali e2 in
          (if (v2=0) then raise (Error "Division par 0")
          else VInt (evali e1 mod evali e2))
        |Lt -> VBool (evali e1 < evali e2)
        |Le -> VBool (evali e1 <= evali e2)
        |Gt -> VBool (evali e1 > evali e2)
        |Ge -> VBool (evali e1 >= evali e2)
        |And-> VBool (evalb e1 && evalb e2)
        |Or -> VBool (evalb e1 || evalb e2)
        |Eq -> VBool(eval e1 = eval e2)
        |Neq -> VBool(eval e1 <> eval e2)
        )
      | Get (mem_acc) -> 
        (match mem_acc with 
           Var id -> 
            (try 
              Hashtbl.find lenv id
            with
              |Not_found -> Hashtbl.find env id
            )
          |Field(eo,att) -> 
            let obj = evalo eo in
            (*!!!!!!!!!!*)
            Hashtbl.find obj.fields att
          |Arr(e1,le) -> 
            let li = List.map (evali) le in
            let arr,i = get_index li (eval e1) in
            arr.(i) 
        )
      | This -> Hashtbl.find lenv "this"
      | New cn -> 
        let c = get_class cn in
        let fields = Hashtbl.create 16 in
        set_All_fields fields c eval;

        VObj ({cls=c.class_name; fields=fields})
      | NewCstr (cn, el) -> 
        let c = get_class cn in
        let fields = Hashtbl.create 16 in
        set_All_fields fields c eval;
        
        eval_call "constructor" {cls=c.class_name; fields=fields} (List.map (fun e -> eval e) el) eval;
        VObj ({cls=c.class_name; fields=fields})
      | MethCall (e, s, el) -> 
        (try 
          eval_call s (evalo e) (List.map (fun e -> eval e) el) eval;
          Null
        with 
          |Return v -> v)

      | ArrayNelts (t,le) -> 
        let ln = List.map (evali) le in
        create_array t ln
      | ArrayList l -> 
        VArray(Array.of_list (List.map (eval) l))
    in
      
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | Expr(e) -> let _ = eval e in () 
      | Return(e) -> raise (Return (eval e))

      | Set(mem_acc, e) ->
        let ve = eval e in
        (match mem_acc with 
          | Var s -> 
            if(Hashtbl.mem lenv s) then 
              Hashtbl.replace lenv s ve
            else
              Hashtbl.replace env s ve 
              
          |Field(eo,s) ->
            (***********) 
            let obj = evalo eo in 
            Hashtbl.replace obj.fields s (eval e) 
          |Arr(e1,le) -> 
            let li = List.map (evali) le in
            let arr,i = get_index li (eval e1) in
            arr.(i) <- ve
        )
      | If(e, s1, s2) -> 
        if(evalb e) then 
          exec_seq s1
        else
          exec_seq s2

      | While(e, s) -> 
        let ve = ref (evalb e) in

        while (!ve) do 
          exec_seq s;
          ve := evalb e;
        done;

    and exec_seq s = 
      List.iter exec s
    in
    
    exec_seq s)
    
  in
  
  exec_seq main_code (Hashtbl.create 1)
