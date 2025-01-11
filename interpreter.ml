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



(* implementation des tableau :
  la précondition de la fonction dont on sait qu'elle est vérifié à travers 
  le typecheker c'est on aura pas TVOid à traiter et la liste vide aussi
  ne va pas apparaitre, et aussi que si il ya un seul element dans la liste
  alors le type fait partie de ceux des type dit primitif (ça c'est l'analyse syntaxique qui l'assure)
  et si il ya plusieurs elements dans la liste alors on fait forcément sur un TArray
*)
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


(* retourne par exemple pour un accès t[0][5] la ligne 0 (mais pas l'accès au 5eme) 
et le dernier indice (5)*)
let rec get_index li array = 
  match li,array with 
  |[],_ -> failwith "N'est jamais censé arriver (traité au typechecker)"
  |[i],VArray array -> array,i
  |i::tl, VArray array -> get_index tl array.(i)
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

  (* 
  - recupere tous les attributs d'un objet avec ceux hérités
  - On remonte jusqu'a la classe la plus haute dans la hiearchie avant de 
  commencer à ajouter, comme ça si y'a les même nom d'attributs entre une classe
  mere et un fils, l'attribut du fils va être utilisé, parceque ajouté en dernier
*)
  let rec set_All_fields fields class_def eval = 
    
    let aux fields class_def = 
      
      List.iter (fun (att,_,_,init) ->
        (match !init with 
          None -> Hashtbl.add fields att Null;
          |Some e -> 
            try 
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

  
    (*retourne la premiere methode rencontré lors de la 
    remonté de la hiéarchie des classes*)
  let rec get_method class_def f = 
    try 
      List.find (fun meth_d -> meth_d.method_name = f) class_def.methods
    with 
    |Not_found -> 
      (match class_def.parent with 
      Some cn -> get_method (get_class cn) f
      |_-> raise (Error "definition de methode introuvable (pas normal)"))
  in

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
        (*1111111*)
        (*** UNe possibilité d'appeler le constructeur des classes sup ?*)
        let c = get_class cn in
        let fields = Hashtbl.create 16 in
        set_All_fields fields c eval;
        VObj ({cls=c.class_name; fields=fields})
        
      | NewCstr (cn, el) -> 
        (*111111111*)
        
        let c = get_class cn in
        let fields = Hashtbl.create 16 in
        set_All_fields fields c eval;
        
        eval_call "constructor" {cls=c.class_name; fields=fields} (List.map (fun e -> eval e) el) eval;
        VObj ({cls=c.class_name; fields=fields})
        
      | MethCall (e, s, el) -> 
        (*11111111*)
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
