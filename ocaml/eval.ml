open Base;;

module MalMap = Mal.MalMap
module Mal = Mal.Mal

let rec macroexpand env form =
  match form with
  | Mal.List ((Mal.Symbol name) :: args, _) as l ->
    begin
      match Env.find env name with
      | Some (Mal.Macro (f, _)) -> f args |> macroexpand env
      | _ -> l
    end
  | x -> x

let rec eval env form =
  match form with
  | Mal.Symbol s -> Env.get env s
  | Mal.List ([], _) as l -> l
  | Mal.List (_, _) as l -> eval_macro env l
  | Mal.Vector (v, meta) -> Mal.Vector (List.map ~f:(eval env) v, meta)
  | Mal.Map (m, meta) -> Mal.Map (MalMap.map (eval env) m, meta) (* Erm, hmm... *)
  | x -> x

and eval_macro env form =
  match macroexpand env form with
  | Mal.List (es, _) -> eval_special env es
  | newForm -> eval env newForm

and eval_special env form =
  match form with
  | (Mal.Symbol "def!") :: (Mal.Symbol name) :: body :: [] ->
    let ebody = (eval env body) in
    Env.set env name ebody; ebody
  | (Mal.Symbol "fn*") :: params :: body :: [] ->
    let f args =
      let newEnv = push_fn env (Mal.to_seq params) args in
      eval newEnv body
    in Mal.Fn (f, Mal.Nil)
  | (Mal.Symbol "let*") :: bindings :: body :: [] ->
    let newEnv = push_let env @@ Mal.to_seq bindings in
    eval newEnv body
  | (Mal.Symbol "if") :: cond :: trueBranch :: maybeFalseBranch ->
    (if Mal.is_truthy @@ eval env cond then
       eval env trueBranch
     else
       match maybeFalseBranch with
       | [] -> Mal.Nil
       | falseBranch :: [] -> eval env falseBranch
       | _ -> failwith "If then has too many branches")
  | (Mal.Symbol "do") :: exprs ->
    let rec eval_all = function
    | [] -> Mal.Nil
    | e :: [] -> eval env e
    | e :: es -> ignore @@ eval env e; eval_all es
    in eval_all exprs
  | (Mal.Symbol "quote") :: form :: [] -> form
  | (Mal.Symbol "quasiquote") :: form :: [] -> eval_quasiquote env form
  | (Mal.Symbol "defmacro!") :: (Mal.Symbol name) :: body :: [] ->
    let macro = match eval env body with
    | Mal.Fn (f, m) -> Mal.Macro (f, m)
    | _ -> failwith "Tried to make non fn* a macro"
    in Env.set env name macro; macro
  | (Mal.Symbol "macroexpand") :: form :: [] -> macroexpand env form
  | (Mal.Symbol "try*") :: tbody :: (Mal.List ((Mal.Symbol "catch*") :: exname :: cbody :: [], _)) :: [] ->
    begin
      try
        eval env tbody
      with
      | Reader.MalException e ->
        let newEnv = push_fn env [exname] [e] in
        eval newEnv cbody
    end
  | l ->
    match List.map ~f:(eval env) l with
    | (Mal.Fn (f, _)) :: rands -> f rands
    | e -> failwith ("Can't apply " ^ (Mal.repr true (Mal.list e)))

and eval_quasiquote env =
  function
  | Mal.List (forms, _) -> eval_quasiquote_list env forms
  | Mal.Vector (forms, _) -> eval_quasiquote_list env forms
  | x -> x

and eval_quasiquote_list env =
  function
  | (Mal.Symbol "unquote") :: form :: [] -> eval env form
  | (Mal.List ((Mal.Symbol "splice-unquote") :: x :: [], _)) :: rest ->
    Mal.append (eval env x) (eval_quasiquote_list env rest)
  | (Mal.Vector ((Mal.Symbol "splice-unquote") :: x :: [], _)) :: rest ->
    Mal.append (eval env x) (eval_quasiquote_list env rest)
  | e :: es -> Mal.list ((eval_quasiquote env e) :: (eval_quasiquote_list env es |> Mal.to_seq))
  | [] -> Mal.list []

and push_let env bindings =
  let nenv = Env.make env in
  let rec p = function
  | [] -> nenv
  | _ :: [] -> failwith "Odd number of binding forms"
  | (Mal.Symbol name) :: expr :: rest ->
    Env.set nenv name (eval nenv expr);
    p rest
  | _ -> failwith "Binding forms must bind to symbols"
  in p bindings

and push_fn env names values =
  let nenv = Env.make env in
  let rec lp ns vs = match (ns, vs) with
  | ([], []) -> nenv
  | ((Mal.Symbol "&") :: (Mal.Symbol name) :: [], rest) ->
    Env.set nenv name (Mal.list rest);
    nenv
  | ((Mal.Symbol n) :: ns, v :: vs) ->
    Env.set nenv n v;
    lp ns vs
  | _ -> failwith "Wrong number of args to fn?  Or some other issue..."
  in lp names values
