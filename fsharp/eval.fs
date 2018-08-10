module Eval

let rec splitLetBindings = function
    | [] -> [], []
    | (Mal.Symbol s) :: expr :: rest ->
        let (names, values) = splitLetBindings rest
        s :: names, expr :: values
    | _ -> Error.evalError "Odd # let bindings or non symbol bindings"

let symbolNames =
    List.map (function
              | (Mal.Symbol s) -> s
              | _ -> Error.evalError "Tried to get name of non symbol")

let rec macroexpand env form =
    match form with
    | Mal.List ((Mal.Symbol name) :: args, _) as l ->
        begin
            match Env.get env name with
            | Some (Mal.Macro(f, _)) -> f args |> macroexpand env
            | _ -> l
        end
    | x -> x

let rec eval env form =
    match form with
    | Mal.Symbol s ->
        match Env.get env s with
        | Some e -> e
        | None -> Error.malErrorStr <| "'" + s + "' not found"
    | Mal.List([], _) as l -> l
    | Mal.List _ as l -> evalMacro env l
    | Mal.Vector(v, meta) -> Mal.Vector(List.map (eval env) v, meta)
    | Mal.Map(m, _) -> Mal.map <| Map.map (fun _ e -> (eval env e)) m
    | x -> x

and evalMacro env form =
    match macroexpand env form with
    | Mal.List(l, _) -> evalSpecial env l
    | newForm -> eval env newForm

and evalSpecial env form =
    match form with
    | [Mal.Symbol "def!"; Mal.Symbol name; body] ->
        let ebody = (eval env body)
        Env.set env name ebody
        ebody
    | (Mal.Symbol "let*") :: bindings :: [body] ->
        let (names, values) = splitLetBindings <| Mal.toList bindings
        let newEnv = evalPushEnv env names values
        eval newEnv body
    | (Mal.Symbol "fn*") :: fParams :: [body] ->
        let f args =
            let newEnv = Env.push env (Mal.toList fParams |> symbolNames) args
            eval newEnv body
        Mal.Fn (f, Mal.Nil)
    | (Mal.Symbol "if") :: cond :: trueBranch :: maybeFalseBranch ->
        if Mal.truthy <| eval env cond then
            eval env trueBranch
        else
            match maybeFalseBranch with
            | [] -> Mal.Nil
            | [falseBranch] -> eval env falseBranch
            | _ -> Error.evalError "If then has too many branches"
    | (Mal.Symbol "do") :: exprs ->
        let rec evalAll = function
            | [] -> Mal.Nil
            | [e] -> eval env e
            | e :: es -> ignore <| eval env e; evalAll es
        evalAll exprs
    | (Mal.Symbol "quote") :: [form] -> form
    | (Mal.Symbol "quasiquote") :: [form] -> evalQuasiquote env form
    | (Mal.Symbol "defmacro!") :: (Mal.Symbol name) :: [body] ->
        let macro = match eval env body with
                    | Mal.Fn(f, m) -> Mal.Macro(f, m)
                    | _ -> Error.evalError "Tried to make non fn* a macro"
        in Env.set env name macro; macro
    | (Mal.Symbol "macroexpand") :: [form] -> macroexpand env form
    | (Mal.Symbol "try*") :: tbody :: [(Mal.List((Mal.Symbol "catch*") :: (Mal.Symbol exname) :: [cbody], _))] ->
        try
            eval env tbody
        with
        | Error.MalException e ->
            let newEnv = Env.push env [exname] [e] in
            eval newEnv cbody
    | l ->
        match List.map (eval env) l with
        | (Mal.Fn(f, _)) :: rands -> f rands
        | e -> Error.evalError <| "Can't apply " + (Mal.repr <| Mal.list e)

and evalQuasiquote env =
  function
  | Mal.List (forms, _) -> evalQuasiquoteList env forms
  | Mal.Vector (forms, _) -> evalQuasiquoteList env forms
  | x -> x

and evalQuasiquoteList env =
  function
  | (Mal.Symbol "unquote") :: [form] -> eval env form
  | (Mal.List ((Mal.Symbol "splice-unquote") :: [x], _)) :: rest ->
    Mal.append (eval env x) (evalQuasiquoteList env rest)
  | (Mal.Vector ((Mal.Symbol "splice-unquote") :: [x], _)) :: rest ->
    Mal.append (eval env x) (evalQuasiquoteList env rest)
  | e :: es -> Mal.list ((evalQuasiquote env e) :: (evalQuasiquoteList env es |> Mal.toList))
  | [] -> Mal.list []

and evalPushEnv env names values =
    let newEnv = Env.Bindings() :: env
    let rec loop ns vs =
        match ns, vs with
        | [], [] -> newEnv
        | n::nns, v::vvs ->
            Env.set newEnv n (eval newEnv v);
            loop nns vvs
        | _ -> Error.evalError "Odd number let* bindings or non symbol bindings"
    loop names values
