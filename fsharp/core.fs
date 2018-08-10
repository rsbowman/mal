module Core
open System

let binaryOp f = function
    | [Mal.Int x; Mal.Int y] -> f x y
    | _ -> Error.malErrorStr "invalid args to bin op"

let add = binaryOp (fun x y -> x + y |> Mal.Int)
let sub = binaryOp (fun x y -> x - y |> Mal.Int)
let mul = binaryOp (fun x y -> x * y |> Mal.Int)
let div = binaryOp (fun x y -> x / y |> Mal.Int)

let unaryPred f = function
    | [expr] -> Mal.Bool <| f expr
    | _ -> Error.malErrorStr "invalid args to unary pred"

let binaryPred p = function
    | [e1; e2] -> Mal.Bool <| p e1 e2
    | _ -> Error.malErrorStr "invalid args to binary pred"

let isList = (function
    | Mal.List _ -> true
    | _ -> false) |> unaryPred

let isEmpty = function
    | [Mal.List([], _)] -> Mal.Bool true
    | [Mal.Vector([], _)] -> Mal.Bool true
    | _ -> Mal.Bool false

let rawStr isReadable sep = function
    | [] -> ""
    | args ->
        args |> List.map (Mal.reprHelper isReadable)
        |> String.concat sep

let str isReadable sep form = Mal.String <| rawStr isReadable sep form

let print isReadable sep form =
    Console.WriteLine(rawStr isReadable sep form)
    Mal.Nil

let malReadString = function
    | [Mal.String s] -> Reader.read s
    | _ -> Error.malErrorStr "invalid args to read-string"

let slurp = function
    | [Mal.String path] -> System.IO.File.ReadAllText(path) |> Mal.String
    | _ -> Error.malErrorStr "invalid args to slurp"

let atom = function
    | [expr] -> Mal.Atom (ref expr)
    | _ -> Error.malErrorStr "bad arg to atom"

let isAtom = (function
    | [Mal.Atom _] -> true
    | _ -> false) >> Mal.Bool

let deref = function
    | [Mal.Atom a] -> !a
    | _ -> Error.malErrorStr "invalid args to deref"

let reset = function
    | [Mal.Atom a; value] -> a := value; value
    | _ -> Error.malErrorStr "invalid args to reset"

let swap = function
    | (Mal.Atom a) :: (Mal.Fn(f, _)) :: args ->
        let newVal = f (!a :: args)
        a := newVal
        newVal
    | _ -> Error.malErrorStr "bad args to swap!"


let cons = function
    | [x; l] -> x :: (Mal.toList l) |> Mal.list
    | _ -> Error.malErrorStr "bad args to cons"

let concat args = Mal.list <| List.collect Mal.toList args

let first = function
    | [e] when e |> Mal.toList |> List.isEmpty -> Mal.Nil
    | [e] -> List.head <| Mal.toList e
    | _ -> Error.malErrorStr "invalid args to first"

let rest = function
    | [Mal.List(_ :: r, _)] -> Mal.list r
    | [Mal.Vector(_ :: r, _)] -> Mal.list r
    | _ -> Mal.list []

let nth = function
    | [seq; (Mal.Int n)] ->
        try
            List.item (int n) <| Mal.toList seq
        with
            | _ -> Error.malErrorStr "Arg to nth OOB"
    | _ -> Error.malErrorStr "Bad args to nth"

let keys = function
    | [Mal.Map(m, _)] -> m |> Map.toList |> List.map fst |> Mal.list
    | _ -> Error.malErrorStr "Bad args to keys"

let vals = function
    | [Mal.Map(m, _)] -> m |> Map.toList |> List.map snd |> Mal.list
    | _ -> Error.malErrorStr "Bad args to vals"

let hashMap args =
    let rec lp acc = function
        | [] -> acc
        | key :: value :: rest -> lp ((key, value) :: acc) rest
        | _ -> Error.malErrorStr "Bad args to hash-map"
    lp [] args |> Map.ofList |> Mal.map

let throw = function
    | [e] -> Error.malError e
    | _ -> Error.malErrorStr "invalid args to throw"

let pmatch p = function
    | [e] -> if p e then Mal.Bool true else Mal.Bool false
    | _ -> Error.malErrorStr "invalid args to bool pattern"

let count = function
    | [e] -> e |> Mal.toList |> List.length |> int64 |> Mal.Int
    | _ -> Error.malErrorStr "invalid args to count"

let assoc = function
    | Mal.Map(m, _) :: keysAndValues ->
        let rec lp m = function
            | [] -> m
            | key :: value :: rest -> lp (Map.add key value m) rest
            | _ -> Error.malErrorStr "invalid args to assoc"
        lp m keysAndValues |> Mal.map
    | _ -> Error.malErrorStr "invalid args to assoc"

let disassoc = function
    | Mal.Map(m, _) :: keys -> List.fold (fun m k -> Map.remove k m) m keys |> Mal.map
    | _ -> Error.malErrorStr "invalid args to dissoc"

let contains = function
    | [Mal.Map(m, _); key] -> Map.containsKey key m |> Mal.Bool
    | _ -> Error.malErrorStr "invalid args to contains?"

let get = function
    | [Mal.Nil; _] -> Mal.Nil
    | [Mal.Map(m, _); key]  ->
        if Map.containsKey key m then
            Map.find key m
        else
            Mal.Nil
    | _ -> Error.malErrorStr "invalid args to get"

let applyArgs args =
  let rec loop out = function
  | [] -> []
  | [l] -> List.append (out |> List.rev) (l |> Mal.toList)
  | l :: ls -> loop (l :: out) ls
  in loop [] args

let apply = function
    | (Mal.Fn(f, _)) :: args -> f <| applyArgs args
    | _ -> Error.malErrorStr "invalid args to apply"

let oneArg f = function
    | [e] -> f e
    | _ -> Error.malErrorStr "invalid args to one arg fn"

let malMap = function
    | [Mal.Fn(f, _); seq] -> seq |> Mal.toList |> List.map (fun x -> f [x]) |> Mal.list
    | _ -> Error.malErrorStr "invalid args to map"

let vector args = Mal.Vector(args, Mal.Nil)

let symbol = function
    | [Mal.String s] -> Mal.Symbol s
    | _ -> Error.malErrorStr "invalid arg to symbol"

let keyword = function
    | [Mal.String s] -> Mal.Keyword(":" + s)
    | _ -> Error.malErrorStr "invalid arg to keyword"

let meta = function
    | [Mal.List(_, m)] -> m
    | [Mal.Vector(_, m)] -> m
    | [Mal.Map(_, m)] -> m
    | [Mal.Fn(_, m)] -> m
    | [Mal.Macro(_, m)] -> m
    | _ -> Mal.Nil

let withMeta = function
    | (Mal.List(l, _)) :: [metadata] -> Mal.List(l, metadata)
    | (Mal.Vector(l, _)) :: [metadata] -> Mal.Vector(l, metadata)
    | (Mal.Map(m, _)) :: [metadata] -> Mal.Map(m, metadata)
    | (Mal.Fn(f, _)) :: [metadata] -> Mal.Fn(f, metadata)
    | (Mal.Macro(f, _)) :: [metadata] -> Mal.Macro(f, metadata)
    | _ -> Error.malErrorStr "Bad with-meta"

let conj = function
    | (Mal.List(l, m)) :: es -> Mal.List(List.append (List.rev es) l, m)
    | (Mal.Vector(v, m)) :: es -> Mal.Vector(List.append v es, m)
    | _ -> Error.malErrorStr "Bad conj"

let counter : int ref = ref 0

let gensym _ =
    counter := !counter + 1;
    Mal.Symbol ("_sym_" + (string !counter))

let seq = function
    | [Mal.String ""] -> Mal.Nil
    | [Mal.List([], _)] -> Mal.Nil
    | [Mal.Vector([], _)] -> Mal.Nil
    | [Mal.String s] -> Seq.toList s |> List.map (string >> Mal.String) |> Mal.list
    | [Mal.List _ as l] -> l
    | [Mal.Vector(v, m)] -> Mal.List (v, m)
    | _ -> Mal.Nil

let timeMs = function
    | _ -> DateTimeOffset(DateTime.Now).ToUnixTimeMilliseconds() |> int64 |> Mal.Int

let readline = function
    | [Mal.String prompt] ->
        Console.Write(prompt)
        Console.ReadLine() |> Mal.String
    | _ -> Error.malErrorStr "invalid args to readline"

let makeCoreFn name fn =
    (name, Mal.Fn(fn, Mal.Nil))

let makeReplEnv () =
    let bindings =
        [makeCoreFn "+" add
         makeCoreFn "-" sub
         makeCoreFn "*" mul
         makeCoreFn "/" div
         makeCoreFn "count" count
         makeCoreFn "<" <| binaryPred (<)
         makeCoreFn ">" <| binaryPred (>)
         makeCoreFn "<=" <| binaryPred (<=)
         makeCoreFn ">=" <| binaryPred (>=)
         makeCoreFn "=" <| binaryPred (=)
         makeCoreFn "list" Mal.list
         makeCoreFn "empty?" isEmpty
         makeCoreFn "str" <| str false ""
         makeCoreFn "pr-str" <| str true " "
         makeCoreFn "prn" <| print true " "
         makeCoreFn "println" <| print false " "
         makeCoreFn "not" <| unaryPred (Mal.truthy >> not)
         makeCoreFn "read-string" malReadString
         makeCoreFn "slurp" slurp
         makeCoreFn "atom" atom
         makeCoreFn "deref" deref
         makeCoreFn "reset!" reset
         makeCoreFn "swap!" swap
         makeCoreFn "cons" cons
         makeCoreFn "concat" concat
         makeCoreFn "first" first
         makeCoreFn "rest" rest
         makeCoreFn "nth" nth
         makeCoreFn "keys" keys
         makeCoreFn "vals" vals
         makeCoreFn "hash-map" hashMap
         makeCoreFn "throw" throw
         makeCoreFn "symbol?" <| pmatch (function | Mal.Symbol _ -> true | _ -> false)
         makeCoreFn "keyword?" <| pmatch (function | Mal.Keyword _ -> true | _ -> false)
         makeCoreFn "list?" <| pmatch (function | Mal.List _ -> true | _ -> false)
         makeCoreFn "vector?" <| pmatch (function | Mal.Vector _ -> true | _ -> false)
         makeCoreFn "number?" <| pmatch (function | Mal.Int _ -> true | _ -> false)
         makeCoreFn "string?" <| pmatch (function | Mal.String _ -> true | _ -> false)
         makeCoreFn "fn?" <| pmatch (function | Mal.Fn _ -> true | _ -> false)
         makeCoreFn "macro?" <| pmatch (function | Mal.Macro _ -> true | _ -> false)
         makeCoreFn "sequential?" <| pmatch (function | Mal.List _ -> true | Mal.Vector _ -> true | _ -> false)
         makeCoreFn "map?" <| pmatch (function | Mal.Map _ -> true | _ -> false)
         makeCoreFn "atom?" <| pmatch (function | Mal.Atom _ -> true | _ -> false)
         makeCoreFn "nil?" <| pmatch (function | Mal.Nil _ -> true | _ -> false)
         makeCoreFn "true?" <| pmatch (function | Mal.Bool b -> b | _ -> false)
         makeCoreFn "false?" <| pmatch (function | Mal.Bool b -> not b | _ -> true)
         makeCoreFn "assoc" assoc
         makeCoreFn "dissoc" disassoc
         makeCoreFn "contains?" contains
         makeCoreFn "get" get
         makeCoreFn "apply" apply
         makeCoreFn "map" malMap
         makeCoreFn "vector" vector
         makeCoreFn "keyword" keyword
         makeCoreFn "symbol" symbol
         makeCoreFn "meta" meta
         makeCoreFn "with-meta" withMeta
         makeCoreFn "conj" conj
         makeCoreFn "gensym" gensym
         makeCoreFn "seq" seq
         makeCoreFn "time-ms" timeMs
         makeCoreFn "readline" readline
         ]
    let env = [Env.ofList bindings]
    let malEval = function
        | [expr] -> Eval.eval env expr
        | _ -> Error.malErrorStr "bad eval"
    Env.set env "eval" <| Mal.Fn(malEval, Mal.Nil)
    Env.set env "*host-language*" <| Mal.String "F#"
    Env.set env "start-time" <| timeMs ()
    env
