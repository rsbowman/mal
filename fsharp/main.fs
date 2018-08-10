// Learn more about F# at http://fsharp.org

open System

open Mal
open Reader
open Eval

let print exp = Mal.repr exp
let rep env str = str |> Reader.read |> eval env |> print

let rec repl env =
    Console.Write("user> ")
    let line = Console.ReadLine()
    try
        Console.WriteLine(rep env line)
    with
        | Error.ParseEofException -> ()
        | Error.ParseException err -> Console.WriteLine(err)
        | Error.EvalException err -> Console.WriteLine(err)
        | Error.MalException expr -> Console.WriteLine(Mal.repr expr)
    repl env

[<EntryPoint>]
let main argv =
    let env = Core.makeReplEnv ()
    let args = List.ofArray argv
    rep env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))" |> ignore
    rep env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))" |> ignore
    rep env "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))" |> ignore
    match args with
    | [] ->
        Env.set env "*ARGV*" <| Mal.list []
        repl env
    | filename :: restArgs ->
        let program = "(load-file \"" + filename + "\")"
        Env.set env "*ARGV*" (restArgs |> List.map Mal.String |> Mal.list)
        rep env program |> ignore
    0
