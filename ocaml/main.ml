open Base;;
open Stdio;;
open Out_channel;;

module MalMap = Mal.MalMap
module Mal = Mal.Mal

let print exp = Mal.repr true exp
let rep env str = print @@ Eval.eval env (Reader.read str)

let drop n l = List.drop l n

let rec mal_argv = function
| [] -> []
| s :: ss -> (Mal.String s) :: mal_argv ss

let mal_eval env = Mal.Fn ((function
  | form :: [] -> Eval.eval env form
  | _ -> failwith "Bad arg to eval"), Mal.Nil)

let rec repl env args =
  output_string stdout "user> ";
  flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    try print_endline @@ rep env line;
      repl env args
    with
    | Failure e -> print_endline e; repl env args
    | Reader.MalException (Mal.String s) -> print_endline s; repl env args
    | End_of_file -> repl env args

let main () =
  let args = Sys.argv |> Array.to_list |> drop 1 in
  let env = Env.repl_env in
  Env.set env "eval" (mal_eval env);
  ignore @@ rep env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))";
  ignore @@ rep env  "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))";
  (* ignore @@ rep env "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"; *)
  ignore @@ rep env "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))";
  ignore @@ rep env "(def! *host-language* \"ocaml\")";
  ignore @@ rep env "(def! start-time (time-ms))";
  match args with
  | [] ->
    Env.set env "*ARGV*" (Mal.List ([], Mal.Nil));
    ignore @@ rep env "(println (str \"Mal [\" *host-language* \"]\"))";
    repl env args
  | filename :: restArgs ->
    let prog = "(load-file \"" ^ filename ^ "\")" in
    Env.set env "*ARGV*" (Mal.List (mal_argv restArgs, Mal.Nil));
    rep env prog |> ignore;;

main ()
