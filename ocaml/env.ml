open Base;;
open Stdio;;

module MalMap = Mal.MalMap
module Mal = Mal.Mal

let (>>) f g x = g ( f x)

type bindings_t = (string, Mal.t) Hashtbl.t

type t =
  | Empty
  | Env of t * bindings_t

let make parent =
  Env (parent, Hashtbl.create (module String))

let rec set env sym value =
  match env with
  | Empty -> set (Env (Empty, Hashtbl.create (module String))) sym value
  | Env (_, bindings) -> Hashtbl.set bindings ~key:sym ~data:value

let rec find env sym =
  match env with
  | Empty -> None
  | Env (parent, bindings) ->
    match Hashtbl.find bindings sym with
    | None -> find parent sym
    | Some f -> Some f

let rec get env sym =
  match env with
  | Empty -> Reader.error ("'" ^ sym ^ "' not found")
  | Env (parent, bindings) ->
    match Hashtbl.find bindings sym with
    | None -> get parent sym
    | Some v -> v

(* REPL environment *)

let make_binop op =
  let f es =
    match es with
    | (Mal.Int n1) :: (Mal.Int n2) :: [] -> Mal.Int (op n1 n2)
    | _ -> Reader.error "Invalid operands to binop"
  in Mal.Fn (f, Mal.Nil)

let make_binpred pred =
  let f es = Mal.Bool (match es with
    | e1 :: e2 :: [] -> pred e1 e2
    | _ -> Reader.error "Invalid operands to bin pred")
  in Mal.Fn (f, Mal.Nil)

let make_unpred pred =
  let f es = Mal.Bool (match es with
    | e :: [] -> pred e
    | _ -> Reader.error "Invalid operands to unary pred")
  in Mal.Fn (f, Mal.Nil)

let mal_is_empty = function
| Mal.List ([], _) -> true
| Mal.Vector ([], _) -> true
| _ -> false

let mal_is_list = function
| Mal.List _ -> true
| _ -> false

let mal_list es = Mal.list es

let one_arg f = function
| e :: [] -> f e
| _ -> Reader.error "Wrong number of args to one_arg"

let two_ints f = function
| (Mal.Int i1) :: (Mal.Int i2) :: [] -> f i1 i2
| _ -> Reader.error "Wrong number of args to one_arg"

let return_bool f args = Mal.Bool (f args)
let return_int f args = Mal.Int (f args)

let make_mal_core f = Mal.Fn (f, Mal.Nil)

let mal_not = Mal.is_truthy >> not

let mal_raw_str is_readable sep = function
| [] -> ""
| args -> (String.concat ?sep:(Some sep) @@ List.map ~f:(Mal.repr is_readable) args)

let mal_str is_readable sep form = Mal.String (mal_raw_str is_readable sep form)

let mal_print is_readable sep form =
  Stdio.print_endline @@ mal_raw_str is_readable sep form;
  Mal.Nil

let mal_read_str = function
| (Mal.String s) :: [] -> Reader.read s
| _ -> Reader.error "Wrong args to read-string"

let mal_slurp = function
| (Mal.String fname) :: [] -> Mal.String (In_channel.read_all fname)
| _ -> Reader.error "Wrong args to slurp"

let mal_atom e = Mal.Atom (ref e)

let mal_is_atom = function
| Mal.Atom _ -> true
| _ -> false

let mal_deref = function
| Mal.Atom r -> !r
| _ -> Reader.error "Can't deref non atom"

let mal_reset = function
| (Mal.Atom r) :: expr :: [] -> r := expr; expr
| _ -> Reader.error "bad args to reset"

let mal_swap = function
| (Mal.Atom r) :: (Mal.Fn (f, _)) :: args ->
  let newval = f (!r :: args) in
  r := newval;
  newval
| _ -> Reader.error "bad args to reset"

let mal_cons = function
| value :: listy :: [] -> Mal.list @@ value :: (Mal.to_seq listy)
| _ -> Reader.error "bad args to cons"

let mal_first =
  function
  | Mal.List (h :: _, _) -> h
  | Mal.Vector (h :: _, _) -> h
  | _ -> Mal.Nil

let mal_rest =
  function
  | Mal.List (_ :: r, _) -> Mal.list r
  | Mal.Vector (_ :: r, _) -> Mal.list r
  | _ -> Mal.list []

let mal_nth =
  function
  | seq :: (Mal.Int n) :: [] ->
    begin
      match List.nth (Mal.to_seq seq) n with
      | Some e -> e
      | None -> Reader.error "OOB nth"
    end
  | _ -> Reader.error "Bad args to nth"

let mal_throw e = raise (Reader.MalException e)

let mal_keys = function
| Mal.Map (m, _) -> Mal.list @@ MalMap.keys m
| _ -> Reader.error "Bad arg to keys"

let mal_vals = function
| Mal.Map (m, _) -> Mal.list @@ MalMap.vals m
| _ -> Reader.error "Bad arg to keys"

let apply_args args =
  let rec loop out = function
  | [] -> []
  | l :: [] -> List.append (out |> List.rev) (l |> Mal.to_seq)
  | l :: ls -> loop (l :: out) ls
  in loop [] args

let mal_apply = function
| (Mal.Fn (f, _)) :: args -> f @@ apply_args args
| _ -> Reader.error "Bad apply"

let mal_count = Mal.to_seq >> List.length

let rec mal_assoc_helper m = function
| [] -> m
| key :: value :: rest -> mal_assoc_helper (MalMap.assoc m key value) rest
| _ -> Reader.error "Bad assoc, need odd number key values"

let mal_assoc = function
| (Mal.Map (m, d)) :: key_vals -> Mal.Map (mal_assoc_helper m key_vals, d)
| _ -> Reader.error "Bad assoc"

let mal_disassoc = function
| (Mal.Map (m, d)) :: keys -> Mal.Map (List.fold keys ~init:m ~f:MalMap.disassoc, d)
| _ -> Reader.error "Bad disassoc"

let mal_map = function
| (Mal.Fn (f, _)) :: seq :: [] -> Mal.to_seq seq |> List.map ~f:(fun x -> f [x]) |> Mal.list
| _ -> Reader.error "Bad map"

let mal_is_true = function
| Mal.Bool b -> b
| _ -> false

let mal_is_nil = function
| Mal.Nil -> true
| _ -> false

let mal_is_false = mal_is_true >> not

let mal_is_keyword = function
| Mal.Keyword _ -> true
| _ -> false

let mal_is_vector = function
| Mal.Vector _ -> true
| _ -> false

let mal_is_symbol = function
| Mal.Symbol _ -> true
| _ -> false

let mal_is_map = function
| Mal.Map _ -> true
| _ -> false

let mal_is_sequential = function
| Mal.List _ | Mal.Vector _ -> true
| _ -> false

let mal_symbol = function
| Mal.String s -> Mal.Symbol s
| _ -> Reader.error "Bad symbol"

let mal_keyword = function
| Mal.String s -> Mal.Keyword (":" ^ s)
| _ -> Reader.error "Bad symbol"

let mal_hash_map args = Mal.Map (MalMap.from_list args, Mal.Nil)

let mal_vector args = Mal.Vector (args, Mal.Nil)

let mal_get = function
| Mal.Nil :: _ -> Mal.Nil
| (Mal.Map (m, _)) :: key :: [] ->
  if MalMap.member m key then
      MalMap.get m key
  else
    Mal.Nil
| _ -> Reader.error "Bad get"

let mal_contains = function
| (Mal.Map (m, _)) :: key :: [] -> MalMap.member m key
| _ -> Reader.error "Bad contains?"

let mal_meta = function
| Mal.List (_, m) -> m
| Mal.Vector (_, m) -> m
| Mal.Map (_, m) -> m
| Mal.Fn (_, m) -> m
| Mal.Macro (_, m) -> m
| _ -> Mal.Nil

let mal_with_meta = function
| (Mal.List (l, _)) :: metadata :: [] -> Mal.List (l, metadata)
| (Mal.Vector (l, _)) :: metadata :: [] -> Mal.Vector (l, metadata)
| (Mal.Map (m, _)) :: metadata :: [] -> Mal.Map (m, metadata)
| (Mal.Fn (f, _)) :: metadata :: [] -> Mal.Fn (f, metadata)
| (Mal.Macro (f, _)) :: metadata :: [] -> Mal.Macro (f, metadata)
| _ -> Reader.error "Bad with-meta"

let mal_seq = function
| Mal.String "" -> Mal.Nil
| Mal.List ([], _) -> Mal.Nil
| Mal.Vector ([], _) -> Mal.Nil
| Mal.String s -> String.to_list s |> List.map ~f:(fun c -> Mal.String (String.of_char c)) |> Mal.list
| Mal.List _ as l -> l
| Mal.Vector (v, m) -> Mal.List (v, m)
| _ -> Mal.Nil

let mal_is_macro = function
| Mal.Macro _ -> true
| _ -> false

let mal_is_fn = function
| Mal.Fn _ -> true
| _ -> false

let mal_is_string = function
| Mal.String _ -> true
| _ -> false

let mal_is_number = function
| Mal.Int _ -> true
| _ -> false

let mal_conj = function
| (Mal.List (l, m)) :: es -> Mal.List (List.append (List.rev es) l, m)
| (Mal.Vector (v, m)) :: es -> Mal.Vector (List.append v es, m)
| _ -> Reader.error "Bad conj"

let counter : int ref = ref 0;;

let mal_gensym _ =
  counter := !counter + 1;
  Mal.Symbol ("_sym_" ^ (Int.to_string !counter))

let readline =
  function
  | (Mal.String prompt) :: [] ->
    begin
      Stdio.Out_channel.output_string stdout prompt;
      Stdio.Out_channel.flush stdout;
      match In_channel.input_line stdin with
      | None -> Mal.Nil
      | Some line -> Mal.String line
    end
  | _ -> Reader.error "Bad readline"

let mal_time _ = Mal.Int (Int.of_float @@ 100.0 *. Unix.gettimeofday ())

let repl_env =
  let bindings = (Hashtbl.of_alist_exn (module String)
                    [ ("+", make_binop (+))
                    ; ("-", make_binop (-))
                    ; ("*", make_binop ( * ))
                    ; ("/", make_binop (/))
                    ; ("=", make_binpred Mal.is_equal)
                    ; ("empty?", make_unpred mal_is_empty)
                    ; ("list?", make_unpred mal_is_list)
                    ; ("list", make_mal_core @@ mal_list)
                    ; ("count", make_mal_core @@ one_arg @@ return_int @@ mal_count)
                    ; ("not", make_mal_core @@ one_arg @@ return_bool @@ mal_not)
                    ; ("<", make_mal_core @@ return_bool @@ two_ints @@ (<))
                    ; ("<=", make_mal_core @@ return_bool @@ two_ints @@ (<=))
                    ; (">=", make_mal_core @@ return_bool @@ two_ints @@ (>=))
                    ; (">", make_mal_core @@ return_bool @@ two_ints @@ (>))
                    ; ("str", make_mal_core @@ mal_str false "")
                    ; ("pr-str", make_mal_core @@ mal_str true " ")
                    ; ("prn", make_mal_core @@ mal_print true " ")
                    ; ("println", make_mal_core @@ mal_print false " ")
                    ; ("read-string", make_mal_core @@ mal_read_str)
                    ; ("slurp", make_mal_core @@ mal_slurp)
                    ; ("atom", make_mal_core @@ one_arg @@ mal_atom)
                    ; ("atom?", make_mal_core @@ return_bool @@ one_arg @@ mal_is_atom)
                    ; ("deref", make_mal_core @@ one_arg @@ mal_deref)
                    ; ("reset!", make_mal_core @@ mal_reset)
                    ; ("swap!", make_mal_core @@ mal_swap)
                    ; ("cons", make_mal_core @@ mal_cons)
                    ; ("concat", make_mal_core @@ Mal.concat)
                    ; ("first", make_mal_core @@ one_arg @@ mal_first)
                    ; ("rest", make_mal_core @@ one_arg @@ mal_rest)
                    ; ("nth", make_mal_core @@ mal_nth)
                    ; ("throw", make_mal_core @@ one_arg @@ mal_throw)
                    ; ("keys", make_mal_core @@ one_arg @@ mal_keys)
                    ; ("vals", make_mal_core @@ one_arg @@ mal_vals)
                    ; ("apply", make_mal_core @@ mal_apply)
                    ; ("assoc", make_mal_core @@ mal_assoc)
                    ; ("dissoc", make_mal_core @@ mal_disassoc)
                    ; ("map", make_mal_core @@ mal_map)
                    ; ("true?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_true)
                    ; ("false?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_false)
                    ; ("nil?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_nil)
                    ; ("symbol?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_symbol)
                    ; ("keyword?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_keyword)
                    ; ("vector?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_vector)
                    ; ("map?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_map)
                    ; ("sequential?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_sequential)
                    ; ("symbol", make_mal_core @@ one_arg @@ mal_symbol)
                    ; ("keyword", make_mal_core @@ one_arg @@ mal_keyword)
                    ; ("hash-map", make_mal_core @@ mal_hash_map)
                    ; ("vector", make_mal_core @@ mal_vector)
                    ; ("get", make_mal_core @@ mal_get)
                    ; ("contains?", make_mal_core @@ return_bool @@ mal_contains)
                    ; ("meta", make_mal_core @@ one_arg @@ mal_meta)
                    ; ("with-meta", make_mal_core @@ mal_with_meta)
                    ; ("seq", make_mal_core @@ one_arg @@ mal_seq)
                    ; ("macro?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_macro)
                    ; ("fn?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_fn)
                    ; ("string?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_string)
                    ; ("number?", make_mal_core @@ one_arg @@ return_bool @@ mal_is_number)
                    ; ("conj", make_mal_core @@ mal_conj)
                    ; ("gensym", make_mal_core @@ mal_gensym)
                    ; ("readline", make_mal_core @@ readline)
                    ; ("time-ms", make_mal_core @@ mal_time)
                    ])
  in Env (Empty, bindings)
