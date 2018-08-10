open Base;;

module MalMap = Mal.MalMap
module Mal = Mal.Mal

exception MalException of Mal.t
let error s = raise @@ MalException (Mal.String s)

let tokenize str =
  let token_re = (Str.regexp "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"\\|;.*\\|[^][  \n{}('\"`,;)]*") in
  (Str.full_split token_re str)
  |> List.filter ~f:(function | Str.Delim "" -> false | Str.Delim _ -> true | Str.Text _ -> false)
  |> List.map ~f:(function | Str.Delim x -> x | Str.Text _ -> raise (Invalid_argument "bad token, ws?"))
  |> List.filter ~f:(fun t -> not (String.is_prefix ~prefix:";" t))

exception Parse_error of string

let read_atom token =
  let num_re = (Str.regexp "-?[0-9]+") in
  match token with
  | "nil" -> Mal.Nil
  | "true" -> Mal.Bool true
  | "false" -> Mal.Bool false
  | num when Str.string_match num_re num 0 -> Mal.Int (Int.of_string num)
  | kw when String.is_prefix kw ~prefix:":" -> Mal.Keyword kw
  | str when String.is_prefix str ~prefix:"\"" -> Mal.String (Util.escape str)
  | s -> Mal.Symbol s

let rec read_form all_tokens =
  match all_tokens with
  | [] -> error "EOF"
  | token :: tokens ->
    match token with
    | "(" -> let (forms, tokens_rest) = read_seq ")" tokens [] in
      (Mal.List (forms, Mal.Nil), tokens_rest)
    | "[" -> let (forms, tokens_rest) = read_seq "]" tokens [] in
      (Mal.Vector (forms, Mal.Nil), tokens_rest)
    | "{" -> let (forms, tokens_rest) = read_seq "}" tokens [] in
      (Mal.Map (MalMap.from_list forms, Mal.Nil), tokens_rest)
    | "'" -> read_quote "quote" tokens
    | "`" -> read_quote "quasiquote" tokens
    | "~" -> read_quote "unquote" tokens
    | "~@" -> read_quote "splice-unquote" tokens
    | "@" -> read_quote "deref" tokens
    | "^" -> let (metadataForm, rest1) = read_form tokens in
      let (valueForm, rest2) = read_form rest1 in
      (Mal.List ([Mal.Symbol "with-meta"; valueForm; metadataForm], Mal.Nil), rest2)
    | _ -> (read_atom token, tokens)

and read_quote symbol tokens =
  let (form, tokens_rest) = read_form tokens in
  (Mal.List ([Mal.Symbol symbol; form], Mal.Nil), tokens_rest)

and read_seq end_tok (tokens : string list) exprs =
  match tokens with
  | [] -> error ("expected '" ^ end_tok ^ "', got EOF ")
  | token :: tokens when String.equal token end_tok -> (List.rev exprs, tokens)
  | _ -> let (form, tokens_rest) = read_form tokens in
    read_seq end_tok tokens_rest (form :: exprs)

let read str =
  let (form, _) = read_form @@ tokenize str in
  form
