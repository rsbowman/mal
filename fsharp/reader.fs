module Reader

open System
open System.Text.RegularExpressions

let (|Int|_|) (s: string) =
    match Int64.TryParse s with
        | true, n -> Some n
        | _ -> None

let tokenize input =
    let malRe = Regex(@"[\s,]*(~@|[\[\]{}()'`~^@]|""(\\.|[^\\""])*""|;.*|[^\s\[\]{}('""`,;)]*)")
    malRe.Matches(input)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Seq.filter (fun s -> not <| (s.StartsWith ";" || s = ""))

let makeMalMap plist =
    let rec loop (m : Map<Mal.T, Mal.T>) = function
    | [] -> m
    | key :: expr :: rest -> loop (m.Add(key, expr)) rest
    | x -> failwith ("Bad map: " + (List.map Mal.repr x |> String.concat ", "))
    in loop Map.empty plist

let readAtom = function
    | "nil" -> Mal.Nil
    | "true" -> Mal.Bool true
    | "false" -> Mal.Bool false
    | Int i -> Mal.Int i
    | kw when kw.StartsWith(":") -> Mal.Keyword kw
    | s when s.StartsWith("\"") -> Mal.String <| Mal.unescape s
    | s -> Mal.Symbol s

let rec readForm =
  function
  | [] -> Error.malErrorStr "EOF"
  | token :: tokens ->
    match token with
    | "(" -> let (forms, restTokens) = readSeq ")" tokens [] in
             (Mal.List (forms, Mal.Nil), restTokens)
    | "[" -> let (forms, restTokens) = readSeq "]" tokens [] in
             (Mal.Vector (forms, Mal.Nil), restTokens)
    | "{" -> let (forms, restTokens) = readSeq "}" tokens [] in
             (Mal.map (makeMalMap forms), restTokens)
    | "'" -> readQuote "quote" tokens
    | "`" -> readQuote "quasiquote" tokens
    | "~" -> readQuote "unquote" tokens
    | "~@" ->readQuote "splice-unquote" tokens
    | "@" -> readQuote "deref" tokens
    | "^" -> let (metadataForm, rest1) = readForm tokens in
             let (valueForm, rest2) = readForm rest1 in
             (Mal.List ([Mal.Symbol "with-meta"; valueForm; metadataForm], Mal.Nil), rest2)
    | _ -> (readAtom token, tokens)

and readQuote symbol tokens =
  let (form, restTokens) = readForm tokens in
  (Mal.List ([Mal.Symbol symbol; form], Mal.Nil), restTokens)

and readSeq endTok tokens exprs =
    match tokens with
        | [] -> Error.malErrorStr ("Expected " + endTok + ", got EOF")
        | token :: tokens when endTok = token -> (List.rev exprs, tokens)
        | _ -> let (form, restTokens) = readForm tokens
               readSeq endTok restTokens (form :: exprs)
let read str =
  let (form, _) = tokenize str |> Seq.toList |> readForm in form

let eval env ast = ast
