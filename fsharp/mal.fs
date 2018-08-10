module Mal
    open System

    let wrap ldelim rdelim s = ldelim + s + rdelim

    let unescape (s : string) =
        let b = System.Text.StringBuilder()
        let rec lp = function
            | [] -> b.ToString()
            | '\\'::'\"'::rest -> b.Append("\"") |> ignore; lp rest
            | '\\'::'\\'::rest -> b.Append("\\") |> ignore; lp rest
            | '\\'::'n'::rest -> b.Append("\n") |> ignore; lp rest
            | c::cs -> b.Append(c) |> ignore; lp cs
        s.[1..(s.Length - 2)] |> Seq.toList |> lp

    let escape s =
        let b = System.Text.StringBuilder()
        // TODO: use String.collect here
        let rec lp = function
            | [] -> b.ToString()
            | '\"'::rest -> b.Append("\\\"") |> ignore; lp rest
            | '\\'::rest -> b.Append("\\\\") |> ignore; lp rest
            | '\n'::rest -> b.Append("\\n") |> ignore; lp rest
            | c::cs -> b.Append(c) |> ignore; lp cs
        lp <| Seq.toList s

    [<CustomEquality; CustomComparison>]
    type T =
        | Nil
        | Bool of bool
        | Int of Int64
        | String of String
        | Keyword of String
        | Symbol of String
        | List of T list * T
        | Vector of T list * T
        | Map of Map<T, T> * T
        | Fn of (T list -> T) * T
        | Macro of (T list -> T) * T
        | Atom of T ref

        // Warning!  These are awful, not real equality or comparison by any means.  The deal is
        // that we only need string, keyword, and (maybe?) symbol map keys, so we only define
        // equality/comparison for those.  Whatever, it works without being a big pain.  This is for
        // fun.
        static member private listEqual a b =
            List.length a = List.length b && List.forall2 (=) a b

        static member private equals x y =
            match x, y with
            | Nil, Nil -> true
            | Bool a, Bool b -> a = b
            | Int a, Int b -> a = b
            | String a, String b -> a = b
            | Keyword a, Keyword b -> a = b
            | Symbol a, Symbol b -> a = b
            | List(a, _), List(b, _) -> T.listEqual a b
            | Vector(a, _), List(b, _) -> T.listEqual a b
            | List(a, _), Vector(b, _) -> T.listEqual a b
            | Vector(a, _), Vector(b, _) -> T.listEqual a b
            | Map(a, _), Map(b, _) -> a = b
            | Atom a, Atom b -> a = b
            | _ -> false

        static member private compare x y =
            match x, y with
            | Nil, Nil -> 0
            | Bool a, Bool b -> compare a b
            | Int a, Int b -> compare a b
            | String a, String b -> compare a b
            | Keyword a, Keyword b -> compare a b
            | Symbol a, Symbol b -> compare a b
            | Atom a, Atom b -> compare a b
            | _ -> 0

        override x.Equals(obj) =
            match obj with
            | :? T as y -> T.equals x y
            | _ -> false

        override x.GetHashCode() =
            match x with
            | Nil -> 0
            | Bool b -> hash b
            | Int x -> hash x
            | String s -> hash s
            | Keyword s -> hash s
            | Symbol s -> hash s
            | Atom s -> hash s
            | _ -> 0

        interface System.IComparable with
            member x.CompareTo obj =
                match obj with
                | :? T as y -> T.compare x y
                | _ -> invalidArg "obj" "Cannot compare values of different types."

    let rec reprHelper isPrintable = function
        | Nil -> "nil"
        | Bool true -> "true"
        | Bool false -> "false"
        | String s ->
            if isPrintable then
                "\"" + (escape s) + "\""
            else
                s
        | Int i -> string i
        | Keyword kw -> kw
        | Symbol s -> s
        | List (l, _) -> l |> List.map (reprHelper isPrintable) |> String.concat " " |> wrap "(" ")"
        | Vector (l, _) -> l |> List.map (reprHelper isPrintable) |> String.concat " " |> wrap "[" "]"
        | Map(m, _) -> m |> Map.toList
                     |> List.map (fun (k, v) -> (reprHelper isPrintable k) + " " + (reprHelper isPrintable v))
                     |> String.concat " "
                     |> wrap "{" "}"
        | Fn _ -> "#fn"
        | Macro _ -> "#macro"
        | Atom a -> reprHelper isPrintable !a |> wrap "(atom " ")"

    let repr = reprHelper true
    // ?? what to call reprHelper false?
    // let repr = reprHelper true
    let list l = List (l, Nil)

    let toList = function
        | List(l, _) -> l
        | Vector(l, _) -> l
        | _ -> []

    let truthy = function
        | Nil -> false
        | Bool b -> b
        | _ -> true

    let append x y = List.append (toList x) (toList y) |> list
    let map x = Map(x, Nil)
