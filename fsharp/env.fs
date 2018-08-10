module Env

open System
open System.Collections.Generic

type Bindings = Dictionary<string, Mal.T>
type Env = Bindings list

let set (env : Env) s expr =
    match env with
        | b :: _ -> b.[s] <- expr
        | [] -> Error.evalError "Can't set empty env"

let rec get (env : Env) s =
    match env with
    | [] -> None
    | b :: renv ->
        match b.TryGetValue(s) with
        | true, v -> Some v
        | _ -> get renv s

let ofList pairs : Bindings =
    pairs
    |> List.fold (fun (m : Bindings) (name, func) -> m.Add(name, func); m) (Bindings())

let push env names values =
    let newBindings = Bindings()
    let rec loop ns vs =
        match ns, vs with
        | [], [] -> newBindings
        | "&"::restName::_, restValues ->
            newBindings.Add(restName, Mal.list restValues);
            newBindings
        | n::nns, v::vvs ->
            newBindings.Add(n, v)
            loop nns vvs
        | ns, vs ->
            Console.WriteLine(ns)
            Console.WriteLine(Mal.repr <| Mal.list vs)
            Error.evalError "Non matching binding names/values or weird rest arg stuff"
    (loop names values) :: env
