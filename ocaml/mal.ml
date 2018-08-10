open Base;;

module rec Mal : sig
  type t =
    | Symbol of string
    | Keyword of string
    | String of string
    | Nil
    | Bool of bool
    | Int of int
    | List of t list * t
    | Vector of t list * t
    | Map of MalMap.t * t
    | Fn of (t list -> t) * t
    | Macro of (t list -> t) * t
    | Atom of t ref
  [@@deriving compare]
  val repr : bool -> t -> string
  val with_meta : t -> t -> t
  val to_seq : t -> t list
  val is_truthy : t -> bool
  val is_equal : t -> t -> bool
  val list : t list -> t
  val concat : t list -> t
  val append : t -> t -> t
end = struct
  include Mal
  let rec repr is_readable v =
    match v with
    | Int i -> Int.to_string i
    | Symbol s -> s
    | Keyword s -> s
    | Nil -> "nil"
    | Bool true -> "true"
    | Bool false -> "false"
    | String s ->
      if is_readable then
        "\"" ^ (Util.unescape s) ^ "\""
      else
        s
    | List (l, _) -> "(" ^ (String.concat ~sep:" " (List.map ~f:(fun s -> repr is_readable s) l)) ^ ")"
    | Vector (l, _) -> "[" ^ (String.concat ~sep:" " (List.map ~f:(fun s -> repr is_readable s) l)) ^ "]"
    | Map (m, _) -> "{" ^ (MalMap.repr_map is_readable m) ^ "}"
    | Fn _ -> "#fn"
    | Macro _ -> "#macro"
    | Atom r -> "(atom " ^ (repr is_readable !r) ^ ")"
  let with_meta v m =
    match v with
    | List (l, _) -> List (l, m)
    | Vector (v, _) -> Vector (v, m)
    | Map (t, _) -> Map (t, m)
    | x -> x
  let list l = Mal.List (l, Mal.Nil)
  let to_seq = function
  | List (l, _) -> l
  | Vector (v, _) -> v
  | _ -> []
  let is_truthy = function
  | Nil -> false
  | Bool b -> b
  | _ -> true
  let rec is_equal x y = match (x, y) with
  | (Mal.Symbol s, Mal.Symbol r) -> String.compare r s = 0
  | (Mal.Keyword s, Mal.Keyword r) -> String.compare r s = 0
  (* TODO: yuck.... *)
  | (Mal.List (g, _), Mal.List (h, _)) -> List.equal g h ~equal:is_equal
  | (Mal.Vector (g, _), Mal.List (h, _)) -> List.equal g h ~equal:is_equal
  | (Mal.List (g, _), Mal.Vector (h, _)) -> List.equal g h ~equal:is_equal
  | (Mal.Vector (g, _), Mal.Vector (h, _)) -> List.equal g h ~equal:is_equal
  | (Mal.Int i, Mal.Int j) -> i = j
  | (Mal.String s, Mal.String t) -> String.equal s t
  | (Mal.Bool a, Mal.Bool b) -> Bool.equal a b
  | (Mal.Nil, Mal.Nil) -> true
  | (Mal.Map (x, _), Mal.Map (y, _)) -> MalMap.is_equal x y
  | _ -> false
  let concat vs = list (vs |> List.map ~f:Mal.to_seq |> List.concat)
  let append x y = concat [x; y]
end
and MalMap : sig
  type t
  val empty : t
  val from_list : Mal.t list -> t
  val to_list : t -> (Mal.t * Mal.t) list
  val map : (Mal.t -> Mal.t) -> t -> t
  val repr_map : bool -> t -> string
  val is_equal : t -> t -> bool
  val keys : t -> Mal.t list
  val vals : t -> Mal.t list
  val assoc : t -> Mal.t -> Mal.t -> t
  val disassoc : t -> Mal.t -> t
  val get : t -> Mal.t -> Mal.t
  val member : t -> Mal.t -> bool
end = struct
  type t = (Mal.t * Mal.t) list
  let empty = []
  let to_list m = m
  let rec from_list = function
  | [] -> []
  | _ :: [] -> failwith "Bad map from_list"
  | key :: expr :: rest -> (key, expr) :: from_list rest
  let repr_map is_readable m =
    List.map ~f:(fun (k, v) -> ((Mal.repr is_readable k) ^ " " ^ (Mal.repr is_readable v))) m
    |> String.concat ~sep:" "
  let map f m = List.Assoc.map m ~f:f
  let rec is_subset x y =
    match x with
    | [] -> true
    | (key, value) :: rest ->
      match List.Assoc.find y ~equal:Mal.is_equal key with
      | Some value2 -> Mal.is_equal value value2 && is_subset rest y
      | None -> false
  let is_equal x y = (is_subset x y) && (is_subset y x)
  let keys = List.map ~f:fst
  let vals = List.map ~f:snd
  let assoc m k v = (k, v) :: m
  let disassoc m k = List.Assoc.remove m ~equal:Mal.is_equal k
  let member m k = List.Assoc.mem m ~equal:Mal.is_equal k
  let get m k = List.Assoc.find_exn m ~equal:Mal.is_equal k
end
