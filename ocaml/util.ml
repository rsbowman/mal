open Base;;

let sub_all re f str =
  Str.full_split re str
  |> List.map ~f:(function | Str.Delim x -> f x | Str.Text x -> x)
  |> String.concat ~sep:""

let unescape s =
  (sub_all (Str.regexp "\\([\"\\\n]\\)")
     (function
     | "\n" -> "\\n"
     | x -> "\\" ^ x)
     s)

let escape s =
  (sub_all (Str.regexp "\\\\.")
     (function
     | "\\n" -> "\n"
     | x -> String.sub x 1 1)
     (String.sub s 1 ((String.length s) - 2)))
