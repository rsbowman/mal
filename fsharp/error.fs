module Error

exception ParseException of string
exception ParseEofException

exception MalException of Mal.T
exception EvalException of string

let evalError msg = raise <| EvalException msg
let malErrorStr s = raise <| MalException (Mal.String s)
let malError form = raise <| MalException form
