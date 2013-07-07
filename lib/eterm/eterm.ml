type t =
  | Small_int     of int
  | Int           of int
  | Float         of float
  | Atom          of string
  | Ref           of string
  | Port          of string
  | Pid           of string
  | Small_tuple   of t list
  | Large_tuple   of t list
  | String        of string
  | List          of t list
  | Binary        of string
  | Small_big_int of string
  | Large_big_int of string
  | New_ref       of string
  | Small_atom    of string
  | Nil


let of_binary s = failwith "nyi"

let to_binary t = failwith "nyi"

let to_string t = failwith "nyi"
