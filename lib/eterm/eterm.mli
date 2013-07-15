type t =
  | Small_int     of int
  | Int           of int32
  | Float         of string
  | Atom          of string
  | Ref           of (t * int32 * int)
  | Port          of string
  | Pid           of string
  | Small_tuple   of t list
  | Large_tuple   of t list
  | String        of string
  | List          of t list
  | Binary        of string
  | Small_big_int of Num.num
  | Large_big_int of Num.num
  | New_ref       of (t * int32 * int)
  | Small_atom    of string
  | Nil

val of_bitstring : Bitstring.bitstring -> (t option * Bitstring.bitstring)
val to_bitstring : t -> Bitstring.bitstring

val of_bytes     : string -> (t option * Bitstring.bitstring)
val to_bytes     : t -> string

val compare      : t -> t -> int
