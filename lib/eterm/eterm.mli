type t =
  | Small_int of int
  | Int       of int32
  | Float     of float
  | Atom      of string
  | Tuple     of t list
  | String    of string
  | List      of t list
  | Binary    of string
  | Big_int   of Num.num
  | Nil

val of_bitstring : Bitstring.bitstring -> (t option * Bitstring.bitstring)
val to_bitstring : t -> Bitstring.bitstring

val of_bytes     : string -> (t option * Bitstring.bitstring)
val to_bytes     : t -> string

val of_string    : string -> t option
val to_string    : t -> string

val to_string_pp : ?max_width:int -> t -> string

val compare      : t -> t -> int
