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
  | Small_big_int of string
  | Large_big_int of string
  | New_ref       of string
  | Small_atom    of string
  | Nil

let cut_at_null s =
  match String.index s '\000' with
    | -1 ->
      s
    | n ->
      String.sub s 0 n

let rec parse_data bs =
  bitmatch bs with
    | { 97   : 8
      ; num  : 8
      ; rest : -1 : bitstring
      } ->
      (Some (Small_int num), rest)
    | { 98   : 8
      ; num  : 32 : bigendian
      ; rest : -1 : bitstring
      } ->
      (Some (Int num), rest)
    | { 99    : 8
      ; float : 31 * 8 : string
      ; rest  : -1     : bitstring
      } ->
      (Some (Float (cut_at_null float)), rest)
    | { 100  : 8
      ; len  : 2 * 8   : bigendian
      ; atom : len * 8 : string
      ; rest : -1      : bitstring
      } ->
      (Some (Atom atom), rest)
    | { 101  : 8
      ; rest : -1 : bitstring
      } -> (
      match parse_data rest with
	| (None, _) ->
	  (None, bs)
	| (Some node, rest) -> (
	  bitmatch rest with
	    | { id       : 4 * 8 : bigendian
	      ; creation : 1 * 8
	      ; rest     : -1 : bitstring
	      } ->
	      (Some (Ref (node, id, creation)), rest)
	    | { _ } ->
	      (None, rest)
	)
    )
    | { 104    : 8
      ; nelems : 8  : bigendian
      ; rest   : -1 : bitstring
      } -> (
      match consume_n nelems rest with
	| (Some elems, rest) ->
	  (Some (Small_tuple elems), rest)
	| (None, rest) ->
	  (None, rest)
    )
    | { 106   : 8
      ; rest  : -1 : bitstring
      } ->
      (Some Nil, rest)
    | { _ } ->
      (None, bs)
and consume_n n bs =
  let rec consume_n' n bs acc =
    if n > 0 then begin
      match parse_data bs with
	| (Some e, rest) ->
	  consume_n' (n - 1) rest (e::acc)
	| (None, rest) ->
	  (None, rest)
    end
    else
      (Some (List.rev acc), bs)
  in
  consume_n' n bs []

let of_bitstring bs =
  bitmatch bs with
    | { 131  : 8
      ; rest : -1 : bitstring
      } ->
      parse_data rest
    | { _ } ->
      (None, bs)

let to_bitstring t =
  failwith "nyi"

let of_bytes b =
  of_bitstring (Bitstring.bitstring_of_string b)

let to_bytes t =
  failwith "nyi"

