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
  | New_ref       of (t * int32 * int)
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
      match consume_n (Int32.of_int nelems) rest with
	| (Some elems, rest) ->
	  (Some (Small_tuple elems), rest)
	| (None, rest) ->
	  (None, rest)
    )
    | { 105    : 8
      ; nelems : 4 * 8 : bigendian
      ; rest   : -1    : bitstring
      } -> (
      match consume_n nelems rest with
	| (Some elems, rest) ->
	  (Some (Large_tuple elems), rest)
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
    if Int32.compare n Int32.zero > 0 then begin
      match parse_data bs with
	| (Some e, rest) ->
	  consume_n' (Int32.pred n) rest (e::acc)
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

let promote = function
  | Small_int n ->
    Int (Int32.of_int n)
  | Small_tuple t ->
    Large_tuple t
  | Small_big_int n ->
    Large_big_int n
  | Small_atom a ->
    Atom a
  | Ref r ->
    New_ref r
  | t ->
    t

let rec compare t1 t2 =
  match (promote t1, promote t2) with
    (* Numbers *)
    | (Int n1, Int n2) ->
      Int32.compare n1 n2
    | (Int n1, Float n2) ->
      Int32.compare n1 (Int32.of_float (float_of_string n2))
    | (Float n1, Int n2) ->
      Int32.compare (Int32.of_float (float_of_string n1)) n2
    | (Float n1, Float n2) ->
      String.compare n1 n2
    | (Large_big_int _, Large_big_int _)
    | (Large_big_int _, Float _)
    | (Large_big_int _, Int _)
    | (Float _, Large_big_int _)
    | (Int _, Large_big_int _) ->
      failwith "nyi"
    | (Int _, _)
    | (Float _, _)
    | (Large_big_int _, _)->
      -1
    | (_, Int _)
    | (_, Float _)
    | (_, Large_big_int _) ->
      1

    (* Atom *)
    | (Atom a1, Atom a2) ->
      String.compare a1 a2
    | (Atom _, _) ->
      -1
    | (_, Atom _) ->
      1

    (* Reference *)
    | (New_ref r1, New_ref r2) ->
      failwith "nyi"
    | (New_ref _, _) ->
      -1
    | (_, New_ref _) ->
      1

    (* Fun - NYI *)

    (* Port *)
    | (Port p1, Port p2) ->
      String.compare p1 p2
    | (Port _, _) ->
      -1
    | (_, Port _) ->
      1

    (* Pid *)
    | (Pid p1, Pid p2) ->
      String.compare p1 p2
    | (Pid _, _) ->
      -1
    | (_, Pid _) ->
      1

    (* Tuple *)
    | (Large_tuple t1, Large_tuple t2) ->
      compare_seq t1 t2
    | (Large_tuple _, _) ->
      -1
    | (_, Large_tuple _) ->
      1

    (* Nil List *)
    | (Nil, Nil) ->
      0
    | (Nil, _) ->
      -1
    | (_, Nil) ->
      1

    (* List *)
    | (List l1, List l2) ->
      compare_seq l1 l2
    | (List _, _) ->
      -1
    | (_, List _) ->
      1

    (* Binary *)
    | (Binary b1, Binary b2) ->
      String.compare b1 b2
    | (_, Binary _) ->
      -1

    (* Should never make it this far *)
    | (_, _) ->
      failwith "Failed in Eterm.compare"
and compare_seq s1 s2 =
  match (s1, s2) with
    | ([], []) ->
      0
    | ([], _) ->
      -1
    | (_, []) ->
      1
    | (s1::ss1, s2::ss2) ->
      let c = compare s1 s2 in
      if c = 0 then
	compare_seq ss1 ss2
      else
	c

