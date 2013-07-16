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

let cut_at_null s =
  match String.index s '\000' with
    | -1 ->
      s
    | n ->
      String.sub s 0 n

let convert_num num =
  let b = Num.num_of_int 256 in
  let rec convert_num' n = function
    | i when i < String.length num ->
      convert_num'
	Num.(n +/ (num_of_int (Char.code num.[i]) */ (b **/ num_of_int i)))
	(i + 1)
    | _ ->
      n
  in
  convert_num' (Num.num_of_int 0) 0

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
    | { 107    : 8
      ; len    : 2 * 8   : bigendian
      ; string : len * 8 : string
      ; rest   : -1      : bitstring
      } ->
      (Some (String string), rest)
    | { 108   : 8
      ; len   : 4 * 8   : bigendian
      ; rest  : -1      : bitstring
      } -> (
      match consume_n len rest with
	| (Some elems, rest) ->
	  (Some (List elems), rest)
	| (None, _) ->
	  (None, rest)
    )
    | { 109    : 8
      ; len    : 4 * 8                : bigendian
      ; string : Int32.to_int len * 8 : string
      ; rest   : -1                   : bitstring
      } ->
      (Some (Binary string), rest)
    | { 110  : 8
      ; len  : 8
      ; sign : 8
      ; num  : len * 8 : string
      ; rest : -1      : bitstring
      } ->
      let big_num = convert_num num in
      let big_num =
	if sign = 0 then
	  big_num
	else
	  Num.minus_num big_num
      in
      (Some (Small_big_int big_num), rest)
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

let of_string s =
  failwith "nyi"

let join_str ~sep =
  let rec join_str' acc = function
  | [] ->
    acc
  | [x] ->
    acc ^ x
  | x::xs ->
    join_str' (acc ^ x ^ sep) xs
  in
  join_str' ""

let list_of_string s =
  let rec list_of_string' l = function
    | i when i < String.length s ->
      (s.[i])::l
    | _ ->
      List.rev l
  in
  list_of_string' [] 0

let join_binary b =
  join_str
    ~sep:","
    (List.map
       (fun c -> string_of_int (Char.code c))
       (list_of_string b))

let rec is_proper = function
  | [] ->
    true
  | [Nil] ->
    true
  | [_] ->
    false
  | _::xs ->
    is_proper xs

let remove_tail =
  let rec remove_tail' acc = function
    | [] ->
      None
    | [x] ->
      Some (List.rev acc, x)
    | x::xs ->
      remove_tail' (x::acc) xs
  in
  remove_tail' []

let rec to_string = function
  | Small_int n ->
    string_of_int n
  | Int n ->
    Int32.to_string n
  | Float n ->
    n
  | Atom atom
  | Small_atom atom ->
    atom
  | Small_tuple tuple
  | Large_tuple tuple ->
    "{" ^ join_tuple tuple ^ "}"
  | String s ->
    "\"" ^ s ^ "\""
  | List l ->
    "[" ^ join_list l ^ "]"
  | Binary b ->
    "<<" ^ join_binary b ^ ">>"
  | Small_big_int n
  | Large_big_int n ->
    Num.string_of_num n
  | Nil ->
    "[]"
  | _ ->
    failwith "nyi"
and join_tuple t =
  join_str ~sep:"," (List.map to_string t)
and join_list l =
  if is_proper l then
    join_proper_list l
  else
    join_improper_list l
and join_proper_list l =
  match remove_tail l with
    | Some (l, _) ->
      join_str ~sep:"," (List.map to_string l)
    | None ->
      ""
and join_improper_list l =
  match remove_tail l with
    | Some (l, tail) ->
      join_str ~sep:"," (List.map to_string l) ^ "|" ^ to_string tail
    | None ->
      ""

let promote = function
  | Small_int n ->
    Large_big_int (Num.num_of_int n)
  | Int n ->
    Large_big_int (Num.num_of_big_int (Big_int.big_int_of_int32 n))
  | Small_big_int n ->
    Large_big_int n
  | Small_tuple t ->
    Large_tuple t
  | Small_atom a ->
    Atom a
  | Ref r ->
    New_ref r
  | t ->
    t

let polymorphic_compare = compare

let rec compare t1 t2 =
  match (promote t1, promote t2) with
    (* Numbers *)
    | (Large_big_int n1, Large_big_int n2) ->
      Num.compare_num n1 n2
    | (Large_big_int n1, Float n2) ->
      let int64   = Int64.of_float (float_of_string n2) in
      let big_int = Big_int.big_int_of_int64 int64 in
      let num     = Num.num_of_big_int big_int in
      Num.compare_num n1 num
    | (Float n1, Large_big_int n2) ->
      let int64   = Int64.of_float (float_of_string n1) in
      let big_int = Big_int.big_int_of_int64 int64 in
      let num     = Num.num_of_big_int big_int in
      Num.compare_num num n2
    | (Float n1, Float n2) ->
      polymorphic_compare (float_of_string n1) (float_of_string n2)
    | (Float _, _)
    | (Large_big_int _, _)->
      -1
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
    | (String s1, String s2) ->
      String.compare s1 s2
    | (String _, List _)
    | (List _, String _) ->
      failwith "nyi"
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

