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

module Indent = struct
  type t = { indent    : int list
	   ; max_width : int
	   ; width     : int
	   ; s         : string
	   }

  let create ~max_width () =
    { indent = [0]; max_width; width = 0; s = "" }

  let push_indent t = { t with indent = t.width::t.indent }
  let pop_indent = function
    | { indent = [] } as t ->
      t
    | { indent = x::xs } as t ->
      { t with indent = xs }

  let get_indent = function
    | { indent = [] } ->
      failwith "get_indent"
    | { indent = x::_ } ->
      x

  let add_string s t =
    let s_len = String.length s in
    if s_len + t.width < t.max_width then
      { t with s = t.s ^ s; width = t.width + s_len }
    else
      let indent = String.make (get_indent t) ' ' in
      let s'     = indent ^ s in
      { t with s = t.s ^ "\n" ^ s'; width = String.length s' }

  let get_string t = t.s
end

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
      (Some (Float (float_of_string (cut_at_null float))), rest)
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
	      (* (Some (Ref (node, id, creation)), rest) *)
	      failwith "nyi"
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
	  (Some (Tuple elems), rest)
	| (None, rest) ->
	  (None, rest)
    )
    | { 105    : 8
      ; nelems : 4 * 8 : bigendian
      ; rest   : -1    : bitstring
      } -> (
      match consume_n nelems rest with
	| (Some elems, rest) ->
	  (Some (Tuple elems), rest)
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
      (Some (Big_int big_num), rest)
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

let to_string_pp ?(max_width = 40) t =
  let rec to_string_pp' indent = function
    | Small_int n ->
      Indent.add_string (string_of_int n) indent
    | Int n ->
      Indent.add_string (Int32.to_string n) indent
    | Float n ->
      Indent.add_string (string_of_float n) indent
    | Atom atom ->
      Indent.add_string atom indent
    | Tuple tuple ->
      indent |>
	  Indent.add_string "{" |>
	      Indent.push_indent |>
		  join_tuple tuple |>
		      Indent.add_string "}"  |>
			  Indent.pop_indent
    | String s ->
      Indent.add_string ("\"" ^ s ^ "\"") indent
    | List l ->
      indent |>
	  Indent.add_string "[" |>
	      Indent.push_indent |>
		  join_list l |>
		      Indent.add_string "]"  |>
			  Indent.pop_indent
    | Binary b ->
      indent |>
	  Indent.add_string "<<" |>
	      Indent.push_indent |>
		  join_binary b |>
		      Indent.add_string ">>"  |>
			  Indent.pop_indent
    | Big_int n ->
      Indent.add_string (Num.string_of_num n) indent
    | Nil ->
      Indent.add_string "[]" indent
  and join_tuple tuple indent =
    join ~sep:"," indent tuple
  and join_list l indent =
    if is_proper l then
      join_proper_list l indent
    else
      join_improper_list l indent
  and join_proper_list l indent =
    match remove_tail l with
      | Some (l, _) ->
	join ~sep:"," indent l
      | None ->
	Indent.add_string "" indent
  and join_improper_list l indent =
    match remove_tail l with
      | Some (l, tail) ->
	join ~sep:"," indent l |>
	    Indent.add_string "|" |>
		(fun indent -> to_string_pp' indent tail)
      | None ->
	Indent.add_string "" indent
  and join_binary b indent =
    let b =
      List.map
	(fun c -> Small_int (Char.code c))
	(list_of_string b)
    in
    join ~sep:"," indent b
  and join ~sep indent = function
    | [] ->
      Indent.add_string "" indent
    | [x] ->
      to_string_pp' indent x
    | x::xs ->
      let indent =
	to_string_pp' indent x |>
	    Indent.add_string sep
      in
      join ~sep indent xs
  in
  Indent.get_string (to_string_pp' (Indent.create ~max_width ()) t)

let to_string = to_string_pp ~max_width:100000

let polymorphic_compare = compare

let rec compare_seq s1 s2 =
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

let rec compare t1 t2 =
  match (t1, t2) with
    (* Numbers *)
    | (Big_int n1, Big_int n2) ->
      Num.compare_num n1 n2
    | (Big_int n1, Float n2) ->
      let int64   = Int64.of_float n2 in
      let big_int = Big_int.big_int_of_int64 int64 in
      let num     = Num.num_of_big_int big_int in
      Num.compare_num n1 num
    | (Float n1, Big_int n2) ->
      let int64   = Int64.of_float n1 in
      let big_int = Big_int.big_int_of_int64 int64 in
      let num     = Num.num_of_big_int big_int in
      Num.compare_num num n2
    | (Float n1, Float n2) ->
      polymorphic_compare n1 n2
    | (Float _, _)
    | (Big_int _, _)->
      -1
    | (_, Float _)
    | (_, Big_int _) ->
      1

    (* Atom *)
    | (Atom a1, Atom a2) ->
      String.compare a1 a2
    | (Atom _, _) ->
      -1
    | (_, Atom _) ->
      1

    (* Reference - NYI *)

    (* Fun - NYI *)

    (* Port - NYI *)

    (* Pid - NYI *)

    (* Tuple *)
    | (Tuple t1, Tuple t2) ->
      compare_seq t1 t2
    | (Tuple _, _) ->
      -1
    | (_, Tuple _) ->
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

