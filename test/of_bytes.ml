open OUnit

type test = { bytes : string; res : Eterm.t; msg : string }

let small_int1 = { bytes = "\131a\001"
		 ; res   = Eterm.Small_int 1
		 ; msg   = "(Small_int 1)"
		 }
let int1       = { bytes = "\131b\000\000\001\000"
		 ; res   = Eterm.Int (Int32.of_int 256)
		 ; msg   = "(Int 256)"
		 }
let float1     = { bytes = "\131c1.19999999999999995559e+00\000\000\000\000\000"
		 ; res   = Eterm.Float "1.19999999999999995559e+00"
		 ; msg   = "(Float \"1.19999999999999995559e+00\")"
		 }
let atom1      = { bytes = "\131d\000\005hello"
		 ; res   = Eterm.Atom "hello"
		 ; msg   = "(Atom \"hello\")"
		 }
let ref1       = { bytes = "\131r\000\003d\000\rnonode@nohost\000\000\000\000a\000\000\000\000\000\000\000\000"
		 ; res   = Eterm.Small_int 1
		 ; msg   = "references broken"
		 }
let nil1       = { bytes = "\131j"
		 ; res   = Eterm.Nil
		 ; msg   = "(Eterm.Nil)"
		 }
let smt1       = { bytes = "\131h\002a\001c2.00000000000000000000e+00\000\000\000\000\000"
		 ; res   = Eterm.Small_tuple [ Eterm.Small_int 1
					     ; Eterm.Float "2.00000000000000000000e+00"
					     ]
		 ; msg   = "Small tuple failed"
		 }

let test_of_bytes test _ =
  match Eterm.of_bytes test.bytes with
    | (Some res, _) ->
      assert_equal
	~msg:test.msg
	test.res
	res
    | _ ->
      failwith "Fail"

let suite = "Eterm Test" >:::
  [ "Small Int"   >:: (test_of_bytes small_int1)
  ; "Int"         >:: (test_of_bytes int1)
  ; "Float"       >:: (test_of_bytes float1)
  ; "Atom"        >:: (test_of_bytes atom1)
  ; "Ref"         >:: (test_of_bytes ref1)
  ; "Nil"         >:: (test_of_bytes nil1)
  ; "Small Tuple" >:: (test_of_bytes smt1)
  ]

let _ = run_test_tt_main suite
