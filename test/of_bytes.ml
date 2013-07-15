open OUnit

type test = { bytes : string; res : Eterm.t; msg : string }

let seq s e =
  let rec seq' s e acc =
    if s <= e then
      seq' (s + 1) e (s::acc)
    else
      List.rev acc
  in
  seq' s e []

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
let lgt1       = { bytes = "\131i\000\000\001\000a\001a\002a\003a\004a\005a\006a\007a\ba\ta\na\011a\012a\ra\014a\015a\016a\017a\018a\019a\020a\021a\022a\023a\024a\025a\026a\027a\028a\029a\030a\031a a!a\"a#a$a%a&a'a(a)a*a+a,a-a.a/a0a1a2a3a4a5a6a7a8a9a:a;a<a=a>a?a@aAaBaCaDaEaFaGaHaIaJaKaLaMaNaOaPaQaRaSaTaUaVaWaXaYaZa[a\\a]a^a_a`aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayaza{a|a}a~a\127a\128a\129a\130a\131a\132a\133a\134a\135a\136a\137a\138a\139a\140a\141a\142a\143a\144a\145a\146a\147a\148a\149a\150a\151a\152a\153a\154a\155a\156a\157a\158a\159a\160a\161a\162a\163a\164a\165a\166a\167a\168a\169a\170a\171a\172a\173a\174a\175a\176a\177a\178a\179a\180a\181a\182a\183a\184a\185a\186a\187a\188a\189a\190a\191a\192a\193a\194a\195a\196a\197a\198a\199a\200a\201a\202a\203a\204a\205a\206a\207a\208a\209a\210a\211a\212a\213a\214a\215a\216a\217a\218a\219a\220a\221a\222a\223a\224a\225a\226a\227a\228a\229a\230a\231a\232a\233a\234a\235a\236a\237a\238a\239a\240a\241a\242a\243a\244a\245a\246a\247a\248a\249a\250a\251a\252a\253a\254a\255b\000\000\001\000"
		 ; res   = Eterm.Large_tuple (List.map (fun i -> Eterm.Small_int i) (seq 1 256))
		 ; msg   = "Large tuple failed"
		 }
let string1    = { bytes = "\131k\000\005hello"
		 ; res   = Eterm.String "hello"
		 ; msg   = "String failed"
		 }
let list1      = { bytes = "\131l\000\000\000\003b\000\000SIb\000\000\t\017b\000\000\001Aj"
		 ; res   = Eterm.List [ Eterm.Int (Int32.of_int 21321)
				      ; Eterm.Int (Int32.of_int 2321)
				      ; Eterm.Int (Int32.of_int 321)
				      ]
		 ; msg   = "List failed"
		 }
let binary1    = { bytes = "\131m\000\000\000\005hello"
		 ; res   = Eterm.Binary "hello"
		 ; msg   = "Binary failed"
		 }

let test_of_bytes test _ =
  match Eterm.of_bytes test.bytes with
    | (Some res, _) ->
      assert_equal
	~msg:test.msg
	(Eterm.compare test.res res)
	0
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
  ; "Large Tuple" >:: (test_of_bytes lgt1)
  ; "String"      >:: (test_of_bytes string1)
  ; "List"        >:: (test_of_bytes list1)
  ; "Binary"      >:: (test_of_bytes binary1)
  ]

let _ = run_test_tt_main suite
