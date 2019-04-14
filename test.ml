open OUnit2
open Extract
open Yojson.Basic.Util




let j = Yojson.Basic.from_file "corpus/data.json"

let extract_test = []


let suite =
  "test suite for extract"  >::: List.flatten [
    extract_test;
  ]


let _ = run_test_tt_main suite