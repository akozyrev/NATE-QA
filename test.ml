open OUnit2
open Counter
open Tokenizer
open Similarity
open Extract
open Yojson.Basic.Util




(* let j = Yojson.Basic.from_file "corpus/data.json" *)

(** tests for counter module --done*)
let counter_test = [
    "test_no_repeats1" >:: (fun _ -> assert_equal true 
    (Counter.no_repeats ["hello"; "world"]));

    "test_no_repeats2" >:: (fun _ -> assert_equal true 
    (Counter.no_repeats []));

    "test_no_repeats3" >:: (fun _ -> assert_equal false 
    (Counter.no_repeats ["hello"; "world"; "hello"]));

    "test_repOK1" >:: (fun _ -> assert_equal true 
    (Counter.rep_ok {dict = [("hello", 1); ("world", 1)]; length = 2}));

    "test_repOK2" >:: (fun _ -> assert_equal false 
    (Counter.rep_ok {dict = [("hello", 1); ("world", 1)]; length = 1}));

    "test_repOK3" >:: (fun _ -> assert_equal false 
    (Counter.rep_ok {dict = [("hello", 1); ("world", 1); ("world", 2)]; length = 2}));

    "test_is_empty1" >:: (fun _ -> assert_equal true 
    (Counter.is_empty {dict = []; length = 0}));

    "test_is_empty2" >:: (fun _ -> assert_equal false 
    (Counter.is_empty {dict = [("hello", 1); ("world", 2)]; length = 2}));

    "test_add_word1" >:: (fun _ -> assert_equal [("hello", 2); ("world", 1)]
    (Counter.add_word "hello"  [("hello", 1); ("world", 1)]) );

    "test_add_word2" >:: (fun _ -> assert_equal [("world", 1)]
    (Counter.add_word "world"  []) );

    "test_add_words1" >:: (fun _ -> assert_equal [("hello", 2); ("world", 1)]
    (Counter.add_words ["hello";"world";"hello"]  []) );

    "test_add_words2" >:: (fun _ -> assert_equal [("world", 1)]
    (Counter.add_words ["world"]  []));

    "test_make_dict1" >:: (fun _ -> assert_equal 
    ({dict = [("hello", 1);  ("world", 2)]; length = 2}:Counter.t )
    (Counter.make_dict ["hello"; "world"; "world"]));

    "test_make_dict2" >:: (fun _ -> assert_equal 
    ({dict = []; length = 0}:Counter.t )
    (Counter.make_dict []));

    "test_mem1" >:: (fun _ -> assert_equal false 
    (Counter.mem "hi" {dict = [("hello", 1); ("world", 2)]; length = 2}));

    "test_mem2" >:: (fun _ -> assert_equal true 
    (Counter.mem "hello" {dict = [("hello", 1); ("world", 2)]; length = 2}));

    "test_get_len" >:: (fun _ -> assert_equal 2 
    (Counter.get_length {dict = [("hello", 1); ("world", 2)]; length = 2}));

    "test_get_dict" >:: (fun _ -> assert_equal ([("hello", 1); ("world", 2)])
    (Counter.get_dictionary {dict = [("hello", 1); ("world", 2)]; length = 2}));

    "test_find_word" >:: (fun _ -> assert_equal 2
    (Counter.find_word "world" {dict = [("hello", 1); ("world", 2)]; length = 2} ) );
]

(** tests for tokenizer module --todo *)
let tokenizer_test = [
  "test_sent_tokenize" >:: (fun _ -> assert_equal ["The cat is fat"; "The cat ate the rat"] 
    (Tokenizer.sent_tokenize "The cat is fat. The cat ate the rat."));

]


(** tests for similarity module --todo*)
let similarity_test = []


(** tests for extract module *)
(*TODO*)
let extract_test = []


let suite =
  "test suite for extract"  >::: List.flatten [
    counter_test;
    tokenizer_test;
    similarity_test;
    extract_test;
  ]


let _ = run_test_tt_main suite