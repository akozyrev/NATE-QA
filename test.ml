open OUnit2
open Counter
open Tokenizer
open Similarity
open Extract
open Yojson.Basic.Util


(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2


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
    (Counter.rep_ok {dict = [("hello", 1); 
    ("world", 1)]; length = 2}));

    "test_repOK2" >:: (fun _ -> assert_equal false 
    (Counter.rep_ok {dict = [("hello", 1); 
    ("world", 1)]; length = 1}));

    "test_repOK3" >:: (fun _ -> assert_equal false 
    (Counter.rep_ok {dict = [("hello", 1); ("world", 1); 
    ("world", 2)]; length = 2}));

    "test_is_empty1" >:: (fun _ -> assert_equal true 
    (Counter.is_empty {dict = []; length = 0}));

    "test_is_empty2" >:: (fun _ -> assert_equal false 
    (Counter.is_empty {dict = [("hello", 1); 
    ("world", 2)]; length = 2}));

    "test_add_word1" >:: (fun _ -> assert_equal 
    [("hello", 2); ("world", 1)]
    (Counter.add_word "hello"  [("hello", 1); ("world", 1)]) );

    "test_add_word2" >:: (fun _ -> assert_equal 
    [("world", 1)]
    (Counter.add_word "world"  []) );

    "test_add_words1" >:: (fun _ -> assert_equal 
    [("hello", 2); ("world", 1)]
    (Counter.add_words ["hello";"world";"hello"]  []) );

    "test_add_words2" >:: (fun _ -> assert_equal 
    [("world", 1)]
    (Counter.add_words ["world"]  []));

    "test_make_dict1" >:: (fun _ -> assert_equal 
    ({dict = [("hello", 1);  ("world", 2)]; 
    length = 2}:Counter.t )
    (Counter.make_dict ["hello"; "world"; "world"]));

    "test_make_dict2" >:: (fun _ -> assert_equal 
    ({dict = []; length = 0}:Counter.t )
    (Counter.make_dict []));

    "test_mem1" >:: (fun _ -> assert_equal false 
    (Counter.mem "hi" {dict = [("hello", 1); 
    ("world", 2)]; length = 2}));

    "test_mem2" >:: (fun _ -> assert_equal true 
    (Counter.mem "hello" {dict = [("hello", 1); 
    ("world", 2)]; length = 2}));

    "test_get_len" >:: (fun _ -> assert_equal 2 
    (Counter.get_length {dict = [("hello", 1); 
    ("world", 2)]; length = 2}));

    "test_get_dict" >:: (fun _ -> assert_equal 
    ([("hello", 1); ("world", 2)])
    (Counter.get_dictionary {dict = [("hello", 1); 
    ("world", 2)]; length = 2}));

    "test_find_word" >:: (fun _ -> assert_equal 2
    (Counter.find_word "world" {dict = [("hello", 1); 
    ("world", 2)]; length = 2} ) );
]

(** tests for tokenizer module --done *)
let tokenizer_test = [
  "test_sent_tokenize" >:: (fun _ -> assert_equal 
  ["The cat is fat"; "The cat ate the rat"] 
    (Tokenizer.sent_tokenize "The cat is fat. The cat ate the rat."));

  "test_make_lower" >:: (fun _ -> assert_equal "rat" 
    (Tokenizer.make_lower "Rat"));

  "test_make_lower2" >:: (fun _ -> assert_equal "rat" 
    (Tokenizer.make_lower "rat"));

  "test_split_on_chars" >:: (fun _ -> assert_equal 
  ["hi"; ""; "my"; "name"; "is"; ""; "blahblahblah"; ""] 
    (Tokenizer.split_on_chars [';';':';'.';' '] 
    "hi; my name is: blahblahblah "));

  "test_word_tokenize" >:: (fun _ -> assert_equal ["hi"; 
        "my"; "name"; "is"; "blahblahblah"] 
    (Tokenizer.word_tokenize "hi; my name is: blahblahblah "));

  let tok = ({sentences = ["The cat is fat"; "The cat ate the rat"];
        words = ["the"; "cat"; "is"; "fat"; "the"; 
          "cat"; "ate"; "the"; "rat"] }:Tokenizer.t ) in

  "test_tokenize" >:: (fun _ -> assert_equal tok
        (Tokenizer.tokenize "The cat is fat. The cat ate the rat."));

  "test_get_sentences" >:: (fun _ -> assert_equal 
        ["The cat is fat"; "The cat ate the rat"]
        (Tokenizer.get_sentences tok));

  "test_get_words" >::  (fun _ -> assert_equal ["the"; 
          "cat"; "is"; "fat"; "the"; "cat"; "ate"; "the"; "rat"]
        (Tokenizer.get_words tok));

]


(** tests for similarity module --done*)
let similarity_test = [
  "test_dot_prod" >:: (fun _ -> assert_equal 
      25 (Similarity.dot_prod [3;4] [3;4]));

  "test_dot_prod2" >:: (fun _ -> assert_equal 
      0 (Similarity.dot_prod [0] [0]));

  "test_norm" >:: (fun _ -> assert_equal 
      true ((Similarity.norm [3;4]) -. 25.0 < 0.01) );

  "test_norm2" >:: (fun _ -> assert_equal 
      0.0 (Similarity.norm [0;0;0]));

  "test_remove_dups" >:: (fun _ -> assert_equal 
      [1;2;3] (Similarity.remove_dups [1;2;3]));

  "test_remove_dups2" >:: (fun _ -> assert_equal 
      [0] (Similarity.remove_dups [0;0;0]));

  "test_union" >:: (fun _ -> assert_equal true 
      (cmp_set_like_lists [1;2;3] (Similarity.union [1;2] [3])));

  "test_union2" >:: (fun _ -> assert_equal true 
      (cmp_set_like_lists [1;2] (Similarity.union [1;2] [])));

  "test_union3" >:: (fun _ -> assert_equal true 
      (cmp_set_like_lists [1;2] (Similarity.union [1;2] [1;2])));

  "test_ucardinality1" >:: (fun _ -> 
      assert_equal 3 (Similarity.union_cardinality [1;2] [3]));

  "test_ucardinality2" >:: (fun _ -> 
      assert_equal 2 (Similarity.union_cardinality [1;2] []));

  "test_ucardinality3" >:: (fun _ -> 
      assert_equal 2 (Similarity.union_cardinality [1;2] [1;2]));

  "test_icardinality1" >:: (fun _ -> 
      assert_equal 0 (Similarity.intersect_cardinality [1;2] [3]));

  "test_icardinality2" >:: (fun _ -> 
      assert_equal 0 (Similarity.intersect_cardinality [1;2] []));

  "test_icardinality3" >:: (fun _ -> 
      assert_equal 2 (Similarity.intersect_cardinality [1;2] [1;2]));

  (*note: we are not testing the jaccard similarity or cosine similarity functions.
  instead, we test all helper functions they use and ensure the they are correct
  by implementing the correct equations witin them*)

]


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