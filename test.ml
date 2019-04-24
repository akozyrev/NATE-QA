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


(** tests for tokenizer module --done *)
let tok = ({sentences = ["The cat is fat"; "The cat ate the rat"];
            words = ["the"; "cat"; "is"; "fat"; "the";
                     "cat"; "ate"; "the"; "rat"] }:Tokenizer.t )
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

let q_vector_test = vectorize ["Who"; "is"; "David"; "Gries?"] 33144 (word2vec_dict 33144)
(* tests for extract module *)
let extract_test = [
  "test_counter_word_in_topic_in_real" >:: (fun _ ->
      assert_equal (count_word_in_topic "youtube" "youTube"  )
        (365) );

  "test_which_dict_has_the_words_1" >:: (fun _ -> assert_equal
                                            (get_topics (which_dict_has_the_word "spacex" all_topic_dict_counter []))
                                            (["Artificial Intelligence"; "Elon Musk"]) );

  "test_which_dict_has_the_words_2" >:: (fun _ ->
      assert_equal (get_topics (which_dict_has_the_word
                                  "askdnjaksjdaojd" all_topic_dict_counter []))
        ([]) );

  "test_max_jaccard_sentence">:: (fun _ ->
      assert_equal
        (max_jaccard_sentence "David Gries" "Where does David Gries live?")
        "David Gries currently lives in Ithaca, New York.");

  "test_vocab_size">:: (fun _ ->
      assert_equal
        (vocab_size) 33144; Pervasives.print_int vocab_size);

  "test_find_max_cosine" >:: (fun _ ->
      assert_equal
        (find_max_cosine "David Gries" ["who"; "is"; "david"; "gries"] q_vector_test [""] 0.0) ["Fuck off"]);
]


(* tests for counter module --done *)

let empty_dict = Counter.get_dictionary (Counter.make_dict [])
let word_dict = Hashtbl.create 5

let counter_test = [

  "test_add_word1" >:: (fun _ -> assert_equal
                           (Hashtbl.add word_dict "hello" 1; word_dict)
                           (Counter.add_word "hello"  word_dict) );

  "test_add_word2" >:: (fun _ -> assert_equal
                           (Hashtbl.add word_dict "world" 1; word_dict)
                           (Counter.add_word "world"  word_dict) );

  "test_add_words" >:: (fun _ -> assert_equal
                           (Hashtbl.add word_dict "world" 2;
                            Hashtbl.add word_dict "hello" 3;
                            word_dict)
                           (Counter.add_words ["hello";"world";"hello"]  word_dict) );


  "test_get_len1" >:: (fun _ -> assert_equal 0
                          (Hashtbl.length empty_dict));

  "test_get_len2" >:: (fun _ -> assert_equal 2
                          (Counter.get_length (Counter.make_dict
                                                 ["hello"; "world"; "world"; "hello";"hello"])));

  "test_mem1" >:: (fun _ -> assert_equal false (Counter.mem "hi"
                                                  (Counter.make_dict ["hello"; "world"; "world"; "hello";"hello"])));

  "test_mem2" >:: (fun _ -> assert_equal true
                      (Counter.mem "hello" (Counter.make_dict
                                              ["hello"; "world"; "world"; "hello";"hello"])));

  "test_find_word1" >:: (fun _ -> assert_equal 3
                            (Counter.find_word "hello" (Counter.make_dict
                                                          ["hello"; "world"; "world"; "hello";"hello"])));

  "test_find_word2" >:: (fun _ -> assert_equal 0
                            (Counter.find_word "hi" (Counter.make_dict
                                                       ["hello"; "world"; "world"; "hello";"hello"])));


]


let lev_test = [
  "lev_test1" >:: (fun _ -> assert_equal 0 (Lev.distance "" ""));
  "lev_test2" >:: (fun _ -> assert_equal 0 (Lev.distance "hello" "hello"));
  "lev_test3" >:: (fun _ -> assert_equal 1 (Lev.distance "hello" "hell"));
  "lev_test4" >:: (fun _ -> assert_equal 1 (Lev.distance "hello" "helloo"));
  "lev_test5" >:: (fun _ -> assert_equal 1 (Lev.distance "hello" "hella"));
  "lev_test6" >:: (fun _ -> assert_equal 4 (Lev.distance "hello" "world"));
]

let suite =
  "test suite for extract"  >::: List.flatten [
    counter_test;
    tokenizer_test;
    similarity_test;
    extract_test;
    lev_test;
  ]


let _ = run_test_tt_main suite
