(*Autocorrection module*)
open Tokenizer
open Counter
open Extract
open Lev
open Filter
open Similarity

(** List of all counters for each document we have in the corpus *)
(* let all_counters_list = 
   List.map (fun a -> Extract.get_counter a) Extract.all_topic_dict_counter *)


(** List of all dictionaries for each document we have in the corpus *)
(* let all_dict_list =
   List.map (fun (a:Counter.t) -> Counter.get_dictionary a) all_counters_list *)


(** [add_elt_to_list doc ht] returns a list of
      tuples where the first value corresponds
    to the document and the value is the combined
    tfidf scores of each word in that document. *)
let rec add_elt_to_list (doc : string*int)
    (ht: (string, int) Hashtbl.t) : (string, int) Hashtbl.t =
  let tfidf = Hashtbl.find_opt ht (fst doc) in
  match tfidf with
  | None -> Hashtbl.add ht (fst doc) (snd doc); ht
  | Some i -> Hashtbl.remove ht (fst doc);
    Hashtbl.add ht (fst doc) (i + (snd doc)); ht


(** [add_list_to_list ht1z ht2] combines the elements
    of ht1 and ht2, where there are no duplicate
    string values. If the string values are equal,
      their float values are added. *)
(* let rec add_list_to_list (ht1: (string, int) Hashtbl.t)
    (ht2: (string, int) Hashtbl.t): (string, int) Hashtbl.t=
   Hashtbl.iter (fun (a : string) (b : int) : unit ->
      match (Hashtbl.find_opt ht1 a) with
      | Some found_b -> Hashtbl.remove ht1 a;
        Hashtbl.add ht1 a (b + found_b)
      | None -> Hashtbl.add ht1 a b
    ) ht2; ht1 *)

(** Large hashtable with key : word, value: num occurences of word,
    for all documents in the corpus, i.e. all the counter dicts
    for each topic combined together *)
(* let big_counter_ht = 
   List.fold_left add_list_to_list (Hashtbl.create 50000) all_dict_list *)


(** List of all the words in document *)
(* let all_words =  Hashtbl.fold (fun k v acc -> k :: acc) big_counter_ht [] *)

(**Return candidates with smallest Levenshtein distance
   from input word, the word we want to autocorrect*)
let rec candidates (input:string) (word_lst: string list) 
    (acc_lst: string list) (acc_ld : int): string list =
  match word_lst with
  | [] -> acc_lst
  | a::t -> 
    let ld = Lev.distance a input in
    if ld < acc_ld then candidates input t [a] ld
    else if ld = acc_ld then candidates input t (a::acc_lst) ld
    else candidates input t acc_lst acc_ld

(** Output a list of words most similar to given input word*)
let find_candidates (input_word:string) : string list = 
  candidates input_word Extract.all_words [] 3

(**Return all the elements of a string list as whole string, 
   ready to be printed*)
let rec str_list = function 
  | [] -> ""
  | h::t ->(h ^ ", ") ^ str_list t

(** Check to see if each word is valid word,
    return hashtable of all bad words and their likely candidates *)
let correctness_ht (toks_filtered:string list) : (string, string list) Hashtbl.t = 
  let all_bad_words = List.filter (fun a -> not (Hashtbl.mem Extract.big_counter_ht a)) toks_filtered in
  let acc_tbl = Hashtbl.create 10 in (*string, string list*)
  List.fold_left (fun ht a-> (Hashtbl.add ht a (find_candidates a); ht)) acc_tbl all_bad_words


(**Check correctness of each word in input sentence
   and return string *)
let check_correctness (input_sent:string) : string = 
  let toks = Similarity.remove_dups (Tokenizer.word_tokenize input_sent) in
  (* Pervasives.print_string (str_list toks) ;  *)
  let toks_f = List.filter (fun a -> not (List.mem a Filter.filter_list)) toks in
  (* Pervasives.print_string (str_list toks_f) ; *)
  let corr_ht = correctness_ht toks_f in

  let rec make_str (tokens: string list) (acc_str : string) : string =
    match tokens with
    | [] -> acc_str
    | h::t -> begin match (Hashtbl.mem corr_ht h) with
        | false -> make_str t acc_str
        | true ->
          let new_acc_str = acc_str ^ "('" ^ h ^
                            "') Did you mean any of these: "
                            ^ ((str_list (Hashtbl.find corr_ht h)) ^ "\n") in
          (make_str t new_acc_str) end
  in

  match Hashtbl.length corr_ht with
  | 0 -> "all correct"
  | _ -> make_str toks_f "\n"




