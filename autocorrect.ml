(*Autocorrection module*)
open Tokenizer
open Counter
open Extract
open Lev
open Filter
open Similarity

(** [all_counters_list] returns a list of all counters for each
  document we have in the corpus. *)
let all_counters_list = 
  List.map (fun a -> Extract.get_counter a) Extract.all_topic_dict_counter


(** [all_dict_list] returns a list of all dictionaries for each 
  document we have in the corpus. *)
let all_dict_list =
  List.map (fun (a:Counter.t) -> Counter.get_dictionary a) all_counters_list


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


(** [add_list_to_list ht1 ht2] combines the elements
    of ht1 and ht2, where there are no duplicate
    string values. If the string values are equal,
      their float values are added. *)
let rec add_list_to_list (ht1: (string, int) Hashtbl.t)
    (ht2: (string, int) Hashtbl.t): (string, int) Hashtbl.t=
  Hashtbl.iter (fun (a : string) (b : int) : unit ->
      match (Hashtbl.find_opt ht1 a) with
      | Some found_b -> Hashtbl.remove ht1 a;
        Hashtbl.add ht1 a (b + found_b)
      | None -> Hashtbl.add ht1 a b
    ) ht2; ht1

(** [big_counter_ht] returns a large hashtable with key : word, 
  value: num occurences of word for all of the documents in the corpus,
  i.e. all the counter dicts for each topic, combined together *)
let big_counter_ht = 
  List.fold_left add_list_to_list (Hashtbl.create 50000) all_dict_list


(** [all_words] returns a list of all the words in all of the documents. *)
let all_words =  Hashtbl.fold (fun k v acc -> k :: acc) big_counter_ht []

(** [candidates input word_lst acc_lst acc_ld] acts as a helper for
[find_candidates input_word]. It returns a string list of all the 
"candidates"/valid words with the smallest Levenshtein distance from 
[input], the word we want to autocorrect. *)
let rec candidates (input:string) (word_lst: string list) 
                (acc_lst: string list) (acc_ld : int): string list =
  match word_lst with
   | [] -> acc_lst
   | a::t -> 
      let ld = Lev.distance a input in
      if ld < acc_ld then candidates input t [a] ld
      else if ld = acc_ld then candidates input t (a::acc_lst) ld
      else candidates input t acc_lst acc_ld

(** [find_candidates input_word] returns a list of words that are
  most similar to [input_word] (Levenshtein distance is less than or
  equal to 3). *)
let find_candidates (input_word:string) : string list = 
  candidates input_word all_words [] 3

(** [str_list] returns all the elements of a string list in the 
  form of a string, ready to be printed. *)
let rec str_list = function 
  | [] -> ""
  | h::[] -> (h ^ "")
  | h::t ->(h ^ ", ") ^ str_list t

(** [correctness_ht toks_filtered] returns a hashtable, where each key
  is a "bad"/invalid word, and the value is a string list of likely
  candidates/valid words. 
  "Valid" is defined as a word that exists within the corpus. *)
let correctness_ht (toks_filtered:string list) : (string, string list) Hashtbl.t = 
  let all_bad_words = List.filter (fun a -> not (Hashtbl.mem big_counter_ht a)) toks_filtered in
  let acc_tbl = Hashtbl.create 10 in (*string, string list*)
  List.fold_left (fun ht a-> (Hashtbl.add ht a (find_candidates a); ht)) acc_tbl all_bad_words

   
(** [check_correctness input_sent] checks the correctness/validity of each word
  in [input_sent] and returns a new string where each word is a correct/valid
  word. 
 "Valid" is defined as a word that exists within the corpus. *)
let check_correctness (input_sent:string) : string = 
  let toks = Similarity.remove_dups (Tokenizer.word_tokenize input_sent) in
  let toks_f = List.filter (fun a -> not (List.mem a Filter.filter_list)) toks in
  let toks_ff = List.filter (fun a -> not (List.mem '\'' (List.init (String.length a) (String.get a)))) toks in
  let corr_ht = correctness_ht toks_ff in

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
    



