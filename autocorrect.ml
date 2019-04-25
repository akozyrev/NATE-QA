(** Module to autocorrect misspellings in user input,
   and return all possible candidates with the lowest possible
   Levenshetein edit distance, max 3 units. *)

open Tokenizer
open Counter
open Extract
open Lev
open Filter
open Similarity

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

(** [find_candidates input_word] returns a list of words that are
    most similar to [input_word] (Levenshtein distance is less than or
    equal to 3). *)
let find_candidates (input_word:string) : string list = 
  candidates input_word Extract.all_words [] 3

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
let correctness_ht (toks_filtered:string list) 
    : (string, string list) Hashtbl.t = 
  let all_bad_words = List.filter (fun a -> 
    not (Hashtbl.mem Extract.big_counter_ht a)) toks_filtered in
  let acc_tbl = Hashtbl.create 10 in (*string, string list*)
  List.fold_left (fun ht a-> (Hashtbl.add ht a (find_candidates a); ht)) 
    acc_tbl all_bad_words

let check_correctness (input_sent:string) : string =
  let toks = Similarity.remove_dups 
    (Tokenizer.word_tokenize input_sent) in
  let toks_f = List.filter (fun a -> not 
    (List.mem a Filter.filter_list)) toks in
  let toks_ff = List.filter (fun a -> not 
    (List.mem '\'' (List.init (String.length a) (String.get a)))) toks in
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




