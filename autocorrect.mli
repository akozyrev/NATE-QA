(** Module to autocorrect misspellings in user input,
   and return all possible candidates with the lowest possible
   Levenshetein edit distance, max 3 units. *)

(** [check_correctness input_sent] checks the 
    correctness/validity of each word in [input_sent] and 
    returns a new string where each word is a correct/valid word. 
    "Valid" is defined as a word that exists within the corpus. *)
val check_correctness : string -> string

