(** Module to autocorrect misspellings in the user's input.*)

(** [check_correctness input_sent] checks the 
    correctness/validity of each word in [input_sent] and 
    returns a new string where each word is a correct/valid word. 
    "Valid" is defined as a word that exists within the corpus. *)
val check_correctness : string -> string

