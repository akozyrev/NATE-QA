(** Module for representing document types. It has a 
dictionary, which is a hashtable of all words (keys) 
and their number of occurrences in the document (values),
and also a length (number of  unique words in the document.*)
module Counter : sig

  (** Type for this module, which is represented as
      a dictionary containing elements in the from of 
      string (unique word) and int (number of occurrences) 
      tuples. *)
  type t = {
    dict: (string, int) Hashtbl.t;   
    length: int;
  }

(** [add_word word dict] adds a word to the list representing a dictionary, where 
    the keys are the unique words and the values are the number of occurrences of 
    each word. This function will create a new element in dictionary with
    occurrence 1 if word not already found in [dict], will add 1 to the word's
    value if the word is a member of [dict]. Used recursively in [add_words]. *)
  val add_word : string -> (string, int) Hashtbl.t -> (string, int) Hashtbl.t

(** [add_words words dict] constructs a dictionary, given [words]
    --a list of words--, with dictionary keys being unique words, and values being 
    the number of occurrences of each word. This function will create a new element
    in dictionary with occurrence 1 if the word is not already found in [dict] and
    will add 1 to the word's value if word is a member of [dict]. Used recursively
    in [make_dict words]. *)
  val add_words: string list -> (string, int) Hashtbl.t -> (string, int) Hashtbl.t

(** [make_dict words] makes new dict from scratch, 
    using [words]--a list of tokenized words. Adds all of the words from 
    [words] to the dictionary. Dictionary keys are unique words, and the 
    value for each word is the number of occurrences of the word in the list. *)
  val make_dict: string list -> t 

(** [mem word d] returns true if [word] is in [d]. *)
  val mem: string -> t -> bool

(** [get_length d] returns the number of elements in [d]. *)
  val get_length: t -> int

(** [get_dictionary d] returns the dictionary of [d]. *)
  val get_dictionary: t -> (string, int) Hashtbl.t

(** [find_word word d] returns the number of occurrences of [word] in 
      Counter [d]. If the word is not found in Counter [d], 0 is returned. *)
  val find_word : string -> t -> int

end