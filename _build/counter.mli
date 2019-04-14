module Counter : sig

  (** Type for this module, which is represented as
      a dictionary containing elements in the from of 
      string (unique word) and int (number of occurences) 
      tuples. *)
  type t = {
    dict: (string * int) list;   
    length: int;
  }

(** Determine whether or not words is a list containing 
  unique elements (each element is present only once). 
  Used as helper for rep_ok *)
  val no_repeats: string list -> bool 

(** Determine whether or not d satisfies the representation
    invariant (dict has no repeats, and length is its actual length)*)
  val rep_ok: t -> bool

 (** Determine whether or not d is an empty dictionary *)
  val is_empty : t -> bool 

  (** Add a word to the list representing dictionary with keys
      being unique words, values being number of occurence of each word.
      Will create new element in dictionary with occurence 1 if word not 
      already found in dictionary keys, will add 1 to corresponding value if
      word is member of dictionary keys. Used as helper for step_dict*)
  val add_word : string -> (string*int) list -> (string*int) list

  (** Construct a dictionary, given a list of words, with dictionary keys
      being unique words, values being number of occurence of each word.
      Will create new element in dictionary with occurence 1 if word not 
      already found in dictionary keys, will add 1 to corresponding value if
      word is member of dictionary keys. Used as helper for make_dict*)
  val add_words: string list -> ((string*int) list) -> (string*int) list 


  (** Make new dict from scratch, using list of TOKENIZED WORDS. 
      Dictionary keys are unique words and values are num occurences of word. *)
  val make_dict: string list -> t 


  (** Returns whether or not word is found in d dictionary *)
  val mem: string -> t -> bool
   
  (** Return length of dictionary *)
  val get_length: t -> int

  (** Return actual dictionary list *)
  val get_dictionary: t -> (string * int) list 




end