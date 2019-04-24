(** Module for representing document types. 
It has dictionary, which is a hashtable of 
all words (keys) and their number of occurences 
in the document (values), and also a length (number
of  unique words in the document*)

(** Module for representing document types. 
It has dictionary, which is a hashtable of 
all words (keys) and their number of occurences 
in the document (values), and also a length (number
of  unique words in the document*)
module Counter = struct

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
  let rec add_word (word:string) (dict: 
          (string, int) Hashtbl.t): (string, int) Hashtbl.t = 
    let w = Hashtbl.find_opt dict word in
    match w with 
    | None -> Hashtbl.add dict word 1; dict
    | Some i -> Hashtbl.replace dict word (i+1); dict 

(** [add_words words dict] constructs a dictionary, given [words]
    --a list of words--, with dictionary keys being unique words, and values being 
    the number of occurrences of each word. This function will create a new element
    in dictionary with occurrence 1 if the word is not already found in [dict] and
    will add 1 to the word's value if word is a member of [dict]. Used recursively
    in [make_dict words]. *)
  let rec add_words (words:string list) (dict: (string, int) 
                    Hashtbl.t):(string, int) Hashtbl.t = 
    match words with
    | [] -> dict
    | h::t -> add_words t (add_word h dict) 


(** [make_dict words] makes new dict from scratch, 
    using [words]--a list of tokenized words. 
    Adds all of the words from [words] to the dictionary. 
    Dictionary keys are unique words, and the value for each word is the number
    of occurrences of the word in the list. *)
  let make_dict (words:string list) : t =
    let new_dict = add_words words (Hashtbl.create 20000) in
    {
      dict = new_dict;
      length = Hashtbl.length new_dict
    }

  (** [mem word d] returns true if [word] is in [d]. *)
  let mem (word:string) (d:t) : bool =
    Hashtbl.mem d.dict word

  (** [get_length d] returns the number of elements in [d]. *)
  let get_length (d:t) : int =
    d.length

  (** [get_dictionary d] returns the dictionary of [d]. *)
  let get_dictionary (d:t) : (string, int) Hashtbl.t = 
    d.dict


  (** [find_word word d] returns the number of occurrences of [word] in 
      Counter [d]. If the word is not found in Counter [d], 0 is returned. *)
  let rec find_word (word:string) (d:t) : int =
    let result = Hashtbl.find_opt d.dict word in
    match result with
    | None -> 0
    | Some n -> n

end
