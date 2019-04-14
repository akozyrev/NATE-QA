module Counter = struct

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
  let rec no_repeats (words:string list) : bool = 
    match words with
    | [] -> true
    | h::t -> if List.mem h t then false
      else no_repeats t 

  (** Determine whether or not d satisfies the representation
      invariant (dict has no repeats, and length is its actual length)*)
  let rep_ok (d:t) : bool =
    (List.length d.dict = d.length) &&
    (let dict_words = List.map (fun a -> fst a) d.dict in
     no_repeats dict_words)

  (** Determine whether or not d is an empty dictionary *)
  let is_empty (d:t) : bool = 
    d.dict = [] && d.length = 0

  (** Add a word to the list representing dictionary with keys
      being unique words, values being number of occurence of each word.
      Will create new element in dictionary with occurence 1 if word not 
      already found in dictionary keys, will add 1 to corresponding value if
      word is member of dictionary keys. Used as helper for step_dict*)
  let rec add_word (word:string) (dict:(string*int) list):(string*int) list = 
    match dict with
    | [] -> [(word, 1)]
    | h::t -> if (fst h = word) then (fst h, (snd h) + 1)::t
      else h::(add_word word t) 

  (** Construct a dictionary, given a list of words, with dictionary keys
      being unique words, values being number of occurence of each word.
      Will create new element in dictionary with occurence 1 if word not 
      already found in dictionary keys, will add 1 to corresponding value if
      word is member of dictionary keys. Used as helper for make_dict*)
  let rec add_words (words:string list) (dict:(string*int) list):(string*int) list = 
    match words with
    | [] -> dict
    | h::t -> add_words t (add_word h dict) 


  (** Make new dict from scratch, using list of TOKENIZED WORDS. 
      Dictionary keys are unique words and values are num occurences of word. *)
  let make_dict (words:string list) : t =
    let new_dict = add_words words [] in
    {
      dict = new_dict;
      length = List.length new_dict
    }


  (** Returns whether or not word is found in d dictionary *)
  let mem (word:string) (d:t) : bool =
    let dict_words = List.map (fun a -> fst a) d.dict 
    in List.mem word dict_words

  (** Return length of dictionary *)
  let get_length (d:t) : int =
    d.length

  (** Return actual dictionary list *)
  let get_dictionary (d:t) : (string * int) list= 
    d.dict

end
