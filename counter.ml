module Counter = struct

  (** Type for this module, which is represented as
      a dictionary containing elements in the from of 
      string (unique word) and int (number of occurences) 
      tuples. *)
  type t = {
    dict: (string, int) Hashtbl.t;   
    length: int;
  }

  (** Add a word to the list representing dictionary with keys
      being unique words, values being number of occurence of each word.
      Will create new element in dictionary with occurence 1 if word not 
      already found in dictionary keys, will add 1 to corresponding value if
      word is member of dictionary keys. Used as helper for step_dict*)
  let rec add_word (word:string) (dict: 
          (string, int) Hashtbl.t): (string, int) Hashtbl.t = 
  (* Pervasives.print_string "e2"; *)
    let w = Hashtbl.find_opt dict word in
    match w with 
    | None -> Hashtbl.add dict word 1; dict
    | Some i -> Hashtbl.replace dict word (i+1); dict 

  (** Construct a dictionary, given a list of words, with dictionary keys
      being unique words, values being number of occurence of each word.
      Will create new element in dictionary with occurence 1 if word not 
      already found in dictionary keys, will add 1 to corresponding value if
      word is member of dictionary keys. Used as helper for make_dict*)
  let rec add_words (words:string list) (dict: (string, int) 
        Hashtbl.t):(string, int) Hashtbl.t = 
  (* Pervasives.print_string "e3"; *)
    match words with
    | [] -> dict
    | h::t -> add_words t (add_word h dict) 


  (** Make new dict from scratch, using list of TOKENIZED WORDS. 
      Dictionary keys are unique words and values are num 
      occurences of word. *)
  let make_dict (words:string list) : t =
  (* Pervasives.print_string "e4"; *)
    let new_dict = add_words words (Hashtbl.create 20000) in
    {
      dict = new_dict;
      length = Hashtbl.length new_dict
    }

  (** Returns whether or not word is found in d dictionary *)
  let mem (word:string) (d:t) : bool =
  (* Pervasives.print_string "e5"; *)
    Hashtbl.mem d.dict word

  (** Return length of dictionary *)
  let get_length (d:t) : int =
    d.length

  (** Return actual dictionary list *)
  let get_dictionary (d:t) : (string, int) Hashtbl.t = 
    d.dict


  (** find number of occurences of word in Counter d *)
  let rec find_word (word:string) (d:t) : int =
  (* Pervasives.print_string "e7"; *)
    let result = Hashtbl.find_opt d.dict word in
    match result with 
    | None -> 0
    | Some n -> n

end
