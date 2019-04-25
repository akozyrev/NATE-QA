(* Module for representing document types. 
It has a dictionary, which is a hashtable of 
all words (keys) and their number of occurences 
in the document (values), and also a length (number
of  unique words in the document. *)
module Counter = struct

  type t = {
    dict: (string, int) Hashtbl.t;   
    length: int;
  }

  let rec add_word (word:string) (dict: 
          (string, int) Hashtbl.t): (string, int) Hashtbl.t = 
    let w = Hashtbl.find_opt dict word in
    match w with 
    | None -> Hashtbl.add dict word 1; dict
    | Some i -> Hashtbl.replace dict word (i+1); dict 

  let rec add_words (words:string list) (dict: (string, int) 
                    Hashtbl.t):(string, int) Hashtbl.t = 
    match words with
    | [] -> dict
    | h::t -> add_words t (add_word h dict) 

  let make_dict (words:string list) : t =
    let new_dict = add_words words (Hashtbl.create 20000) in
    {
      dict = new_dict;
      length = Hashtbl.length new_dict
    }

  let mem (word:string) (d:t) : bool =
    Hashtbl.mem d.dict word

  let get_length (d:t) : int =
    d.length

  let get_dictionary (d:t) : (string, int) Hashtbl.t = 
    d.dict

  let rec find_word (word:string) (d:t) : int =
    let result = Hashtbl.find_opt d.dict word in
    match result with
    | None -> 0
    | Some n -> n

end
