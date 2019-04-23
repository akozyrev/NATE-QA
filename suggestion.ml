let lst : (string * string list) list = [
  ("person", ["david gries"; "gries"; "john hopcroft"; "hopcroft"; "alan turing"]) ;
  ("company", ["facebook"; "amazon"; "google"]) ;
  ("programming language", ["java";"python";"ocaml"]);
  ("machine learning concept", ["natural language processing"; "machine learning"; "deep learning"]) ;
  ("field", ["machine learning";"deep learning"]);
]


let list_snd : (string list) list = 
    List.map (fun a -> snd a) lst


let big_list : string list = 
  List.flatten list_snd


let rec populate_ht (lst: (string * string list) list) 
      (acc_tbl: (string, string list) Hashtbl.t) : 
      (string, string list) Hashtbl.t= 
  match lst with 
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl (fst h) (snd h); populate_ht t acc_tbl


let topic_to_elt_ht : (string, string list) Hashtbl.t = 
    populate_ht lst (Hashtbl.create 50)


(**populate acc_tbl with keys as elements of lst,
topic as given topic param*)
let rec populate_ht_list (lst: string list) 
          (acc_tbl: (string, string) Hashtbl.t) (topic: string) : 
          (string, string) Hashtbl.t = 
  match lst with 
   | [] -> acc_tbl
   | h::t -> Hashtbl.add acc_tbl h topic; populate_ht_list t acc_tbl topic


let rec populate_ht_inv (lst: (string * string list) list) 
    (acc_tbl: (string, string) Hashtbl.t) : (string, string) Hashtbl.t = 
  List.fold_left (fun ht elt -> populate_ht_list (snd elt) ht (fst elt)) acc_tbl lst
  (* match lst with 
  | [] -> acc_tbl
  | h::t -> let new_acc_tbl = List.fold_left (fun ht elt -> populate_ht_list (snd elt) ht (fst elt)) acc_tbl h in
            populate_ht_inv t new_acc_tbl *)


let elt_to_topic_ht: (string, string) Hashtbl.t = populate_ht_inv lst (Hashtbl.create 500)

(**check to see if s1 contains s2*)
let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let suggestion (input_sent:string) : string = 
  let sent_lower = String.lowercase_ascii input_sent in

  let rand lst =
    List.nth lst (Random.int (List.length lst)) in

  let rec remove lst elt = 
    match lst with 
    | [] -> []
    | h::t -> if h = elt then t else h::(remove t elt) in

  let rec suggestion2 sl b_list= 
    match b_list with 
    | [] -> ""
    | h::t -> if contains sl h then begin
      let topic = Hashtbl.find elt_to_topic_ht h in
      let topic_lst = Hashtbl.find topic_to_elt_ht topic in
      "Another " ^ topic ^ " you may be interested in learning about: " ^ (rand (remove topic_lst topic)) ^ "\n" end

      else suggestion2 sl t in 

  suggestion2 sent_lower big_list

