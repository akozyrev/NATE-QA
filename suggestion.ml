(**list of topic clusters for topic suggestion*)
let lst : (string * string list) list = [
  ("person", ["david gries";"john hopcroft"; "alan turing";
              "andreessen horowitz"; "anita borg"; "barbara liskov"; "bill gates";
              "brian acton"; "david easley"; "elon musk"; "eva tardos"; "grace hopper";
              "jeff bezos"; "jon kleinberg"; "karlie kloss"; "kevin systrom"; 
              "mark zuckerberg"; "mike krieger"; "robert kleinberg"; "sheryl sandberg";
              "steve ballmer"; "steve jobs"; "steve wozniak"; "tracy chou"]) ;

  ("company", ["facebook"; "amazon"; "google"; "apple"; "deepmind"; "instagram"; "intel";
              "linkedin"; "microsoft"; "mozilla"; "netflix"; "paypal"; "pinterest"; "youtube";
              "quora"; "slack"; "spacex"; "spotify"; "tesla"; "twitter"; "uber"; "lyft"]) ;
  
  ("programming language", ["java";"python";"ocaml";"c++"; "haskell"; "sql"; "c language"]);
  
  ("field", ["machine learning";"deep learning";"natural language processing";"information retrieval";
            "reinforcement learning"; "programming languages"; "lambda calculus"; "databases";
            "artificial intelligence"; "algorithms"; "computer vision"; "data structures";
            "distributed computing"; "embedded systems"; "human computer interaction"; "mathematics";
            "logic"; "privacy"; "software engineering"]);
  
  ("neural network", ["perceptron"; "recurrent neural network"; "convolutional neural network";
                      "generative adversarial network"; "artificial neural network"]);
  
  ("algorithm", ["mergesort"; "bubble sort"; "stable marriage"; "dijkstra's shortest path";
                "k nearest neighbors"; "SVM"; "decision tree"; "minimax"; "hill climbing";
                "genetic algorithm"; "breadth first search"; "depth first search";"heapsort"]);
  
  ("university", ["caltech"; "carnegie mellon"; "cornell"; "georgia tech"; "harvard"; "mit"; "stanford";
                  "berkeley"])
]

(**list of all clusters, without the cluster topic*)
let list_snd : (string list) list = 
    List.map (fun a -> snd a) lst

(**list of all elements of all topics, flattened out*)
let big_list : string list = 
  List.flatten list_snd

(**populates our key: topic, value: topic cluster list
hashtable *)
let rec populate_ht (lst: (string * string list) list) 
      (acc_tbl: (string, string list) Hashtbl.t) : 
      (string, string list) Hashtbl.t= 
  match lst with 
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl (fst h) (snd h); populate_ht t acc_tbl

(**global constant for our key: topic, value: topic cluster list
hashtable *)
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


(**populate an inverse hashtable, with keys as example of topic,
value being the topic it belongs to*)
let rec populate_ht_inv (lst: (string * string list) list) 
    (acc_tbl: (string, string) Hashtbl.t) : (string, string) Hashtbl.t = 
  List.fold_left (fun ht elt -> populate_ht_list (snd elt) ht (fst elt)) acc_tbl lst


(**global contant where our inverse hashtable is stored*)
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

(**output suggestion string from user input question*)
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
      "Another " ^ topic ^ " you may be interested in learning about: " ^ (rand (remove topic_lst h)) ^ "\n" end

      else suggestion2 sl t in 

  suggestion2 sent_lower big_list

