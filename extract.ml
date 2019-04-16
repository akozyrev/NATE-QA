open Yojson.Basic.Util
open Tokenizer
open Counter
open Similarity

let topics = [
  "David Gries";
  "Alan Turing";
  "Algorithm";
  "Anita Borg";
  "Apple";
  "Artificial Intelligence";
  "Barbara Liskov";
  "Bill Gates";
  "Computer Graphics";
  "Computer Science";
  "Computer Vision";
  "Cornell University";
  "David Gries";
  "Deep Learning";
  "Deepmind";
  "Distributed Computing";
  "Elon Musk";
  "Embedded Systems";
  "Facebook";
  "Grace Hopper";
  "Human Computer Interaction";
  "Intel";
  "iPad";
  "iPhone";
  "Jeff Bezos";
  "Logic";
  "Machine Learning";
  "Mark Zuckerberg";
  "Mathematics";
  "Microsoft";
  "Natural Language Processing";
  "Pinterest";
  "Privacy";
  "Programming Languages";
  "Reinforcement Learning";
  "Scott Belsky";
  "Sheryl Sandberg";
  "Silicon Valley";
  "Slack Technologies";
  "Steve Jobs";
  "Tesla";
  "Tracy Chou";
  "Turing Award";
  "Twitter";
  "Uber";
  "Venture Capital";
  "Warby Parker";
  "Amazon Company";
  "YouTube";
]

type counter = Counter.t

type topic_dict = {
  topic : string;
  counter : counter;
}

type topic = {
  topic : string;
  content : string list;
}

(* open up the json file *)
let j = Yojson.Basic.from_file "corpus/data.json"

(* create topic type that has the topic and its content *)
let get_content (key_word:string) 
    (json: Yojson.Basic.json) : topic= 
    (* Pervasives.print_string "track 2" ; *)
  {
    topic = key_word;
    content = json |> member key_word  
              |> member "content" |> to_list 
              |> List.map to_string;
  }

(* given a string list of all topics, 
    this one gives you list of topic type *)
let rec all_topics (topics : string list) 
  (json : Yojson.Basic.json) (acc : topic list):topic list = 
    (* Pervasives.print_string "track 1" ; *)
  match topics with 
  |word::t -> all_topics t (json) 
        (get_content word json :: acc)
  |[] -> acc

(* given a topic/key word, this one gives 
   you a list of tokenized word of the content 
   of the topic *)
let tokenized_word_list (key_word:string) 
    (json: Yojson.Basic.json) 
    (acc:string list): string list = 
    (* Pervasives.print_string "track 0" ; *)
  List.concat (List.map Tokenizer.word_tokenize 
                 (get_content key_word json).content)

(* gives a counter type of the topic *)
let count_word (key_word:string) 
    (json: Yojson.Basic.json) 
    (acc:string list): counter = 
    (* Pervasives.print_string "help 0" ; *)
  Counter.make_dict (tokenized_word_list key_word json acc)

(* returns topic_dict data type that has 
    the topic and the counter dictionary 
    via a representation of record *)
let full_topic_dict (key_word:string) 
    (json: Yojson.Basic.json) : topic_dict = 
    (* Pervasives.print_string "help 1" ; *)
  {
    topic = key_word;
    counter = (count_word key_word json []);
  }

(* returns a list of topic_dict 
    type from a list of topics *)
let rec all_full_topics (topics : string list) 
    (json : Yojson.Basic.json) 
    (acc : topic_dict list) : topic_dict list = 
    (* Pervasives.print_string "help 3" ; *)
  match topics with 
  |word::t -> all_full_topics t 
                json (full_topic_dict word json :: acc)
  |[] -> acc

(* this function actually creates a list of 
   topic_dict from the topic list we have above *)
let all_topic_dict_counter = all_full_topics topics j []

(* this function actually creates a HASHTABLE of 
   topic_dict from the topic list we have above *)
let rec get_td_counter_ht (td: topic_dict list)  
  (acc_tbl: (string, Counter.t) Hashtbl.t) : 
  ((string, Counter.t) Hashtbl.t ) = 
  match td with 
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl h.topic h.counter; 
            get_td_counter_ht t acc_tbl


(* this variable is a HASHTABLE of 
   topic_dict from the topic list we have above *)
let all_topic_dict_counter_ht = get_td_counter_ht 
      all_topic_dict_counter (Hashtbl.create 100)

(* a getter function that allows us to use 
    List.map in the following *)
let get_counter topic_dict =
(* Pervasives.print_string "help 4" ; *)
  topic_dict.counter

(* print helper function to visualize 
    `topic_dict list` from `all_topic_dict_counter` *)
let rec print_topic_dict_list = function 
  |  [] -> ()
  |  (e:topic_dict)::l -> print_string e.topic ; 
    print_string " | " ; print_topic_dict_list l

(* given a word, this function checks every 
    topic_dict generated from the above topic list, 
    and if the word is in the topic_dict's counter 
    dictionary, then we add that entire topic_dict 
    type to the accumulator  *)
    (*OPTIMIZED*)
let rec which_dict_has_the_word (word:string)
    (topic_dict_lst:topic_dict list)
    (acc:topic_dict list): topic_dict list =
    (* Pervasives.print_string "help 5" ; *)
  match topic_dict_lst with
  |topic_dict::lst-> 
    if Counter.mem word topic_dict.counter 
    then which_dict_has_the_word word 
        lst (topic_dict::acc) 
    else which_dict_has_the_word word lst acc
  |[]->acc


(*OPTIMIZED*)
let rec count_word_in_topic (word:string) 
      (topic:string) : int =
  (* Pervasives.print_string "here 0" ; *)
  let counter = Hashtbl.find all_topic_dict_counter_ht topic in 
  let output = Counter.find_word word (counter) in
  begin
  (* Pervasives.print_string "here 2"; *)
  output; 
  end

(** [tf word topic] is term frequency of [word] in [topic]
    calculated as follows:
    # of times [word] appears in [topic] / total number of topics *)
    (*OPTIMIZED*)
let rec tf (word:string) (topic:string) =
  float_of_int (count_word_in_topic word topic) /.
  float_of_int (Hashtbl.length all_topic_dict_counter_ht)

(** [idf word] is a statistical measure of 
    how important [word] is, based on the following 
    calculation: log (total # of documents / # of 
    documents with [word] in them) *)
    (*OPTIMIZED*)
let rec idf (word:string) =
  Pervasives.log(
    float_of_int (List.length topics) /.
    float_of_int (List.length
    ((which_dict_has_the_word word all_topic_dict_counter) [])))

(** [tfidf input_word topics] is the TF-IDF 
  of an input word computed for the given [topic] .*)
let rec tfidf (input_word:string) 
      (topic:string): float =
  (tf input_word topic) *. (idf input_word)

(** [construct_tfidf input_word] is a list of tuples, 
    where the first element of each tuple is a topic title 
    (string) and the second element is the TF-IDF value for 
    that topic with respect to [input_word] *)
let rec construct_tfidf (input_word:string) : 
    (string, float) Hashtbl.t =
  let ht = Hashtbl.create 200000 in
    begin
      List.map (fun (topic : string) : unit -> 
        Hashtbl.add ht topic (tfidf input_word topic)) topics;
      ht
    end

(* [get_topics_tfidf input_sent] takes in a 
  sentence, tokenizes it, and for each word, 
  computes a list of tfidf scores of each word 
  in each document. *)
let get_topics_tfidf (input_sent:string): 
    ((string, float) Hashtbl.t list) = 
  let input_tokens = Similarity.remove_dups 
        (Tokenizer.word_tokenize input_sent) in
  List.map (fun w -> construct_tfidf w) input_tokens

(** Return sentence in specified document topic containing the 
    maximum jaccard similarity metric, compared with the 
    input sentence (question we ask the chatbot).
    This is the last function we will need to return the 
    calculated response to the user's input *)
let max_jaccard_sentence (topic:string) 
    (input_sent:string) (json): string = 
  let topic_we_want = get_content topic json in
  let doc_sentences = topic_we_want.content in
  let input_tokens = Similarity.remove_dups 
      (Tokenizer.word_tokenize input_sent) in

  (* create [key: sentence, value: sentence's 
  word token list] dict *)
  let doc_sent_tok_dict = List.map (fun s -> 
      (s, Similarity.remove_dups 
        (Tokenizer.word_tokenize s))) doc_sentences in

  (* create [key: sentence, value: sentence's 
      jaccard score] dict *)
  let doc_sent_jac_dict = List.map (fun e -> 
      (fst e,  Similarity.jaccard_sim 
         (snd e) (input_tokens))) doc_sent_tok_dict in

  let rec find_max_j dsj_dict acc_sent acc_int = 
    (* Pervasives.print_string "iter"; *)
    match dsj_dict with
    | [] -> acc_sent
    | h::t -> if (snd h > acc_int) then 
        begin
          (* Pervasives.print_float (snd h);
             Pervasives.print_newline (); *)
          find_max_j t (fst h) (snd h)
        end 
      else find_max_j t acc_sent acc_int

  in find_max_j doc_sent_jac_dict "" 0.0

let get_topic (td:topic_dict) =
  td.topic

let get_topics (td_lst:topic_dict list) =
  List.map get_topic td_lst

(** [add_elt_to_list doc lst] returns a list of 
      tuples where the first value corresponds
  to the document and the value is the combined 
    tfidf scores of each word in that document. *)
let rec add_elt_to_list (doc : string*float) 
    (lst: (string, float) Hashtbl.t) : (string, float) Hashtbl.t =
  let tfidf = Hashtbl.find_opt lst (fst doc) in
  match tfidf with 
  | None -> Hashtbl.add lst (fst doc) (snd doc); lst
  | Some i -> Hashtbl.remove lst (fst doc); 
          Hashtbl.add lst (fst doc) (i +. (snd doc)); lst


(* [add_list_to_list lst1 lst2] combines the elements 
    of lst1 and lst2, where there are no duplicate 
  string values. If the string values are equal, 
      their float values are added. *)
let rec add_list_to_list (lst1: (string, float) Hashtbl.t) 
      (lst2: (string, float) Hashtbl.t): (string, float) Hashtbl.t=
  Hashtbl.iter (fun (a : string) (b : float) : unit -> 
    match (Hashtbl.find_opt lst1 a) with
      | Some found_b -> Hashtbl.remove lst1 a; 
            Hashtbl.add lst1 a (b +. found_b)
      | None -> Hashtbl.add lst1 a b
  ) lst2; lst1

let print_hashtable (ht : 
        (string, float) Hashtbl.t) : unit = 
    Hashtbl.iter (fun x y -> print_string x; 
          print_float y; print_newline ()) ht

(* [add_tfidf input_sent] computes the sum of 
TFIDF scores for each word in each document and returns
  the document with the highest sum. *)
let add_tfidf (input_sent : string) : string =
  let doc_list = get_topics_tfidf input_sent in
  let temp_list = List.fold_left add_list_to_list 
        (Hashtbl.create 20000) doc_list in
  let good_tup = Hashtbl.fold (fun (a : string) 
            (b: float) (c : string * float) : (string * float) -> 
        if b > (snd c) then (a, b) else c) 
        temp_list ("David Gries", 0.0) in

  begin
  (* print_hashtable temp_list; *)
  fst good_tup
  end

let get_response (input_sent : string) : string =
  let topic_doc = add_tfidf input_sent in
  let response = begin (*  Pervasives.print_string topic_doc;  *)
          max_jaccard_sentence topic_doc input_sent j 
          end 
  in response