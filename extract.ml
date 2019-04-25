open Yojson.Basic.Util
open Tokenizer
open Counter
open Similarity

(** Module for processing the data from the json file, 
    storing each document's data in a dictionary, and executing
    the main computations for constructing the bot's response. *)

type topic_dict = {
  topic : string;
  counter : Counter.t;
}

type topic = {
  topic : string;
  content : string list;
}

(** [unpack_Yojson json] parses the given [json] file. *)
let unpack_Yojson (json: Yojson.Basic.json): topic =  {
  topic = member "topic" json|>to_string;
  content = member "content" json
            |>to_list              |> List.map to_string;
}

(** [j] opens our json file. *)
let j = Yojson.Basic.from_file "corpus/improved_data.json"


(** [get_topic tp] returns the title of topic [tp]. *)
let get_topic (tp: topic) : string =
  tp.topic

(** [get_all_topics] is all topic bindings of [topic_lst]. *)
let get_all_topics (topic_lst: topic list) : string list =
  List.map (get_topic) topic_lst

(** [find_the_topics_content key_word top_lst] is the tokenized
    content of the given topic [key_word]. *)
let find_the_topics_content (key_word:string) (top_lst: topic list) =
  match (List.filter (fun x -> x.topic = key_word) top_lst) with
  |topic::lst -> topic.content
  |[]->failwith"This topc does not exist"

(** [get_content key_word json] creates the topic type that has
    the topic title [key_word] and its content, all extracted from [json]. *)
let get_content (key_word:string) (json: Yojson.Basic.json) : topic =
  (* Pervasives.print_string "track 2" ; *)
  {
    topic = key_word;
    content = json
              |>to_list
              |>List.map unpack_Yojson
              |>find_the_topics_content key_word
  }

(** [string_topic_dict tp acc_tbl] returns a dictionary containing
    bindings of topic name -> topic type. *)
let rec string_topic_dict (tp: string list)
    (acc_tbl : ((string, topic) Hashtbl.t)): ((string, topic) Hashtbl.t) =
  match tp with
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl h (get_content h j);
    string_topic_dict t acc_tbl

(** [json_lst] is a formatted version of the json file in a list. *)
let json_lst = to_list j

(** [all_topics] is a list of all topic titles parsed from the json. *)
let all_topics = List.map unpack_Yojson json_lst

(** [topics] is a list of topic_dicts. *)
let topics = get_all_topics all_topics

(** [content_dict] is a topic dictionary, which is created and stored
    for fast access during calculations. *)
let content_dict = string_topic_dict topics (Hashtbl.create 200)

(** [all_topics topics json acc], given a string list of all topics,
    returns a list of topic data type. *)
let rec all_topics (topics : string list) (json : Yojson.Basic.json)
    (acc : topic list) : (topic list) = match topics with
  | word::t -> all_topics t (json) (get_content word json :: acc)
  | [] -> acc

(** [tokenized_word_list key_word acc] returns, given a [key_word] topic,
    a list of tokenized words from the content of the topic. *)
let tokenized_word_list (key_word:string) (acc:string list): string list =
  List.concat (List.map Tokenizer.word_tokenize
                 (Hashtbl.find content_dict key_word).content)

(** [count_word key_word acc] gives you the counter data type of
    the topic [key_word]. It is a helper function for [full_topic_dict] *)
let count_word (key_word:string) (acc:string list): Counter.t =
  Counter.make_dict (tokenized_word_list key_word acc)

(** [full_topic_dict key_word] returns a topic_dict
    data type that contains the topic and the counter dictionary
    using a record. *)
let full_topic_dict (key_word:string) : topic_dict =
  {
    topic = key_word;
    counter = (count_word key_word []);
  }

(** [all_full_topics topics acc] returns a list of topic_dict
    from a list of topics. *)
let rec all_full_topics (topics : string list)
    (acc : topic_dict list) : topic_dict list =
  match topics with
  | word::t -> all_full_topics t
                 (full_topic_dict word :: acc)
  | [] -> acc

let all_topic_dict_counter = all_full_topics topics []

(** [get_td_counter_ht td] this function creates a Hashtable of the
    topic_dict instantiations from the topic list we have above
    (all of the topics in the JSON file). *)
let rec get_td_counter_ht (td: topic_dict list)
    (acc_tbl: (string, Counter.t) Hashtbl.t) :
  ((string, Counter.t) Hashtbl.t ) =
  match td with
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl h.topic h.counter;
    get_td_counter_ht t acc_tbl

(** [all_topic_dict_counter_ht] is a Hashtable of the
    topic_dict instantiations from the topic list we have above
    (all of the topics in the JSON file). *)
let all_topic_dict_counter_ht = get_td_counter_ht
    all_topic_dict_counter (Hashtbl.create 100)

(** [get_counter topic_dict] returns the counter of [topic_dict]. *)
let get_counter topic_dict =
  (* Pervasives.print_string "help 4" ; *)
  topic_dict.counter

(** [print_topic_dict_list] is a print helper function to visualize 
    the topic_dict list from [all_topic_dict_counter]. *)
let rec print_topic_dict_list = function
  |  [] -> ()
  |  (e:topic_dict)::l -> print_string e.topic ;
    print_string " | " ; print_topic_dict_list l

let rec which_dict_has_the_word (word:string)
    (topic_dict_lst:topic_dict list)
    (acc:topic_dict list): topic_dict list =
  match topic_dict_lst with
  |topic_dict::lst->
    if Counter.mem word topic_dict.counter
    then which_dict_has_the_word word
        lst (topic_dict::acc)
    else which_dict_has_the_word word lst acc
  |[]->acc

let rec count_word_in_topic (word:string)
    (topic:string) : int =
  let counter = Hashtbl.find all_topic_dict_counter_ht topic in
  let output = Counter.find_word word (counter) in
  begin
    output;
  end

(** [tf word topic] is term frequency of [word] in [topic]
    calculated as follows:
    # of times [word] appears in [topic] / total number of topics *)
let rec tf (word:string) (topic:string) =
  float_of_int (count_word_in_topic word topic) /.
  float_of_int (Hashtbl.length all_topic_dict_counter_ht)

(** [idf word] is a statistical measure of
    how important [word] is, based on the following
    calculation: log (total # of documents / # of
    documents with [word] in them) *)
let rec idf (word:string) =
  Pervasives.log(
    float_of_int (List.length topics) /.
    float_of_int (List.length
                    ((which_dict_has_the_word word all_topic_dict_counter) [])))

(** [tfidf input_word topics] is the TF-IDF
    of an input word computed for the given [topic]. *)
let rec tfidf (input_word:string)
    (topic:string): float =
  (tf input_word topic) *. (idf input_word)

(** [construct_tfidf input_word] is a list of tuples,
    where the first element of each tuple is a topic title
    (string) and the second element is the TF-IDF value for
    that topic with respect to [input_word]. *)
let rec construct_tfidf (input_word:string) :
  (string, float) Hashtbl.t =
  let ht = Hashtbl.create 200000 in
  begin
    List.map (fun (topic : string) : unit ->
        Hashtbl.add ht topic (tfidf input_word topic)) topics;
    ht
  end

(** [get_topics_tfidf input_sent] takes in a
    sentence, tokenizes it, and for each word,
    computes a list of tfidf scores of each word
    in each document. *)
let get_topics_tfidf (input_sent:string):
  ((string, float) Hashtbl.t list) =
  let input_tokens = Similarity.remove_dups
      (Tokenizer.word_tokenize input_sent) in
  List.map (fun w -> construct_tfidf w) input_tokens

(** [questions] is a list of questions that are matched to likely 
    keywords. *)
let questions = [("who", [["is"; "are"];["a"; "an" ;"the"]]);
                 ("where", [["at";"in"]]); 
                 ("what", [["is";"are"];["a";"an";"the"]]);
                 ("when", [["on"; "in"]])]

(** [question_helper acc_tbl lst] is a helper function for 
    [question_ht] that creates a hashtable from [lst]. *)
let rec question_helper acc_tbl lst =
  match lst with
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl (fst h) (snd h);
    question_helper acc_tbl t

(** [question_ht] is a hash table version of questions. *)
let question_ht = question_helper (Hashtbl.create 4) questions

(** [check_by_category category input_tokens] returns a boolean 
    value of whether any of the elements of [lst] are present in 
    [acc_tbl]. *)
let rec check_by_category category (input_tokens:string list) = 
  match input_tokens with
  | [] -> false
  | h::t -> if List.mem h category then true else
      check_by_category category t 

(** [check_all_categories question_lst input_tokens] returns true if 
    [input_tokens] contains at least one element in each list of 
    [question_lst] and false otherwise. *)
let rec check_all_categories question_lst input_tokens = 
  match question_lst with
  | [] -> true
  | h::t -> (check_by_category h input_tokens &&
             check_all_categories t input_tokens)

(** [filter_tokens input_tokens sentence] returns true if [sentence] 
    contains the keywords associated with the question word, if a question
    word is the first element of [input_tokens]. A question word is defined
    as "who", "what", "when", and "where". *)
let filter_tokens input_tokens sentence =
  let tokenized_sentence = Tokenizer.word_tokenize sentence in
  match input_tokens with
  | [] -> true
  | h::t ->
    if (Hashtbl.mem question_ht h) then 
      let q = Hashtbl.find question_ht h in
      (check_all_categories q tokenized_sentence)
    else true

let max_jaccard_sentence (topic:string)
    (input_sent:string) : string =
  let topic_we_want = (Hashtbl.find content_dict topic) in
  let doc_sentences = topic_we_want.content in
  let input_tokens = Similarity.remove_dups
      (Tokenizer.word_tokenize input_sent) in
  let filter_tokens_fixed sentence = filter_tokens input_tokens sentence in

  (* [doc_sent_tok_dict] creates [key: sentence, value: sentence's
     word token list] dict *)
  let doc_sent_tok_dict = 
    let filtered_sentences = 
      (List.filter (filter_tokens_fixed) doc_sentences) in
    List.map (fun s ->
        (s, Similarity.remove_dups
           (Tokenizer.word_tokenize s))) filtered_sentences in

  (* [doc_sent_jac_dict] creates [key: sentence, value: sentence's
      jaccard score] dict *)
  let doc_sent_jac_dict = List.map (fun e ->
      (fst e,  Similarity.jaccard_sim
         (snd e) (input_tokens))) doc_sent_tok_dict in

  (** [find_max_j dsj_dict acc_sent acc_int]
      finds the maximum jaccard value in [dsj_dict] *)
  let rec find_max_j dsj_dict acc_sent acc_int =
    match dsj_dict with
    | [] -> acc_sent
    | h::t -> if (snd h > acc_int) then
        begin
          find_max_j t (fst h) (snd h)
        end
      else find_max_j t acc_sent acc_int

  in find_max_j doc_sent_jac_dict "" 0.0

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

(** [add_list_to_list lst1 lst2] combines the elements
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

(** [print_hashtable ht] is a helper function to print hashtable [ht]
    for debugging. *)
let print_hashtable (ht :
                       (string, float) Hashtbl.t) : unit =
  Hashtbl.iter (fun x y -> print_string x;
                 print_float y; print_newline ()) ht

(** [add_tfidf input_sent] computes the sum of
    TFIDF scores for each word in each document and returns
    the document with the highest sum. *)
let add_tfidf (input_sent : string) : string =
  let doc_list = get_topics_tfidf input_sent in
  let temp_list = List.fold_left add_list_to_list
      (Hashtbl.create 20000) doc_list in
  let good_tup = Hashtbl.fold (fun 
                (a : string)
                (b : float) 
                (c : string * float) : (string * float)
                -> if b > (snd c) then (a, b) else c)
      temp_list ("David Gries", 0.0) in

  begin
    fst good_tup
  end

let get_response (input_sent : string) : string =
  let topic_doc = add_tfidf input_sent in
  let response = begin (*  Pervasives.print_string topic_doc;  *)
    max_jaccard_sentence topic_doc input_sent
  end
  in response

(** [get_topic_dict_topic topic_dict] returns the topic of [topic_dict]. *)
let get_topic_dict_topic (topic_dict:topic_dict) = topic_dict.topic

let get_topics (topic_dict_lst:topic_dict list) = List.map 
    get_topic_dict_topic topic_dict_lst

(** Useful helpers to be called in Autocorrect. *)

(** List of all counters for each document we have in the corpus *)
let all_counters_list =
  List.map (fun a -> get_counter a) all_topic_dict_counter

(** [all_dict_list] is a list of all dictionaries for each document we have 
    in the corpus. *)
let all_dict_list =
  List.map (fun (a:Counter.t) -> Counter.get_dictionary a) all_counters_list

(** [add_list_to_list ht1z ht2] combines the elements
    of ht1 and ht2, where there are no duplicate
    string values. If the string values are equal,
      their float values are added. *)
let rec add_list_to_list (ht1: (string, int) Hashtbl.t)
    (ht2: (string, int) Hashtbl.t): (string, int) Hashtbl.t=
  Hashtbl.iter (fun (a : string) (b : int) : unit ->
      match (Hashtbl.find_opt ht1 a) with
      | Some found_b -> Hashtbl.remove ht1 a;
        Hashtbl.add ht1 a (b + found_b)
      | None -> Hashtbl.add ht1 a b
    ) ht2; ht1

let big_counter_ht =
  List.fold_left add_list_to_list (Hashtbl.create 50000) all_dict_list

let all_words =  Hashtbl.fold (fun k v acc -> k :: acc) big_counter_ht []

(** [count_all_unique_words] is the number of all unique words in all of 
    the documents. *)
let count_all_unique_words = Hashtbl.fold (fun k v acc -> acc+1) big_counter_ht 0

(* Embeddings functions start here *)

let vocab_size = count_all_unique_words 

let word2vec_dict vocab_size =
  let new_hashtbl = Hashtbl.create vocab_size in
  List.fold_left (fun ht word -> (Hashtbl.add ht word 
    (Hashtbl.length ht); ht)) new_hashtbl all_words

let vectorize input_sent vocab_size word2vec_dict =
  let vec = Array.init vocab_size (function i -> 0) in

  let rec vectorize_sent input_sent vocab_size 
    word2vec_dict (acc:int array) =
    match input_sent with
    | [] -> acc
    | h::t ->
      match Hashtbl.find_opt word2vec_dict h with
      (* | None -> vec *)
      | None -> vectorize_sent t vocab_size word2vec_dict (acc)
      (* | Some i -> Array.set acc i ((Array.get acc i) + 1);*)
       | Some i -> Array.set acc i 1;
        vectorize_sent t vocab_size word2vec_dict (acc)
  in
  vectorize_sent input_sent vocab_size word2vec_dict vec

(** [find_max_cosine topic acc_sent acc_score] finds the max cosine similarity
    of a score and sentence. *)
let find_max_cosine (topic:string) (input_tokens: string list) 
    q_vector (acc_sent_o:string) (acc_score:float) =
  let acc_sent = Tokenizer.word_tokenize acc_sent_o in
  let topic_we_want = (Hashtbl.find content_dict topic) in
  let sentences = topic_we_want.content in
  let filter_tokens_fixed sentence = filter_tokens 
        input_tokens sentence in
  let filtered_sentences = 
      (List.filter (filter_tokens_fixed) sentences) in
  let sentences_of_topic = List.map (fun s -> 
            (s, Tokenizer.word_tokenize s)) filtered_sentences; in
  let rec helper sentences q_vector acc_sent acc_sent_o acc_score =
    match sentences with
    | [] -> acc_sent_o
    | h::t ->
      let sent_vec = vectorize (snd h)
          vocab_size (word2vec_dict vocab_size)
      in
      let new_score = Similarity.cosine_sim (Array.to_list sent_vec)
          (Array.to_list q_vector)
      in
      if new_score > acc_score then helper t q_vector 
            (snd h) (fst h) new_score
      else helper t q_vector acc_sent acc_sent_o acc_score
  in
  helper sentences_of_topic q_vector acc_sent acc_sent_o acc_score

let get_response_2 (input:string) : string = 
    let question_vec = vectorize (Tokenizer.word_tokenize input) 
    count_all_unique_words (word2vec_dict count_all_unique_words) in 
    let topic = add_tfidf input in 
    let input_tokens = Similarity.remove_dups 
    (Tokenizer.word_tokenize input) in 
    (find_max_cosine topic input_tokens question_vec "" 0.0)