open Yojson.Basic.Util
open Tokenizer
open Counter
open Similarity

(* module Extract = struct  *)
(* type counter = Counter.t *)

(** Topic Dictionary type, which contains the title of the topic (document)
    and a Counter.t which is a dictionary mapping each unique word to its
    number of occurrences in the topic/document.  *)
type topic_dict = {
  topic : string;
  counter : Counter.t;
}

(** Topic type containing the title of the topic/document and a content string
    list, which is a list of sentence tokens extracted from
    that particular topic  *)
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


(** [get_topic tp] returns the title of topic [tp] *)
let get_topic (tp: topic) : string =
  tp.topic

(** [get_all_topics] is all topic bindings of tp list *)
let get_all_topics (topic_lst: topic list) : string list =
  List.map (get_topic) topic_lst

(** [find_the_topics_content key_word top_lst] is the tokenized
    content of given topic [key_word] *)
let find_the_topics_content (key_word:string) (top_lst: topic list) =
  match (List.filter (fun x -> x.topic = key_word) top_lst) with
  |topic::lst -> topic.content
  |[]->failwith"This topc does not exist"

(** [get_content key_word json] creates topic type that has
    the topic title [key_word] and its content, all extracted from [json] *)
let get_content (key_word:string) (json: Yojson.Basic.json) : topic =
  (* Pervasives.print_string "track 2" ; *)
  {
    topic = key_word;
    content = json
              |>to_list
              |>List.map unpack_Yojson
              |>find_the_topics_content key_word
  }

(** [string_topic_dict tp acc_tbl] makes a dictionary containing
    bindings of topic name -> topic type *)
let rec string_topic_dict (tp: string list)
    (acc_tbl : ((string, topic) Hashtbl.t)): ((string, topic) Hashtbl.t) =
  match tp with
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl h (get_content h j);
    string_topic_dict t acc_tbl

(** [json_lst ] formats the json to a list *)
let json_lst = to_list j

(** [all_topics] list of all topic titles parsed from the json *)
let all_topics = List.map unpack_Yojson json_lst

(** [topics] list of topic_dicts *)
let topics = get_all_topics all_topics

(** [content_dict] topic dictionary stored for fast access
    during calculations *)
let content_dict = string_topic_dict topics (Hashtbl.create 200)

(** [all_topics topics json acc] given a string list of all topics, this
    function gives you list of topic type *)
let rec all_topics (topics : string list) (json : Yojson.Basic.json)
    (acc : topic list) : (topic list) = match topics with
  | word::t -> all_topics t (json) (get_content word json :: acc)
  | [] -> acc

(** [tokenized_word_list key_word acc] given a [key_word] topic, this function
    returns a list of tokenized words from the content of the topic *)
let tokenized_word_list (key_word:string) (acc:string list): string list =
  List.concat (List.map Tokenizer.word_tokenize
                 (Hashtbl.find content_dict key_word).content)

(** [count_word key_word acc] gives you a counter type of
    the topic [key_word] *)
let count_word (key_word:string) (acc:string list): Counter.t =
  Counter.make_dict (tokenized_word_list key_word acc)

(** [full_topic_dict key_word] returns topic_dict
    data type that has the topic and the counter dictionary
    via a representation of record *)
let full_topic_dict (key_word:string) : topic_dict =
  {
    topic = key_word;
    counter = (count_word key_word []);
  }

(** [all_full_topics topics acc] returns a list of topic_dict
    type from a list of topics *)
let rec all_full_topics (topics : string list)
    (acc : topic_dict list) : topic_dict list =
  match topics with
  | word::t -> all_full_topics t
                 (full_topic_dict word :: acc)
  | [] -> acc

(** [all_topic_dict_counter] is the list of topic_dict counters from
    the topic list we have above *)
let all_topic_dict_counter = all_full_topics topics []

(** [get_td_counter_ht td] this function actually creates a Hashtable of
    topic_dict from the topic list we have above *)
let rec get_td_counter_ht (td: topic_dict list)
    (acc_tbl: (string, Counter.t) Hashtbl.t) :
  ((string, Counter.t) Hashtbl.t ) =
  match td with
  | [] -> acc_tbl
  | h::t -> Hashtbl.add acc_tbl h.topic h.counter;
    get_td_counter_ht t acc_tbl

(** [all_topic_dict_counter_ht] is a HASHTABLE of
    topic_dict from the topic list we have above *)
let all_topic_dict_counter_ht = get_td_counter_ht
    all_topic_dict_counter (Hashtbl.create 100)

(** [get_counter topic_dict] is a getter function that allows us to use
    List.map in the following *)
let get_counter topic_dict =
  (* Pervasives.print_string "help 4" ; *)
  topic_dict.counter

(** [print_topic_dict_list] is a print helper function to visualize
    `topic_dict list` from `all_topic_dict_counter` *)
let rec print_topic_dict_list = function
  |  [] -> ()
  |  (e:topic_dict)::l -> print_string e.topic ;
    print_string " | " ; print_topic_dict_list l

(** [which_dict_has_the_word word topic_dict lst acc]
    Given a word, this function checks every
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

(** [count_word_in_topic word topic] counts how many times [word] appears
    in given [topic] *)
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

(** [get_topics_tfidf input_sent] takes in a
    sentence, tokenizes it, and for each word,
    computes a list of tfidf scores of each word
    in each document. *)
let get_topics_tfidf (input_sent:string):
  ((string, float) Hashtbl.t list) =
  let input_tokens = Similarity.remove_dups
      (Tokenizer.word_tokenize input_sent) in
  List.map (fun w -> construct_tfidf w) input_tokens

(** [max_jaccard_sentence topic input_sent]
    Returns sentence in specified document topic containing the
    maximum jaccard similarity metric, compared with the
    input sentence (question we ask the chatbot).
    This is the last function we will need to return the
    calculated response to the user's input *)
let max_jaccard_sentence (topic:string)
    (input_sent:string) : string =
  let topic_we_want = (Hashtbl.find content_dict topic) in
  let doc_sentences = topic_we_want.content in
  let input_tokens = Similarity.remove_dups
      (Tokenizer.word_tokenize input_sent) in

  (* [doc_sent_tok_dict] creates [key: sentence, value: sentence's
     word token list] dict *)
  let doc_sent_tok_dict = List.map (fun s ->
      (s, Similarity.remove_dups
         (Tokenizer.word_tokenize s))) doc_sentences in

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
    for debugging *)
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
  let good_tup = Hashtbl.fold (fun (a : string)
                                (b: float) (c : string * float) : (string * float) ->
                                if b > (snd c) then (a, b) else c)
      temp_list ("David Gries", 0.0) in

  begin
    fst good_tup
  end

(** [get_response input_sent] calculates a response based on [input_sent] sentence
    and returns the response*)
let get_response (input_sent : string) : string =
  let topic_doc = add_tfidf input_sent in
  let response = begin (*  Pervasives.print_string topic_doc;  *)
    max_jaccard_sentence topic_doc input_sent
  end
  in response

let get_topic_dict_topic (topic_dict:topic_dict) = topic_dict.topic

let get_topics (topic_dict_lst:topic_dict list) = List.map get_topic_dict_topic topic_dict_lst

(** Useful helpers to be called in Autocorrect *)

(** List of all counters for each document we have in the corpus *)
let all_counters_list =
  List.map (fun a -> get_counter a) all_topic_dict_counter

(** List of all dictionaries for each document we have in the corpus *)
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

(** Large hashtable with key : word, value: num occurences of word,
    for all documents in the corpus, i.e. all the counter dicts
    for each topic combined together *)
let big_counter_ht =
  List.fold_left add_list_to_list (Hashtbl.create 50000) all_dict_list

(** List of all the unique words in documents *)
let all_words =  Hashtbl.fold (fun k v acc -> k :: acc) big_counter_ht []

(** Number of all unique words in the documents *)
let count_all_unique_words = Hashtbl.fold (fun k v acc -> acc+1) big_counter_ht 0

(* Embeddings functions start here *)

(** [vocab_size] is the number of unique words in all of the data provided by
    json. *)
let vocab_size =
  count_all_unique_words

(** [word2vec_dict] creates a hastable that maps each unique word to a unique
    index, starting from 0.  *)
let word2vec_dict vocab_size =
  let new_hashtbl = Hashtbl.create vocab_size in
  List.fold_left (fun ht word -> (Hashtbl.add ht word (Hashtbl.length ht); ht)) new_hashtbl all_words

(** [vectorize_sent input_sent vocab_size word2vec_dict] constructs
    a vector representation of a sentence by incrementing a vector of
    size [vocab_size] at indices corresponding to specific vocabulary
    words found in word2vec_dict.
    For example: if the sentence is "the dog ate my homework"
    then the vector will have values of 1 at indices corresponding
    to words like "the", "dog" and others, and the rest of the vector will be
    all 0 values. *)
let wrap input_sent vocab_size word2vec_dict =
  let vec = Array.init vocab_size (function i -> 0) in

  let rec vectorize_sent input_sent vocab_size word2vec_dict (acc:int array) =
    match input_sent with
    | [] -> acc
    | h::t ->
      match Hashtbl.find_opt word2vec_dict h with
      | None -> vec
      | Some i -> Array.set vec i (Array.get vec i + 1);
        vectorize_sent t vocab_size word2vec_dict (vec)
  in
  vectorize_sent input_sent vocab_size word2vec_dict vec

(** [find_max_cosine topic acc_sent acc_score] finds the max cosine similarity
    of a score and sentence. *)
let find_max_cosine topic q_vector acc_sent acc_score =
  let topic_we_want = (Hashtbl.find content_dict topic) in
  let doc_sentences = topic_we_want.content in
  let rec helper sentences q_vector acc_sent acc_score =
    match sentences with
    | [] -> acc_sent
    | h::t ->
      let sent_vec = wrap (Tokenizer.word_tokenize h)
          vocab_size (word2vec_dict vocab_size)
      in
      let new_score = Similarity.cosine_sim (Array.to_list sent_vec)
          (Array.to_list q_vector)
      in
      (* Pervasives.print_float new_score; Pervasives.prerr_float acc_score; *)
      if new_score > acc_score then helper t q_vector h new_score
      else helper t q_vector acc_sent acc_score
  in
  helper doc_sentences q_vector acc_sent acc_score

(* let print_hash_debug ht =
   Hashtbl.iter (fun x y -> print_string x;
                 print_int y; print_newline ()) ht *)
let debug =
  let q_vector_test = wrap ["Who"; "is"; "David"; "Gries?"] 21913 (word2vec_dict 21913) in
  Pervasives.print_string (find_max_cosine "David Gries" q_vector_test "" 0.0);
  (* print_hash_debug (word2vec_dict vocab_size) *)
  (* Pervasives.print_int vocab_size; *)
  (* Pervasives.print_int (List.length all_words) *)
  (* Array.iter (Pervasives.print_int) (wrap ["elon"; "musk"; "tesla"; "david"; "gries"; "facebook"] vocab_size (word2vec_dict vocab_size)) *)
