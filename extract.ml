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
  "Artifical Intelligence";
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
  "youTube";
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
  {
    topic = key_word;
    content = json |> member key_word  
              |> member "content" |> to_list 
              |> List.map to_string;
  }

(* given a string list of all topics, 
    this one gives you list of topic type *)
let rec all_topics (topics : string list) 
    (json : Yojson.Basic.json) (acc : topic list) : topic list = 
  match topics with 
  |word::t -> all_topics t (json) (get_content word json :: acc)
  |[] -> acc

(* given a topic/key word, this one gives 
   you a list of tokenized word of the content 
   of the topic *)
let tokenized_word_list (key_word:string) 
    (json: Yojson.Basic.json) 
    (acc:string list): string list = 
  List.concat (List.map Tokenizer.word_tokenize 
                 (get_content key_word json).content)

(* gives a counter type of the topic *)
let count_word (key_word:string) 
    (json: Yojson.Basic.json) 
    (acc:string list): counter = 
  Counter.make_dict (tokenized_word_list key_word json acc)

(* returns topic_dict data type that has 
    the topic and the counter dictionary 
    via a representation of record *)
let full_topic_dict (key_word:string) 
    (json: Yojson.Basic.json) : topic_dict = 
  {
    topic = key_word;
    counter = (count_word key_word json []);
  }

(* returns a list of topic_dict 
    type from a list of topics *)
let rec all_full_topics (topics : string list) 
    (json : Yojson.Basic.json) 
    (acc : topic_dict list) : topic_dict list = 
  match topics with 
  |word::t -> all_full_topics t 
                json (full_topic_dict word json :: acc)
  |[] -> acc

(* this function actually creates a list of 
   topic_dict from the topic list we have above *)
let all_topic_dict_counter = all_full_topics topics j []

(* a getter function that allows us to use 
    List.map in the following *)
let get_counter topic_dict =
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
let rec which_dict_has_the_word (word:string)
    (topic_dict_lst:topic_dict list)
    (acc:topic_dict list): topic_dict list =
  match topic_dict_lst with
  |topic_dict::lst-> 
    if Counter.mem (Tokenizer.make_lower word) topic_dict.counter 
    then which_dict_has_the_word (Tokenizer.make_lower word) 
        lst (topic_dict::acc) 
    else which_dict_has_the_word
        (Tokenizer.make_lower word) lst acc
  |[]->acc

(* the above function only process one word;
      this one just patterns matches a string
      list and merge each `topic_dict list` into
      a whole list with removing duplicates *)
let rec process_phrase (words:string list)
    (topic_dict_lst:topic_dict list)
    (acc:topic_dict list) : topic_dict list =
  match words with
  |word::lst -> (process_phrase lst topic_dict_lst
                   (which_dict_has_the_word word topic_dict_lst []))
  |[]->acc

(* a wrapper pretty much that uses tokenize a string into a
   string list and apply the above function to generate a
   `topic_dict list` and use `Similarity.remove_dups` to remove duplicates*)
let which_dict_has_the_words (words)(topic_dict_lst)(acc) : topic_dict list=
  let tokens = Tokenizer.word_tokenize words in
  Similarity.remove_dups (process_phrase tokens topic_dict_lst acc)

let rec most_common_dict (word:string)
    (topic_dict_lst:topic_dict list) : string =
  let relevant_dicts = which_dict_has_the_word word topic_dict_lst [] in
  let rec find_max_k tds acc_int acc_topic =
    match tds with
    | [] -> acc_topic
    | h::t ->
      let num_occurence = (Counter.find_word word h.counter) in
      if num_occurence > acc_int then find_max_k t num_occurence h.topic
      else find_max_k t acc_int acc_topic

  in (find_max_k relevant_dicts 0 "")

let rec count_word_in_topic (word:string) (topic:string) (json): int =
  let topic_dict_we_want = full_topic_dict topic json in
  let counter = get_counter topic_dict_we_want in
  Counter.find_word word (counter)


(** [tf word topic] is term frequency of [word] in [topic]
    calculated as follows:
    # of times [word] appears in [topic] / total number of topics *)
let rec tf (word:string) (topic:string) =
  float_of_int (count_word_in_topic word topic j) /.
  float_of_int (Counter.get_length (get_counter (full_topic_dict topic j)))

(** [idf word] is a statistical measure of how important [word] is, based
    on the following calculation:
    log (total # of documents / # of documents with [word] in them) *)
let rec idf (word:string) =
  Pervasives.log(
    float_of_int (List.length topics) /.
    float_of_int (List.length
                    ((which_dict_has_the_word word all_topic_dict_counter) [])))

(** [tfidf input_word topics] is the TF-IDF of an input word computed for the 
    given [topic] .*)
let rec tfidf (input_word:string) (topic:string) =
  (tf input_word topic) *. (idf input_word)

(** [construct_tfidf input_word] is a list of tuples, where the first 
    element of each tuple is a topic title (string) and the second element 
    is the TF-IDF value for that topic with respect to [input_word] *)
let rec construct_tfidf (input_word:string) =
  let rec tfidf_topics topics =
    match topics with 
    | [] -> []
    | h::t -> (h, (tfidf input_word h)) :: [] @ tfidf_topics t 
  in 
  tfidf_topics topics 

(* [get_topics_tfidf input_sent] takes in a sentence, tokenizes it, and for each word, 
  computes a list of tfidf scores of each word in each document. *)
let get_topics_tfidf (input_sent:string): ((string*float) list list) = 
  let input_tokens = Similarity.remove_dups 
        (Tokenizer.word_tokenize input_sent) in
  List.map (fun w -> construct_tfidf w) input_tokens

(** [add_elt_to_list doc lst] appends the tfidf score of a word for each document. It returns a list
  where the string is the "topic" and the value is the combined tfidf scores of each word
  in that document. *)
let rec add_elt_to_list (doc : string*float) (lst: (string*float) list) =
  match lst with
  | [] -> [doc]
  | h::t -> if fst doc = fst h then (fst h, (snd doc) +. (snd h))::t else h::(add_elt_to_list doc t)

(* [add_list_to_list lst1 lst2] adds the elements of lst1 to lst2. *)
let rec add_list_to_list (lst1: (string*float) list) (lst2: (string*float) list) =
  List.fold_left (fun y x -> add_elt_to_list x y) lst1 lst2

(* [add_tfidf input_sent] computes the sum of TFIDF scores for each word in each document and returns
  the document with the highest sum. *)
let add_tfidf (input_sent : string) : string =
  let doc_list = get_topics_tfidf input_sent in
  let temp_list = List.fold_left (fun y x -> add_list_to_list x y) [] doc_list in
  fst (List.fold_left (fun y x -> (if ((snd y) >= (snd x)) then y else x)) ("", 0.0) temp_list)