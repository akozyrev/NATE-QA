type counter
type topic_dict 
type topic

(**
   Returns a topic type that the topic and its content
*)
val get_content : string -> Yojson.Basic.json -> topic

(** 
   Returns all topics in the json file, represented by a topic list 
*)
val all_topics : string list -> Yojson.Basic.json -> topic list -> topic list

(** 
   Returns tokenizedd content of a topic
*)
val tokenized_word_list : string -> Yojson.Basic.json -> string list -> string list

(**
   Returns a counter of the content of a topic
*)
val count_word : string -> Yojson.Basic.json -> string list -> counter

(** 
   Returns a topic_dict type of a topic
*)
val full_topic_dict : string -> Yojson.Basic.json -> topic_dict

(**
   Returns a list of topic_dict of a list of topics
*)
val all_full_topics : string list -> Yojson.Basic.json -> topic_dict list -> topic_dict list

(**
   Returns a list of topict_dict from a certain list of topics
*)
val all_topic_dict_counter : topic_dict list

(**
   Prints a list of topic_dict
*)
val print_topic_dict_list : topic_dict list -> unit

(**
   [which_dict_has_the_word word t_d_lst [] ]returns all topic_dicts that have the word 
*)
val which_dict_has_the_word : string -> topic_dict list -> topic_dict list -> topic_dict list

(**
   Returns the number of occurance of a word in a topic's content
*)
val count_word_in_topic : string -> string -> int

(** Return sentence in specified document topic containing the 
    maximum jaccard similarity metric, compared with the 
    input sentence (question we ask the chatbot).
    This is the last function we will need to return the 
    calculated response to the user's input *)
val max_jaccard_sentence : string ->  string -> Yojson.Basic.json -> string

val get_topic : topic_dict -> string

val get_topics : topic_dict list -> string list

(** [add_elt_to_list doc lst] returns a list of tuples where the first value corresponds
  to the document and the value is the combined tfidf scores of each word in that document. *)
val add_elt_to_list : (string * float) -> (string, float) Hashtbl.t -> (string, float) Hashtbl.t

(* [add_list_to_list lst1 lst2] combines the elements of lst1 and lst2, where there are no duplicate 
  string values. If the string values are equal, their float values are added. *)
val add_list_to_list : (string, float) Hashtbl.t -> (string, float) Hashtbl.t -> (string, float) Hashtbl.t 

(* [add_tfidf input_sent] computes the sum of TFIDF scores for each word in each document and returns
  the document with the highest sum. *)
val add_tfidf : string -> string

val get_response : string -> string

(* val get_response_2 : string -> string *)