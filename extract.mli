(** Module for processing the data from the json file, 
    storing each document's data in a dictionary, and executing
    the main computations for constructing the bot's response. *)

open Counter

(** Topic Dictionary type, which contains the title of the topic (document)
    and a Counter.t, a dictionary mapping each unique word to its
    number of occurrences in the topic/document.  *)
type topic_dict

(** Topic type containing the title of the topic/document and an attribute
    called content, which is a string list of sentence tokens extracted from
    that particular topic. *)
type topic

(** [get_response input_sent] calculates a response based on [input_sent]
    and returns the response using the Jaccard Similarity algorithm. *)
val get_response : string -> string

(** Large hashtable with key : word, value: num occurences of word,
    for all documents in the corpus, i.e. all the counter dicts
    for each topic combined together *)
val big_counter_ht : (string, int) Hashtbl.t

(** List of all the unique words in documents *)
val all_words : string list

(** [get_response_2 input_sent] calculates a response based on 
    [input_sent] sentence and returns the response using the Cosine 
    Similarity algorithm. *)
val get_response_2 : string -> string

(** [vectorize input_sent vocab_size word2vec_dict] constructs
    a vector representation of a sentence by incrementing a vector of
    size [vocab_size] at indices corresponding to specific vocabulary
    words found in word2vec_dict.
    For example: if the sentence is "the dog ate my homework"
    then the vector will have values of 1 at indices corresponding
    to words like "the", "dog" and others, and the rest of the vector will be
    all 0 values. *)
val vectorize : 'a list -> int -> ('a, int) Hashtbl.t -> int array

(** [word2vec_dict] creates a hastable that maps each unique word to a unique
    index, starting from 0.  *)
val word2vec_dict : int -> (string, int) Hashtbl.t

(** [count_word_in_topic word topic] counts how many times [word] appears
    in given [topic]. *)
val count_word_in_topic : string -> string -> int

(** [get_topics topic_dict_lst] returns a topic list of each topic
    in each topic_dict in [topic_dict_lst]. *)
val get_topics : topic_dict list -> string list

(** [which_dict_has_the_word word topic_dict lst acc]
    Given a word, this function checks every
    topic_dict generated from the above topic list,
    and if the word is in the topic_dict's counter
    dictionary, then we add that entire topic_dict
    type to the accumulator.  *)
val which_dict_has_the_word : string -> topic_dict list -> 
topic_dict list -> topic_dict list

(** [all_topic_dict_counter] is the list of topic_dict counters from
    the topic list we have above *)
val all_topic_dict_counter : topic_dict list

(** [max_jaccard_sentence topic input_sent]
    Returns sentence in specified document topic containing the
    maximum jaccard similarity metric, compared with the
    input sentence (question we ask the chatbot).
    This is the last function we will need to return the
    calculated response to the user's input *)
val max_jaccard_sentence : string -> string -> string

(** [vocab_size] is the number of unique words in all of the data provided by
    json. *)
val vocab_size : int