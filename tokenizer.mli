(** Module used to tokenize the words in a document, as well
as tokenize the sentences within the doc. Contains functionality 
to tokenize sentences and tokenize words.*)
module Tokenizer : sig

  (** Tokenizer type, containing tokenized 
      sentences and tokenized words in 2 list 
      attributes *)
  type t = {
    sentences : string list;
    words : string list;
  }

  (** [sent_tokenize text] tokenizes the string text into sentences,
      returns list of sentences with process:
      1. seperate sentences into string list by '.'
      2. trimming all from start with spaces, \n, \t
  *)
  val sent_tokenize: (string) -> string list

  (** [make_lower word] make all letters of the word lowercase,
      if applicable. *)
  val make_lower : (string) -> string 

  (** [split_on_chars chars s] splits string 
      s into list of strings separated
      by any one of the chars in specified char list*)
  val split_on_chars : (char list) -> (string) -> string list 

  (** [word_tokenize text] tokenizes the text and returns a string list in
    two steps:
      1. making all the words lowercase
      2. splitting the words into a string list by punctuation and spaces
  *)
  val word_tokenize : (string) -> string list

  (** [tokenize text] is the tokenized form of [text] in a document, 
  containing two attributes: a string list of sentence tokens and a 
  string list of word tokens. *)
  val tokenize : (string) -> t 

  (** [get_sentences] returns a string list of tokenized sentences 
  in a document [tok]. *)
  val get_sentences : (t) -> string list 

  (** [get_words tok] returns a tokenized word list of [tok]. *)
  val get_words : (t) -> string list 


end
