(**Module for tokenizing the document into 
sentences, and the sentences into words.*)
module Tokenizer : sig

  (** Tokenizer type, containing tokenized 
      sentences and tokenized words in 2 list 
      attributes *)
  type t = {
    sentences : string list;
    words : string list;
  }

  (** Tokenizes the string text into sentences,
      returns list of sentences with process:
      1. seperate sentences into string list by '.'
      2. trimming all from start with spaces, \n, \t
  *)
  val sent_tokenize: (string) -> string list

  (** Make all letters of word lowercase, 
      if applicable *)
  val make_lower : (string) -> string 

  (** Split string s into list of strings separated by
      any one of the chars in specified char list*)
  val split_on_chars : (char list) -> (string) -> string list 

  (** Tokenizes the text and returns string list by
      1. making words all lowercase
      2. split into list by punctuation and spaces
  *)
  val word_tokenize : (string) -> string list

  (** Return tokenized form of text, 
      containing sentences tokens and word token
      attributes *)
  val tokenize : (string) -> t 

  (** Return tokenized sentence list of token*)
  val get_sentences : (t) -> string list 

  (** Return tokenized word list of token*)
  val get_words : (t) -> string list 


end
