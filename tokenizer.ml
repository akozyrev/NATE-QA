(** Module used to tokenize the words in a document, as well
as tokenize the sentences within the doc. Contains functionality 
to tokenize sentences and tokenize words. *)
module Tokenizer = struct

  (** Tokenizer type, which contains tokenized
      sentences and tokenized words in 2 separate list
      attributes. *)
  type t = {
    sentences : string list;
    words : string list;
  }

  (** [sent_tokenize text] tokenizes the string text into sentences,
      returns list of sentences with process:
      1. seperate sentences into string list by '.'
      2. trimming all from start with spaces, \n, \t
  *)
  let sent_tokenize (text:string) : string list =
    let sentences = String.split_on_char '.'  text in
    let strip_sents = List.map (fun s -> String.trim s) sentences in
    List.filter (fun s -> s <> "") strip_sents

  (** [make_lower word] make all letters of the word lowercase,
      if applicable. *)
  let make_lower (word:string) : string =
    String.lowercase_ascii word

  (** [split_on_chars chars s] splits string 
      s into list of strings separated
      by any one of the chars in specified char list*)
  let rec split_on_chars (chars : char list) 
    (s : string) : string list =
    match chars with
    | h::t ->
      let split_strings = String.split_on_char h s in
      let split_strings2 = List.map (fun x -> 
          split_on_chars t x) split_strings in
      List.concat split_strings2
    | [] -> [s]

  (** [word_tokenize text] tokenizes the text and returns a string list in
    two steps:
      1. making all the words lowercase
      2. splitting the words into a string list by punctuation and spaces
  *)
  let word_tokenize (text:string) : string list =
    let puncts = [' '; '\n'; '\t'; ';'; ','; '.'; ':'; '?'; '$';
                  '-'; '!'; '#'; '%'; '^'; '&'; '*';
                  '('; ')'; '+'; '='; '/'; '\\';
                  '\"' ] in
    let words = split_on_chars puncts text in
    let filter_words = List.filter (fun w -> w <> "") words in
    let strip_words = List.map (fun w -> String.trim w) filter_words in
    let all_lower = List.map (fun w -> make_lower w) strip_words in
    all_lower

  (** [tokenize text] is the tokenized form of [text] in a document, 
  containing two attributes: a string list of sentence tokens and a 
  string list of word tokens. *)
  let tokenize (text:string) : t =
    {
      sentences = sent_tokenize text;
      words = word_tokenize text;
    }

  (** [get_sentences] returns a string list of tokenized sentences 
  in a document [tok]. *)
  let get_sentences (tok:t) : string list =
    tok.sentences

  (** [get_words tok] returns a tokenized word list of [tok]. *)
  let get_words (tok:t) : string list =
    tok.words

end
