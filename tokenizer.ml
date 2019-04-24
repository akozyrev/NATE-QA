(**Module used to tokenize the document, as well
as sentences within the doc. Contains sentence tokenize
and word tokenize functionality*)

(**Module used to tokenize the document, as well
as sentences within the doc. Contains sentence tokenize
and word tokenize functionality*)
module Tokenizer = struct

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
  let sent_tokenize (text:string) : string list =
    let sentences = String.split_on_char '.'  text in
    let strip_sents = List.map (fun s -> String.trim s) sentences in
    List.filter (fun s -> s <> "") strip_sents

  (** [make_lower word] make all letters of word lowercase,
      if applicable *)
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

  (** [word_tokenize text] tokenizes the text and returns string list by
      1. making words all lowercase
      2. split into list by punctuation and spaces
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

  (** [tokenize] is the tokenized form of text,
      containing sentences tokens and word token
      attributes *)
  let tokenize (text:string) : t =
    {
      sentences = sent_tokenize text;
      words = word_tokenize text;
    }

  (** [get_sentences] returns tokenized sentence list of token *)
  let get_sentences (tok:t) : string list =
    tok.sentences

  (** [get_words tok] returns a tokenized word list of [tok] *)
  let get_words (tok:t) : string list =
    tok.words

end
