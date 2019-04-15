module Tokenizer = struct

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
  let sent_tokenize (text:string) : string list =
    let sentences = String.split_on_char '.'  text in
    let strip_sents = List.map (fun s -> String.trim s) sentences in
    strip_sents


  (** Make all letters of word lowercase, 
      if applicable *)
  let make_lower (word:string) : string = 
    String.lowercase_ascii word

  (** Split string s into list of strings separated by
      any one of the chars in specified char list*)
  let rec split_on_chars (chars : char list) (s : string) : string list = 
    match chars with
    | h::t -> 
      let split_strings = String.split_on_char h s in
      let split_strings2 = List.map (fun x -> split_on_chars t x) split_strings in
      List.concat split_strings2
    | [] -> [s] 


  (** Tokenizes the text and returns string list by
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


  (** Return tokenized form of text, 
      containing sentences tokens and word token
      attributes *)
  let tokenize (text:string) : t =
    {
      sentences = sent_tokenize text;
      words = word_tokenize text;
    }

  (** Return tokenized sentence list of token*)
  let get_sentences (tok:t) : string list =
    tok.sentences

  (** Return tokenized word list of token*)
  let get_words (tok:t) : string list =
    tok.words


end