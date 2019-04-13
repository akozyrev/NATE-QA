module Tokenizer = struct

type t = {
  sentences : string list;
  words : string list;
}

(*
1. seperate by period
2. remove all \n if applicable
*)
  let sent_tokenize (text:string) : string list =
    failwith "Unimplemented"


(*TODO: remove filler words, i.e. a, the, is ???
1. make all lowercase
2. remove all punctuation except - ;
*)
  let word_tokenize (text:string) : string list = 
    failwith "Unimplemented"


  let tokenize (text:string) : t =
    {
      sentences = sent_tokenize text;
      words = word_tokenize text;
    }


end
