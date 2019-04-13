module Tokenizer = struct

type t = {
  sentences : string list;
  words : string list;
}


let string_to_cl (word:string) : char list = 
  List.init (String.length word) (String.get word)

let cl_to_string (lst:char list) : string = 
    String.concat "" (List.map (String.make 1) lst)


(*not used*)
let preprocess (text: string) : string = 
  let cl = string_to_cl text in
  let rec delete_chars lst = 
    match lst with 
    | [] -> []
    | h::t -> if h = '\\' || h = '\"' then delete_chars t
              else h::(delete_chars t) in

   cl_to_string (delete_chars cl)


(* TODO:
1. seperate by period
2. remove all \n if applicable
*)
  let sent_tokenize (text:string) : string list =
    let sentences = String.split_on_char '.'  text in
    let strip_sents = List.map (fun s -> String.trim s) sentences in

    strip_sents


let rec filter_punct (lst: char list) : char list =
  let puncts = [';'; ','; '.'; ':'; '?'; '$'; 
                '-'; '!'; '#'; '%'; '^'; '&'; '*';
                '('; ')'; '+'; '='; '/'; '_'; '\\';
                 '\"' ] in
  match lst with 
    | [] -> []
    | h::t -> if ((h >= 'a' && h <= 'z') 
                  || (h >= 'A' && h <= 'Z')
                  || (h >= '0' && h <= '9')) then lst
                  (*TODO: should we only filter edges, or everything?*)
              else if List.mem h puncts then filter_punct t
              else h::filter_punct t

  

let drop_punct (word:string) : string =
  let filter_left = cl_to_string (filter_punct (string_to_cl word)) in
  let filter_right = cl_to_string (List.rev 
            (filter_punct (List.rev (string_to_cl filter_left)))) in

  filter_right

let make_lower (word:string) : string = 
  String.lowercase_ascii word

(*TODO: 
0. remove filler words, i.e. a, the, is ???
1. make all lowercase
2. remove all punctuation except - ;
*)
  let word_tokenize (text:string) : string list = 
    let words = String.split_on_char ' '  text in
    let strip_words = List.map (fun w -> String.trim w) words in
    let strip_punct = List.map (fun w -> drop_punct w) strip_words in
    let all_lower = List.map (fun w -> make_lower w) strip_punct in

    all_lower


  let tokenize (text:string) : t =
    {
      sentences = sent_tokenize text;
      words = word_tokenize text;
    }

  let x = Str.regexp
  
end
