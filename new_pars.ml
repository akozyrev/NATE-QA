open Yojson.Basic.Util
open Tokenizer
open Counter
open Similarity


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
let j = Yojson.Basic.from_file "corpus/improved_data.json"

let unpack_Yojson (json: Yojson.Basic.json): topic = 
  {
    topic = member "topic" json|>to_string;
    content = member "content" json
              |>to_list 
              |> List.map to_string;
  }

let json_lst = to_list j
let topic_list = List.map unpack_Yojson json_lst