open Yojson.Basic.Util
open Tokenizer
open Counter
open Similarity

let topics = [
  "David Gries";
  "Alan Turing";
  "Algorithm";
  "Anita Borg";
  "Apple";
  "Artifical Intelligence";
  "Barbara Liskov";
  "Bill Gates";
  "Computer Graphics";
  "Computer Science";
  "Computer Vision";
  "Cornell University";
  "David Gries";
  "Deep Learning";
  "Deepmind";
  "Distributed Computing";
  "Elon Musk";
  "Embedded Systems";
  "Facebook";
  "Grace Hopper";
  "Human Computer Interaction";
  "Intel";
  "iPad";
  "iPhone";
  "Jeff Bezos";
  "Logic";
  "Machine Learning";
  "Mark Zuckerberg";
  "Mathematics";
  "Microsoft";
  "Natural Language Processing";
  "Pinterest";
  "Privacy";
  "Programming Languages";
  "Reinforcement Learning";
  "Scott Belsky";
  "Sheryl Sandberg";
  "Silicon Valley";
  "Slack Technologies";
  "Steve Jobs";
  "Tesla";
  "Tracy Chou";
  "Turing Award";
  "Twitter";
  "Uber";
  "Venture Capital";
  "Warby Parker";
  "Amazon Company";
  "youTube";
]

type counter = Counter.t

type topic_dict = {
  topic : string;
  counter : counter;
}

type topic = {
  topic : string;
  content : string list;
}


let j = Yojson.Basic.from_file "corpus/data.json"

let get_content (key_word:string) (json: Yojson.Basic.json) : topic= 
  {
    topic = key_word;
    content = json |> member key_word  |> member "content" |> to_list |> List.map to_string;
  }

let rec all_topics (topics : string list) (json : Yojson.Basic.json) (acc : topic list) : topic list = 
  match topics with 
  |word::t -> all_topics t (json) (get_content word json :: acc)
  |[] -> acc

let tokenized_word_list (key_word:string) (json: Yojson.Basic.json) (acc:string list): string list = 
  List.concat (List.map Tokenizer.word_tokenize (get_content key_word json).content)

let count_word (key_word:string) (json: Yojson.Basic.json) (acc:string list): counter = 
  Counter.make_dict (tokenized_word_list key_word json acc)

let full_topic_dict (key_word:string) (json: Yojson.Basic.json) : topic_dict = 
  {
    topic = key_word;
    counter = (count_word key_word json []);
  }

let rec all_full_topics (topics : string list) (json : Yojson.Basic.json) (acc : topic_dict list) : topic_dict list = 
  match topics with 
  |word::t -> all_full_topics t json (full_topic_dict word json :: acc)
  |[] -> acc

let all_topic_dict_counter = all_full_topics topics j []

let get_counter topic_dict =
  topic_dict.counter


let rec print_topic_dict_list = function 
  |  [] -> ()
  |  (e:topic_dict)::l -> print_string e.topic ; print_string " | " ; print_topic_dict_list l

let rec which_dict_has_the_word (word:string)(topic_dict_lst:topic_dict list)(acc:topic_dict list): topic_dict list =
  match topic_dict_lst with
  |topic_dict::lst-> 
    if Counter.mem (Tokenizer.make_lower word) topic_dict.counter then which_dict_has_the_word (Tokenizer.make_lower word) lst (topic_dict::acc) 
    else which_dict_has_the_word (Tokenizer.make_lower word) lst acc
  |[]->acc

let rec process_phrase (words:string list)(topic_dict_lst:topic_dict list)(acc:topic_dict list) : topic_dict list = 
  match words with
  |word::lst -> (process_phrase lst topic_dict_lst (which_dict_has_the_word word topic_dict_lst []))
  |[]->acc

let which_dict_has_the_words (words)(topic_dict_lst)(acc)= 
  let tokens = Tokenizer.word_tokenize words in
  Similarity.remove_dups (process_phrase tokens topic_dict_lst acc)
