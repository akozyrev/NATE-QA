open Tokenizer
open Counter
open Extract
open Suggestion

(** The bot's introductory welcome message *)
let intro_txt = "\nMy name is NATE [Nonhuman Abstract Tech Expert]," ^ 
                "\nand I'm here to answer all your CS-related questions.\n"^
                "Ask me about common subfields, people, or companies.\n\n"

(** The bot's example questions *)
let examples = "Some things you may ask me include:" 
               ^ " \n- Where does David Gries live?\n- " ^ 
               "What is natural language processing used for?"^
               "\n- Recommend me a good machine learning course.\n"

(** A list of commands the bot can respond to *)
let help = "Commands: 
| about: introduce myself and my thought process
| examples: provide examples of questions you can ask
| help: give list of commands you can type
| <your question>: ask me a question to receive response
| bye: exit the program \n"

(** More in depth summary about the bot *)
let about = "I was developed by a CS 3110 group and named after their professor, 
Dr. Nate Foster. I gather my knowledge from an extensive text corpus of Wikipedia 
articles related to the field of CS. I use 2 NLP/IR algorithms to answer your 
question: TFIDF--to find the most relevant document to search for the answer--and 
Jaccard similarity--to find the best sentence in the document that answers your question.
I can also find misspelled words in your question and provide possible corrections 
using my autocorrection feature, which implements the Levenshtein distance algorithm.
Lastly, I can recommend other topics for you to learn about, given your input question. 
That being said, I am still getting smarter and faster with each day!\n"

(** [process input] will return the correct response to the input
    the user provides. *)
let process input = 
  let response = Extract.get_response input in
  let a_response = Autocorrect.check_correctness input in
  let sug = Suggestion.suggestion input in

  match a_response with 
  | "all correct" -> begin match input, response with 
      | "about", _ -> about
      | "examples", _ -> examples 
      | "help", _ -> help 
      | _ , "" -> "Please input a valid question.\n"
      | _, _  ->  response ^ "\n" ^ sug ^ "\n" end
  | _ -> "Autocorrect found word(s) not identified: " ^ a_response 


(** [response input] provides the user with a response to the input
    they provide. It also prompts the user for another question, if
    the user wishes to continue speaking to the chatbot. *)
let rec response input =
  match input with
  | "bye" -> ANSITerminal.(print_string [magenta; Bold] (("\nThank you for talking to me!\n")^("")))
  | input ->  
    let output = process input in
    print_string  "\n";
    if output = "Please input a valid question.\n" then 
      ANSITerminal.(print_string [red; Bold] output) else
      ANSITerminal.(print_string [blue; Bold] output);
    print_string  "\n> ";

    let new_input = Pervasives.read_line () in response new_input

(** [main ()] greets the user and starts the chatbot. *)
let main () =
  ANSITerminal.(print_string [Bold; Blink; red] "\nHello!\n");
  ANSITerminal.(print_string [Bold; magenta] intro_txt);
  (* print_endline (examples); *)
  (* print_endline (help); *)
  ANSITerminal.(print_string [blue; Bold] help);
  print_string  "\n> ";
  response (Pervasives.read_line ())

(* Executes the chatbot. *)

let () = main ()
