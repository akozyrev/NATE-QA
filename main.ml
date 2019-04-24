open Tokenizer
open Counter
open Extract
open Suggestion
open Similarity

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
question: TFIDF--to find the most relevant document to search for the answer--and either
Jaccard or Cosine similarity--to find the best sentence in the document that answers your question.
I can also find misspelled words in your question and provide possible corrections 
using my autocorrection feature, which implements the Levenshtein distance algorithm.
Lastly, I can recommend other topics for you to learn about, given your input question. 
That being said, I am still getting smarter and faster with each day!\n"

let process_cos input = 
  let a_response = Autocorrect.check_correctness input in
  let sug = Suggestion.suggestion input in
  match a_response with 
  | "all correct" -> 
    begin 
    match input with
    | "about" -> about
    | "examples" -> examples
    | "help" -> help
    | "" -> "Please input a valid question.\n"
    | _ -> 
    let cos_response = Extract.get_response_2 input in 
    (cos_response) ^ "\n\n" ^ sug 
    end
  | _ -> "Autocorrect found word(s) not identified: " ^ a_response 

let process_jac input = 
  match input with
    | "about" -> about
    | "examples" -> examples
    | "help" -> help
    | _ -> begin
        let a_response = Autocorrect.check_correctness input in
        match a_response with 
        | "all correct" -> begin 
          let response = Extract.get_response input in
          let sug = Suggestion.suggestion input in
          match response with  
            | "" -> "Please input a valid question.\n"
            | _  ->  response ^ "\n\n" ^ sug
             end
        | _ -> "Autocorrect found word(s) not identified: " ^ a_response end

(** [response input] provides the user with a response to the input
    they provide. It also prompts the user for another question, if
    the user wishes to continue speaking to the chatbot. *)
let rec response algo input =
  match input with
  | "bye" -> ANSITerminal.(print_string [magenta; Bold] 
        (("\nThank you for talking to me!\n")^("")))
  | input ->  
    let output = algo input in
    print_string  "\n";
    if output = "Please input a valid question.\n" then 
      ANSITerminal.(print_string [red; Bold] output) else
      ANSITerminal.(print_string [blue; Bold] output);
    print_string  "\n> ";

    let new_input = Pervasives.read_line () in response algo new_input

(** [main ()] greets the user and starts the chatbot. *)
let main () =
  ANSITerminal.(print_string [Bold; Blink; red] "\nHello!\n");
  ANSITerminal.(print_string [Bold; magenta] intro_txt);
  (* print_endline (examples); *)
  (* print_endline (help); *)
  ANSITerminal.(print_string [green; Bold] 
        "Which search algorithm would you like to use for this session?\n");
  ANSITerminal.(print_string [cyan; Bold] 
        "\n> 1 for Jaccard Similarity\n> 2 for Cosine Similarity\n");
  print_string "\n> ";
  let algo = Pervasives.read_line () in
  begin
  match algo with
  | "1" -> 
  begin 
  ANSITerminal.(print_string [green; Bold] 
      "\nJaccard Similarity will be used for this session.\n\n");
  ANSITerminal.(print_string [blue; Bold] help);
  print_string  "\n> ";
  response process_jac (Pervasives.read_line ());
  end
  | "2" ->
  begin 
  ANSITerminal.(print_string [green; Bold] 
      "\nCosine Similarity will be used for this session.\n\n");
  ANSITerminal.(print_string [blue; Bold] help);
  print_string  "\n> ";
  response process_cos (Pervasives.read_line ());
  end
  | _ -> 
  begin 
  ANSITerminal.(print_string [red; Bold] 
      "\nInvalid input; Jaccard will be used by default.\n\n");
  ANSITerminal.(print_string [blue; Bold] help);
  print_string  "\n> ";
  response process_jac (Pervasives.read_line ());
  end
  end

(* Executes the chatbot. *)

let () = main ()
