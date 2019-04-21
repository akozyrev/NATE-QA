open Tokenizer
open Counter
open Extract

let intro_txt = "\nHello! My name is NATE [Nonhuman Abstract Tech Expert]," ^ 
                  "\nand I'm here to answer all your CS-related questions.\n"^
                  "Ask me about common subfields, people, or companies.\n\n"


let examples = "Some things you may ask me include:" 
            ^ " \n- Where does David Gries live?\n- " ^ 
            "What is natural language processing used for?"^
            "\n- Recommend me a good machine learning course.\n"

let help = "Commands: 
| about: introduce myself and my thought process
| examples: provide examples of questions you can ask
| help: give list of commands you can type
| <your question>: ask me a question to recieve response
| bye: exit the program \n"


let about = "I was developed by a CS 3110 group and named after their professor, 
Dr. Nate Foster. I gather my knowledge from an extensive text corpus of Wikipedia 
articles related to the field of CS. I use 2 NLP/IR algorithms to answer your 
input question: TFIDF, to find the most relevant document to search for the answer, 
and Jaccard similarity, to find the best sentence in the doc that answers the question. 
I was optimized by using hashing, so I can answer questions almost instantaneously. 
With that being said, I am still getting smarter and faster with each day!\n"

(* [process input] will return the correct response to the input
    the user provides. *)
let process input = 
    let response = Extract.get_response input in
    match input, response with
    | "about", _ -> about
    | "examples", _ -> examples 
    | "help", _ -> help 
    | _ , "" -> "I don’t have the answer for that.\n"
    | _, _ ->  response ^ "\n"

(* [response input] provides the user with a response to the input
    they provide. It also prompts the user for another question, if
    the user wishes to continue speaking to the chatbot. *)
let rec response input =
    match input with
    | "bye" -> Pervasives.print_endline "Thank you for talking to me!\n"
    | input ->  
    let output = process input in
    Pervasives.print_endline output;
    print_string  "> ";

    let new_input = Pervasives.read_line () in response new_input

(* [main ()] greets the user and starts the chatbot. *)
let main () = 
    ANSITerminal.(print_string [red] intro_txt);
    (* print_endline (examples); *)
    print_endline (help);
    print_string  "> ";
    response (Pervasives.read_line ())

(* Executes the chatbot. *)

let () = main ()