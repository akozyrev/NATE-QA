open Tokenizer
open Counter

(* [process input] will return the correct response to the input
    the user provides. *)
let process input = 
    match input with
    | "" -> ""
    | _ -> "I don’t have the answer for that."

(* [response input] provides the user with a response to the input
    they provide. It also prompts the user for another question, if
    the user wishes to continue speaking to the chatbot. *)
let rec response input =
    match input with
    | "bye" -> Pervasives.print_endline "Thank you for talking to me!"
    | input ->  
    let output = process input in
    Pervasives.print_endline output;

    let new_input = Pervasives.read_line () in response new_input

(* [main ()] greets the user and starts the chatbot. *)
let main () = 
    ANSITerminal.(print_string [red]
                  "\nHello! My name is CSBot, and I am here to introduce you to the field of computer science and answer any questions you may have.\n");
    print_endline "Some things you can ask me include: \n'What is artificial intelligence?'\n'Who founded Apple?'\n'What are some popular programming languages?'\n";
    response (Pervasives.read_line ())

(* Executes the chatbot. *)

let () = main ()