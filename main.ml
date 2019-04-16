open Tokenizer
open Counter
open Extract

(* [process input] will return the correct response to the input
    the user provides. *)
let process input = 
    Extract.get_response input
    (* match input with
    | "" -> ""
    | _ -> "I don’t have the answer for that." *)

(* [response input] provides the user with a response to the input
    they provide. It also prompts the user for another question, if
    the user wishes to continue speaking to the chatbot. *)
let rec response input =
    match input with
    | "bye" -> Pervasives.print_endline "Thank you for talking to me!"
    | input ->  
    let output = process input in
    Pervasives.print_endline output;
    print_string  "> ";

    let new_input = Pervasives.read_line () in response new_input

(* [main ()] greets the user and starts the chatbot. *)
let main () = 
    ANSITerminal.(print_string [red]
                  ("\nHello! My name is CSCamlBot, and I am here to introduce" ^ 
                  " you to the field of computer science \n(famous people, concepts, and companies)"^
                  " and answer any questions you may have.\n\n" ));
    print_endline ("Some things you can ask me include:" 
            ^ " \n- Where does David Gries live?\n- " ^ 
            "What is natural language processing used for?"^
            "\n- Recommend me a good machine learning course.\n" );
    print_string  "> ";
    response (Pervasives.read_line ())

(* Executes the chatbot. *)

let () = main ()