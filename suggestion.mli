(** Module used for topic clustering and suggesting new topics
relevant to user's input question. *)

(** [suggestion] outputs a suggestion string from user input and 
outputs "" if cannot find a suggestion *)
val suggestion : string -> string