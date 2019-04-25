(**Module used for topic clustering and suggesting new topics
relevant to user's input question. Will output a neighbor
in the same cluster as the topic the user asked about, nothing
if cannot find a similar cluster *)

(** [suggestion] outputs a suggestion string from user input and 
outputs "" if cannot find a suggestion *)
val suggestion : string -> string