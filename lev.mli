(** Module for calculating Levenshtein distance
between 2 strings. This uses a famous, math-based 
generic algorithm. We credit this
page for inspiration for our implementation:
en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance *)

(** [distance w1 w2] returns the calculated 
Levenshtein between strings w1 and w2 *)
val distance: string -> string -> int