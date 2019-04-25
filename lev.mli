(** Module for calculating Levenshtein distance
between 2 strings. This uses a famous, math-based 
generic algorithm. We credit this
page for inspiration for our implementation:
en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance*)

(** [distance word1 word2] compute the Lev. distance between 
  [word1] and [word2]. *)
val distance: string -> string -> int