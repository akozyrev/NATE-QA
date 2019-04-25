(** Module for filtering out unimportant words in
the input question from evaluation. *)

(** [interrogative] is a list of interrogative words as strings. *)
let interrogative = ["who";"when";"where";"what";"how";"whom";"this";"whose";
"that"]

(** [linking_vb] is a list of linking verbs as strings. *)
let linking_vb = ["be"; "am"; "is"; "are"; "was"; "were";"been";
  "have"; "has"; "had"; "do"; "does";"did";"can";"could";
  "shall"; "should";"will";"would";"may";"might";"must";"being"]

(** [auxillary] is a list of common auxillaries as strings. *)
let auxillary = ["the"; "an"; "a"; "any"; "some";"of"]

(** [pronoun] is a list of pronouns as strings. *)
let pronoun = ["I"; "me"; "myself"; "he"; "him";"himself";
"she";"her";"herself";"they";"them";"themselves";"we";
"us";"ourselves";"you";"you're";"yourself";"yourselves";
"his";"hers";"mine";"my";"their";"it";"its"]

(** [filter_list] is a list of all the unimportant words
    we want to filter from the input question before
    we continue with the jaccard similarity calculations. *)
let filter_list = List.flatten [auxillary; pronoun]