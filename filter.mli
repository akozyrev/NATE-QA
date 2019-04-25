(** Module for filtering out unimportant words in
the input question from evaluation.*)

(** [filter_list] is a list of all the unimportant words
    we want to filter from the input question before
    we continue with the jaccard similarity calculations. *)
val filter_list: string list