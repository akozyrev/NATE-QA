(** [.mli] Module for calculating and storing the
simlarity metrics of 2 sentences, one being the user's
input and the other being a sentence in a document decided
by TFIDF. Contains both Cosine Similarity and Jaccard Similarity
algorithms. *)
module Similarity : sig

  (**Type of cosine similarity calculation*)
  type cos_sim = float

  (**Type of jaccard similarity calculation*)
  type jac_sim = float

(** Compute dot product of vectors a and b,
  represented as int lists
  PRECONDITION: length of a == length of b*)
  val dot_prod : (int list) -> (int list) -> int 
    

  (** Compute norm (length) of vector a, 
  represented as int list *)
  val norm : (int list) -> float 
    

(** Compute cosine similarity metric of vectors a and b,
  represented as int lists*)
  val cosine_sim : int list-> int list -> cos_sim 

(** Remove all duplicates from lst, i.e.
  the set operation *)
  val remove_dups : ('a list) -> 'a list 

(** Compute union of set1 and set2, represented as lists *)
  val union : 'a list -> 'a list -> 'a list 

(** Compute cardinality of union of set1 and set2, represented as lists *)
  val union_cardinality : 'a list -> 'a list -> int 

(** Compute cardinality of intersection of set1 and set2, represented as lists *)
  val intersect_cardinality : 'a list -> 'a list -> int


(** Compute jaccard similarity of sets a and b, with element being strings
  (unique words represented) 
  PRECONDITION: a and b have no duplicates. *)
  val jaccard_sim : string list -> string list -> jac_sim 

end