(** Module for calculating and storing the
similarity metrics of 2 sentences, one being the user's
input and the other being a sentence in a document decided
by TFIDF. Contains both Cosine Similarity and Jaccard Similarity
algorithms. *)
module Similarity : sig

  (**Type of cosine similarity calculation*)
  type cos_sim = float

  (**Type of jaccard similarity calculation*)
  type jac_sim = float

  (** [dot_prod a b] computes and returns the dot product of vectors 
    a and b, represented as int lists.
    PRECONDITION: length of a == length of b*)
  val dot_prod : (int list) -> (int list) -> int 
    
  (** [norm a] computes norm (length) of vector a,
      represented as an int list. *)
  val norm : (int list) -> float 
    
  (** [cosine_sim a b] computes cosine similarity metric of vectors a and b,
      represented as int lists*)
  val cosine_sim : int list-> int list -> cos_sim 

  (** [remove_dups lst] removes all duplicates from lst, i.e.
      the set operation. *)
  val remove_dups : ('a list) -> 'a list 

  (** [union set1 set2] compute union of set1 and set2, represented as lists *)
  val union : 'a list -> 'a list -> 'a list 

  (** [union_cardinality set1 set2] compute cardinality of union of set1
      and set2, represented as lists *)
  val union_cardinality : 'a list -> 'a list -> int 

  (** [intersect_cardinality set1 set2] computes cardinality of
      intersection of set1 and set2, represented as lists *)
  val intersect_cardinality : 'a list -> 'a list -> int

  (** [jaccard_sim a b] computes jaccard similarity of sets a and b,
      with element being strings (unique words represented)
      PRECONDITION: a and b have no duplicates*)
  val jaccard_sim : string list -> string list -> jac_sim 

end