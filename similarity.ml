open Pervasives

(** Module for calculating and storing the
simlarity metrics of 2 sentences, one being the user's
input and the other being a sentence in a document decided
by TFIDF. Contains both Cosine Similarity and Jaccard Similarity
algorithms. *)
module Similarity = struct

  (**Type of cosine similarity calculation*)
  type cos_sim = float

  (**Type of jaccard similarity calculation*)
  type jac_sim = float

  (** [dot_prod a b] computes and returns the dot product of vectors 
    a and b, represented as int lists.
    PRECONDITION: length of a == length of b*)
  let rec dot_prod (a: int list) (b: int list): int =
    match a, b with
    | [], [] -> 0
    | h1::t1, h2::t2 -> (h1 * h2) + dot_prod t1 t2
    | _ -> failwith "Error: vector lengths are different"

  (** [norm a] computes norm (length) of vector a,
      represented as an int list. *)
  let norm (a: int list) : float =
    let a_floats = List.map float_of_int a in
    let rec sum_squares vec =
      match vec with
      | [] -> 0.0
      | h::t -> (h ** 2.0) +. sum_squares t in
    sqrt (sum_squares a_floats)

  (** [cosine_sim a b] computes cosine similarity metric of vectors a and b,
      represented as int lists*)
  let cosine_sim (a:int list) (b: int list) : cos_sim =
    let num = dot_prod a b in
    (* Pervasives.print_int num; *)
    let denom = (norm a) *. (norm b) in
    (* Pervasives.print_float denom; *)
    (float_of_int num) /. denom

  (** [remove_dups lst] removes all duplicates from lst, i.e.
      the set operation. *)
  let rec remove_dups (lst: 'a list): 'a list =
    match lst with
    | [] -> []
    | h::t -> if List.mem h t then remove_dups t
      else h::remove_dups t

  (** [union set1 set2] compute union of set1 and set2, represented as lists *)
  let union (set1: 'a list) (set2: 'a list) : 'a list =
    let bigset = set1 @ set2 in
    remove_dups bigset

  (** [union_cardinality set1 set2] compute cardinality of union of set1
      and set2, represented as lists *)
  let union_cardinality (set1: 'a list) (set2: 'a list) : int =
    let u = union set1 set2 in
    List.length u

  (** [intersect_cardinality set1 set2] computes cardinality of
      intersection of set1 and set2, represented as lists *)
  let intersect_cardinality (set1: 'a list) (set2: 'a list) : int =
    let u_val = union_cardinality (set1) (set2) in
    (List.length (set1)) + (List.length (set2)) - u_val
  (* not using remove_dups on sets' length calculations, bc we assume
     set1 and set2 are valid SETS *)

  (** [jaccard_sim a b] computes jaccard similarity of sets a and b,
      with element being strings (unique words represented)
      PRECONDITION: a and b have no duplicates*)
  let jaccard_sim (a: string list) (b: string list) : jac_sim =
    (* if (not (Counter.Counter.no_repeats a)) ||
       (not (Counter.Counter.no_repeats b))
       then failwith "Violated precondition:
       either a or b or both contains duplicates!"
       else   *)
    let a_mod = List.filter (fun a -> not (List.mem a Filter.filter_list)) a in
    let b_mod = List.filter (fun b -> not (List.mem b Filter.filter_list)) b in

    (float_of_int (intersect_cardinality a_mod b_mod)) /.
    (float_of_int (union_cardinality a_mod b_mod))

end
