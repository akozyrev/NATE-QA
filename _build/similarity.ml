open Pervasives

module Similarity = struct

  type cos_sim = float
  type jac_sim = float

(** Compute dot product of vectors a and b,
  represented as int lists
  PRECONDITION: length of a == length of b*)
  let rec dot_prod (a: int list) (b: int list): int = 
    match a, b with
    | [], [] -> 0
    | h1::t1, h2::t2 -> (h1 * h2) + dot_prod t1 t2
    | _ -> failwith "Error: vector lengths are different"

  (** Compute norm (length) of vector a, 
  represented as int list *)
  let norm (a: int list) : float = 
    let a_floats = List.map float_of_int a in
    let rec sum_squares vec = 
      match vec with 
      | [] -> 0.0
      | h::t -> (h ** 2.0) +. sum_squares t in
    sqrt (sum_squares a_floats)

(** Compute cosine similarity metric of vectors a and b,
  represented as int lists*)
  let cosine_sim (a:int list) (b: int list) : cos_sim =
    let num = dot_prod a b in
    let denom = (norm a) *. (norm b) in
    (float_of_int num) /. denom

(** Remove all duplicates from lst, i.e.
  the set operation *)
  let rec remove_dups (lst: 'a list): 'a list =
      match lst with 
      | [] -> []
      | h::t -> if List.mem h t then remove_dups t 
            else h::remove_dups t  

(** Compute union of set1 and set2, represented as lists *)
  let union (set1: 'a list) (set2: 'a list) : 'a list = 
    let bigset = set1 @ set2 in
    remove_dups bigset

(** Compute cardinality of union of set1 and set2, represented as lists *)
  let union_cardinality (set1: 'a list) (set2: 'a list) : int =
    let u = union set1 set2 in
    List.length u

(** Compute cardinality of intersection of set1 and set2, represented as lists *)
  let intersect_cardinality (set1: 'a list) (set2: 'a list) : int = 
    let u_val = union_cardinality (set1) (set2) in
    (List.length (set1)) + (List.length (set2)) - u_val
    (* not using remove_dups on sets' length calculations, bc we assume
    set1 and set2 are valid SETS *)

  (** Compute jaccard similarity of sets a and b, with element being strings
  (unique words represented) 
  PRECONDITION: a and b have no duplicates*)
  let jaccard_sim (a: string list) (b: string list) : jac_sim = 
    (* if (not (Counter.Counter.no_repeats a)) || (not (Counter.Counter.no_repeats b))
      then failwith "Violated precondition: either a or b or both contains duplicates!"
    else   *)
      (float_of_int (intersect_cardinality a b)) /. (float_of_int (union_cardinality a b))

end