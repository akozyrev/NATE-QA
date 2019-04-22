(*Module for calculating Levenshtein distance
between 2 strings. This uses a generic algorithm
commonly found on the internet. We credit this
page for inspiration for our implementation:
https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance *)


(* Minimum of three integers.  *)
let minimum (x:int) y z =
  let m' (a:int) b = if a < b then a else b in
    m' (m' x y) z

(* Matrix initialization. *)
let init_matrix n m =
  let init_col = Array.init m in
  Array.init n (function
    | 0 -> init_col (function j -> j)
    | i -> init_col (function 0 -> i | _ -> 0)
  )

(* Computes the Levenshtein distance between two arrays*)
let distance_array x y =
  match Array.length x, Array.length y with
    | 0, n -> n
    | m, 0 -> m
    | m, n ->
       let matrix = init_matrix (m + 1) (n + 1) in
         for i = 1 to m do
           let s = matrix.(i) and t = matrix.(i - 1) in
             for j = 1 to n do
               let cost = abs (compare x.(i - 1) y.(j - 1)) in
                 s.(j) <- minimum (t.(j) + 1) (s.(j - 1) + 1) (t.(j - 1) + cost)
             done
         done;
         matrix.(m).(n)

(**Convert string to char list*)
let string_to_cl s = List.init (String.length s) (String.get s)

(** Compute Lev. distance between word1 and word2 *)
let distance word1 word2 =
  let cl1 = string_to_cl word1 in
  let cl2 = string_to_cl word2 in
  distance_array (Array.of_list cl1) (Array.of_list cl2)