(* file: main.ml
   author: Robert Muller, J. Tassarotti

  Problem set 0: exercises for coming up to speed in OCaml.

  Usage from parent directory:

   > dune exec bin/main.exe test

   Note that omitting "test" in the above won't run the tests.

   There are 10 problems worth 6 points total.

   1. isDigit (.25)
   2. newFileName (.25)
   3. homePath (.25)
   4. atoi (.25)
   5. tokenize (1)
   6. formatTree (1)
   7. subpalindrome (.75)
   8. listComparisons (.5)
   9. patience (1)
   10. FilteredSet module (.75)
*)

(* Problem 1: .25 points

   isDigit : char -> bool
*)
let isDigit (c : char) : bool = 
  '0' <= c && c <= '9'

(* Problem 2: .25 points

   newFileName : string -> string -> string
*)
let newFileName (filename : string) (new_ext : string) : string =
  try 
    let ri = String.rindex filename '.' in    (* find the right-most occurrence of a dot *)
      let base = String.sub filename 0 ri in  (* extract name without extension *)
        base ^ "." ^ new_ext
  with Not_found -> filename                  (* if the filename does not include a dot *)

(* Problem 3: .25 points

   homePath : unit -> string list
*)
let homePath () : string list = 
  let home = 
    match Sys.getenv_opt "HOME" with                      (* look up $HOME *)
    | Some s when s <> "" -> s                            (* if $HOME is set and is not empty *)
    | _ -> (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir   (* if $HOME is not set, try manual lookup *)
  in
  List.filter (fun s -> s <> "") (String.split_on_char '/' home)

(* Problem 4: .25 points

   atoi : string -> int
*)
let atoi (a : string) : int = 
  let chars = Lib.explode a in                            (* "345" -> ['3', '4', '5'] *)
  let ctoi c = Char.code c - Char.code '0' in             (* helper that converts char to int *)
  List.fold_left (fun acc c -> acc * 10 + ctoi c) 0 chars (* We want 300 + 40 + 5 *)

type token = If | And | Or

(* Problem 5: 1 points

   tokenize : string -> token list
*)
let tokenize (s : string) : token list =
  let n = String.length s in
  let rec helper i acc =
    if i >= n then List.rev acc
    else if s.[i] = ' ' then helper (i + 1) acc
    else if s.[i] = '|' then helper (i + 2) (Or :: acc)
    else if s.[i] = '&' then helper (i + 2) (And :: acc)
    else if s.[i] = 'i' then helper (i + 2) (If :: acc)
    else failwith "ill-formed input"
  in
  helper 0 []

(* Two problems related to trees. We have arrows as interior nodes
   and leaves that include a unique constant C and variables
   {v1, v2, ... }

               ->             ->
              /  \           /  \
            v0   ->         C    v1
                /  \
               C    C
 *)

(* Note that the 'too' field should probably be called 'to' (that is the names would be from and to), but 'to'
   is already a reserved keyword in OCaml, so we cannot use it for a field name. *)

type t = C
       | Var of int
       | Arrow of { from : t
                  ; too  : t
                  }

(* Problem 6: 1 point

   formatTree : t -> string
*)
let rec formatTree (tr : t) : string =
  match tr with
  | C -> "C"
  | Var n -> "v" ^ string_of_int n
  | Arrow {from ; too} -> "(" ^ formatTree from ^ " -> " ^ formatTree too ^ ")" 

(* Problem 7: .75 point 

   subplaindrome : string -> string
*)
let subpalindrome (s : string) : string = 
  let n = String.length s in
  let rec helper l r = 
    if l > r then ""                        (* base case: when length is odd *)
    else if l = r then String.make 1 s.[l]  (* base case: when length is even *)
    else
      let inner = helper (l+1) (r-1) in     (* find the longest sub-palindrome in l+1:r-1 *)
      if s.[l] = s.[r] && String.length inner = (r-l-1) then
        String.sub s l (r - l + 1)          (* check if the sub-palindrome can be extended to l:r *)
      else
      inner
  in
  helper 0 (n-1)

(* Problem 8: .5 point

   list_comparisons : int list -> comparison list
*)
type comparison = GEQ | LT

let listComparisons (arr : int list) : comparison list = 
  match arr with
  | [] -> []
  | first :: rest ->
      let rec helper prev rest acc = 
        match rest with
        | [] -> List.rev acc
        | first_of_rest::rest_of_rest -> 
          let c = if prev <= first_of_rest then GEQ else LT in
          helper first_of_rest rest_of_rest (c :: acc)
      in
      helper first rest [GEQ]

(* Problem 9: 1 point

   patience : (int list) list -> int -> (int list) list
*)
let patience (piles : (int list) list) (x : int) : (int list) list =
  let rec helper ps =
    match ps with
    | [] -> [[x]]
    | pile :: rest ->
      match pile with
      | [] -> [[x]]
      | top :: _ ->
        if top >= x then (x :: pile) :: rest
        else pile :: helper rest
  in
  helper piles

(* Problem 10 : .75 points *)

module type FilteredSetType = sig
  type t
  val newSet : (int -> bool) -> t
  val insert : int -> t -> t
  val member : int -> t -> bool
  val mapAndFilter : (int -> int) -> t -> t
end

module FilteredSet : FilteredSetType =
struct
  type t = { fi : int -> bool; elem : int list }

  let newSet (f : int -> bool) : t = 
    { fi = f; elem = [] }

  let insert (x : int) (set : t) : t =
    if set.fi x then { set with elem = x :: set.elem } else set
  
  let member (x : int) (set : t) : bool = 
    List.mem x set.elem
  
  let mapAndFilter (g : int -> int) (set : t) : t = 
    let mapped = List.map g set.elem in
    let mapped_and_filtered = List.filter set.fi mapped in
    let rec remove_duplicate arr acc =
      match arr with
      | [] -> List.rev acc
      | first :: rest ->
          if List.mem first acc then remove_duplicate rest acc
          else remove_duplicate rest (first :: acc)
    in
    { set with elem = remove_duplicate mapped_and_filtered [] }

end

(* TESTING **********************************************************)

type parts = One       (* isDigit *)
           | Two       (* newFileName *)
           | Three     (* homePath *)
           | Four      (* atoi *)
           | Five      (* tokenize *)
           | Six       (* formatTree *)
           | Seven       (* subpalindrome *)
           | Eight       (* listComparisons *)
           | Nine       (* patience *)
           | Ten       (* FilteredSet module *)

(* Some simple test data
*)
let v0 = Lib.fresh()
let v1 = Lib.fresh()
let v2 = Lib.fresh()

(*            t0 = ->        t1 = ->
                  /  \           /  \
                v0   ->         C    v1
                    /  \
                   C    C
*)
let t0 = Arrow { from = Var v0
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }

let t1 = Arrow { from = C
               ; too  = Var v1
               }

let t2 = Arrow { from = t1
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }
let t3 = Arrow { from = C
               ; too  = t0
               }

(********************************************************************)

(* Test isDigit
*)
let isDigitTest1 () = isDigit '0'
let isDigitTest2 () = isDigit '9'
let isDigitTest3 () = not (isDigit 'A')
let isDigitTests () =
  Lib.run_test "isDigit test1" isDigitTest1 ;
  Lib.run_test "isDigit test2" isDigitTest2 ;
  Lib.run_test "isDigit test3" isDigitTest3

(* Test newFileName
*)
let newFileNameTest1 () = newFileName "A.java" "class" = "A.class"
let newFileNameTest2 () = newFileName "A" "class" = "A"
let newFileNameTest3 () = newFileName "A.B.java" "class" = "A.B.class"
let newFileNameTest4 () = newFileName "A." "class" = "A.class"
let newFileNameTests () =
  Lib.run_test "newFileName test1" newFileNameTest1 ;
  Lib.run_test "newFileName test2" newFileNameTest2 ;
  Lib.run_test "newFileName test3" newFileNameTest3 ;
  Lib.run_test "newFileName test4" newFileNameTest4

(* Test homePath
*)
let homePathTest () =
  let answer = homePath () in
  let combiner name1 name2 = Lib.fmt "%s/%s" name1 name2 in
  let path = List.fold_left combiner "" answer
  in
  try
    Unix.chdir path = ()
  with
    Unix.Unix_error _ -> failwith "homeTest failed"
let homePathTests () = Lib.run_test "home test" homePathTest

(* Test atoi
*)
let atoiTest1 () = atoi "345" = 345
let atoiTest2 () = atoi "0" = 0
let atoiTests () =
  Lib.run_test "atoi test1" atoiTest1 ;
  Lib.run_test "atoi test2" atoiTest2

(* Test tokenize
*)
let tokenizeTest1 () =
  (tokenize "|| if && if && ") = [Or; If; And; If; And]
let tokenizeTest2 () = (tokenize "||") = [Or]
let tokenizeTest3 () = (tokenize "       ") = []
let tokenizeTests () =
  Lib.run_test "tokenize test1" tokenizeTest1 ;
  Lib.run_test "tokenize test2" tokenizeTest2 ;
  Lib.run_test "tokenize test3" tokenizeTest3

(* Test formatTree
*)
let formatTreeTest1 () = (formatTree t0) = "(v0 -> (C -> C))"
let formatTreeTest2 () = (formatTree t1) = "(C -> v1)"
let formatTreeTest3 () = (formatTree (Var v2)) = "v2"
let formatTreeTests () =
  Lib.run_test "formatTree test1" formatTreeTest1 ;
  Lib.run_test "formatTree test2" formatTreeTest2 ;
  Lib.run_test "formatTree test3" formatTreeTest3

(* Test subpalindrome
*)
let subpalindromeTest1 () = subpalindrome "aba" = "aba"
let subpalindromeTest2 () = subpalindrome "dabac" = "aba"
let subpalindromeTest3 () = subpalindrome "xx" = "xx"
let subpalindromeTest4 () = subpalindrome "x1amanaplanacanalpanamax1" = "amanaplanacanalpanama"
let subpalindromeTest5 () = subpalindrome "civic" = "civic"
let subpalindromeTest6 () = subpalindrome "deified" = "deified"
let subpalindromeTest7 () = subpalindrome "2eifie+" = "eifie"
let subpalindromeTest8 () = subpalindrome "xyz" = "y"
let subpalindromeTest9 () = subpalindrome "" = ""
let subpalindromeTests () =
  Lib.run_test "subpalindrome test1" subpalindromeTest1 ;
  Lib.run_test "subpalindrome test2" subpalindromeTest2 ;
  Lib.run_test "subpalindrome test3" subpalindromeTest3 ;
  Lib.run_test "subpalindrome test4" subpalindromeTest4 ;
  Lib.run_test "subpalindrome test5" subpalindromeTest5 ;
  Lib.run_test "subpalindrome test6" subpalindromeTest6 ;
  Lib.run_test "subpalindrome test7" subpalindromeTest7 ;
  Lib.run_test "subpalindrome test8" subpalindromeTest8 ;
  Lib.run_test "subpalindrome test9" subpalindromeTest9

let listComparisonsTest1 () = listComparisons [3] = [GEQ]
let listComparisonsTest2 () = listComparisons [3;4;5] = [GEQ; GEQ; GEQ]
let listComparisonsTest3 () = listComparisons [1;-1;1] = [GEQ; LT; GEQ]
let listComparisonsTest4 () = listComparisons [-1;-1;1] = [GEQ; GEQ; GEQ]
let listComparisonsTest5 () = listComparisons [9;8;7] = [GEQ; LT; LT]
let listComparisonsTest6 () = listComparisons [9;8;7;10] = [GEQ; LT; LT; GEQ]
let listComparisonsTests () =
  Lib.run_test "listComparisons test1" listComparisonsTest1 ;
  Lib.run_test "listComparisons test2" listComparisonsTest2 ;
  Lib.run_test "listComparisons test3" listComparisonsTest3 ;
  Lib.run_test "listComparisons test4" listComparisonsTest4 ;
  Lib.run_test "listComparisons test5" listComparisonsTest5 ;
  Lib.run_test "listComparisons test6" listComparisonsTest6 

let patienceTest1 () = patience [[3]] 4 = [[3]; [4]]
let patienceTest2 () = patience [] 3 = [[3]]
let patienceTest3 () = patience [[4]; [5]] 3 = [[3;4]; [5]]
let patienceTest4 () = patience [[2]; [6]] 4 = [[2]; [4;6]]
let patienceTest5 () = patience [[2]; [6]; [10]] 8 = [[2]; [6]; [8; 10]]
let patienceTest6 () = patience [[2]; [6]; [10]] 12 = [[2]; [6]; [10]; [12]]
let patienceTest7 () = patience [[2]; [3;6]; [10]] 3 = [[2]; [3;3;6]; [10]]
let patienceTest8 () = patience [[2]; [3]; [4]; [5]; [6]] 4 = [[2]; [3]; [4;4]; [5]; [6]]
let patienceTests () =
  Lib.run_test "patience test1" patienceTest1 ;
  Lib.run_test "patience test2" patienceTest2 ;
  Lib.run_test "patience test3" patienceTest3 ;
  Lib.run_test "patience test4" patienceTest4 ;
  Lib.run_test "patience test5" patienceTest5 ;
  Lib.run_test "patience test6" patienceTest6 ;
  Lib.run_test "patience test7" patienceTest7 ;
  Lib.run_test "patience test8" patienceTest8


let isEven n = (n mod 2 = 0)

open FilteredSet

let rec insert_list xs s =
  match xs with
  | [] -> s
  | x :: xs -> insert_list xs (insert x s)

let filteredSetTests_wrapper () =
  let evenSet_empty = newSet isEven in

  let evenSet_1 = insert_list [1;2;3;4;5;6] evenSet_empty in
  let evenSet_2 = insert_list [10;12;13] evenSet_empty in

  let lt5Set_empty = newSet ((>) 5) in

  let lt5Set_1 = insert_list [1;2;3;4;5;6] lt5Set_empty in
  let lt5Set_2 = mapAndFilter ((+) 2) lt5Set_1 in
  let filteredSetTest1 () = member 5 evenSet_1 = false in
  let filteredSetTest2 () = member 2 evenSet_1 = true in
  let filteredSetTest3 () = member 2 evenSet_2 = false in
  let filteredSetTest4 () = member 12 evenSet_2 = true in
  let filteredSetTest5 () = member 4 lt5Set_1 = true in
  let filteredSetTest6 () = member 5 lt5Set_1 = false in
  let filteredSetTest7 () = member 6 lt5Set_2 = false in
  let filteredSetTest8 () = member 4 lt5Set_2 = true in
  let filteredSetTest9 () = member 1 lt5Set_2 = false in
  Lib.run_test "filteredSet test1" filteredSetTest1 ;
  Lib.run_test "filteredSet test2" filteredSetTest2 ;
  Lib.run_test "filteredSet test3" filteredSetTest3 ;
  Lib.run_test "filteredSet test4" filteredSetTest4 ;
  Lib.run_test "filteredSet test5" filteredSetTest5 ;
  Lib.run_test "filteredSet test6" filteredSetTest6 ;
  Lib.run_test "filteredSet test7" filteredSetTest7 ;
  Lib.run_test "filteredSet test8" filteredSetTest8 ;
  Lib.run_test "filteredSet test9" filteredSetTest9

let filteredSetTests () =
  try filteredSetTests_wrapper () with
  | Failure s -> print_endline ("filteredSet tests error: `" ^ s ^ "`\n")
  | e -> print_endline ("filteredSet tests error: `" ^ Printexc.to_string e ^ "`\n")

(******************************************************************)

(******************************************************************)

let test part =
  match part with
  | One   -> isDigitTests()
  | Two   -> newFileNameTests()
  | Three -> homePathTests()
  | Four  -> atoiTests()
  | Five  -> tokenizeTests()
  | Six   -> formatTreeTests()
  | Seven   -> subpalindromeTests ()
  | Eight   -> listComparisonsTests ()
  | Nine -> patienceTests ()
  | Ten  -> filteredSetTests ()

let run () =
  let () = test One in
  let () = test Two in
  let () = test Three in
  let () = test Four in
  let () = test Five in
  let () = test Six in
  let () = test Seven in
  let () = test Eight in
  let () = test Nine in
  let () = test Ten in
  ()

let () =
  if (Array.length Sys.argv = 2 && Sys.argv.(1) = "test") then
    run ()
  else
    ()
