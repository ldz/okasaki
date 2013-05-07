exception EMPTY
exception SUBSCRIPT

module type STACK = sig
  type 'a stack
  val empty : 'a stack
  val is_empty : 'a stack -> bool
  val cons : 'a * 'a stack -> 'a stack (* raises EMPTY if stack is empty *)
  val head : 'a stack -> 'a            (* raises EMPTY if stack is empty *)
  val tail : 'a stack -> 'a stack
  val (++) : 'a stack -> 'a stack -> 'a stack
end

module List : STACK =
struct
  type 'a stack = 'a list
  let empty = []
  let is_empty s = s = []
  let cons (x, s) = x :: s
  let head = function [] -> raise EMPTY | h :: _ -> h
  let tail = function [] -> raise EMPTY | _ :: t -> t
  let (++) = (@)
end

module CustomStack : STACK =
struct
  type 'a stack = NIL | CONS of 'a * 'a stack

  let empty = NIL
  let is_empty s = s = NIL
  let cons (x, s) = CONS (x, s)
  let head = function NIL -> raise EMPTY | (CONS (h, _)) -> h
  let tail = function NIL -> raise EMPTY | (CONS (_, t)) -> t
  let rec (++) ax bx =
    match ax, bx with
        NIL, bx -> bx
      | (CONS (a, ax)), bx -> cons (a, (ax ++ bx))
end

let rec update = function
    ([], i, y) -> raise SUBSCRIPT
  | (x :: xs, 0, y) -> y :: xs
  | (x :: xs, i, y) -> x :: (update (xs, i - 1, y))

let rec suffixes = function
    [] -> [[]]
  | (x :: xs) -> (x :: xs) :: (suffixes xs)

module type SET = sig
  type elem
  type set
  val empty : set
  val insert : elem * set -> set
  val member : elem * set -> bool
end


module type ORDERED = sig
  type t
  val eq : t * t -> bool
  val lt : t * t -> bool
  val leq : t * t -> bool
end

module UnbalancedSet (Element : ORDERED) : SET
    with type elem = Element.t =
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree

  let empty = E

  let rec member = function
    (x, E) -> false
  | (x, T(a, y, b)) ->
    if Element.lt (x, y) then member (x, a)
    else if not (Element.leq (x, y)) then member (x, b)
    else true

  let rec insert = function
    (x, E) -> T (E, x, E)
  | (x, (T (a, y, b) as s)) ->
    if Element.lt (x, y) then T (insert (x, a), y, b)
    else if not (Element.leq (x, y)) then T (a, y, insert (x, b))
    else s
end

module OrderedInt : ORDERED 
    with type t = int =
struct
  type t = int
  let eq (a, b) = a == b
  let lt (a, b) = a < b
  let leq (a, b) = a <= b
end

module Set = UnbalancedSet (OrderedInt)

let rec set2list = function
  [] -> Set.empty
  | (x::xs) -> Set.insert (x, set2list xs)

open OUnit

let set = set2list [3;1;5;2;8;9;6;7]

let test_set_member _ =
  assert_equal true (Set.member (3, set));
  assert_equal true (Set.member (1, set));
  assert_equal true (Set.member (5, set));
  assert_equal true (Set.member (2, set));
  assert_equal true (Set.member (8, set));
  assert_equal true (Set.member (9, set));
  assert_equal true (Set.member (6, set));
  assert_equal true (Set.member (7, set));
  assert_equal false (Set.member (11, set));
  assert_equal false (Set.member (0, set))

let suite = "IntSet" >::: ["test_set_member" >:: test_set_member]

let _ = run_test_tt_main suite
