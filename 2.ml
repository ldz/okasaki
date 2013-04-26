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
