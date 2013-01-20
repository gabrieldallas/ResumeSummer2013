(*This was a problem on the final, so I coded this with no partner*)
(*The premise to this problem was that instead of having a binary tree store data at all the nodes and not at the leaves, it would store data at all the leaves and not at the nodes.*)
(*Then we were asked to code some familiar higher order procedures, and then use them creatively to accomplish other assigned procedures.*)
(*I included this because I think it shows my ability to think creatively about unfamiliar structures and higher order procedures.*)

type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree;;

(*I/P a one argument procedure and btree, and O/P a btree which is the result of performing that procedure on every datum in the tree*)
let rec maptree (proc : 'a -> 'b) (tree : 'a btree) : ('b btree) = 
  match tree with 
  |Leaf (n) -> Leaf (proc n)
  |Node (l,r) -> Node (maptree proc l,maptree proc r);;
(*Test Cases*)
maptree (fun x -> x + 5) (Leaf 4) = Leaf 9;;
maptree (fun x -> (x && true)) (Leaf true) = Leaf true;;
maptree (fun x -> (x && true)) (Leaf false) = Leaf false;;
maptree (not) (Node ((Leaf true),(Node ((Leaf false),(Leaf true))))) = (Node ((Leaf false),(Node ((Leaf true),(Leaf false)))));;
maptree ((+) 3) (Node ((Node ((Leaf 1),(Leaf 3))),(Node ((Node ((Leaf 5),(Leaf 8))),(Leaf 9))))) = (Node ((Node ((Leaf 4),(Leaf 6))),(Node ((Node ((Leaf 8),(Leaf 11))),(Leaf 12)))));;

(*I/P a two argument procedure and a btree, and O/P the result when doing that procedure to everything together
It will be equivalent to folding in order left to right and right to left for commutative, associative procedures
*)
(*We were told that we only had to deal with commutative, associative procedures.*)
let rec foldtree (proc : 'a -> 'a -> 'a) (tree : 'a btree) : 'a = 
  match tree with 
  |Leaf n -> n
  |Node (l,r) -> (proc (foldtree proc l) (foldtree proc r));;
(*Test Cases*)
foldtree (+) (Leaf 4) = 4;;
foldtree (&&) (Leaf true) = true;;
foldtree (+) (Node ((Leaf 1),(Leaf 3))) = 4;;
foldtree (+) (Node ((Node ((Leaf 1),(Leaf 3))),(Node ((Node ((Leaf 5),(Leaf 8))),(Leaf 9))))) = 26;;
foldtree (&&) (Node ((Leaf true),(Node ((Leaf false),(Leaf true))))) = false;;
foldtree (&&) (Node ((Leaf true),(Node ((Leaf true),(Leaf true))))) = true;;

(*I/P a btree and O/P the number of leaves*)
let breadth (tree : 'a btree) : int = foldtree (+) (maptree (fun x -> 1) tree);;
(*Test Cases*)
breadth (Leaf 9) = 1;;
breadth (Node ((Node ((Leaf 5),(Leaf 6))),(Node ((Leaf 7),(Leaf 8))))) = 4;;
breadth (Node ((Leaf true),(Node ((Leaf false),(Leaf true))))) = 3;;

(*I/P a btree and O/P the depth of the deepest node*)
let depth (tree : 'a btree) = (foldtree (fun x y -> if (x > y) then (x + 1) else (y + 1)) (maptree (fun x -> 0) tree));;
depth (Leaf 3) = 0;; 
depth (Node ((Leaf true),(Node ((Leaf false),(Leaf true))))) = 2;;
depth (Node ((Leaf true),(Node ((Leaf false),(Node ((Leaf true),(Leaf true))))))) = 3;;
 
