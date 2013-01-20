(*This was our last lab of the year. Labs, like projects, are done with a partner. I worked with Peter Trammel on this lab.*)
(*I included this because I thought it does a good job of displaying my use of higher order procedures, anonymous procedures, and currying in ocaml*)
(*This is also some of the only work we did involving databases.*)

(*a datum in this problem is either a string or an int*)
type datum = String of string | Integer of int ;;

(*an avpair is a tuple of an attribute and a value, the values describing specific instances of the attributes*)
type avpair = AVPair of string * datum ;;

(*a record is a list of avpairs, and the values in the list are supposed to describe the same object*)
(*I/P a list of strings and a list of datums, and O/P a list of avpairs such that the first string and first datum are one avpair, the second string and second datum are the next, and so on*)
let make_record (x : string list) (y : datum list) : (avpair list) =
  List.map2 (fun a b -> AVPair (a , b)) x y;;

(*a relation is a list of records*)
(*I/P a list of strings and a list of lists of datum, each list being the values for a relation, and O/P a list of lists of avpairs*)
let make_relation (x : string list) (y : datum list list) : (avpair list list) =
  List.map ((make_record) x) y;;  

(*member is a helper for the procedures below relating to databases*)
(*I/P a datum and a list, and O/P whether the datum is in that list*)
let rec member (x : 'a) (y : 'a list) : bool =
  match y with
	| [] -> false
	| hd::tl -> if x = hd then true else member x tl ;;

(*I/P two relations with the same attributes, and O/P a union of the two relations without duplicates*)
let rec union (x : avpair list list) (y : avpair list list) : (avpair list list) =
  match x with
	| [] -> y
	| hd::tl -> if member hd y
				then union tl y
				else hd::(union tl y) ;;

(*I/P two relations with the same attributes, and O/P a relation of all the records that are in the first relation that aren't in the second*)				
let rec difference (x : avpair list list) (y : avpair list list) : (avpair list list) =
  match x with
	| [] -> []
	| hd::tl -> if member hd y
				then difference tl y
				else hd::(difference tl y) ;;

(*I/P a relation, an attribute in that relation, and a new attribute name, and O/P the relation where every instance of the old attribute name in the original relation are replaced by the new attribute name*)
let rename (x : avpair list list) (y : string) (z : string) : (avpair list list) =
  (List.map (fun l -> List.map (fun (AVPair(a,v)) -> if (a = y) then (AVPair(z,v)) else (AVPair(a,v))) l) x);;






