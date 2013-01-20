(*This was a problem on the final, so I coded this with no partner. I included this because I think it is fairly elegant and very efficient.*)

(*I/P a list of only parentheses, and O/P whether or not that list is well formed, meaning there cannot be unmatched parentheses*)
let is_well_formed (parens : string list) : bool = 
  let rec well_formed (parens : string list) (counter : int) : bool = 
    match parens,counter with 
    |([],0) -> true
    |([],_) -> false
    |(hd::tl,n) when (n < 0) -> false
    |(hd::tl,n) -> 
	if hd = "("
	then (well_formed tl (counter + 1)) 
	else 
	  if hd = ")"
	  then (well_formed tl (counter - 1))
	  else failwith "invalid input"
  in
  well_formed parens 0;;
(*Test Cases*)
is_well_formed [] = true;;
is_well_formed ["(";"(";")";"(";"(";")";")";")"] = true;;
is_well_formed ["(";")"] = true;;
is_well_formed ["(";")";"(";")";"(";"(";")";")"] = true;;
is_well_formed [")"] = false;;
is_well_formed ["(";"(";"(";")";")"] = false;;

(*I/P a list of only parentheses, and O/P a list of ints so that parentheses that open and close each other are labeled with the same number,
and there can be no repeat numbers
*)
let number_pairs (parens : string list) : (int list) = 
  let rec pairs (parens : string list) (unmatched : int list) (start : int) : (int list) = 
    match parens,unmatched with 
    |([],[]) -> []
    |([],hd::tl) -> failwith "mismatched left parenthesis" 
    |(hd::tl,[]) when (hd = ")") -> failwith "mismatched right parenthesis"
    |(hd::tl,[]) when (hd = "(") -> start::(pairs tl [start] (start + 1))
    |(hd::tl,[]) -> failwith "invalid input"
    |(hd1::tl1,hd2::tl2) when (hd1 = ")") -> hd2::(pairs tl1 tl2 start)
    |(hd1::tl1,hd2::tl2) when (hd1 = "(") -> start::(pairs tl1 (start::unmatched) (start + 1))
    |(hd1::tl1,hd2::tl2) -> failwith "invalid input"
  in
  pairs parens [] 1;;
(*Test Cases*)
number_pairs [] = [];;
number_pairs ["(";"(";")";"(";"(";")";")";")"] = [1;2;2;3;4;4;3;1];;
number_pairs ["(";")"] = [1;1];;
number_pairs ["(";")";"(";")";"(";"(";")";")"] = [1;1;2;2;3;4;4;3];;
