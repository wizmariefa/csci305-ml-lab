(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Marie Morin
* mariecolee1@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)
fun f [] = [] (* a *)
   | f (x::xs) = (x + 1) :: (f xs) (* b *)

(* set datatype *)
datatype 'element set = Empty | Set of 'element * 'element set;

(* isMember function definition. Checks to see if the number passed in is a
   member of the queried set *)
fun isMember (element, Set(check, recursiveSet)) = (*pass in value looking for, value checking, and rest of set*)
  if element = check then true (*we found the value we are looking for and break out*)
  else if recursiveSet = Empty then false (*we have nothing else to look at, so we reject*)
  else isMember(element, recursiveSet); (*otherwise, recurse again*)

(*Takes the first value, creates set from rest recursively until you have a set of sets *)
fun list2Set [] = Empty
  | list2Set (element::rest) = Set(element, list2Set(rest));

  fun list2Set [] = Empty
    |
    list2Set (element::rest) =
      if isMember (element, rest) then
        list2Set(element, list2Set(rest));
      else list2Set(rest);
      
(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

(*list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];*)

(* Question 1
f [3, 1, 4, 1, 5, 9] *)

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
(*val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";*)

(* Question 9 *)
(*print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));*)

(* Question 10 *)
(*print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
*)
