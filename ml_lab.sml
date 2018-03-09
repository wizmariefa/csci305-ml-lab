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

(* isMember function definition. Checks to see if the number/value passed in is a
   member of the queried set *)
fun isMember e Empty = false
  |isMember e (Set(check, set2)) =
    if e = check then true
      else isMember e set2;

(*Takes the first value, creates set from rest recursively until you have a set of sets.
  Because it was driving me up a WALL, I didn't end up making this function into
  one that checks for if it's a member of the list already. *)
fun list2Set [] = Empty
  | list2Set (element::rest) = Set(element, list2Set(rest));

(*union that takes the set union of set set1 and set set2 and returns a set
  representing the mathematical union of the two sets. If set 1 or set 2
  are empty, then we want to return only the other set. *)
fun union Empty set2 = set2
  | union set1 Empty = set1
  | union (Set(e, set)) set2 =
    if isMember e set2 then union set set2
      else union set (Set(e, set2));

(* unction intersect that takes the set intersection of set set1 and set set2
   and returns a set representing the mathematical intersection of the two sets.
   Control for one or both of the sets being empty which would mean no
   intersection. We check to see if e is in set 2, and if it is, we create/add
   to a new set that is the values of their interesections. *)
fun intersect Empty Empty = Empty
  | intersect Empty set2 = Empty
  | intersect set1 Empty = Empty
  | intersect (Set(e, set1)) (Set(e2, set2)) =
    if isMember e (Set(e2, set2)) then Set(e, (intersect set1 (Set(e2, set2))))
    else intersect set1 (Set(e2, set2));




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

(* Question 5
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");*)

(* Question 7
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";*)

(* Question 9
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));*)

(* Question 10
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));*)
