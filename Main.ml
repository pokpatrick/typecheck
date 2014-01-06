(*Main.ml*)

(**)
open Syntax;;
open Type;;
open Fsm;;
open Regexp;;
open Semantic;;
open Unix;;

(*dependence Tuareg
#use "Syntax.ml";;
#use "Type.ml";;
#use "Fsm.ml";;
#use "Regexp.ml";;
#use "Semantic.ml";;*)

let gammatest = [("m", SIMPLE(INT)); ("n", SIMPLE(INT)); ("a", REF(INT)); 
		 ("b", SIMPLE(BOOL)); ("c", SIMPLE(COMM)); ("d", REF(BOOL));];;

let gammavide = [];;

let gamma =[("x", SIMPLE(BOOL)); ("a", SIMPLE(INT))];;

let gammabis =[("x", REF(BOOL)); ("a", SIMPLE(INT))];;

print_endline ("debut des tests");;

print_endline ("test du TD numero 8");;

let exo1q1_8 =
  IF(EQUAL(VAR("n"), VAR("m")), WHILE(NOT(TRUE), SEQ(SKIP, SKIP)) , DIVCOMM);;	
print_endline (string_of_expr exo1q1_8);;
print_endline (string_of_dataType (checkAndType gammatest exo1q1_8));;

print_endline ("test du td numero 9");;

let exo1q1_9 =
  POINT(POINT(SYMB(Sym("I")), ETOILE(PLUS(PLUS(SYMB(Sym("A")),
  SYMB(Sym("B"))), SYMB(Sym("C"))))), SYMB(Sym("O")));;

print_endline (string_of_regexp exo1q1_9);;

print_endline ("test du td numero 10");;

let exo2q1_10 =
  semantic gammavide TRUE (SIMPLE(BOOL));;

print_endline (string_of_regexp exo2q1_10);;
print_endline (string_of_regexp (simplify_regexp exo2q1_10));;

let exo2q2_10 =
  print_endline (string_of_regexp(semantic gamma (VAR("x")) (SIMPLE(BOOL))));;

let exo2q3_10 =
  print_endline (string_of_regexp(semantic gammavide (NOT(NOT(TRUE)))
  (SIMPLE(BOOL))));;
let exo2q3test_10 =
  print_endline (string_of_regexp(semantic gammavide TRUE (SIMPLE(BOOL))));;

if(progEquiv gamma TRUE gamma (NOT(NOT(TRUE))))then
  print_endline ("equivalence1 reussie")
else
  print_endline ("equivalence1 rate");;

let exo2q4_10 =
  print_endline (string_of_regexp(simplify_regexp(semantic gammavide (ADD(N(1), N(2))) (SIMPLE(INT)))));;

let exo2q5_10 =
  print_endline (string_of_regexp(semantic gamma (ADD(VAR("a"), N(1))) (SIMPLE(INT))));;

(*
let exo2q7_10 =
  print_endline (string_of_regexp(semantic gamma (ADD(VAR("a"), VAR("x"))) (SIMPLE(INT))));;*)

print_endline ("test du td numero 11");;

let exo1q1_11 =
  print_endline (string_of_regexp(semantic gamma (SEQ(SKIP, ADD(N(1), VAR("a")))) (SIMPLE(INT))));;

let exo1q2_11 =
  print_endline (string_of_regexp(semantic gamma (IF(NOT(VAR("x")), N(0), N(2))) (SIMPLE(INT))));;

let exo1q3_11 =
  print_endline (string_of_regexp(semantic gammavide (WHILE(NOT(NOT(TRUE)), SKIP)) (SIMPLE(COMM))));;

let exo2q1_11 =
  semantic gamma (ADD(N(1), VAR("a"))) (SIMPLE(INT));;

let exo2q3_11 =
  semantic gamma (DIVCOMM) (SIMPLE(COMM));;

if(progEquiv gamma (SEQ(SKIP, ADD(N(1), VAR("a")))) gamma (ADD(N(1), VAR("a"))))then
  print_endline ("equivalence2 reussie")
else
  print_endline ("equivalence2 rate");;

if(progEquiv gammavide (WHILE(NOT(NOT(TRUE)), SKIP)) gammavide (DIVCOMM))then
  print_endline ("equivalence4 reussie")
else
  print_endline ("equivalence4 rate");;

print_endline ("test des exemples du cours");;

let exemple1 =
  print_endline(string_of_regexp(semantic gamma (N(3)) (SIMPLE(INT))));;

let exemple2 =
  print_endline(string_of_regexp(semantic gammavide (AND(TRUE, TRUE)) (SIMPLE(BOOL))));;

let exemple3 =
  print_endline(string_of_regexp(semantic gammavide (NOT(TRUE))
  (SIMPLE(BOOL))));;

let t1 = semantic gamma (LECTBOOL("x")) (SIMPLE(BOOL));; 
  

print_endline ("fin des tests");;
