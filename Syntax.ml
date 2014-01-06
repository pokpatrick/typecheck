(*Syntax.ml*)

(*types*)

type expr =
  |SKIP 
  |TRUE
  |DIVINT
  |DIVBOOL
  |DIVCOMM
  |N of int
  |VAR of string
  |LECTBOOL of string
  |LECTINT of string
  |NOT of expr
  |AND of expr * expr
  |EQUAL of expr * expr
  |ADD of expr * expr
  |SEQ of expr * expr
  |WHILE of expr * expr
  |IF of expr * expr * expr
  |AFFBOOL of string * expr
  |AFFINT of string * expr;;

(*fonctions*)

(* expr -> string *)
let rec string_of_expr expression =
  match expression with
    |SKIP -> "skip"
    |TRUE -> "vrai"
    |DIVINT -> "erreur sur entier"
    |DIVBOOL -> "erreur sur boolean"
    |DIVCOMM -> "erreur type unit"
    |N(n) -> string_of_int n
    |VAR(s) -> s
    |LECTBOOL(s) -> "lecture de la variable booleenne " ^ s
    |LECTINT(s) -> "lecture de la variable entiere " ^ s
    |NOT(e) -> "negation de " ^ (string_of_expr e)
    |AND(e1, e2) -> (string_of_expr e1) ^ " et " ^ (string_of_expr e2)
    |EQUAL(e1, e2) -> (string_of_expr e1) ^ " = " ^ (string_of_expr e2) ^ " ?"
    |ADD(e1, e2) -> (string_of_expr e1) ^ " plus " ^ (string_of_expr e2)
    |SEQ(e1, e2) -> "faire " ^ (string_of_expr e1) ^ " puis " ^ (string_of_expr e2)
    |WHILE(c, e2) -> "tant que " ^ (string_of_expr c) ^ " faire " ^ (string_of_expr e2)
    |IF(c, e1, e2) -> "si " ^ (string_of_expr c) ^ " alors " ^
       (string_of_expr e1) ^ " sinon " ^ (string_of_expr e2)
    |AFFBOOL(s, e) -> "affecte a la variable booleenne " ^ s ^ " le booleean "
       ^ (string_of_expr e)
    |AFFINT(s, e) -> "affecte a la variable booleenne " ^ s ^ " l'entier "
       ^ (string_of_expr e);;

(*tests*)

string_of_expr SKIP;;
string_of_expr TRUE;;
string_of_expr (N(3));;
string_of_expr (VAR("x"));;
string_of_expr (NOT(TRUE));;
string_of_expr (AND(TRUE, TRUE));;
string_of_expr (ADD(N(3), N(1)));;
string_of_expr (EQUAL(N(3), N(1)));;
string_of_expr (SEQ(SKIP, SKIP));;
string_of_expr (WHILE(TRUE, SKIP));;
string_of_expr (IF(TRUE, SKIP, SKIP));;
string_of_expr (DIVINT);;
string_of_expr (DIVCOMM);;
string_of_expr (LECTBOOL ("x"));;
string_of_expr (LECTINT ("x"));;
string_of_expr (AFFBOOL ("x", TRUE));;
string_of_expr (AFFBOOL ("x", (N(3))));;
string_of_expr (IF(EQUAL(VAR("n"), VAR("m")), WHILE(TRUE, SEQ(SKIP, ADD(VAR("n"), N(1)))), DIVINT));;
