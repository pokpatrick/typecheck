(*Type.ml*)

(*dependences*)

#use "Syntax.ml"

(*types*)

type simpleType =
  |BOOL
  |INT
  |COMM;;

type dataType =
  |SIMPLE of simpleType
  |REF of simpleType
  |FUN of simpleType * dataType;;

type envType = (string * dataType) list;;

(*exceptions*)

exception Introuvable;;
exception TypeError of string;;

(*fonctions*)

let rec valeur_de environnement variable =
  match environnement with
    |[] -> raise Introuvable
    |(a, b)::tail ->
       if (variable = a) then
	 b
       else
	 valeur_de tail variable;;

(*envType -> expr -> dataType*)
let rec checkAndType environnement expression =
  try
    match expression with
      |SKIP -> SIMPLE(COMM)
      |TRUE -> SIMPLE(BOOL)
      |DIVINT -> SIMPLE(INT)
      |DIVBOOL -> SIMPLE(BOOL)
      |DIVCOMM -> SIMPLE(COMM)
      |N(n) -> SIMPLE(INT)
      |VAR(s) -> valeur_de environnement s
      |LECTBOOL(s) ->
	 (match valeur_de environnement s with
	   |SIMPLE(BOOL) -> SIMPLE(BOOL)
	   |_ -> raise (TypeError "Erreur lecture sur variable
	 booleenne"))
      |LECTINT(s) ->
	 (match valeur_de environnement s with
	   |SIMPLE(INT) -> SIMPLE(INT)
	   |_ -> raise (TypeError "Erreur lecture sur variable integer"))
      |NOT(e) -> SIMPLE(BOOL)
      |AND(e1, e2) ->
	 (match ((checkAndType environnement e1), (checkAndType environnement e2)) with
	   |(SIMPLE(BOOL), SIMPLE(BOOL)) -> SIMPLE(BOOL)
	   |_ -> raise (TypeError "Et sur autre chose que des booleens"))
      |EQUAL(e1, e2) ->
	 (match ((checkAndType environnement e1), (checkAndType environnement e2)) with
	   |(SIMPLE(INT), SIMPLE(INT)) -> SIMPLE(BOOL)
	   |(SIMPLE(BOOL), SIMPLE(BOOL)) -> SIMPLE(BOOL)
	   |_ -> raise (TypeError "Test d'egalite impossible"))
      |ADD(e1, e2) ->
	 (match ((checkAndType environnement e1), (checkAndType environnement e2)) with
	   |(SIMPLE(INT), SIMPLE(INT)) -> SIMPLE(INT)
	   |_ -> raise (TypeError "Addition sur autre chose que des entiers"))
      |SEQ(e1, e2) ->
	 (match checkAndType environnement e1 with
	   |SIMPLE(COMM) -> checkAndType environnement e2
	   |_ -> raise (TypeError "Sequence impossible"))
      |WHILE(c, e) ->
	 (match checkAndType environnement c with
	    |SIMPLE(BOOL) -> 
	       (match checkAndType environnement e with
		 |SIMPLE(COMM) -> SIMPLE(COMM)
		 |_ -> raise (TypeError "Boucle Tant Que impossible"))
	    |_ -> raise (TypeError "Erreur dans la condition"))
      |IF(c, e1, e2) ->
	 (match checkAndType environnement c with
	   |SIMPLE(BOOL) ->
	      (match ((checkAndType environnement e1), (checkAndType environnement e2)) with
		|(SIMPLE(INT), SIMPLE(INT)) -> SIMPLE(INT)
		|(SIMPLE(BOOL), SIMPLE(BOOL)) -> SIMPLE(BOOL)
		|(SIMPLE(COMM), SIMPLE(COMM)) -> SIMPLE(COMM)
		|_ -> raise (TypeError "Alternative impossible"))
	   |_ -> raise (TypeError "Erreur dans la condition"))
      |AFFBOOL(s, e) ->
	 (match valeur_de environnement s with
	   |REF(BOOL) ->
	       (match checkAndType environnement e with
		 |SIMPLE(BOOL) -> SIMPLE(COMM) 
		 |_ -> raise (TypeError "Erreur affectation"))
	   |_ -> raise (TypeError "Erreur affectation"))
      |AFFINT(s, e) ->
	 (match valeur_de environnement s with
	   |REF(INT) ->
	      (match checkAndType environnement e with
		|SIMPLE(INT) -> SIMPLE(COMM) 
		|_ -> raise (TypeError "Erreur affectation"))
	   |_ -> raise (TypeError "Erreur affectation"))
  with
      TypeError s ->
	failwith s;;

(*dataType -> string*)
let rec string_of_dataType dataType =
  match dataType with
    |SIMPLE(s) ->
       (match s with
	 |INT -> "c'est un truc de type integer !"
	 |BOOL -> "c'est un truc booleen !"
	 |COMM -> "c'est un truc de type unit")
    |REF(s) ->
       (match s with
	 |INT -> "c'est une reference de type integer !"
	 |BOOL -> "c'est une reference booleen !"
	 |COMM -> "une reference unit, c'est pas possible")
    |_ -> "c'est un truc de type FUN, mais c'est pas possible encore...";;

(*tests*)

let gamma = [("m", SIMPLE(INT)); ("n", SIMPLE(INT)); ("a", REF(INT)); 
	     ("b", SIMPLE(BOOL)); ("c", SIMPLE(COMM)); ("d", REF(BOOL));];;
let condition = EQUAL(VAR("n"), VAR("m"));;
let alors1 = WHILE(NOT(TRUE), SEQ(SKIP, ADD(N(1), N(1))));;
let alors = WHILE(NOT(TRUE), SKIP);;
let sinon = DIVCOMM;;
let programme = IF(condition, alors, sinon);;
		
(*string_of_dataType (checkAndType gamma (VAR("x")));;*)
string_of_dataType (checkAndType gamma (VAR("m")));;
string_of_dataType (checkAndType gamma (VAR("a")));;
string_of_dataType (checkAndType gamma (VAR("b")));;
string_of_dataType (checkAndType gamma (VAR("c")));;
string_of_dataType (checkAndType gamma (VAR("d")));;

string_of_dataType (checkAndType gamma condition);;
string_of_dataType (checkAndType gamma alors);;
string_of_dataType (checkAndType gamma sinon);;
string_of_dataType (checkAndType gamma programme);;

