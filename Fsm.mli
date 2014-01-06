
type automaton

(* graphical representation *)
val fsmToDotString : automaton -> string
val fsmToDotFile : string -> automaton -> unit

(* construction of automata *)
val fsmEmpty : unit -> automaton
val fsmEpsilon : unit -> automaton
val fsmSingle : string -> automaton
val fsmConcat : automaton -> automaton -> automaton
val fsmUnion : automaton -> automaton -> automaton
val fsmIter : automaton -> automaton

(* low-level construction *)
type fsmElem =
    Node of string
  | InitNode of string
  | AcceptNode of string
  | InitAcceptNode of string
  | Edge of string * string * string
      
val buildFsm : fsmElem list -> automaton

(* algorithms *)
val fsmReverse: automaton -> automaton
val fsmIsDet : automaton -> bool
val fsmDeterminize : automaton -> automaton
val fsmMinimize: automaton -> automaton
val fsmEquiv: automaton -> automaton -> bool


