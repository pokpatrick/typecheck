
(* a quick and dirty implementation of finite-state machines *)


(* printing functions *)

let string_of_collection (op:string) (cl:string) (sep:string) (tostr: 'a -> string) (lst: 'a list) = 
  let rec str = function
    | [] -> ""
    | e::[] -> tostr e
    | e::es -> (tostr e) ^ sep ^ (str es)
  in
  op ^ (str lst) ^ cl

let string_of_list tostr lst = string_of_collection "[" "]" ";" tostr lst

let string_of_set tostr lst = string_of_collection "{" "}" "," tostr lst

(* list utilities *)

let rec last = function
  | [] -> failwith "last"
  | e::[] -> e
  | _::r -> last r

let rec front = function
  | [] -> failwith "front"
  | e::[] -> []
  | e::r -> e::(front r)

let forget e = ()

(* dictionaries and string sets *)

module HashedString (* : Hashtbl.HashedType  -- let's infer this otherwise it's opaque *) =
struct
  type t = string
  let equal s1 s2 = (String.compare s1 s2) = 0
  let hash = Hashtbl.hash
end

module Dictionary = Hashtbl.Make(HashedString)

type stringSet = string Dictionary.t

let forget e = ()

let stringSet_create n = Dictionary.create n
let stringSet_size s = Dictionary.length s
let stringSet_empty s = (stringSet_size s) == 0
let stringSet_fold f s init =  Dictionary.fold (fun k _ ks -> f k ks) s init
let stringSet_copy s = Dictionary.copy s
let stringSet_iter f s = Dictionary.iter (fun k _ -> f k) s
let stringSet_add s v = Dictionary.replace s v v
let stringSet_addAll s1 s2 = stringSet_iter (fun e -> stringSet_add s1 e) s2 
let stringSet_mem s v = Dictionary.mem s v
let stringSet_allMem s1 s2 = stringSet_fold (fun w ret -> ret && (stringSet_mem s2 w)) s1 true
let stringSet_equal s1 s2 = (stringSet_size s1 = stringSet_size s2) && stringSet_allMem s1 s2
let stringSet_remove s v = Dictionary.remove s v
let stringSet_removeAll s1 s2 = stringSet_iter (fun e -> stringSet_remove s1 e) s2
let stringSet_choose s =  let ck = ref "" 
			  in begin 
			    forget (stringSet_fold (fun k r -> r && (ck := k ; false)) s true)  ;
			    !ck
			  end
let stringSet_pickup s = let e = stringSet_choose s
			 in begin
			   stringSet_remove s e ;
			   e
			 end
			     

let string_of_stringSet s = "{" ^ (stringSet_fold (fun e str -> e ^ "," ^ str) s "}")
 
let stringSet_of_list l =
  let s = stringSet_create (List.length l)
  in begin
    List.iter (fun e -> stringSet_add s e) l ;
    s
  end

let list_of_stringSet s =
  stringSet_fold (fun e l -> e::l) s []

let prefixedSet prefix e = let ne = stringSet_create (stringSet_size e) in
			   begin 
			     stringSet_iter (fun k -> stringSet_add ne (prefix ^ k)) e ;
			     ne 
			   end

module HashedStringSet =
struct
  type t = stringSet
  let equal = stringSet_equal
  let hash s = stringSet_fold (fun k h -> h + (Hashtbl.hash k)) s 0
end

module StringSetMap = Hashtbl.Make(HashedStringSet)

let stringSetMap_choose s =  let ck = ref (stringSet_create 8) 
			     in begin 
			       forget (StringSetMap.fold (fun k _ r -> r && (ck := k ; false)) s true)  ;
			       (!ck,StringSetMap.find s !ck)
			     end
let stringSetMap_pickup s = let (k,v) = stringSetMap_choose s
			 in begin
			   StringSetMap.remove s k ;
			   (k,v)
			 end

let stringSetMap_is_empty s = (StringSetMap.length s) = 0

type stringSetSet = bool StringSetMap.t

(* internal representation *)

type node = { id:string; outEdges: stringSet Dictionary.t }
and edge = { eid:string; label:string; src: string; dest: string }
    
type automaton = { mutable init: string; 
		   nodes: node Dictionary.t; 
		   edges: edge Dictionary.t;
		   accept:stringSet }
    
(* accessors and basic utilities *)

let fetchNode fsm id = 
  try Dictionary.find fsm.nodes id
  with Not_found -> failwith ("Fsm.fetchNode: " ^ id)

let hasNode fsm id = Dictionary.mem fsm.nodes id

let fetchEdge fsm id = Dictionary.find fsm.edges id

let hasEdge fsm id = Dictionary.mem fsm.edges id

let fetchNodes fsm = Dictionary.fold 
  (fun nid _ nodes -> (fetchNode fsm nid)::nodes)
  fsm.nodes
  []

let fetchEdges fsm = Dictionary.fold
  (fun eid _ edges -> (fetchEdge fsm eid)::edges)
  fsm.edges
  []

let fetchLabels fsm node = stringSet_of_list (Dictionary.fold (fun lbl _ labels -> lbl::labels) node.outEdges [])

let fetchNodeNexts fsm node lbl = 
  try Dictionary.fold (fun id _ ids -> id::ids) (Dictionary.find node.outEdges lbl) []
  with Not_found -> []			 

let fetchNexts fsm id lbl =
  stringSet_of_list (fetchNodeNexts fsm (fetchNode fsm id) lbl)

let nodeToDotString n fsm = 
  if stringSet_mem fsm.accept n.id
  then "  N" ^ n.id ^ " [label=\"" ^ (* n.id *) "" ^ "\",shape=doublecircle];\n"
  else "  N" ^ n.id ^ " [label=\"" ^ (* n.id *) "" ^ "\"];\n"

let edgeToDotString e = "  N" ^ e.src ^ " -> N" ^ e.dest ^ " [label=\"" ^ e.label ^ "\"];\n"

let fsmToDotString fsm = 
  "digraph { \n"
  ^ "  rankdir = \"LR\"\n"
  ^ (List.fold_left (fun s n -> (nodeToDotString n fsm) ^ s) "" (fetchNodes fsm))
  ^ "  __hidden_init [shape=none,label=\"\"];\n"
  ^ "  __hidden_init" ^ " -> N" ^ fsm.init ^ "\n"
  ^ (List.fold_left (fun s e -> (edgeToDotString e) ^ s) "" (fetchEdges fsm)) 
  ^ "}"

let fsmToDotFile fname fsm =
  let out = open_out fname
  in begin 
    output_string out (fsmToDotString fsm) ;
    close_out out
  end

(* building finite state machines *)

type fsmElem =
  | Node of string
  | InitNode of string
  | AcceptNode of string
  | InitAcceptNode of string
  | Edge of string * string * string

let string_of_fsmElem = function
  | Node s -> "Node("^s^")"
  | InitNode s -> "InitNode("^s^")"
  | AcceptNode s -> "AcceptNode("^s^")"
  | InitAcceptNode s -> "InitAcceptNode("^s^")"
  | Edge (src,lbl,dest) -> "Edge("^src^","^lbl^","^dest^")"
  
let string_of_fsmElems els = string_of_list string_of_fsmElem els

let fsmNode_of_id fsm id =
    if id = fsm.init
    then (if stringSet_mem fsm.accept id
      then InitAcceptNode(id)
      else InitNode(id))
    else (if stringSet_mem fsm.accept id
      then AcceptNode(id)
      else Node(id))

let fsmElems_of_fsm fsm =
  let nodes = Dictionary.fold (fun id _ nodes ->
    (fsmNode_of_id fsm id)::nodes) fsm.nodes []
  and edges = Dictionary.fold (fun _ edge edges ->
    (Edge (edge.src,edge.label,edge.dest))::edges) fsm.edges []
  in nodes @ edges

let fsmAddNode fsm node = 
  if hasNode fsm node.id
  then 
    failwith "fsm: add node"
  else
    Dictionary.add fsm.nodes node.id node

let fsmAddEdge fsm edge =
  if hasEdge fsm edge.eid
  then
    failwith "fsm:add edge (id already defined)"
  else 
    let nsrc = fetchNode fsm edge.src
    and _ = fetchNode fsm edge.dest 
    in
    let dests = 
      try Dictionary.find nsrc.outEdges edge.label 
      with Not_found -> let ds = Dictionary.create 5
			in 
			Dictionary.add nsrc.outEdges edge.label ds;
			ds
    in
    if Dictionary.mem dests edge.dest
    then
      (* failwith "fsm:add edge (duplicate dest)" *)
      ()     (* do nothing *)
    else
      begin
	Dictionary.add dests edge.dest edge.dest ;
	Dictionary.add fsm.edges edge.eid edge
      end
	
let fsmAdd fsm elem = match elem with
  | Node(id) -> fsmAddNode fsm { id=id ; outEdges=Dictionary.create 5 }
  | InitNode(id) -> 
    if (String.compare fsm.init "") = 0 
    then
      begin
	fsmAddNode fsm { id=id ; outEdges=Dictionary.create 5 } ;
	fsm.init <- id
      end
    else
      failwith "fsm: duplicate init"
  | AcceptNode(id) ->
    begin
      fsmAddNode fsm { id=id ; outEdges=Dictionary.create 5 } ;
      Dictionary.add fsm.accept id id
    end
  | InitAcceptNode(id) -> 
    if (String.compare fsm.init "") = 0 
    then
      begin
	fsmAddNode fsm { id=id ; outEdges=Dictionary.create 5 } ;
	fsm.init <- id ;
	Dictionary.add fsm.accept id id

      end
    else
      failwith "fsm: duplicate init"
  | Edge(src,label,dest) -> let fresh = Dictionary.length fsm.edges
			    in fsmAddEdge fsm { eid="edge" ^ (string_of_int fresh); src=src; label=label; dest=dest }

let fsmEmpty () = { init="" ; nodes=Dictionary.create 256 ; edges=Dictionary.create 1024 ; accept=Dictionary.create 32 } 
				     
let buildFsm elems =
  let fsm = fsmEmpty ()
  in
  let rec build = function
    | [] -> fsm
    | e::es -> fsmAdd fsm e ; build es
in build elems

let fsmSingle lbl =
  buildFsm [ InitNode "start" ;
             AcceptNode "end" ;
             Edge ("start", lbl, "end") ]

let fsmEpsilon () =
  fsmSingle "eps"

let copyNodes prefix fsm cfsm =
  List.iter 
    (fun node -> 
      let nnode = { id = prefix ^ node.id ;
		    outEdges = Dictionary.create (Dictionary.length node.outEdges)  }
      in fsmAddNode cfsm nnode)  (fetchNodes fsm)
    
let copyEdges prefix fsm cfsm =
  List.iter
    (fun edge ->
      let nedge = { eid = prefix ^ edge.eid ;
		    src = prefix ^ edge.src ;
		    label = edge.label ; 
		    dest = prefix ^ edge.dest }
      in fsmAddEdge cfsm nedge) (fetchEdges fsm) 
  
let addAllAccept prefix fsm cfsm =
  Dictionary.iter
    (fun k v -> Dictionary.add cfsm.accept (prefix ^ k) (prefix ^ v))
    fsm.accept

(* test for determinism *)

let fsmIsDet fsm =
  let isDet node = let labels = list_of_stringSet (fetchLabels fsm node)
		   in let rec test = function
		     | [] -> true
		     | lbl::lbls -> (match (fetchNodeNexts fsm node lbl) with
			 | [] -> true
			 | _::[] -> true
			 | _ -> false) && test lbls
		      in
		      test labels
  in
  let rec testNodes = function
    | [] -> true
    | n::ns -> (isDet n) && testNodes ns
  in
  testNodes (fetchNodes fsm)

(* concatenation *)

let fsmConcat fsm1 fsm2 =
  let fsm = fsmEmpty () in
  begin
    (* copy nodes of first fsm (with renaming to avoid clashes) *)
    copyNodes "cat1_" fsm1 fsm ;
    (* copy edges of first fsm (with renaming) *)
    copyEdges "cat1_" fsm1 fsm ;
    (* init state renamed *)
    fsm.init <- "cat1_" ^ fsm1.init ;
    (* copy nodes of second fsm *)
    copyNodes "cat2_" fsm2 fsm ;
    (* copy edges of second fsm *)
    copyEdges "cat2_" fsm2 fsm ;
    (* copy the accept nodes of second fsm (with renaming) *)
    addAllAccept "cat2_" fsm2 fsm ;
    (* link fsm1 and fsm2 by epsilon *)
    Dictionary.iter
      (fun k _ -> 
	fsmAddEdge fsm { eid="cat12_" ^ (string_of_int ((Dictionary.length fsm.edges) + 1)) ; src="cat1_" ^ k ; label="eps" ; dest="cat2_" ^ fsm2.init })
      fsm1.accept ;
    (* return the resulting fsm *)
    fsm
  end
		 
(* union *)

let fsmUnion fsm1 fsm2 =
  let fsm = fsmEmpty () 
  in begin
    (* copy the nodes of the first fsm *)
    copyNodes "sum1_" fsm1 fsm ;
    (* copy the edges of the first fsm *)
    copyEdges "sum1_" fsm1 fsm ;
    (* add the accept nodes of the first fsm *)
    addAllAccept "sum1_" fsm1 fsm ;
    (* copy the nodes of the second fsm *)
    copyNodes "sum2_" fsm2 fsm ;
    (* copy the edges of the second fsm *)
    copyEdges "sum2_" fsm2 fsm ;
    (* add the accept nodes of the second fsm *)
    addAllAccept "sum2_" fsm2 fsm ;
    (* add an initial state *)
    (let init = { id="sum_init"; outEdges = Dictionary.create 2 }
     in begin 
       fsmAddNode fsm init ;
       fsm.init <- "sum_init"
     end) ;
    (* add a transition with the init state of first fsm *)
    fsmAddEdge fsm { eid="sum_init1"; src="sum_init"; label="eps"; dest="sum1_" ^ fsm1.init } ;
    fsmAddEdge fsm { eid="sum.init2"; src="sum_init"; label="eps"; dest="sum2_" ^ fsm2.init } ;
    fsm
  end

(* iteration *)

let fsmIter fsm =
  let fsm' = fsmEmpty ()
  in begin
    (* copy the nodes *)
    copyNodes "iter_" fsm fsm' ;
    (* init state renamed *)
    fsm'.init <- "iter_" ^ fsm.init ;
    (* add the accept nodes of the first fsm *)
    addAllAccept "iter_" fsm fsm' ;
    (* copy the edges *)
    copyEdges "iter_" fsm fsm' ;    
    (* link init and final nodes *)
    Dictionary.iter
      (fun k _ -> 
        begin
	  fsmAddEdge fsm' { eid="iter_" ^ (string_of_int ((Dictionary.length fsm'.edges) + 1)) ; src=k ; label="eps" ; dest=fsm'.init } ;
	  fsmAddEdge fsm' { eid="iter_" ^ (string_of_int ((Dictionary.length fsm'.edges) + 1)) ; src=fsm'.init ; label="eps" ; dest=k } ;
        end)
      fsm'.accept ;
    (* return the resulting fsm *)
    fsm' ;
  end

(* reversal *)

let fsmReverse fsm =
  let rec rev = function
    | InitNode s -> AcceptNode s
    | InitAcceptNode s -> AcceptNode s
    | AcceptNode s -> Node s
    | Node s -> Node s
    | Edge(src,lbl,dest) -> Edge(dest,lbl,src)
  and accept = function
    | [] -> []
    | (InitAcceptNode s)::elems -> Edge("r_I","eps",s)::(accept elems)
    | (AcceptNode s)::elems -> Edge("r_I","eps",s)::(accept elems)
    | _::elems ->accept elems
  in
  let elems = fsmElems_of_fsm fsm
  in begin
    (* print_string ("reverse: "^ (string_of_fsmElems elems) ^ "\n") ; *)
    let relems = (List.map rev elems) @ (InitNode("r_I")::(accept elems))
    in begin 
      (* print_string ("reversed: "^ (string_of_fsmElems relems) ^ "\n") ; *) 
      buildFsm relems
    end
  end
    
(* determinization *)

let genNameFromStates states =
  Dictionary.fold (fun id _ ids -> ids ^ id) states ""

(* epsilon-closure: automaton -> stringSet -> stringSet *)
let rec epsilonClosure fsm (states:stringSet) =
  let estates = stringSet_create 32
  in begin
    stringSet_addAll estates states ;
    (* print_endline ("epsilon closure of: " ^ (string_of_stringSet estates)) ; *)
    stringSet_iter (fun state ->
      let nexts = fetchNexts fsm state "eps"
      in begin
        stringSet_removeAll nexts estates ;
        stringSet_addAll estates nexts ;
        if not (stringSet_empty nexts)
        then stringSet_addAll estates (epsilonClosure fsm estates)
      end)
      estates ;
    estates
  end
and epsilonClosure1 fsm (state:string) =
  let states = stringSet_create 10
  in
  stringSet_add states state ;
  epsilonClosure fsm states

let fetchAllLabels fsm ids =
  stringSet_fold (fun id lbls -> (stringSet_addAll lbls (fetchLabels fsm (fetchNode fsm id))) ; lbls) ids (stringSet_create 5)
  
let fetchAllNexts fsm ids lbl =
  stringSet_fold (fun id nexts -> 
    begin 
      stringSet_addAll nexts (fetchNexts fsm id lbl) ;
      nexts
    end ) ids (stringSet_create 16)
	
let fsmDeterminize fsm =
  (* let rec det (dstates: string stringSetMap.t) (dtrans:(string*string) Dictionary.t) = *)
  let rec det ndstates dstates dtrans =
    if not (stringSetMap_is_empty ndstates)
    then let (state,id)=stringSetMap_pickup ndstates
	 in begin
	   (* print_string ("state="^(string_of_stringSet state)^" id="^id^"\n") ; *)
(* 	   print_string ("how many more states ? " ^ (string_of_int (StringSetMap.length dstates)) ^ "\n") ;  *)
	   stringSet_iter (fun (lbl:string) -> if lbl<>"eps" then
	       begin
		 (* print_string ("for label: " ^ lbl ^ " :\n") ; *)
		 let nstate = epsilonClosure fsm (fetchAllNexts fsm state lbl)
		 in begin
		  (*  print_string ("new state = " ^ (string_of_stringSet nstate) ^ "\n") ; *)
		   if (stringSet_size nstate)>0
		   then 
		     let (nid:string) = 
		       try StringSetMap.find dstates nstate with
			 | Not_found ->
			   (* print_string "new state not found (really new)\n" ; *)
			   let nid = "N" ^ (string_of_int (StringSetMap.length dstates))
			   in begin 
			     StringSetMap.add ndstates nstate nid ;
			     StringSetMap.add dstates nstate nid ;
			     nid
			   end
		     in Dictionary.add dtrans id (lbl,nid) 
		 end 
	       end)
	     (fetchAllLabels fsm state) ;
	   det ndstates dstates dtrans
	 end
  and mkNodes dstates = StringSetMap.fold (fun (dstate:stringSet) (id:string) dnodes ->
    let (init,accept) = (stringSet_fold (fun state (ini,acc) -> 
      (ini or (fsm.init = state), acc or (stringSet_mem fsm.accept state))) dstate (false,false))
    in (if init then
	(if accept 
	 then
	    InitAcceptNode id
	 else InitNode id)
      else (if accept
	then AcceptNode id
	else Node id))::dnodes) dstates []
  and mkEdges dtrans = Dictionary.fold (fun src (lbl,dest) edges ->
    (Edge (src,lbl,dest))::edges) dtrans []
  in 
  let dstates = StringSetMap.create 32
  and dtrans = Dictionary.create 64
  in begin
    (* print_string ("determinize fsm with init=" ^ fsm.init) ; print_newline() ; *)
    StringSetMap.add dstates (epsilonClosure1 fsm fsm.init) "N0" ;
    let ndstates = StringSetMap.copy dstates
    in begin
      (* print_endline "determinize states" ;  *)
      det ndstates dstates dtrans ;
      let nodes = mkNodes dstates
      and edges = mkEdges dtrans
      in begin
	(* print_string (string_of_fsmElems (nodes @ edges)) ; print_newline () ; *)
	buildFsm (nodes @ edges)
      end
    end
  end
					  
let fsmMinimize fsm =
  fsmDeterminize (fsmReverse (fsmDeterminize (fsmReverse fsm)))


let fsmEquiv fsm1 fsm2 =
  let fsmtest = 
    fsmUnion
      (fsmConcat (fsmSingle "__left__") fsm1)
      (fsmConcat (fsmSingle "__right__") fsm2)
  in let fsmeq = fsmMinimize fsmtest
     in let lefts = fetchNexts fsmeq fsmeq.init "__left__"
     and rights = fetchNexts fsmeq fsmeq.init "__right__"
        in (stringSet_size lefts)==1 
        && (stringSet_size rights)==1
        && (stringSet_pickup lefts) == (stringSet_pickup rights)

