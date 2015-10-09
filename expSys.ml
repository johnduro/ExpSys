	(* ( and ) which are fairly obvious. Example : A + (B | C) => D *)
	(* ! which means NOT. Example : !B *)
	(* + which means AND. Example : A + B *)
	(* | which means OR. Example : A | B *)
	(* ˆ which means XOR. Example : A ˆ B *)
	(* => which means "implies". Example : A + B => C *)
	(* <=> which means "if and only if". Example : A + B <=> C *)

exception EvalExcp of string

module EvalError =
	struct
		let raiseEvalExcp s =
			EvalExcp s
		let noOp (nbLine, ogStr) =
			raiseEvalExcp ("Evaluation error : can't evaluate rule line " ^ (string_of_int nbLine) ^ ", an operator is not supported : " ^ ogStr)
	end

module type ExpertsysSig =
	sig
		type t
		type expr = Value of t | Not of expr | And of (expr * expr) | Or of (expr * expr) | Xor of (expr * expr)
		type rule = Impl of (expr * expr) | Ifoif of (expr * expr)
		type fact = Facts of (expr list * expr list) (* (TRUE * FALSE) *)
		val eval : rule -> fact -> (int * string) -> bool -> fact list
		val makeEval : rule -> fact list -> (int * string) -> bool -> fact list
		val printFacts : fact -> unit
		val stringOfExpr : expr -> string
		val compareListOfFactsList : fact list -> fact list -> bool
		val evalBool : expr -> fact -> bool
	end

module Expertsys : (ExpertsysSig with type t = char) =
	struct
		type t = char
		type expr = Value of t | Not of expr | And of (expr * expr) | Or of (expr * expr) | Xor of (expr * expr)
		type rule = Impl of (expr * expr) | Ifoif of (expr * expr)
		type fact = Facts of (expr list * expr list) (* (TRUE * FALSE) *)

		let rec stringOfExpr e =
			match e with
			| Not (exp)		-> ("( NOT " ^ (stringOfExpr exp) ^ " )")
			| And (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " AND " ^ (stringOfExpr e2) ^ " )")
			| Or (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " OR " ^ (stringOfExpr e2) ^ " )")
			| Xor (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " XOR " ^ (stringOfExpr e2) ^ " )")
			| Value v		-> Char.escaped v

		let rec addNewFacts oldFacts newFacts =
			match newFacts with
			| []									-> oldFacts
			| hd::tl when (Utils.notIn hd oldFacts)	-> addNewFacts (oldFacts @ [hd]) tl
			| hd::tl								-> addNewFacts oldFacts tl

		let mergeFacts factList1 factList2 =
			let rec loop2 fl (Facts (tf1, ff1)) ret =
				match fl with
				| []						-> ret
				| (Facts (tf2, ff2))::tl	-> loop2 tl (Facts (tf1, ff1)) (ret @ [(Facts ((addNewFacts tf1 tf2), (addNewFacts ff1 ff2)))])
			in
			let rec loop1 fl ret =
				match fl with
				| []		-> ret
				| hd::tl	-> loop1 tl (ret @ (loop2 factList2 hd []))
			in
			loop1 factList1 []

		let rec removeFromFacts oldFacts newFacts ret =
			match oldFacts with
			| []									-> ret
			| hd::tl when (Utils.notIn hd newFacts)	-> removeFromFacts tl newFacts (ret @ [hd])
			| hd::tl								-> removeFromFacts tl newFacts ret

		let getBoolValue value (Facts (trueFacts, falseFacts)) =
			let checkValue (vl:expr) =
				match vl with
				| Value v ->
					begin
						match value with
						| Value v2 when v = v2	-> true
						| _						-> false
					end
				| _ -> false
			in
			let rec loop tf =
				match tf with
				| []							-> false
				| hd::tl when (checkValue hd)	-> true
				| hd::tl						-> loop tl
			in
			loop trueFacts

		let rec evalBool e (facts:fact) =
			match e with
			| Not (exp)		-> not (evalBool exp facts)
			| And (e1, e2)	-> (evalBool e1 facts) && (evalBool e2 facts)
			| Or (e1, e2)	-> (evalBool e1 facts) || (evalBool e2 facts)
			| Xor (e1, e2)	-> (evalBool e1 facts) <> (evalBool e2 facts)
			| Value v		-> (getBoolValue (Value v) facts)

		let addExprToFacts (Facts (trueFacts, falseFacts)) exp infos boolz firstTurn =
			let rec injectNewFacts fl ret =
				match fl with
				| []							-> ret
				| (Facts (newTf, newFf))::tl	-> injectNewFacts tl (ret @ [(Facts ((addNewFacts (removeFromFacts trueFacts newFf []) newTf), (addNewFacts (removeFromFacts falseFacts newTf []) newFf)))])
			in
			let rec exprToFact (tf, ff) ex bol =
				match ex with
				| Not (e)						-> exprToFact (tf, ff) e (not bol)
				| And (e1, e2)					-> mergeFacts (exprToFact (tf, ff) e1 bol) (exprToFact (tf, ff) e2 bol)
				| Or (e1, e2) when firstTurn	-> (exprToFact (tf, ff) e1 bol) @ (exprToFact (tf, ff) e2 bol)
				| Or (e1, e2)					-> if (evalBool e1 (Facts (trueFacts, falseFacts))) = bol then (exprToFact (tf, ff) e1 bol) else (exprToFact (tf, ff) e2 bol)
				| Xor (e1, e2) when firstTurn	-> (mergeFacts (exprToFact (tf, ff) e1 bol) (exprToFact (tf, ff) e2 (not bol))) @ (mergeFacts (exprToFact (tf, ff) e1 (not bol)) (exprToFact (tf, ff) e2 bol))
				| Xor (e1, e2)					-> if (evalBool e1 (Facts (trueFacts, falseFacts))) = bol then (mergeFacts (exprToFact (tf, ff) e1 bol) (exprToFact (tf, ff) e2 (not bol))) else (mergeFacts (exprToFact (tf, ff) e1 (not bol)) (exprToFact (tf, ff) e2 bol))
				| Value v when bol 				-> [(Facts ((tf @ [(Value v)]), ff))]
				| Value v when not bol 			-> [(Facts (tf, (ff @ [(Value v)])))]
				| _								-> raise (EvalError.noOp infos)
			in
			let factsList = exprToFact ([], []) exp boolz in
			injectNewFacts factsList []


		let mergeIfoif factsList1 factsList2 =
			let rec loop2 (Facts (tf1, ff1)) fl ret =
				match fl with
				| []						-> ret
				| (Facts (tf2, ff2))::tl	-> loop2 (Facts (tf1, ff1)) tl (ret @ [(Facts ((addNewFacts (removeFromFacts tf1 ff2 []) tf2), (addNewFacts (removeFromFacts ff1 tf2 []) ff2)))])
			in
			let rec loop1 fl ret =
				match fl with
				| [] 		-> ret
				| hd::tl	-> loop1 tl (ret @ (loop2 hd factsList2 []))
			in
			loop1 factsList1 []

		let rec eval e (facts:fact) infos firstTurn =
			match e with
			| Impl (e1, e2) when (evalBool e1 facts) = true						-> (addExprToFacts facts e2 infos true firstTurn)
			| Impl (e1, e2)														-> [facts]
			| Ifoif (e1, e2) when (evalBool e1 facts) && (evalBool e2 facts)	-> (mergeIfoif (addExprToFacts facts e1 infos true firstTurn) (addExprToFacts facts e2 infos true firstTurn))
			| Ifoif (e1, e2)													-> (mergeIfoif (addExprToFacts facts e1 infos false firstTurn) (addExprToFacts facts e2 infos false firstTurn))

		let rec makeEval e factz infos firstTurn =
			let rec loop fac ret =
				match fac with
				| []		-> ret
				| hd::tl	-> loop tl (ret @ (eval e hd infos firstTurn))
			in
			loop factz []

		let printFacts (Facts (trueFacts, falseFacts)) =
			let rec loop lst =
				match lst with
				| [] -> print_char '\n'
				| hd::[] -> print_string (stringOfExpr hd); loop []
				| hd::tl -> print_string ((stringOfExpr hd) ^ ", "); loop tl
			in
			print_string "True facts : ";
			loop trueFacts;
			print_string "False facts : ";
			loop falseFacts

		let rec compareListOfFactsList list1 list2 =
			let rec compFlist flist1 flist2 =
				match flist1 with
				| [] -> begin
							match flist2 with
							| [] -> true
							| hdd::tll -> false
						end
				| hd1::tll1 ->
							begin
								match flist2 with
								| [] -> false
								| hd2::tll2 when hd1 = hd2 -> compFlist tll1 tll2
								| hd2::tll2 -> false
							end
			in
			match list1 with
			| [] -> begin
						match list2 with
						| [] -> true
						| hd::tl -> false
					end
			| (Facts (tf1, ff1))::tl1 ->
						begin
							match list2 with
							| [] -> false
							| (Facts (tf2, ff2))::tl2 when (compFlist tf1 tf2) && (compFlist ff1 ff2) -> compareListOfFactsList tl1 tl2
							| hd2::tl2 -> false
						end
	end



(* LISTE DE RETOUR = (RULES (RULE * NB_LINE * ORIGINAL_STR)) * (FACTS (TRUE * FALSE)) * QUERIES *)
let executeExpSys (rules, facts, queries) debug verbose =
	let checkQueries factz =
		let rec matchQueriesWithResults (Expertsys.Facts(trueFacts, falseFacts)) qr =
			match qr with
			| []										-> ()
			| hd::tl when (Utils.notIn hd falseFacts)	-> print_endline ((Expertsys.stringOfExpr hd) ^ " is true");
														matchQueriesWithResults (Expertsys.Facts(trueFacts, falseFacts)) tl
			| hd::tl									-> print_endline ((Expertsys.stringOfExpr hd) ^ " is false");
														matchQueriesWithResults (Expertsys.Facts(trueFacts, falseFacts)) tl
		in
		let rec loop ft total nb =
			match ft with
			| []		-> ()
			| hd::tl	-> print_endline ("State " ^ (string_of_int nb) ^ "/" ^ (string_of_int total) ^ " :");
						matchQueriesWithResults hd queries;
						if verbose || debug then Expertsys.printFacts hd; print_string "\n\n";
						loop tl total (nb + 1)
		in
		let rec printQueries qr =
			match qr with
			| []		-> print_string "\n\n"
			| hd::[]	-> print_string (Expertsys.stringOfExpr hd); printQueries []
			| hd::tl	-> print_string ((Expertsys.stringOfExpr hd) ^ ", "); printQueries tl
		in
		print_endline "\nFinal results :\n";
		print_endline ("There is " ^ (string_of_int (List.length factz)) ^ " different(s) state(s) for the facts :\n");
		print_string "The queries that where asked for are the following : ";
		printQueries queries;
		print_endline "The results are :\n";
		loop factz (List.length factz) 1
	in
	let rec startEval (rule, nbLine, ogStr) factz old firstTurn =
		let rec printFactsList fl nbState =
			match fl with
			| []		-> print_char '\n'
			| hd::tl	-> print_endline ("State number " ^ (string_of_int nbState) ^ " :"); Expertsys.printFacts hd; print_char '\n'; printFactsList tl (nbState + 1)
		in
		let nf = Expertsys.makeEval rule factz (nbLine, ogStr) firstTurn in
		if verbose || debug then
			begin
				print_endline ("Evaluating rule line " ^ (string_of_int nbLine) ^ " : " ^ ogStr);
				print_endline ("Before evaluation there is " ^ (string_of_int (List.length factz)) ^ " different(s) state(s)");
			end;
		if verbose || debug then
			begin
				print_endline ("After evaluation there is " ^ (string_of_int (List.length nf)) ^ " different(s) state(s)\n");
				if debug then printFactsList nf 1;
			end;
		if (Expertsys.compareListOfFactsList nf factz) = false then
			loop old nf [] false
		else
			nf
	and loop rulez factz old firstTurn =
		match rulez with
		| [] -> factz
		| hd::tl -> loop tl (startEval hd factz old firstTurn) (old @ [hd]) firstTurn
	in
	if verbose then print_endline "\nEvaluating ...\n";
	let finalFacts = loop rules [facts] [] true in
	checkQueries finalFacts;
	if verbose then print_endline "\n... done"

