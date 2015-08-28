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
		(* val addExprToFacts : fact -> expr -> (int * string) -> bool -> fact (\* ????? *\) *)
		(* val getBoolValue : expr -> fact -> bool *) (* TEST *)
		(* val evalBool : expr -> fact -> bool *) (* TEST *)
		val eval : rule -> fact -> (int * string) -> fact
		val printFacts : fact -> unit
		val stringOfExpr : expr -> string
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

 (* **** A TESTER **** *)
		(* let rec addToFalseFacts (Facts (trueFacts, falseFacts)) exp alter = *)
		(* 	match exp with *)
		(* 	| Not (exp)		-> addToTrueFacts (Facts (trueFacts, falseFacts)) exp alter *)
		(* 	| And (e1, e2)	-> addToFalseFacts (fst (addToFalseFacts (Facts (trueFacts, falseFacts)) e1 alter)) e2 alter *)
		(* 	| Or (e1, e2)	-> addToFalseFacts (fst (addToFalseFacts (Facts (trueFacts, falseFacts)) e1 alter)) (alter @ [fst (addToFalseFacts (Facts (trueFacts, falseFacts)) e2 alter)]) *)
		(* 	(\* | Xor (e1, e2)	-> *\) *)
		(* 	| Value v		-> *)
		(* 		begin *)
		(* 			if (Utils.notIn v falseFacts) then *)
		(* 				(Facts ((removeFact trueFacts v), falseFacts @ [v]), alter) *)
		(* 			else *)
		(* 				(Facts (trueFacts, falseFacts), alter) *)
		(* 		end *)
		(* 	| _				-> (Facts (trueFacts, falseFacts), alter) *)

		(* let rec addToTrueFacts (Facts (trueFacts, falseFacts)) exp alter = *)
		(* 	match exp with *)
		(* 	| Not (exp)		-> addToFalseFacts (Facts (trueFacts, falseFacts)) exp alter *)
		(* 	| And (e1, e2)	-> addToTrueFacts (fst (addToTrueFacts (Facts (trueFacts, falseFacts)) e1 alter)) e2 alter *)
		(* 	| Or (e1, e2)	-> addToTrueFacts (fst (addToTrueFacts (Facts (trueFacts, falseFacts)) e1 alter)) (alter @ [fst (addToTrueFacts (Facts (trueFacts, falseFacts)) e2 alter)]) *)
		(* 	(\* | Xor (e1, e2)	-> *\) *)
		(* 	| Value v		-> *)
		(* 		begin *)
		(* 			if (Utils.notIn v trueFacts) then *)
		(* 				(Facts (trueFacts @ [v], (removeFact falseFacts v)), alter) *)
		(* 			else *)
		(* 				(Facts (trueFacts, falseFacts), alter) *)
		(* 		end *)
		(* 	| _				-> (Facts (trueFacts, falseFacts), alter) *)


(* ************************************************************************* *)
	(* A + B => !(C + D) *)

		let rec addNewFacts oldFacts newFacts =
			match newFacts with
			| []									-> oldFacts
			| hd::tl when (Utils.notIn hd oldFacts)	-> addNewFacts (oldFacts @ [hd]) tl
			| hd::tl								-> addNewFacts oldFacts tl

		let mergeFacts (Facts (tf1, ff1)) (Facts (tf2, ff2)) =
			let ntf = addNewFacts tf1 tf2 in
			let nff = addNewFacts ff1 ff2 in
			(Facts (ntf, nff))

		let rec removeFromFacts oldFacts newFacts ret =
			match oldFacts with
			| []									-> ret
			| hd::tl when (Utils.notIn hd newFacts)	-> removeFromFacts tl newFacts (ret @ [hd])
			| hd::tl								-> removeFromFacts tl newFacts ret


		let addExprToFacts (Facts (trueFacts, falseFacts)) exp infos boolz =
			let rec exprToFact (tf, ff) ex bol =
				match ex with
				| Not (e)					-> exprToFact (tf, ff) e (not bol)
				| And (e1, e2)				-> mergeFacts (exprToFact (tf, ff) e1 bol) (exprToFact (tf, ff) e2 bol)
				| Value v when bol 			-> (Facts ((tf @ [(Value v)]), ff))
				| Value v when not bol 		-> (Facts (tf, (ff @ [(Value v)])))
				| _							-> raise (EvalError.noOp infos)
			in
			let (Facts (newTf, newFf)) = exprToFact ([], []) exp boolz in
			Facts ((addNewFacts (removeFromFacts trueFacts newFf []) newTf), (addNewFacts (removeFromFacts falseFacts newTf []) newFf))

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

		let mergeIfoif (Facts (tf1, ff1)) (Facts (tf2, ff2)) =
			Facts ((addNewFacts (removeFromFacts tf1 ff2 []) tf2), (addNewFacts (removeFromFacts ff1 tf2 []) ff2))

		let rec eval e (facts:fact) infos =
			match e with
			| Impl (e1, e2) when (evalBool e1 facts) = true						-> (addExprToFacts facts e2 infos true)
			| Impl (e1, e2)														-> facts
			| Ifoif (e1, e2) when (evalBool e1 facts) && (evalBool e2 facts)	-> (mergeIfoif (addExprToFacts facts e1 infos true) (addExprToFacts facts e2 infos true))
			| Ifoif (e1, e2)													-> (mergeIfoif (addExprToFacts facts e1 infos false) (addExprToFacts facts e2 infos false))

		let printFacts (Facts (trueFacts, falseFacts)) =
			let rec loop lst boolVal =
				match lst with
				| [] -> print_char '\n'
				| hd::tl -> print_endline ((stringOfExpr hd) ^ " is " ^ boolVal); loop tl boolVal
			in
			loop trueFacts "true";
			loop falseFacts "false"
	end


(* faire une liste de facts, a chaque fact correspond un etat possible en cas de (expr => expr | expr) *)

(* LISTE DE RETOUR = (RULES (RULE * NB_LINE * ORIGINAL_STR)) * (FACTS (TRUE * FALSE)) * QUERIES *)
let executeExpSys (rules, facts, queries) =
	let checkQueries (Expertsys.Facts (trueFacts, falseFacts)) qrz =
		let rec printQueries qr =
			match qr with
			| []		-> print_char '\n'
			| hd::tl	-> print_string ((Expertsys.stringOfExpr hd) ^ " "); printQueries tl
		in
		print_endline "Final state for facts : ";
		Expertsys.printFacts (Expertsys.Facts (trueFacts, falseFacts));
		printQueries qrz
	(* A REFAIRE  *)
	in
	let startEval (rule, nbLine, ogStr) factz =
		print_endline ("Evaluating rule line " ^ (string_of_int nbLine) ^ " : " ^ ogStr);
		let nf = Expertsys.eval rule factz (nbLine, ogStr) in
		nf
	in
	let rec loop rulez factz =
		match rulez with
		| [] -> factz
		| hd::tl -> loop tl (startEval hd factz)
	in
	let finalFacts = loop rules facts in
	checkQueries finalFacts queries

(* let main () = *)
(* 	(\* A | B => E *\) *)
(* 	(\* =AC *\) *)
(* 	print_endline "A | B => E"; *)
(* 	print_endline "with facts =AC"; *)
(* 	let factz = Expertsys.Facts ([(Expertsys.Value 'A'); (Expertsys.Value 'C')], []) in *)
(* 	let exp1 = Expertsys.Impl (Expertsys.Or (Expertsys.Value 'A', Expertsys.Value 'B'), Expertsys.Value 'E') in *)
(* 	print_endline "BEFORE :"; *)
(* 	Expertsys.printFacts factz; *)
(* 	let result = Expertsys.eval exp1 factz in *)
(* 	print_endline "RESULT :"; *)
(* 	Expertsys.printFacts result; *)
(* 	(\* =============================================================== *\) *)
(* 	(\* A + B = A + B *\) *)
(* 	(\* A + B = A | B *\) *)
(* 	let expt1 = Expertsys.And (Expertsys.Value 'A', Expertsys.Value 'B') in *)
(* 	let expt2 = Expertsys.And (Expertsys.Value 'A', Expertsys.Value 'B') in *)
(* 	let expt3 = Expertsys.Or (Expertsys.Value 'A', Expertsys.Value 'B') in *)
(* 	print_endline "test A + B = A + B"; *)
(* 	print_endline ("result : " ^ (string_of_bool (expt1 = expt2))); *)
(* 	print_endline "test A + B = A | B"; *)
(* 	print_endline ("result : " ^ (string_of_bool (expt1 = expt3))); *)
(* 	print_char '\n'; *)
(* 	(\* =============================================================== *\) *)
(* 	(\* A | B + C => E *\) *)
(* 	(\* (F | G) + H => E *\) *)
(* 	(\* With =AC, E should be FALSE ===> non TRUE *\) *)
(* 	print_endline "A | B + C => E"; *)
(* 	print_endline "(F | G) + H => E"; *)
(* 	print_endline "with facts =AC"; *)
(* 	let exp2 = Expertsys.Impl (Expertsys.Or (Expertsys.Value 'A', (Expertsys.And (Expertsys.Value 'B', Expertsys.Value 'C'))), Expertsys.Value 'E') in *)
(* 	let exp3 = Expertsys.Impl (Expertsys.And ((Expertsys.Or (Expertsys.Value 'F', Expertsys.Value 'G')), Expertsys.Value 'C'), Expertsys.Value 'E') in *)
(* 	print_endline "BEFORE :"; *)
(* 	Expertsys.printFacts factz; *)
(* 	let result2 = Expertsys.eval exp2 factz in *)
(* 	print_endline "RESULT :"; *)
(* 	Expertsys.printFacts result2; *)
(* 	let result3 = Expertsys.eval exp3 result2 in *)
(* 	print_endline "RESULT :"; *)
(* 	Expertsys.printFacts result3 *)

(* let () = main () *)




