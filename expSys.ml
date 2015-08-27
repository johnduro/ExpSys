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
		(* type expr *)
		(* type fact *)
		(* type expr = Value of t | Not of expr | And of (expr * expr) | Or of (expr * expr) | Xor of (expr * expr) | Impl of (expr * expr) | Ifoif of (expr * expr) *)
		type expr = Value of t | Not of expr | And of (expr * expr) | Or of (expr * expr) | Xor of (expr * expr)
		type rule = Impl of (expr * expr) (* | Ifoif of (expr * expr) *)
		(* type fact = (t list * t list) (\* (TRUE * FALSE) *\) *)
		type fact = Facts of (expr list * expr list) (* (TRUE * FALSE) *)
		(* type eval : expr -> t *)
		(* val eval : expr -> bool (\* ? *\) *)
		val addExprToFacts : fact -> expr -> (int * string) -> fact (* ????? *)
		(* val addToTrueFacts : fact -> expr -> fact *)
		(* val getBoolValue : t -> fact -> bool *)
		val getBoolValue : expr -> fact -> bool
		val evalBool : expr -> fact -> bool
		(* val eval : rule -> fact -> fact (\* ? *\) *)
		val eval : rule -> fact -> (int * string) -> fact (* ? *)
		val printFacts : fact -> unit
		val stringOfExpr : expr -> string
		(* val eval : expr -> fact -> fact (\* ? *\) *)
	end


(* let getBoolValue value = *)
(* 	(print_char value); *)
(* 	true *)
(* 	(\* blabla function *\) *)


module Expertsys : (ExpertsysSig with type t = char) =
	struct
		type t = char
		type expr = Value of t | Not of expr | And of (expr * expr) | Or of (expr * expr) | Xor of (expr * expr)
		type rule = Impl of (expr * expr)  (*| Ifoif of (expr * expr) *)
		(* type expr = Value of t | Not of expr | And of (expr * expr) | Or of (expr * expr) | Xor of (expr * expr) | Impl of (expr * expr) (\* | Ifoif of (expr * expr) *\) *)
		(* type fact = (t list * t list) (\* (TRUE * FALSE) *\) *)
		type fact = Facts of (expr list * expr list) (* (TRUE * FALSE) *)
		(* let rec makeTrue value = *)
		(* 	match value with *)
		(* 	| Value v -> print_endline ((Char.escaped v) ^ " is now true") *)
		(* 	| _ -> print_endline "yolo" *)
		(* let rec eval e = *)

		let rec stringOfExpr e =
			match e with
			| Not (exp)		-> ("( NOT " ^ (stringOfExpr exp) ^ " )")
			| And (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " AND " ^ (stringOfExpr e2) ^ " )")
			| Or (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " OR " ^ (stringOfExpr e2) ^ " )")
			| Xor (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " XOR " ^ (stringOfExpr e2) ^ " )")
			| Value v		-> Char.escaped v
			(* | _ -> "wut?" *)



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

		(* let addToTrueFacts (Facts (trueFacts, falseFacts)) exp = *)
		let addExprToFacts (Facts (trueFacts, falseFacts)) exp infos =
			let rec removeFromFacts oldFacts newFacts ret =
				match oldFacts with
				| []									-> ret
				| hd::tl when (Utils.notIn hd newFacts)	-> removeFromFacts tl newFacts (ret @ [hd])
				| hd::tl								-> removeFromFacts tl newFacts ret
			in
			let rec addNewFacts oldFacts newFacts =
				match newFacts with
				| []									-> oldFacts
				| hd::tl when (Utils.notIn hd oldFacts)	-> addNewFacts (oldFacts @ [hd]) tl
				| hd::tl								-> addNewFacts oldFacts tl
			in
			let mergeFacts (tf1, ff1) (tf2, ff2) =
				let rec looping l1 l2 =
					match l2 with
					| []								-> l1
					| hd::tl when (Utils.notIn hd l1)	-> looping (l1 @ [hd]) tl
					| hd::tl							-> looping l1 tl
				in
				(* ((looping tf1 tf2), (looping ff1 ff2)) *)
				(* print_endline "MERGE1"; *)
				let ntf = looping tf1 tf2 in
				(* print_endline "MERGE2"; *)
				let nff = looping ff1 ff2 in
				(* print_endline "RET MERGE"; *)
				(ntf, nff)
			in
			let rec exprToFact (tf, ff) ex bol =
				match ex with
				| Not (e)					-> exprToFact (tf, ff) e (not bol)
				| And (e1, e2)				-> mergeFacts (exprToFact (tf, ff) e1 bol) (exprToFact (tf, ff) e2 bol)
				| Value v when bol 			-> ((tf @ [(Value v)]), ff)
				| Value v when not bol 		-> (tf, (ff @ [(Value v)]))
				| _							-> raise (EvalError.noOp infos)
			in
			(* exprToFact trueFacts falseFacts exp true *) (* LA JE RECUPERE UNE LISTE DE FAITS *)
			(* print_endline "YOLO ?"; *)
			let (newTf, newFf) = exprToFact ([], []) exp true in (* ?????????? meilleure solution *)
			Facts ((addNewFacts (removeFromFacts trueFacts newFf []) newTf), (addNewFacts (removeFromFacts falseFacts newTf []) newFf))
			(* merge les nouvelles listes avec les anciennes en supprimant d'abords les ff dans les tf
			et inversement, ensuite verifier si il n'y a pas de conflits *)



			(* if (Utils.notIn exp trueFacts) then *)
			(* 	Facts (trueFacts @ [exp], (removeFact falseFacts exp)) *)
			(* else *)
			(* 	Facts (trueFacts, falseFacts) *)
			(* A FAIRE !! *)
			(* Facts (trueFacts @ [exp], falseFacts) *)

		let getBoolValue value (Facts (trueFacts, falseFacts)) =
			let checkValue (vl:expr) = (* (Value toCmp) = *)
				match vl with
				| Value v ->
					begin (* when v = value -> true *)
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
				(* | hd::tl when hd = value -> true *)
				| hd::tl						-> loop tl
			in
			loop trueFacts

		let rec evalBool e (facts:fact) =
			match e with
			| Not (exp)		-> not (evalBool exp facts)
			| And (e1, e2)	-> (evalBool e1 facts) && (evalBool e2 facts)
			| Or (e1, e2)	-> (evalBool e1 facts) || (evalBool e2 facts)
			| Xor (e1, e2)	-> (evalBool e1 facts) <> (evalBool e2 facts)
			| Value v		-> (getBoolValue (Value v) facts) (* v a le type t = char mais il attend un type value*)
			(* | _ -> false *)

		let rec eval e (facts:fact) infos =
			match e with
			(* | Impl (e1, e2) when (eval e1) = true -> (makeTrue e2); true (\* va pas marcher *\) *)
			(* | Impl (e1, e2) when (eval e1) = true -> (addToTrueFacts facts e2) *)
			(* | Impl (e1, e2) when (evalBool e1 facts) = true -> (addToTrueFacts facts e2 infos) (\* e2 fail *\) *)
			| Impl (e1, e2) when (evalBool e1 facts) = true -> (addExprToFacts facts e2 infos) (* e2 fail *)
			(* | Impl (e1, e2) -> false (\* va pas marcher *\) *)
			| Impl (e1, e2) -> facts (* va pas marcher *)
			(* | Ifoif (e1, e2) when *) (* BONUS !!! *)
			(* | Impl (e1, e2) -> (eval e1) <> (eval e2) *)
			(* | Ifoif (e1, e2) -> (eval e1) <> (eval e2) *)

			(* | Value v -> v *)
			(* | Value v -> (getBoolValue v) *)

		let printFacts (Facts (trueFacts, falseFacts)) =
			(* let rec stringOfExpr e = *)
			(* 	match e with *)
			(* 	| Not (exp) -> ("( NOT " ^ (stringOfExpr exp) ^ ")") *)
			(* 	| And (e1, e2) -> ("( " ^ (stringOfExpr e1) ^ " AND " ^ (stringOfExpr e2) ^ " )") *)
			(* 	| Or (e1, e2) -> ("( " ^ (stringOfExpr e1) ^ " OR " ^ (stringOfExpr e2) ^ " )") *)
			(* 	| Xor (e1, e2) -> ("( " ^ (stringOfExpr e1) ^ " XOR " ^ (stringOfExpr e2) ^ " )") *)
			(* 	| Value v -> Char.escaped v *)
			(* 	(\* | _ -> "wut?" *\) *)
			(* in *)
			let rec loop lst boolVal =
				match lst with
				| [] -> print_char '\n'
				(* | hd::tl -> print_endline ((Char.escaped hd) ^ " is " ^ boolVal); loop tl boolval *)
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
	(* A FAIRE  *)
	in
	let startEval (rule, nbLine, ogStr) factz =
		print_endline ("Evaluating rule line " ^ (string_of_int nbLine) ^ " : " ^ ogStr);
		let nf = Expertsys.eval rule factz (nbLine, ogStr) in
		(* print_endline "RET EVAL"; *)
		(* Expertsys.printFacts nf; *)
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




