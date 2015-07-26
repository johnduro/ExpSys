	(* ( and ) which are fairly obvious. Example : A + (B | C) => D *)
	(* ! which means NOT. Example : !B *)
	(* + which means AND. Example : A + B *)
	(* | which means OR. Example : A | B *)
	(* ˆ which means XOR. Example : A ˆ B *)
	(* => which means "implies". Example : A + B => C *)
	(* <=> which means "if and only if". Example : A + B <=> C *)


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
		val addToTrueFacts : fact -> expr -> fact
		(* val getBoolValue : t -> fact -> bool *)
		val getBoolValue : expr -> fact -> bool
		val evalBool : expr -> fact -> bool
		val eval : rule -> fact -> fact (* ? *)
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
			| Not (exp) -> ("( NOT " ^ (stringOfExpr exp) ^ ")")
			| And (e1, e2) -> ("( " ^ (stringOfExpr e1) ^ " AND " ^ (stringOfExpr e2) ^ " )")
			| Or (e1, e2) -> ("( " ^ (stringOfExpr e1) ^ " OR " ^ (stringOfExpr e2) ^ " )")
			| Xor (e1, e2) -> ("( " ^ (stringOfExpr e1) ^ " XOR " ^ (stringOfExpr e2) ^ " )")
			| Value v -> Char.escaped v
			(* | _ -> "wut?" *)

		let addToTrueFacts (Facts (trueFacts, falseFacts)) exp =
			Facts (trueFacts @ [exp], falseFacts)

		let getBoolValue value (Facts (trueFacts, falseFacts)) =
			let checkValue (vl:expr) = (* (Value toCmp) = *)
				match vl with
				| Value v ->
					begin (* when v = value -> true *)
						match value with
						| Value v2 when v = v2 -> true
						| _ -> false
					end
				| _ -> false
			in
			let rec loop tf =
				match tf with
				| [] -> false
				| hd::tl when (checkValue hd) -> true
				(* | hd::tl when hd = value -> true *)
				| hd::tl -> loop tl
			in
			loop trueFacts

		let rec evalBool e (facts:fact) =
			match e with
			| Not (exp) -> not (evalBool exp facts)
			| And (e1, e2) -> (evalBool e1 facts) && (evalBool e2 facts)
			| Or (e1, e2) -> (evalBool e1 facts) || (evalBool e2 facts)
			| Xor (e1, e2) -> (evalBool e1 facts) <> (evalBool e2 facts)
			| Value v -> (getBoolValue (Value v) facts) (* v a le type t = char mais il attend un type value*)
			(* | _ -> false *)

		let rec eval e (facts:fact) =
			match e with
			(* | Impl (e1, e2) when (eval e1) = true -> (makeTrue e2); true (\* va pas marcher *\) *)
			(* | Impl (e1, e2) when (eval e1) = true -> (addToTrueFacts facts e2) *)
			| Impl (e1, e2) when (evalBool e1 facts) = true -> (addToTrueFacts facts e2) (* e2 fail *)
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




