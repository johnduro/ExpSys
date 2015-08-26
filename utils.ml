

let rec notIn el lst =
	match lst with
	| []					-> true
	| hd::tl when hd = el	-> false
	| hd::tl				-> notIn el tl

(* let rec stringOfExpr e = *)
(* 	match e with *)
(* 	| Not (exp)		-> ("( NOT " ^ (stringOfExpr exp) ^ " )") *)
(* 	| And (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " AND " ^ (stringOfExpr e2) ^ " )") *)
(* 	| Or (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " OR " ^ (stringOfExpr e2) ^ " )") *)
(* 	| Xor (e1, e2)	-> ("( " ^ (stringOfExpr e1) ^ " XOR " ^ (stringOfExpr e2) ^ " )") *)
(* 	| Value v		-> Char.escaped v *)
(* 	(\* | _ -> "wut?" *\) *)


