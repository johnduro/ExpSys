
	(* LIGNE DE COMPILATION : ocamllex lexerES.mll *)

	(* symboles par ordre d'importance du plus important au moins important : *)
	(* ( ) *)
	(* !	NOT *)
	(* +	AND *)
	(* |	OR *)
	(* ^	XOR *)
	(* =>	IMPLIES *)
	(* <=>	IF AND ONLY IF *)
	(* =	FACTS *)
	(* ?	REQUEST *)

{
	type tokenz =
		| ParentIn
		| ParentOut
		| Not
		| And
		| Or
		| Xor
		| Impl
		| Ifoif
		| TrueFacts
		| Requests
		| Fact of char
		| NewLine
}

(* rajouter les arguments apres token sans le lexbuf *)
rule tokenLex lst = parse
	| [' ' '\t' ]+			{ tokenLex lst lexbuf } (* skip space *)
	| "#"					{ comment lst lexbuf } (* activate comment rule *)
	| '('					{ tokenLex (lst @ [ParentIn]) lexbuf }
	| ')'					{ tokenLex (lst @ [ParentOut]) lexbuf }
	| '!'					{ tokenLex (lst @ [Not]) lexbuf }
	| '+'					{ tokenLex (lst @ [And]) lexbuf }
	| '|'					{ tokenLex (lst @ [Or]) lexbuf }
	| '^'					{ tokenLex (lst @ [Xor]) lexbuf }
	| "=>"					{ tokenLex (lst @ [Impl]) lexbuf }
	| "<=>"					{ tokenLex (lst @ [Ifoif]) lexbuf }
	| '='					{ tokenLex (lst @ [TrueFacts]) lexbuf }
	| '?'					{ tokenLex (lst @ [Requests]) lexbuf }
	| [ 'A' - 'Z' ] as fact	{ tokenLex (lst @ [(Fact fact)]) lexbuf }
	| '\n'					{ tokenLex (lst @ [NewLine]) lexbuf }
	| _ as err				{ raise (invalid_arg ("invalid char : " ^ (Char.escaped err))) }
	| eof					{ lst }
and comment lst = parse
	| '\n'					{ tokenLex (lst @ [NewLine]) lexbuf } (* go to the token rule *)
	| _						{ comment lst lexbuf } (* skip comment *)
	| eof					{ lst }

{
let rec printMyList ls =
	match ls with
	| [] -> print_char '\n'
	| hd::tl ->
		begin
			match hd with
			| ParentIn 	-> print_string " -( " ; printMyList tl
			| ParentOut 	-> print_string " )- " ; printMyList tl
			| Not		-> print_string " NOT" ; printMyList tl
			| Or		-> print_string " OR " ; printMyList tl
			| And		-> print_string " AND " ; printMyList tl
			| Xor		-> print_string " XOR " ; printMyList tl
			| Impl		-> print_string " => " ; printMyList tl
			| Ifoif		-> print_string " <=> " ; printMyList tl
			| TrueFacts -> print_string " TRUE FACTS : " ; printMyList tl
			| Requests	-> print_string " REQUEST : " ; printMyList tl
			| Fact (fx) -> print_string (" -" ^ (Char.escaped fx)^ "- ") ; printMyList tl
			| NewLine	-> print_string "  -NL-\n" ; printMyList tl
		end

let lexExpSys chan debug =
	let lexbuf = Lexing.from_channel chan in
	let returnList = tokenLex [] lexbuf in
	if debug = true then printMyList returnList;
	returnList

(* let main () = *)
(* 	let cin = if Array.length Sys.argv > 1 *)
(* 		then open_in Sys.argv.(1) *)
(* 		else stdin *)
(* 	in *)
(* 	let lexbuf = Lexing.from_channel cin in *)
(* 	let returnList = tokenLex [] lexbuf in *)
(* 	printMyList returnList; *)
(* 	print_endline "yolo" *)

(* let () = main () *)
}
