
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
		| NewLine of int
}

(* rajouter les arguments apres token sans le lexbuf *)
(* rule tokenLex lst = parse *)
rule tokenLex line lst = parse
	| [' ' '\t' ]+			{ tokenLex line lst lexbuf } (* skip space *)
	| "#"					{ comment line lst lexbuf } (* activate comment rule *)
	| '('					{ tokenLex line (lst @ [ParentIn]) lexbuf }
	| ')'					{ tokenLex line (lst @ [ParentOut]) lexbuf }
	| '!'					{ tokenLex line (lst @ [Not]) lexbuf }
	| '+'					{ tokenLex line (lst @ [And]) lexbuf }
	| '|'					{ tokenLex line (lst @ [Or]) lexbuf }
	| '^'					{ tokenLex line (lst @ [Xor]) lexbuf }
	| "=>"					{ tokenLex line (lst @ [Impl]) lexbuf }
	| "<=>"					{ tokenLex line (lst @ [Ifoif]) lexbuf }
	| '='					{ tokenLex line (lst @ [TrueFacts]) lexbuf }
	| '?'					{ tokenLex line (lst @ [Requests]) lexbuf }
	| [ 'A' - 'Z' ] as fact	{ tokenLex line (lst @ [(Fact fact)]) lexbuf }
	| '\n'					{ tokenLex (line + 1) (lst @ [(NewLine line)]) lexbuf }
	| _ as err				{ raise (invalid_arg ("invalid char : " ^ (Char.escaped err))) }
	| eof					{ lst }
and comment line lst = parse
	| '\n'					{ tokenLex (line + 1) (lst @ [(NewLine line)]) lexbuf } (* go to the token rule *)
	| _						{ comment line lst lexbuf } (* skip comment *)
	| eof					{ lst }

{

module Tag = struct type t = Nl | Ot end

let matchTag itm =
	match itm with
	| NewLine _		-> Tag.Nl
	| _				-> Tag.Ot

let rec printMyList ls =
	match ls with
	| [] -> print_char '\n'
	| hd::tl ->
		begin
			match hd with
			| ParentIn 		-> print_string " -( " ; printMyList tl
			| ParentOut 	-> print_string " )- " ; printMyList tl
			| Not			-> print_string " NOT" ; printMyList tl
			| Or			-> print_string " OR " ; printMyList tl
			| And			-> print_string " AND " ; printMyList tl
			| Xor			-> print_string " XOR " ; printMyList tl
			| Impl			-> print_string " => " ; printMyList tl
			| Ifoif			-> print_string " <=> " ; printMyList tl
			| TrueFacts 	-> print_string " TRUE FACTS : " ; printMyList tl
			| Requests		-> print_string " REQUEST : " ; printMyList tl
			| Fact (fx) 	-> print_string (" -" ^ (Char.escaped fx) ^ "- ") ; printMyList tl
			| NewLine (nl)	-> print_string ("  -NL (" ^ (string_of_int nl) ^ ")-\n") ; printMyList tl
		end

let cleanList lst =
	let rec loop ls rl =
		match ls with
		| [] -> rl
		| hd::tl when (matchTag hd) = Tag.Nl	-> skipNL tl (rl @ [hd])
		| hd::tl 								-> loop tl (rl @ [hd])
	and skipNL ls rl =
		match ls with
		| [] -> rl
		| hd::tl when  (matchTag hd) = Tag.Nl 	-> skipNL tl rl
		| hd::tl 					-> loop tl (rl @ [hd])
	in
	skipNL lst []



let lexExpSys chan debug =
	let lexbuf = Lexing.from_channel chan in
	let returnList = cleanList (tokenLex 1 [] lexbuf) in
	if debug = true then printMyList returnList;
	returnList

}
