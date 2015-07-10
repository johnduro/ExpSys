
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
	(* type tokenz = *)
	(* 	| Parentesis *)
	(* 	| Not *)
	(* 	| And *)
	(* 	| Or *)
	(* 	| Xor *)
	(* 	| Impl *)
	(* 	| Ifoif *)
}

(* rajouter les arguments apres token sans le lexbuf *)
rule token lst = parse
	| [' ' '\t' ]+			{ token lst lexbuf } (* skip space *)
	| "#"					{ comment lst lexbuf } (* activate comment rule *)
	| '('					{ token (lst @ ["("]) lexbuf }
	| ')'					{ token (lst @ [")"]) lexbuf }
	| '!'					{ token (lst @ ["!"]) lexbuf }
	| '+'					{ token (lst @ ["+"]) lexbuf }
	| '|'					{ token (lst @ ["|"]) lexbuf }
	| '^'					{ token (lst @ ["^"]) lexbuf }
	| "=>"					{ token (lst @ ["=>"]) lexbuf }
	| "<=>"					{ token (lst @ ["<=>"]) lexbuf }
	| '='					{ token (lst @ ["="]) lexbuf }
	| '?'					{ token (lst @ ["?"]) lexbuf }
	| [ 'A' - 'Z' ] as fact	{ token (lst @ [(Char.escaped fact)]) lexbuf }
	| '\n'					{ token (lst @ ["NL"]) lexbuf }
	| _ as err				{ raise (invalid_arg ("invalid char : " ^ (Char.escaped err))) }
	| eof					{ lst } (* ICI METTRE LE RETURN DE LA FUNCTION *)
and comment lst = parse
	| '\n'					{ token lst lexbuf } (* go to the token rule *)
	| _						{ comment lst lexbuf } (* skip comment *)
	| eof					{ lst } (* ICI METTRE LE RETURN DE LA FUNCTION *)

{
let rec printMyList ls =
	match ls with
	| [] -> print_char '\n'
	| hd::tl -> print_endline hd ; printMyList tl

let main () =
	let cin = if Array.length Sys.argv > 1
		then open_in Sys.argv.(1)
		else stdin
	in
	let lexbuf = Lexing.from_channel cin in
	let returnList = token [] lexbuf in
	printMyList returnList;
	print_endline "yolo"

let () = main ()
}
