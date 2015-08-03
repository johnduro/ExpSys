


let main () =
	if Array.length Sys.argv > 1
	then
		begin
			(* try *)
				begin
					let cin = open_in Sys.argv.(1) in
					let lst = LexerES.lexExpSys cin true in
					(* ignore (lst); *)
					let prs = ParserES.parseExpSys lst in
					ExpSys.executeExpSys prs;
					print_endline "YOLO"
				end
			(* with *)
			(* | parserES.ParsingExcp err		-> print_endline err *) (* PAS TESTE !!! *)
			(* | Invalid_argument errS			-> print_endline ("INVALID_ARG : " ^ errS) *)
			(* | Failure es					-> print_endline es *)
			(* | e						-> print_endline "Failed to open the stream" *)
		end
	else print_endline ("Usage " ^ Sys.argv.(0) ^ " <filename>")

let () = main ()
