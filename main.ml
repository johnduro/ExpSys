


let main () =
	if Array.length Sys.argv > 1
	then
		begin
			try
				begin
					let cin = open_in Sys.argv.(1) in
					let lst = LexerES.lexExpSys cin true in
					ignore (lst);
					print_endline "YOLO"
				end
			with e -> print_endline "Failed to open the stream"
		end
	else print_endline ("Usage " ^ Sys.argv.(0) ^ " <filename>")

let () = main ()
