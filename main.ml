

(* arguments :
				-v verbose (unit)
				-f files (rest)
*)

let verboseBool = ref false
let debugBool = ref false

let fileList = ref []

(* STOCKER LES NOMS DE FICHIERS !! *)

(* let specList = [("-v", Arg.Set verboseBool, "Enable verbose mode"), ("-f", Arg.Rest (fun arg -> fileList := !fileList @ [arg]), "Get remaining arguments as filename to execute Expert System on them")] *)
let specList = [("-v", Arg.Set verboseBool, "Enable verbose mode"); ("-d", Arg.Set debugBool, "Enable debug mode")]

let collect arg = fileList := !fileList @ [arg]

let usage = "expertSystem options :"


let treatFile file =
	let tf fName =
		try
			begin
				(* RAJOUTER LE VERBOSE BOOL !!!!! *)
				let cin = open_in fName in
				let lst = LexerES.lexExpSys cin !debugBool !verboseBool in
				let prs = ParserES.parseExpSys lst !debugBool !verboseBool in
				ExpSys.executeExpSys prs !debugBool !verboseBool;
			end
		with
		| ParserES.ParsingExcp err		-> print_endline err (* PAS TESTE !!! *)
		| ExpSys.EvalExcp err			-> print_endline err (* PAS TESTE !!! *)
		| Invalid_argument errS			-> print_endline ("INVALID_ARG : " ^ errS)
		| Failure es					-> print_endline es
		| e								-> print_endline "Failed to open the stream"
	in
	print_endline ("\n**********************************\nOpening : " ^ file ^ "\n**********************************\n");
	tf file;
	print_endline "\n**********************************\n**********************************\n"



let rec loopFiles fl =
	match fl with
	| []		-> print_endline "********** END OF FILES **********"
	| hd::tl	-> treatFile hd; loopFiles tl

let main () =
	if Array.length Sys.argv > 1
	then
		begin
			ignore (Arg.parse specList collect usage);
			loopFiles !fileList
		end
	else print_endline ("Usage " ^ Sys.argv.(0) ^ " [-v (verbose)] <filename(s)> ...")

(* let main () = *)
(* 	if Array.length Sys.argv > 1 *)
(* 	then *)
(* 		begin *)
(* 			(\* try *\) *)
(* 				begin *)
(* 					let cin = open_in Sys.argv.(1) in *)
(* 					let lst = LexerES.lexExpSys cin true in *)
(* 					(\* ignore (lst); *\) *)
(* 					let prs = ParserES.parseExpSys lst in *)
(* 					ExpSys.executeExpSys prs; *)
(* 					print_endline "YOLO" *)
(* 				end *)
(* 			(\* with *\) *)
(* 			(\* | parserES.ParsingExcp err		-> print_endline err *\) (\* PAS TESTE !!! *\) *)
(* 			(\* | Invalid_argument errS			-> print_endline ("INVALID_ARG : " ^ errS) *\) *)
(* 			(\* | Failure es					-> print_endline es *\) *)
(* 			(\* | e						-> print_endline "Failed to open the stream" *\) *)
(* 		end *)
(* 	else print_endline ("Usage " ^ Sys.argv.(0) ^ " <filename>") *)

let () = main ()
