

(* Parenthese | Facts | Operateur | NewLine *)
module Tag = struct type t = Ps | Nt | Fs | Op | Nl end

let checkList lst =
	let matchTag value =
		match value with
			| LexerES.ParentIn
			| LexerES.ParentOut -> Tag.Ps
			| LexerES.Not		-> Tag.Nt
			| LexerES.Or
			| LexerES.And
			| LexerES.Xor
			| LexerES.Impl
			| LexerES.Ifoif
			| LexerES.TrueFacts
			| LexerES.Requests	-> Tag.Op
			| LexerES.Fact _	-> Tag.Fs
			| LexerES.NewLine	-> Tag.Nl
	in
	let rec loop ls =
		match ls with
		(* | [] -> () *)
		| [] -> []
		| hd::tl when hd = LexerES.TrueFacts || hd = LexerES.Requests		-> checkFactList tl
		(* | hd::tl when hd = ParentIn 								-> ignore(checkParenthesis ls true) ; loop tl *)
		| hd::tl when hd = LexerES.ParentIn 								-> loop (checkParenthesis tl)
		(* | hd::tl when hd = ParentOut 								-> raise (invalid_arg "Parsing error : no matching parenthesis for )") *)
		| hd::tl when hd = LexerES.ParentOut 								-> ls
		(* | hd::tl when hd = LexerES.ParentOut 								-> tl *)
		(* | hd::tl when hd = ParentIn									-> raise (invalid_arg "Parsing error : ") *)
		(* | hd::tl when (matchTag hd) = Tag.Ps			-> loop (checkParenthesis ls true) *)
		| hd::tl when (matchTag hd) = Tag.Op								-> checkOperator tl
		| hd::tl when hd = LexerES.Not										-> checkNot tl
		| hd::tl when (matchTag hd) = Tag.Fs								-> checkFact tl
		| hd::tl 															-> loop tl
	(* A REVOIR  checkparenthesis devient un simple check des parenthese *)
	(* and checkParenthesis ls isIn = *)
	and checkParenthesis ls =
		match ls with
		(* | [] -> [] *)
		| []											-> raise (invalid_arg "Parsing error : no matching parenthesis for (")
		(* | hd::tl when hd = ParentIn && isIn				-> checkParenthesis tl false *)
		(* | hd::tl when hd = LexerES.ParentIn						-> checkParenthesis (checkParenthesis ls true) false *)
		| hd::tl when hd = LexerES.ParentIn				-> checkParenthesis (checkParenthesis ls)
		(* | hd::tl when hd = ParentOut && isIn			-> raise (invalid_arg "Parsing error : no matching parenthesis") *)
		| hd::tl when hd = LexerES.ParentOut			-> tl
		| hd::tl										-> checkParenthesis (loop tl)
		(* | hd::tl										-> checkParenthesis (loop ls) *)
		(* | hd::tl										-> checkParenthesis tl false *)
		(* | hd::tl										-> loop ls *)
	(* A REVOIR *)
	and checkOperator ls =
		match ls with
		| []									-> raise (invalid_arg "Parsing error : operator at the end of file")
		| hd::tl when (matchTag hd) = Tag.Op	-> raise (invalid_arg "Parsing error : operator following another")
		| hd::tl when (matchTag hd) = Tag.Nl	-> raise (invalid_arg "Parsing error : operator alone at the end of the line")
		| hd::tl when hd = LexerES.ParentOut	-> raise (invalid_arg "Parsing error : operator alone at the end of parenthesis")
		(* | hd::tl								-> loop tl *)
		| hd::tl								-> loop ls
	and checkNot ls =
		match ls with
		| []															-> raise (invalid_arg "Parsing error : Not (!) at the of file")
		| hd::tl when (matchTag hd) = Tag.Fs || hd = LexerES.ParentIn	-> loop tl
		| hd::tl														-> raise (invalid_arg "Parsing error : Not (!) is not followed by a fact or a parentese") (* orthographe *)
	and checkFact ls =
		match ls with
		| []									-> []
		| hd::tl when (matchTag hd) = Tag.Op	-> checkOperator tl
		| hd::tl when hd = LexerES.NewLine		-> loop tl
		| hd::tl when hd = LexerES.ParentIn		-> raise (invalid_arg "Parsing error : fact followed by a openning parenthese")
		| hd::tl when (matchTag hd) = Tag.Fs	-> raise (invalid_arg "Parsing error : fact followed by another fact")
		| hd::tl when hd = LexerES.Not			-> raise (invalid_arg "Parsing error : fact followed by Not (!) operator")
		| hd::tl								-> loop ls
	and checkFactList ls =
		match ls with
		| [] -> []
		(* | [] -> () *)
		| hd::tl when (matchTag hd) = Tag.Fs			-> checkFactList tl
		| hd::tl when hd = LexerES.NewLine				-> loop tl
		| hd::tl										-> raise (invalid_arg "Parsing error : wrong value after true facts or request")
	in
	let finalCheck ls =
		match ls with
		| [] -> ()
		| hd::tl -> LexerES.printMyList ls; raise (invalid_arg "Parsing error : no matching parenthesis for )")
	in
	finalCheck (loop lst)



let parseExpSys tokenList =
	checkList tokenList;
	print_endline "WUT ?!"
