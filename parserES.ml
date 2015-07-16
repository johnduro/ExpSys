

(* Parenthese | Facts | Operateur | NewLine *)
module Tag = struct type t = Ps | Nt | Fs | Op | Nl end

let checkList lst =
	let matchTag value =
		match value with
			| ParentIn
			| ParentOut -> Tag.Ps
			| Not		-> Tag.Nt
			| Or
			| And
			| Xor
			| Impl
			| Ifoif
			| TrueFacts
			| Requests	-> Tag.Op
			| Fact _	-> Tag.Fs
			| NewLine	-> Tag.Nl
	in
	let rec loop ls =
		match ls with
		| [] -> ()
		| hd::tl when hd = TrueFacts || hd = Requests	-> checkFactList tl
		| hd::tl when (matchTag hd) = Tag.Ps			-> loop (checkParenthesis ls true)
		| hd::tl 										-> loop tl
	(* A REVOIR *)
	and checkParenthesis ls isIn =
		match ls with
		| [] -> []
		| hd::tl when hd = ParentIn && isIn				-> checkParenthesis tl false
		| hd::tl when hd = ParentIn						-> checkParenthesis (checkParenthesis ls true) false
		| hd::tl when hd = ParentOut && isIn			-> raise (invalid_arg "Parsing error : no matching parenthesis")
		| hd::tl when hd = ParentOut					-> tl
		| hd::tl										-> checkParenthesis tl false
	(* A REVOIR *)
	and checkOperator ls =
		| [] -> []
		| hd::tl -> loop tl
	and checkFactList ls =
		match ls with
		| [] -> ()
		| hd::tl when (matchTag hd) = Tag.Fs			-> checkFactList tl
		| hd::tl when hd = NewLine						-> loop tl
		| hd::tl										-> raise (invalid_arg "Parsing error : wrong value after true facts or request")
	in
	loop lst



let parseExpSys tokenList =
	checkList tokenList;
	print_endline "WUT ?!"
