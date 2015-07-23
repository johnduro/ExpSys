

(* Parenthese | Facts | Operateur | NewLine *)
module Tag = struct type t = Ps | Nt | Fs | Op | Nl end

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
	| LexerES.NewLine _	-> Tag.Nl

exception ParsingExcp of string


let getLine nl =
	match nl with
	| LexerES.NewLine (line)	-> (string_of_int line)
	| _							-> "wut!?"

let rec getPosition lst =
	match lst with
	| [] 									-> " the last line"
	| hd::tl when (matchTag hd) = Tag.Nl	-> (" line " ^ (getLine hd))
	| hd::tl								-> getPosition tl

module ParsingError =
	struct
		let exp err lst =
			let position = getPosition lst in
			ParsingExcp ("Parsing error : " ^ err ^ position)
	end

let checkList lst =
	let rec loop ls =
		match ls with
		| [] -> []
		| hd::tl when hd = LexerES.TrueFacts || hd = LexerES.Requests	-> checkFactList tl
		| hd::tl when hd = LexerES.ParentIn 							-> loop (checkParenthesis tl tl)
		| hd::tl when hd = LexerES.ParentOut 							-> ls
		| hd::tl when (matchTag hd) = Tag.Op							-> checkOperator tl
		| hd::tl when hd = LexerES.Not									-> checkNot tl
		| hd::tl when (matchTag hd) = Tag.Fs							-> checkFact tl
		| hd::tl 														-> loop tl
	and checkParenthesis ls endLst =
		match ls with
		| []									-> raise (ParsingError.exp "no matching parenthesis for ( in" endLst)
		| hd::tl when hd = LexerES.ParentIn		-> checkParenthesis (checkParenthesis ls endLst) endLst
		| hd::tl when hd = LexerES.ParentOut	-> tl
		| hd::tl								-> checkParenthesis (loop tl) endLst
	and checkOperator ls =
		match ls with
		| []									-> raise (ParsingError.exp "operator at the end of line in" [])
		| hd::tl when (matchTag hd) = Tag.Op	-> raise (ParsingError.exp "operator following another" tl)
		| hd::tl when (matchTag hd) = Tag.Nl	-> raise (ParsingError.exp "operator alone at the end of" tl)
		| hd::tl when hd = LexerES.ParentOut	-> raise (ParsingError.exp "operator alone at the end of parenthesis" tl)
		| hd::tl								-> loop ls
	and checkNot ls =
		match ls with
		| []															-> raise (ParsingError.exp "Not (!) with no expression following in" [])
		| hd::tl when (matchTag hd) = Tag.Fs || hd = LexerES.ParentIn	-> loop ls
		| hd::tl														-> raise (ParsingError.exp "Not (!) is not followed by a fact or an expression" tl)
	and checkFact ls =
		match ls with
		| []									-> []
		| hd::tl when (matchTag hd) = Tag.Op	-> checkOperator tl
		| hd::tl when (matchTag hd) = Tag.Nl	-> loop tl
		| hd::tl when hd = LexerES.ParentIn		-> raise (ParsingError.exp "fact followed by a openning parenthese" tl)
		| hd::tl when (matchTag hd) = Tag.Fs	-> raise (ParsingError.exp "fact followed by another fact" tl)
		| hd::tl when hd = LexerES.Not			-> raise (ParsingError.exp "fact followed by Not (!) operator" tl)
		| hd::tl								-> loop ls
	and checkFactList ls =
		match ls with
		| [] -> []
		| hd::tl when (matchTag hd) = Tag.Fs	-> checkFactList tl
		| hd::tl when (matchTag hd) = Tag.Nl	-> loop tl
		| hd::tl								-> raise (ParsingError.exp "wrong value after true facts or request" tl)
	in
	let finalCheck ls =
		match ls with
		| [] 		-> ()
		| hd::tl 	-> raise (ParsingError.exp "no matching parenthesis for )" tl)
	in
	finalCheck (loop lst)



let parseExpSys tokenList =
	checkList tokenList;
	print_endline "WUT ?!"
